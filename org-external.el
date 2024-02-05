;;; org-external.el -- utility macro to sync with external systems -*- lexical-binding: t; -*-

;; Copyright (C) 2024   Tristan de Cacqueray

;; This file is NOT part of Emacs.

;; This  program is  free  software; you  can  redistribute it  and/or
;; modify it  under the  terms of  the GNU  General Public  License as
;; published by the Free Software  Foundation; either version 2 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT  ANY  WARRANTY;  without   even  the  implied  warranty  of
;; MERCHANTABILITY or FITNESS  FOR A PARTICULAR PURPOSE.   See the GNU
;; General Public License for more details.

;; You should have  received a copy of the GNU  General Public License
;; along  with  this program;  if  not,  write  to the  Free  Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
;; USA

;; Version: 0.1
;; Author: Tristan de Cacqueray
;; Keywords: org gitlab github jira
;; URL: https://github.com/TristanCacqueray/emacs-toolbox
;; License: GNU General Public License >= 3
;; Package-Requires: ((emacs "29") (s "1.2.0") (plz "0.7.2"))

;;; Commentary:

;; The goal of this package is to provide helper functions to synchronize external
;; system such as GitLab with org tasks.
;;
;; org-external uses two properties to track references:
;;
;; CATEGORY_EXTERNAL: The type and location of the external system, to be set on the parent heading.
;; Here are some example values:
;;  - gitlab.com:REPO
;;  - github.com:OWNER/REPO
;;  - jira:issues.redhat.com:PROJ_NAME
;;
;; EXTERNAL_REF: Once created, the external task is registered to this org task heading property.
;; This is used to add comment and close the task. Here are some example values:
;;  - REPO#42
;;  - PROJ_NAME-42
;;
;; Here are the list of available commands:
;;
;; M-x org-external-push  - Push the heading at point to the external system
;;
;; Checkout the test for the expected document structure.
;;
;;; Code:

(require 'org)
(require 's)
(require 'plz)

(defun oe/parse-external-cat (category-external)
  "Parse the CATEGORY-EXTERNAL property and return '(type info)."
  (pcase (s-split ":" category-external)
    (`("jira" ,host ,proj)
     (let ((token (auth-info-password (car (auth-source-search :host host :max 1)))))
       `(jira (:host ,host :token ,token :proj ,proj))))
    (`("gitlab.com" ,proj) `(gitlab (:host "gitlab.com" :proj ,proj)))
    (_ (error "Unknown CATEGORY_EXTERNAL property: %s" category-external))))

(defun oe/has-external (item)
  "Return '(type info) if ITEM or its parent has the CATEGORY_EXTERNAL property."
  (if-let ((ext (org-element-property :CATEGORY_EXTERNAL item)))
      (oe/parse-external-cat ext)
    (if-let (parent (org-element-property :parent item))
        (oe/has-external parent))))

(defun oe/get-description (body)
  "Parse the org BODY content."
  (let*  ((props-end (when (s-starts-with? ":PROPERTIES:" body t) (s-index-of ":END:" body t)))
          (chop-props (if props-end (+ props-end 5) 0))
          (body-without-props (s-chop-left chop-props body)))
    (s-trim
     (s-replace-regexp "^SCHEDULE.*$" ""
                       (s-replace-regexp "^CLOSED.*$" ""
                                         (car (s-split "^\\*\\*" body-without-props)))))))

(defun oe/get-body-contents (item)
  "Return the contents of the ITEM until the next heading."
  (let* ((beg (org-element-property :contents-begin item))
         (end (org-element-property :contents-end item)))
    (oe/get-description (buffer-substring-no-properties beg end))))

(defun oe/parse-heading (item parent)
  "Create the action body for heading ITEM and optional PARENT ref."
  ;; (message "story: %s" item)
  (let* ((title (org-element-property :title item))
         (ref (org-element-property :EXTERNAL_REF item))
         (desc (oe/get-body-contents item))
         (body `(:summary ,title :description ,desc :parent ,parent)))
    (if ref `(update ,ref ,body) `(create ,body))))

(defun oe/parse-task (item)
  "Create the action body for a sub task at ITEM."
  (if-let ((parent (org-element-property :EXTERNAL_REF (org-element-property :parent item))))
      (oe/parse-heading item parent)))

(defun oe/parse-comment (item)
  "Create the action body for a comment at ITEM."
  (if-let ((parent (org-element-property :EXTERNAL_REF (org-element-property :parent item))))
      (let* ((title (org-element-property :title item))
             (ref (org-element-property :EXTERNAL_COMMENT item))
             (desc (oe/get-body-contents item))
             (body (if (s-blank? desc) title (format "%s.\n%s" title desc))))
        (if ref (error "Comment already submitted") `(create-comment ,parent ,body)))
    (error "Missing EXTERNAL_REF on parent to post comment")))

(defun oe/push (item)
  "Return '(system action) or the ITEM."
  (if-let ((system (oe/has-external item))
           (level (org-element-property :level item)))
      (cons system
            (cond
             ((= level 4) (oe/parse-comment item))
             ((= level 3) (oe/parse-task item))
             ((= level 2) (oe/parse-heading item nil))
             (t (error "Unknown heading"))))
    (error "Missing CATEGORY_EXTERNAL or point is not on heading")))

(defun oe/run-push (args)
  "Perform the action from ARGS."
  (let* ((system (car args))
         (action (cdr args))
         (result (pcase (car system)
                   ('jira (oe/jira-action (cadr system) action))
                   (_ (error "Unknown system")))))
    (when result (org-set-property "EXTERNAL_REF" result))))

(defun oe/jira-parse-id (json-str)
  "Return the key of JSON-STR jira response."
  (if-let ((json (json-parse-string json-str)))
      (gethash "key" json nil)))

(defun oe/jira-create-payload (project body)
  "Create the JSON-STR jira create payload from action PROJECT and BODY."
  (let* ((parent (plist-get body :parent))
         (type (if parent "Sub-task" "Story"))
         (base `(project (key ,project)
                         summary ,(plist-get body :summary)
                         description ,(plist-get body :description)
                         issuetype (name ,type)))
         (fields (if parent (plist-put base 'parent `(key ,parent)) base)))
    (json-serialize (list 'fields fields))))

(defun oe/jira-update-payload (body)
  "Create the JSON-STR jira create payload from action BODY."
  (json-serialize `(fields (summary ,(plist-get body :summary)
                                    description ,(plist-get body :description)))))

(defun oe/jira-action (system-info action)
  "Perform the ACTION on jira SYSTEM-INFO."
  (let* ((host (plist-get system-info :host))
         (auth (format "Bearer %s" (plist-get system-info :token)))
         (headers `(("Content-Type" . "application/json") ("Authorization" . ,auth))))
    (pcase (car action)
      ('create
       (let* ((url (format "https://%s/rest/api/2/issue/" host))
              (body (oe/jira-create-payload (plist-get system-info :proj) (cadr action)))
              (resp (plz 'post url :headers headers :body body :as 'string)))
         (oe/jira-parse-id resp)))
      ('update
       (let* ((url (format "https://%s/rest/api/2/issue/%s" host (cadr action)))
              (body (oe/jira-update-payload (caddr action))))
         (plz 'put url :headers headers :body body)))
      ('create-comment
       (let* ((url (format "https://%s/rest/api/2/issue/%s/comment" host (cadr action)))
              (body (caddr action)))
         (plz 'post url :headers headers :body (json-encode `(("body" . ,body))))
         (org-set-property "EXTERNAL_COMMENT" "true")))
      (_ (error "Unknown action %s" action)))))

;;;###autoload
(defun org-external-push ()
  "Push the heading at point to an external system."
  (interactive)
  (oe/run-push (oe/push (org-element-at-point))))

(provide 'org-external)
;;; org-external.el ends here
