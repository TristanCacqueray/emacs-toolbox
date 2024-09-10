;;; org-report.el -- utility macro to create sprint report -*- lexical-binding: t; -*-

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
;; Keywords: org sprint report
;; URL: https://github.com/TristanCacqueray/emacs-toolbox
;; License: GNU General Public License >= 3
;; Package-Requires: ((emacs "29") (org-ql "0"))

;;; Commentary:

;;; Code:
(require 's)

(defcustom org-report-files nil
  "The files used for org-report.")

(defun s-unlines (xs)
  "The inverse of 's-lines'."
  (s-join "\n" xs))

;;; remove elem when f return true
(defun or/drop (f xs)
  "The inverse of 'seq-filter'."
  (seq-filter (lambda (s) (not (funcall f s))) xs))

(defun or/remove-links (s)
  (s-trim (s-replace-regexp "\\[\\[.*\\]\\]" "" s)))

(defun or/get-cat (properties)
  (or (org-entry-get (plist-get properties :org-marker) "ARCHIVE_CATEGORY")
      (org-entry-get (plist-get properties :org-marker) "CATEGORY")))

(defun or/get-refs (item)
  (org-entry-get (plist-get (cadr item) :org-marker) "EXTERNAL_REF" t))

(defun or/daily-format-item (node)
  "Format 'org-ql-select' output. ITEM is a prop list."
  (let* ((item (org-element-properties-resolve node 'force-undefer))
         (properties (cadr item))
         (title (plist-get properties :raw-value))
         (status (plist-get properties :todo-keyword))
         (status-str (if (string= status "DONE") "" (concat status ": ")))
         (category (or/get-cat properties))
         (ref (or/get-refs item))
         (ref-str (if ref (format "%s " ref) "")))
    (format "- %s - %s%s%s" (s-pad-left 9 " " category) ref-str status-str (or/remove-links title))))

(defun or/daily-format (items)
  "Format all ITEMS."
  (let ((today (format-time-string "*** %Y-%m-%d %A"))
        (report (mapcar 'or/daily-format-item items)))
    (format "%s\n%s:\n%s" today "tdecacqu" (s-unlines report))))

(defun or/mk-daily-query ()
  "The daily query."
  ;; TODO: make that one day during the week
  '(and (or (todo "DONE") (todo "NEXT")) (ts :from -3 :to today)))

(defun or/mk-review-query ()
  "The review query."
  '(and (or (todo "DONE") (todo "NEXT")) (ts :from -21 :to today)))

(defun or/compare-entry (b a)
  "Order entry A and B so that they appears from newest to oldest.
This is like org-ql--date< but considering closed date too."
  (cl-macrolet ((ts (item)
                  `(or (org-element-property :closed ,item)
                       (org-element-property :deadline ,item)
                       (org-element-property :scheduled ,item))))
    (org-ql--org-timestamp-element< (ts a) (ts b))))

(defun or/compare-cat-entry (a b)
  (cl-macrolet ((cat (item)
                  `(or
                    (org-element-property :category ,item))))
    (string< (cat a) (cat b))))

(defun or/monthly-format-item (acc node)
  "Format an entry for the review.
ACC is tuple of current content and category string.
ITEM is an org entry."
  (let* ((item (org-element-properties-resolve node 'force-undefer))
         (properties (cadr item))
         (prev-cat (cadr acc))
         (content (car acc))
         (category (or/get-cat properties))
         (cat-sep (if (string= category prev-cat) ""
                    (format "%s\n# %s" (if prev-cat "\n" "") category)))
         (title (plist-get properties :raw-value)))
    (list (format "%s%s\n- %s" content cat-sep (or/remove-links title)) category)))

(defun or/monthly-format (items)
  "Format all ITEMS."
  (let ((report (car (seq-reduce 'or/monthly-format-item items '("" nil)))))
    ;; todo: compute the date of 3 weeks ago
    (format "**** Sprint review (from )\n%s" report)))

;;;###autoload
(defun org-report-todos ()
  "Improved org-todo-list."
  (interactive)
  (org-ql-search org-report-files '(or (todo "WAITING") (todo "TODO"))
    :super-groups '((:auto-category))))

;; TODO: append the report to the journal (e.g. running org-capture "j")
(defun org-report-daily ()
  "Produce a report for team daily."
  (interactive)
  (let* ((entries (org-ql-select org-report-files
                    (or/mk-daily-query)
                    :action 'element-with-markers
                    :sort 'or/compare-entry
                    ))
         (report (or/daily-format entries))
         (*buffer* (get-buffer-create "*org-report-daily*")))
    (with-current-buffer *buffer*
      (erase-buffer)
      (insert report)
      (set-text-properties (point-min) (point-max) nil)
      ;; move the cursor at begining of entries
      (goto-char (point-min))
      (forward-line 2)
      (cl-dolist (window (get-buffer-window-list nil nil t))
        (set-window-point window (point)))
      )
    (switch-to-buffer-other-window *buffer*)
    )
  )

(defun org-report-daily-show ()
  "Show daily report."
  (interactive)
  (org-ql-search org-report-files (or/mk-daily-query)
    :super-groups '((:auto-ts t))))

(defun org-report-review ()
  "Produce a report for team review."
  (interactive)
  (let* ((entries (org-ql-select org-report-files
                    (or/mk-review-query)
                    :action 'element-with-markers
                    :sort 'or/compare-cat-entry
                    ))
         (report (or/monthly-format entries))
         (*buffer* (get-buffer-create "*org-report-review*")))
    (with-current-buffer *buffer*
      (erase-buffer)
      (insert report)
      (set-text-properties (point-min) (point-max) nil)
      ;; move the cursor at begining of entries
      (goto-char (point-min))
      (forward-line 2)
      (cl-dolist (window (get-buffer-window-list nil nil t))
        (set-window-point window (point)))
      )
    (switch-to-buffer-other-window *buffer*)
    )
  )

(defun org-report-review-show ()
  "Show review report."
  (interactive)
  (org-ql-search org-report-files (or/mk-review-query)
    :super-groups '((:auto-category))))

(provide 'org-report)
;;; org-report.el ends here
