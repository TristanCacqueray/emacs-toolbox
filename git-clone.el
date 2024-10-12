;;; git-clone.el -- helper to clone repository -*- lexical-binding: t; -*-

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
;; Keywords: git gerrit gitlab github
;; URL: https://github.com/TristanCacqueray/emacs-toolbox
;; License: GNU General Public License >= 3
;; Package-Requires: ((emacs "29"))

;;; Commentary:

;; M-x git-clone
;;
;;; Code:

(require 'url-parse)
(require 'comint)

(defcustom git-clone-root-dir
  (if (file-writable-p "/srv") "/srv/"
    (concat (getenv "HOME") "/src/"))
  "Base path for the `git-clone' command."
  :type 'string :group 'magit)

(defun giturl-to-dir (url)
  "Convert a git URL to a local path."
  (let ((inf (url-generic-parse-url url)))
    (when (null (url-host inf))
      (error "Invalid url: %s" url))
    (concat
     git-clone-root-dir
     ;; the remote hostname
     (url-host inf)
     ;; remove useless known url path
     (seq-reduce
      (lambda (string replacement-pair)
        (string-replace
         (car replacement-pair)
         (cdr replacement-pair)
         string))
      '((" " . "")
        ("/r/" . "/")
        ("/code/" . "/")
        ("/pub/scm/git/" . "/")
        ("/static/repos/git/" . "/")
        ("/git/" . "/")
        ("/gerrit/" . "/"))
      ;; remove trailing .git
      (replace-regexp-in-string
       "\\.git$" "" (url-filename inf)))
     "/")))

(defun git-clone-url (url dir)
  "Call git clone URL DIR."
  ;; ensure destination exists
  (mkdir dir t)
  (let ((*buffer* (get-buffer-create "*git-clone-log*")))
    ;; show a new *git-clone-log* buffer
    (switch-to-buffer-other-window *buffer*)
    (with-current-buffer *buffer*
      ;; activate comint-mode to handle terminal sequence
      (comint-mode))
    (make-process
     :name "git-clone"
     :buffer "*git-clone-log*"
     :command (list "git" "clone" "--depth" "1" "--shallow-submodules" "--recurse-submodules" url dir)
     ;; interpret the terminal sequence like \r
     :filter 'comint-output-filter
     :coding 'utf-8
     ;; sentinel is called when the process terminates
     :sentinel (lambda (_process event)
                 (if (string= "finished\n" event)
                     ;; the clone succeeded
                     (if (fboundp 'project--remember-dir)
                         (progn
                           ;; automitcally register the project to project.el
                           (project--remember-dir dir)
                           ;; show the file listing
                           (dired dir))
                         (project-switch-project dir))
                   ;; todo: remove empty directory created
                   (message "git clone died %s" event))))))

(defun f-git? (path)
  "Check if PATH is a git clone."
  (file-directory-p (concat path "/.git")))

;;;###autoload
(defun git-clone (url)
  "Create directory, clone and open URL."
  (interactive "Murl: ")
  (let ((d (giturl-to-dir url)))
    (if (f-git? d) (dired d)
      (git-clone-url url d))))

(provide 'git-clone)
;;; git-clone.el ends here
