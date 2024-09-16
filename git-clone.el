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
;; Package-Requires: ((emacs "29") (f "*"))

;;; Commentary:

;; M-x git-clone
;;
;;; Code:

(require 'f)

(setq git-clone-root-dir
      (if (f-writable-p "/srv") "/srv/"
        (concat (getenv "HOME") "/src/")))

(defun giturl-to-dir (url)
  "Convert a git URL to a local path."
  (let ((inf (url-generic-parse-url url)))
    (when (null (url-host inf))
      (error "Invalid url: %s" url))
    (concat
     git-clone-root-dir
     (url-host inf)
     (seq-reduce
      (lambda (string replacement-pair)
        (string-replace
         (car replacement-pair)
         (cdr replacement-pair)
         string))
      '((" " . "")
        ("/r/" . "/")
        ("/git/" . "/")
        ("/static/repos/git/" . "/")
        ("/gerrit/" . "/"))
      (replace-regexp-in-string
       "\\.git$" "" (url-filename inf))))))


;; TODO: support shallow clone with `--depth 1 --shallow-submodules'
(defun git-clone-url (url dir)
  "Call git clone URL DIR."
  (mkdir dir t)
  (let ((*buffer* (get-buffer-create "*git-clone-log*")))
    (switch-to-buffer-other-window *buffer*)
    (with-current-buffer *buffer*
      (comint-mode))
    (make-process
     :name "git-clone"
     :buffer "*git-clone-log*"
     :command (list "git" "clone" "--recurse-submodules" url dir)
     :filter 'comint-output-filter
     :sentinel (lambda (_process event)
                 (if (string= "finished\n" event)
                     (progn
                       (when (fboundp 'project--remember-dir)
                         (project--remember-dir dir))
                       (dired dir))
                   (message "git clone died %s" event))))))

(defun f-git? (path)
  "Check if PATH is a git clone."
  (f-directory? (concat path "/.git")))

;;;###autoload
(defun git-clone (url)
  "Create directory, clone and open URL."
  (interactive "Murl: ")
  (let ((d (giturl-to-dir url)))
    (if (f-git? d)
        (dired d)
      (git-clone-url url d))))

(provide 'git-clone)
;;; git-clone.el ends here
