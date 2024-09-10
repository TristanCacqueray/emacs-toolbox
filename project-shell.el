;;; project-shell.el -- helper to create a shell history per project -*- lexical-binding: t; -*-

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
;; Keywords: project shell
;; URL: https://github.com/TristanCacqueray/emacs-toolbox
;; License: GNU General Public License >= 3
;; Package-Requires: ((emacs "30") (f "*"))

;;; Commentary:

;; Use this package to handle a per project history for shell:
;;
;; Call the function 'project-shell-history' to start a new shell with the HISTFILE
;; and comint input ring loaded from ~/.emacs.d/shell-history/{project-name}
;;
;; Setup f1/f2 keybinding like this:
;;
;; (use-package project-shell
;;   :load-path "~/src/github.com/TristanCacqueray/emacs-toolbox"
;;   :config
;;     (global-set-key (kbd "<f1>") (lambda () (interactive) (project-shell-history)))
;;     (global-set-key (kbd "<f2>") (lambda () (interactive) (project-shell-history "<2>")))
;; )

;;; Code:

(require 'f)
(require 'project)

(defcustom project-shell-history-dir (concat user-emacs-directory "shell-history/")
  "The location of shells history."
  :group 'project-shell
  :type 'string)

(defun project-shell-history-name ()
  "Get the project root dir name and it's parent directory."
  (let ((path (project-root (project-current))))
    (f-join (f-filename (f-dirname path)) (f-filename path))))

(defun project-shell-history (&optional suffix)
  (interactive)
  "Start a shell for the given project with a buffer named after SUFFIX."
  (let* ((name (cond ((string= default-directory "~/") (getenv "USER"))
		     ((string= default-directory "/home/fedora/") "fedora")
		     (t (project-shell-history-name))))
	 (histfile (expand-file-name (concat project-shell-history-dir (string-replace "/" "_" name))))
	 (*buffer* (get-buffer-create (concat "*shell: " name (or suffix "") "*"))))
    (if (get-buffer-process *buffer*)
        (switch-to-buffer-other-window *buffer*)
      (with-environment-variables
	  (("HISTFILE" histfile))
	(mkdir project-shell-history-dir t)
	(make-local-variable 'comint-input-ring-file-name)
	(setq comint-input-ring-file-name histfile)
	(when (f-file? histfile)
	  (comint-read-input-ring t))
	(setq comint-input-ring-file-name nil)
	(if-let ((proj (project-current))
		 (dir (project-root proj)))
	    (cd dir))
	(shell *buffer*)))))

(provide 'project-shell)
;;; project-shell.el ends here
