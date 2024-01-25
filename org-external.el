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
;; Package-Requires: ((emacs "29") (s "1.2.0"))

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
;;  - https://gitlab.com/REPO/issue/42
;;  - https://github.com/OWNER/REPO/issue/42
;;  - https://issues.redhat.com/browse/PROJ_NAME-42
;;
;; Here are the list of available commands:
;;
;; M-x org-external-create  - Create an external task and set the EXTERNAL_REF
;; M-x org-external-comment - Show a comment buffer like a git commit and submit the result as an external comment
;; M-x org-external-close   - Close the external task

;;; Code:

(require 'org)

(defun oe/has-external (item)
  "Return '(type id) if ITEM has the right property"
  (let* ((prop (org-entry-get (plist-get item :org-marker) "EXTERNAL")))
    (message "got %s prop for %s" prop item)
))

(provide 'org-external)
;;; org-external.el ends here
