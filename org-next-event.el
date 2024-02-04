;;; org-next-event.el -- utility macro for gnome-org-next-schedule -*- lexical-binding: t; -*-

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

;; Enable with: (add-hook 'org-agenda-mode-hook 'org-next-event-render)

;;; Code:
(require 'org-report)
(require 'org-ql)

(defun one/query ()
  "The next meeting query."
  '(and (not (done)) (not (habit)) (scheduled :from ,(ts-now))))

(defun one/format (item)
  (let* ((properties (cadr item))
         (title (plist-get properties :raw-value))
         (scheduled (plist-get properties :scheduled))
         (ts (format-time-string "%FT%T%z" (org-timestamp-to-time scheduled))))
    (format "%s %s" ts (or/remove-links title))))

(defvar one/schedule-events ""
  "The last schedule render to update the files when it changes.")

(defun org-next-event-render ()
  "Render the events for gnome-org-next-schedule."
  (let* ((entries (org-ql-select org-agenda-files (one/query)
                    :action 'element-with-markers
                    :sort 'or/compare-entry
                    ))
         (report (s-unlines (reverse (mapcar 'one/format entries)))))
    (when (not (string= report one/schedule-events))
      (setq one/schedule-events report)
      (f-write-text report 'utf-8 "~/.local/share/gnome-org-next-schedule/events"))))

;;;###autoload
(defun org-next-event-show ()
  (interactive)
  (org-ql-search org-agenda-files (one/query)
    :sort '(date)))

(provide 'org-next-event)
;;; org-next-event.el ends here
