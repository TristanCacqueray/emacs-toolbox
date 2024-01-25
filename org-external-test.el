;;; package --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'org-external)

(ert-deftest test-property ()
  (should
   (string=
    (with-temp-buffer
      (org-mode)
      (insert "* new entry")
      (goto-char (point-min))
      (oe/has-external (org-element-at-point)))
    nil)))
