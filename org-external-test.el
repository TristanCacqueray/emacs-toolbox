;;; package --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'org-external)
(require 's)

(ert-deftest oe/test/parse-external-cat ()
  (should
   (equal '(gitlab (:host "gitlab.com" :proj "sf-op")) (oe/parse-external-cat "gitlab.com:sf-op"))
   )
  )

(setq oe/example (s-join "\n" (list
                               "* my goals"
                               ":PROPERTIES:"
                               ":CATEGORY_EXTERNAL: jira:localhost:MY_PROJ"
                               ":END:"
                               "blah"
                               "** a goal"
                               ":PROPERTIES:"
                               ":END:"
                               "Goal description:"
                               ""
                               "- Do x"
                               "- Do y"
                               "*** DONE a task"
                               "Definition of done: merge X"
                               )))

(ert-deftest oe/test/push-story ()
  (with-temp-buffer
    (org-mode)
    (insert oe/example)
    (goto-char (point-min))
    (search-forward "a goal")
    ;; Test the calculation
    (should
     (equal (oe/push (org-element-at-point))
            (cons '(jira (:host "localhost" :token nil :proj "MY_PROJ"))
                  '(create (:summary "a goal" :description "Goal description:\n\n- Do x\n- Do y" :parent nil))
                  )))
    ;; The action should create the story
    (cl-letf (((symbol-function 'plz)
               (lambda (verb url &rest e)
                 (should (equal 'post verb))
                 (should (string= "https://localhost/rest/api/2/issue/" url))
                 "{\"key\":\"PROJ-1\"}")))
      (org-external-push))
    (should
     (string= "PROJ-1"
              (org-element-property :EXTERNAL_REF (org-element-at-point))))

    ;; The action should update the story
    (cl-letf (((symbol-function 'plz)
               (lambda (verb url &rest e)
                 (should (equal 'put verb))
                 (should (string= "https://localhost/rest/api/2/issue/PROJ-1" url))
                 nil)))
      (org-external-push))
    )
  )

(ert-deftest oe/test/push-task ()
  (with-temp-buffer
    (org-mode)
    (insert oe/example)
    (goto-char (point-min))
    ;; Create goal
    (search-forward "a goal")
    (org-set-property "EXTERNAL_REF" "PROJ-42")
    (search-forward "a task")
    ;; Test the calculation
    (should
     (equal (oe/push (org-element-at-point))
            (cons '(jira (:host "localhost" :token nil :proj "MY_PROJ"))
                  '(create (:summary "a task" :description "Definition of done: merge X" :parent "PROJ-42"))
                  )))
    ;; The action should create the story
    (cl-letf (((symbol-function 'plz)
               (lambda (verb url &rest e)
                 (should (equal 'post verb))
                 (should (string= "https://localhost/rest/api/2/issue/" url))
                 "{\"key\":\"PROJ-1\"}")))
      (org-external-push))
    (should
     (string= "PROJ-1"
              (org-element-property :EXTERNAL_REF (org-element-at-point))))

    ;; The action should update the story
    (cl-letf (((symbol-function 'plz)
               (lambda (verb url &rest e)
                 (should (equal 'put verb))
                 (should (string= "https://localhost/rest/api/2/issue/PROJ-1" url))
                 nil)))
      (org-external-push))
    )
  )


(ert-deftest oe/test/jira-parse-id ()
  (let ((resp (s-join "\n" (list
                            "{"
                            "\"id\":\"39000\","
                            "\"key\":\"TEST-101\","
                            "\"self\":\"http://localhost:8080/rest/api/2/issue/39000\""
                            "}"
                            ))))
    (should
     (string= "TEST-101" (oe/jira-parse-id resp))))
  )

(ert-deftest oe/test/jira-payload ()
  (let* ((base (list
                "\"project\":{\"key\":\"TEST\"},"
                "\"summary\":\"REST ye merry gentlemen.\","
                "\"description\":\"Creating\","
                "\"issuetype\":{\"name\":\"Story\"}"
                ))
         (task (list
                "\"project\":{\"key\":\"TEST\"},"
                "\"summary\":\"REST ye merry gentlemen.\","
                "\"description\":\"Creating\","
                "\"issuetype\":{\"name\":\"Sub-Task\"},"
                "\"parent\":{\"key\":\"TEST-42\"}"
                ))
         (mk-resp (lambda (body) (format "{\"fields\":{%s}}" (s-join "" body)))))
    (should
     (string= (funcall mk-resp base)
              (oe/jira-create-payload "TEST"
                                      (list :summary "REST ye merry gentlemen." :description "Creating"))))
    (should
     (string= (funcall mk-resp task)
              (oe/jira-create-payload "TEST"
                                      (list :summary "REST ye merry gentlemen." :description "Creating" :parent "TEST-42"))))
    )
  )

;; (ert "oe/test/**")

(provide 'org-external-test)
;;; org-external-test.el ends here
