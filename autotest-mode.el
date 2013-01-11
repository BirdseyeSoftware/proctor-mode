;;; autotest-mode.el --- Utils for the ClojureScript 'lein cljsbuild' command

;; Copyright 2012 Birdseye Software.

;; Authors: <tavis at birdseye-sw com>, <roman at birdeseye-sw com>
;; Version: 0.0.1
;; Package-version: 0.0.1
;; Package-Requires: ((multi-term "0.8.7") (multi-term-ext "0.1.0"))
;; Keywords: tooling, testing
;; URL: http://github.com/BirdseyeSoftware/autotest-mode

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the MIT License.

;; Comentary:
;;
;; This is a work in progress
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar autotest-mode-buffer nil "..")
(defcustom autotest-mode-buffer-name "autotest" "..")
(defcustom autotest-screen-session-name "autotest" "..")
(defcustom autotest-mode-command nil "..")
(defcustom autotest-mode-working-directory nil "..")

(defcustom autotest-mode-before-test-hook '() "..")
(defcustom autotest-mode-after-test-hook '() "..")
(defcustom autotest-mode-success-test-hook '() "..")
(defcustom autotest-mode-fail-test-hook '() "..")

;; Hooks ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun autotest/add-before-hook (fname)
  (interactive "aWhich function: ")
  (add-hook 'autotest-mode-before-test-hook fname t t))

(defun autotest/add-after-hook (fname)
  (interactive "aWhich function: ")
  (add-hook 'autotest-mode-after-test-hook fname t t))

(defun autotest/add-success-test-hook (fname)
  (interactive "aWhich function: ")
  (add-hook 'autotest-mode-success-test-hook fname t t))

(defun autotest/add-fail-test-hook (fname)
  (interactive "aWhich function: ")
  (add-hook 'autotest-mode-fail-test-hook fname t t))

;; Setting test command / function ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun autotest/set-function (fname)
  (interactive "aWhich function: ")
  (make-local-variable 'autotest-mode-command)
  (setq autotest-mode-command fname))

(defun autotest/set-command (command)
  (interactive
   (list
    (read-string "aCommand: " autotest-mode-command)))
  (make-local-variable 'autotest-mode-command)
  (setq autotest-mode-command command))

;; Navigation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun autotest/switch-to-buffer ()
  "Switch back and forth between autotest and the previous buffer"
  (interactive)
  (if (string= (format "*%s*" autotest-mode-buffer-name) (buffer-name))
      (switch-to-buffer (other-buffer))
    (switch-to-buffer (format "*%s*" autotest-mode-buffer-name))))

;; Notifications ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun -autotest-mode-flash-modeline (&optional time color)
  (interactive)
  (let ((orig-modeline-fg (face-background 'modeline))
        (time (or time 2))
        (color (or color "#d70000")))
        (set-face-background 'modeline color)
        (run-with-timer time nil
                        'set-face-background
                        'modeline
                        orig-modeline-fg)))

(defun autotest/begin-notification ()
  (interactive)
  (-autotest-mode-flash-modeline 0.3 "purple")
  (message "running tests :-o"))

(defun autotest/succeed ()
  (interactive)
  (-autotest-mode-flash-modeline 0.6 "green")
  (run-with-timer 0.5 nil 'message "all tests pass \\o/"))

(defun autotest/warning (&optional msg)
  (interactive)
  (-autotest-mode-flash-modeline 0.6 "yellow")
  (when msg
    (run-with-timer 0.5 nil 'message msg)))

(defun autotest/fail (&optional buffername)
  (interactive)
  (-autotest-mode-flash-modeline 0.6 "red")
  (run-with-timer 0.5 nil 'message "tests failed T_T"))

;; Process Filter setup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun -autotest-mode-decorate-process-filter (term-proc)
  "Decorates the existing process filter that exists on the term
buffer with the cljsbuild-mode one"
  (set-process-filter term-proc
                      (-autotest-mode-process-filter (process-filter term-proc))))

(defun -autotest-mode-process-filter (orig-filter)
  "This process filter tracks the key output of cljsbuild to do
various things (show cljsbuild buffer on errors, hide it on
success, display messages to the minibuffer, etc.)"
  `(lambda (term-proc output)
     (cond
      ((and (not (string-match "\"autotest_mode_test_success\"" output))
            (string-match "autotest_mode_test_success" output))
       (progn
         (autotest-mode-succeed)
         (run-hooks 'autotest-mode-after-success-hook)))
      ;;
      ((and (not (string-match "\"autotest_mode_test_fail\"" output))
            (string-match "autotest_mode_test_fail" output))
       (progn
         (autotest-mode-fail)
         (pop-to-buffer autotest-mode-buffer)
         (run-hooks 'autotest-mode-after-fail-hook))))
     ;; call lower level process filter
     (run-hooks 'autotest-mode-after-test-hook)
     (if (fboundp #',orig-filter) (funcall ',orig-filter term-proc output))))


;; Buffer management ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun -autotest-mode-lookup-buffer (l)
  (when l
    (if (and (eq 'term-mode (with-current-buffer (car l) major-mode))
             (string=  (format "*%s*" autotest-mode-buffer-name)
                       (with-current-buffer (car l) (buffer-name))))
        (car l)
      (-autotest-mode-lookup-buffer (cdr l)))))

(defun -autotest-mode-get-buffer ()
  (or (-autotest-mode-lookup-buffer (buffer-list))
      (let* ((multi-term-buffer-name autotest-mode-buffer-name)
             (multi-term-ext-screen-session-name autotest-screen-session-name)
             (term-buffer (if autotest-screen-session-name
                              (multi-term-persistent)
                            (multi-term-ext))))
        (with-current-buffer term-buffer
          (term-send-raw-string "
function autotest_mode_check_test_result {
  if [[ $? == 0 ]]; then
    echo \"autotest_mode_test_success\"
  else
    echo \"autotest_mode_test_fail\"
  fi
  return $?
}\n"))
        (-autotest-mode-decorate-process-filter (get-buffer-process term-buffer))
        term-buffer)))

;; Main functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun -autotest-mode-cd-to-working-directory ()
  (let ((autotest-mode-working-directory (or autotest-mode-working-directory
                                             default-directory)))
    (with-current-buffer autotest-mode-buffer
      (term-send-raw-string (format "cd %s\n" autotest-mode-working-directory)))))

(defun -autotest-mode-execute-command ()
  (cond
   ;; this is a function
   ((symbolp autotest-mode-command)
    (funcall autotest-mode-command))
   ((stringp autotest-mode-command)
    (with-current-buffer autotest-mode-buffer
      (term-send-raw-string (format "{ %s; autotest_mode_check_test_result; }\n"
                                    autotest-mode-command))))))

(defun autotest/run-tests (&optional cmd-or-fn)
  (interactive)
  (setq autotest-mode-command (or cmd-or-fn
                                  autotest-mode-command
                                  (read-from-minibuffer "Command: ")))
  (make-local-variable 'autotest-mode-buffer)
  (setq autotest-mode-buffer (-autotest-mode-get-buffer))
  (-autotest-mode-cd-to-working-directory)
  (-autotest-mode-execute-command))

;; After save utilities ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun autotest/enable-after-save ()
;;   (interactive)
;;   (dss/add-after-save-hook 'dss/autotest-run-tests))

(provide 'autotest-mode)
;;; autotest-mode.el ends here
