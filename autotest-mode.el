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

(require 'tramp)
(require 'multi-term)
(require 'multi-term-ext)
(require 'notify)

(defvar autotest-mode-buffer nil
  "..")

(defcustom autotest-mode-buffer-name "autotest"
  ".."
  :group 'autotest-mode)

(defcustom autotest-screen-session-name "autotest"
  ".."
  :group 'autotest-mode)

(defcustom autotest-mode-command nil
  ".."
  :group 'autotest-mode)

(defcustom autotest-mode-working-directory nil
  ".."
  :group 'autotest-mode)

(defcustom autotest-mode-before-test-hook '()
  ".."
  :group 'autotest-mode)

(defcustom autotest-mode-after-test-hook '()
  ".."
  :group 'autotest-mode)

(defcustom autotest-mode-success-test-hook '()
  ".."
  :group 'autotest-mode)

(defcustom autotest-mode-fail-test-hook '()
  ".."
  :group 'autotest-mode)

;; Hooks ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun autotest/add-before-hook (fname)
  (interactive "sWhich function: ")
  (add-hook 'autotest-mode-before-test-hook fname t t))

(defun autotest/add-after-hook (fname)
  (interactive "sWhich function: ")
  (add-hook 'autotest-mode-after-test-hook fname t t))

(defun autotest/add-success-test-hook (fname)
  (interactive "sWhich function: ")
  (add-hook 'autotest-mode-success-test-hook fname t t))

(defun autotest/add-fail-test-hook (fname)
  (interactive "sWhich function: ")
  (add-hook 'autotest-mode-fail-test-hook fname t t))

;; Setting test command / function ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun autotest/set-function (fname)
  (interactive "aWhich function: ")
  (make-local-variable 'autotest-mode-command)
  (setq autotest-mode-command fname))

(defun autotest/set-command (command)
  (interactive
   (list
    (read-string "Command: " autotest-mode-command)))
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
  (message "running tests"))

(defun autotest/succeed ()
  (interactive)
  (-autotest-mode-flash-modeline 0.6 "green")
  (notify "autotest-mode" "all tests pass \\o/")
  ;;(run-with-timer 0.5 nil 'notify "autotest-mode" "all tests pass \\o/")
  )

(defun autotest/warning (&optional msg)
  (interactive)
  (-autotest-mode-flash-modeline 0.6 "yellow")
  (when msg
    (run-with-timer 0.5 nil 'notify "autotest-mode" msg)))

(defun autotest/fail (&optional buffername)
  (interactive)
  (-autotest-mode-flash-modeline 0.6 "red")
  (notify "autotest-mode" "tests failed T_T")
  ;;(run-with-timer 0.5 nil 'notify "autotest-mode" "tests failed T_T")
  )

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
         (autotest/succeed)
         (run-hooks 'autotest-mode-after-success-hook)))
      ;;
      ((and (not (string-match "\"autotest_mode_test_fail\"" output))
            (string-match "autotest_mode_test_fail" output))
       (progn
         (autotest/fail)
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

(defun -autotest-mode-remote-info ()
  (when (tramp-tramp-file-p default-directory)
    (let* ((tramp-info  (tramp-dissect-file-name default-directory))
           (remote-port (number-to-string (tramp-file-name-port tramp-info)))
           (remote-host (format "%s@%s"
                                (tramp-file-name-user tramp-info)
                                (tramp-file-name-real-host tramp-info))))
      `(("remote-port" . ,remote-port)
        ("remote-host" . ,remote-host)))))

(defun -autotest-mode-create-buffer ()
  (or (-autotest-mode-lookup-buffer (buffer-list))
      (let* ((remote-info (-autotest-mode-remote-info))
             (multi-term-ext-remote-ssh-port (cdr (assoc "remote-port" remote-info)))
             (multi-term-ext-remote-host (cdr (assoc "remote-host" remote-info)))
             (multi-term-ext-screen-session-name autotest-screen-session-name)
             (multi-term-buffer-name autotest-mode-buffer-name)
             (term-buffer (multi-term-open-terminal)))
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
        ;; making buffer and command local to this buffer only
        (-autotest-mode-decorate-process-filter (get-buffer-process term-buffer))
        (setq autotest-mode-buffer term-buffer)
        term-buffer)))

;; Main functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun -autotest-mode-get-working-directory (directory-path)
  ;; if the file is remote, get the dir path without the
  ;; tramp remote protocol crap
  (if (tramp-tramp-file-p directory-path)
      (tramp-file-name-localname
       (tramp-dissect-file-name directory-path))
    directory-path))

(defun -autotest-mode-cd-to-working-directory ()
  (let* ((autotest-mode-working-directory (-autotest-mode-get-working-directory
                                           (or autotest-mode-working-directory
                                               default-directory))))
    (with-current-buffer autotest-mode-buffer
      (term-send-raw-string (format "cd %s\n" autotest-mode-working-directory)))))

(defun -autotest-mode-execute-command (command)
  (autotest/begin-notification)
  (cond
   ;; this is a function
   ((symbolp command)
    (funcall command))
   ((stringp command)
    (with-current-buffer autotest-mode-buffer
      (term-send-raw-string (format "{ %s; autotest_mode_check_test_result; }\n"
                                    command))))))

(defun autotest/run-tests ()
  (interactive)
  ;; (setq autotest-mode-command (or cmd-or-fn
  ;;                                 autotest-mode-command
  ;;                                 (read-from-minibuffer "Command: ")))
  (-autotest-mode-create-buffer)
  (-autotest-mode-cd-to-working-directory)
  (-autotest-mode-execute-command autotest-mode-command))

;; After save utilities ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun autotest/enable-after-save ()
;;   (interactive)
;;   (dss/add-after-save-hook 'dss/autotest-run-tests))

(provide 'autotest-mode)
;;; autotest-mode.el ends here
