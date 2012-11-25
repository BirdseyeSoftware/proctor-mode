;;; autotest-mode.el --- Utils for the ClojureScript 'lein cljsbuild' command

;; Copyright 2012 Birdseye Software.

;; Authors: <tavis at birdseye-sw com>, <roman at birdeseye-sw com>
;; Version: 0.0.1
;; Package-version: 0.0.1
;; Package-Requires: ((multi-term "0.8.8"))
;; Keywords: tooling, testing
;; URL: http://github.com/BirdseyeSoftware/autotest-mode

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the MIT License.

;; Comentary:
;;
;; This is a work in progress
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defvar autotest-mode-last-buffer nil)
;; (defvar autotest-mode-buffer-name "*autotest*")
;; (defvar autotest-mode-command "")
;; (defvar autotest-mode-buffer-workingdir "")
;; (defvar autotest-mode-buffer-setup-command "")
;; (defvar autotest-mode-before-test-hook '())

(defun autotest-mode-flash-modeline (&optional time color)
  (interactive)
  (let ((orig-modeline-fg (face-background 'modeline))
        (time (or time 2))
        (color (or color "#d70000")))
        (set-face-background 'modeline color)
        (run-with-timer time nil
                        'set-face-background
                        'modeline
                        orig-modeline-fg)))

;; (defun autotest-mode-add-before-hook (fname)
;;   (interactive "aWhich function: ")
;;   (add-hook 'autotest-mode-before-test-hook fname t t))

;; (defun autotest-mode-set-function (fname)
;;   (interactive "aWhich function: ")
;;   (make-local-variable 'autotest-mode-command)
;;   (setq autotest-mode-command fname))

;; (defun autotest-mode-set-command (command)
;;   (interactive
;;    (list
;;     (read-string "Command: " autotest-mode-command)))
;;   (make-local-variable 'autotest-mode-command)
;;   (setq autotest-mode-command command))

;; (defun autotest-mode-switch-to-output ()
;;   "Switch back and forth between autotest and the previous buffer"
;;   (interactive)
;;   (if (equal autotest-mode-buffer-name (buffer-name))
;;       (switch-to-buffer (other-buffer))
;;     (switch-to-buffer autotest-mode-buffer-name)))

(defun autotest-mode-begin-notification ()
  (interactive)
  (autotest-mode-flash-modeline 0.1 "purple")
  (message "running tests..."))

(defun autotest-mode-succeed ()
  (interactive)
  (autotest-mode-flash-modeline 0.5 "green")
  (message "all tests pass"))

(defun autotest-mode-warning (&optional msg)
  (interactive)
  (autotest-mode-flash-modeline 0.5 "yellow")
  (when msg
    (message msg)))

(defun autotest-mode-fail (&optional buffername)
  (interactive)
  (autotest-mode-flash-modeline 0.5 "red")
  (run-with-timer 2 nil 'message "**tests failed**"))

;; (defun autotest-mode-report (exit-status)
;;   (interactive "nExit code:")
;;   (if (not (eq 0 exit-status))
;;       (dss/autotest-fail)
;;     (dss/autotest-succeed)))

;; (defun autotest-mode-run-shell-test-command (command)
;;   (interactive)
;;   (let ((term-buffer (autotest-mode-buffer)))
;;     (set-buffer term-buffer)
;;     (term-send-raw-string
;;      (concat
;;       command
;;       "; emacsclient -e \"(autotest-mode-report $?)\" 2>1&> /dev/null \n"))))

;; (defun autotest-mode-run-tests (&optional test-command)
;;   (interactive)
;;   (setq test-command (or test-command autotest-mode-command))
;;   (unless test-command
;;     (autotest-mode-set-command)
;;     (setq test-command autotest-mode-command))
;;   (setq autotest-mode-last-buffer (current-buffer))
;;   (autotest-mode-begin-notification)
;;   (run-hooks 'autotest-mode-before-test-hook)
;;   (cond
;;    ((symbolp test-command) (funcall test-command))
;;    ((stringp test-command)
;;     (autotest-mode-run-shell-test-command test-command))))

;; (defun autotest-mode-enable-after-save ()
;;   (interactive)
;;   (dss/add-after-save-hook 'dss/autotest-run-tests))

(provide 'autotest-mode)
;;; autotest-mode.el ends here
