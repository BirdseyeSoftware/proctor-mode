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

(defgroup autotest-mode '()  "...")

(defvar -autotest-mode-modeline-in-use nil
  "Private var to check usage of modeline before changing")

(defvar -autotest-mode-modeline-usage-delay 0.5
  "Number of secs before trying to modify the modeline again.")

(defvar autotest-mode-buffer nil
  "Contains the buffer created by autotest")

(defcustom autotest-mode-buffer-name "autotest"
  "Specifies the name of the buffer created by autotest (the name
is going to contain earmufs later on)."
  :type 'string
  :group 'autotest-mode)

(defcustom autotest-screen-session-name "autotest"
  "Name of the GNU screen session name created for the autotest
terminal"
  :type 'string
  :group 'autotest-mode)

(defcustom autotest-mode-command nil
  "Command that gets executed. This variable is going to be buffer-local
if setted with `autotest/set-command'."
  :group 'autotest-mode)

(defcustom autotest-mode-working-directory nil
  "The directory where the command test is executed."
  :type 'string
  :group 'autotest-mode)

(defcustom autotest-mode-before-test-hook '()
  "Hook that is called before a test execution."
  :group 'autotest-mode)

(defcustom autotest-mode-after-test-hook '()
  "Hook that is called after a test execution."
  :group 'autotest-mode)

(defcustom autotest-mode-success-test-hook '()
  "Hook that is called after a successful test execution."
  :group 'autotest-mode)

(defcustom autotest-mode-fail-test-hook '()
  "Hook that is called after a failed test execution."
  :group 'autotest-mode)

(defcustom autotest-mode-success-message nil
  "String that will be shwon in the notification when tests
  execution is successful."
  :type 'string
  :group 'autotest-mode)

(defcustom autotest-mode-failure-message nil
  "String that will be shwon in the notification when tests
  execution fails."
  :type 'string
  :group 'autotest-mode)

(defcustom autotest-mode-popup-buffer-on-failure nil
  "If true, pops out the autotest buffer with failure output."
  :type 'bool
  :group 'autotest-mode)


(defface autotest-mode-failure-face
  '((((class color) (background light))
     :background "orange red") ;; TODO: Hard to read strings over
    (((class color) (background dark))
     :background "firebrick"))
  "Face for failures in autotest-mode."
  :group 'autotest-mode)

;; (defface autotest-mode-error-face
;;     '((((class color) (background light))
;;             :background "orange1")
;;           (((class color) (background dark))
;;                 :background "orange4"))
;;       "Face for errors in autotest-mode."
;;         :group 'autotest-mode)

(defface autotest-mode-success-face
  '((((class color) (background light))
     :foreground "black"
     :background "green")
    (((class color) (background dark))
     :foreground "black"
     :background "green"))
  "Face for success in autotest-mode."
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

(defun -autotest-mode-restore-modeline (orig-modeline-fg)
  (set-face-background 'modeline orig-modeline-fg)
  (setq -autotest-mode-modeline-in-use nil))

(defun -autotest-mode-flash-modeline (&optional time color)
  (interactive)
  (if (not -autotest-mode-modeline-in-use)
      (let ((orig-modeline-fg (face-background 'modeline))
            (time (or time 2))
            (color (or color "#d70000")))
        (setq -autotest-mode-modeline-in-use t)
        (set-face-background 'modeline color)
        (run-with-timer time nil
                        '-autotest-mode-restore-modeline
                        orig-modeline-fg))
    (run-with-timer -autotest-mode-modeline-usage-delay nil
                    '-autotest-mode-flash-modeline
                    time
                    color)))

(defun autotest/notify (header msg)
  (notify header msg)
  (message msg))

(defun autotest/begin-notification ()
  (interactive)
  (-autotest-mode-flash-modeline 0.3 "purple")
  (autotest/notify "autotest-mode"
          (format "TESTING: %s"
                  autotest-mode-command)))
(defun autotest/succeed ()
  (interactive)
  (-autotest-mode-flash-modeline 0.6 "green")
  (autotest/notify "autotest-mode"
                   (or autotest-mode-success-message
                       (propertize (format "SUCCESS: %s"
                                           autotest-mode-command)
                                   'face
                                   'autotest-mode-success-face))))

(defun autotest/warning (&optional msg)
  (interactive)
  (-autotest-mode-flash-modeline 0.6 "yellow")
  (when msg
    (run-with-timer 0.5 nil 'notify "autotest-mode" msg)))

(defun autotest/fail (&optional buffername)
  (interactive)
  (-autotest-mode-flash-modeline 0.6 "red")
  (autotest/notify "autotest-mode"
                   (or autotest-mode-failure-message
                       (propertize (format "FAILURE: %s"
                                           autotest-mode-command)
                                   'face
                                   'autotest-mode-failure-face))))

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
      ((string-match "autotest_mode_test_success" output)
       (progn
         (autotest/succeed)
         (run-hooks 'autotest-mode-after-success-hook)))
      ;;
      ((string-match "autotest_mode_test_fail" output)
       (progn
         (autotest/fail)
         (when autotest-mode-popup-buffer-on-failure
           (pop-to-buffer autotest-mode-buffer))
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
             (term-buffer (save-window-excursion
                            (multi-term-open-terminal))))
        (with-current-buffer term-buffer
          (term-send-raw-string "
autotest_prefix=\"autotest_mode\"
function autotest_mode_check_test_result {
  if [[ $? == 0 ]]; then
    echo \"${autotest_prefix}_test_success\"
  else
    echo \"${autotest_prefix}_test_fail\"
  fi
  return $?
}\n"))
        ;; making buffer and command local to this buffer only
        (-autotest-mode-decorate-process-filter (get-buffer-process term-buffer))
        (setq autotest-mode-buffer term-buffer)
        (display-buffer term-buffer)
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

(defun autotest/kill-buffer ()
  (interactive)
  (with-current-buffer autotest-mode-buffer
    (term-send-raw-string "exit\n"))
  (setq autotest-mode-buffer nil)
  (autotest/disable-after-save))

(defun autotest/run-tests ()
  (interactive)
  (-autotest-mode-create-buffer)
  (-autotest-mode-cd-to-working-directory)
  (-autotest-mode-execute-command autotest-mode-command))

;; After save utilities ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun autotest/enable-after-save ()
  (interactive)
  (add-hook 'after-save-hook 'autotest/run-tests t t))

(defun autotest/disable-after-save ()
  (interactive)
  (remove-hook 'after-save-hook 'autotest/run-tests t))

(provide 'autotest-mode)
;;; autotest-mode.el ends here
