;;; proctor-mode.el --- Test supervisor and enforcer.

;; Copyright 2012 Birdseye Software.

;; Authors: <tavis at birdseye-sw com>, <roman at birdeseye-sw com>
;; Version: 0.0.1
;; Package-version: 0.0.1
;; Package-Requires: ((multi-term "0.8.7")
;;                    (multi-term-ext "0.1.0")
;;                    (notify ""))
;; Keywords: tooling, testing
;; URL: http://github.com/BirdseyeSoftware/proctor-mode

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

(defgroup proctor-mode '()  "...")

(defvar -proctor-mode-modeline-in-use nil
  "Private var to check usage of modeline before changing")

(defvar -proctor-mode-modeline-usage-delay 0.5
  "Number of secs before trying to modify the modeline again.")

(defvar proctor-mode-buffer nil
  "Contains the buffer created by proctor")

(defcustom proctor-mode-buffer-name "proctor"
  "Specifies the name of the buffer created by proctor (the name
is going to contain earmufs later on)."
  :type 'string
  :group 'proctor-mode)

(defcustom proctor-screen-session-name "proctor"
  "Name of the GNU screen session name created for the proctor
terminal"
  :type 'string
  :group 'proctor-mode)

(defcustom proctor-mode-command nil
  "Command that gets executed. This variable is going to be buffer-local
if setted with `proctor/set-command'."
  :group 'proctor-mode)

(defcustom proctor-mode-working-directory nil
  "The directory where the command test is executed."
  :type 'string
  :group 'proctor-mode)

(defcustom proctor-mode-before-test-hook '()
  "Hook that is called before a test execution."
  :group 'proctor-mode)

(defcustom proctor-mode-after-test-hook '()
  "Hook that is called after a test execution."
  :group 'proctor-mode)

(defcustom proctor-mode-success-test-hook '()
  "Hook that is called after a successful test execution."
  :group 'proctor-mode)

(defcustom proctor-mode-fail-test-hook '()
  "Hook that is called after a failed test execution."
  :group 'proctor-mode)

(defcustom proctor-mode-success-message nil
  "String that will be shwon in the notification when tests
  execution is successful."
  :type 'string
  :group 'proctor-mode)

(defcustom proctor-mode-failure-message nil
  "String that will be shwon in the notification when tests
  execution fails."
  :type 'string
  :group 'proctor-mode)

(defcustom proctor-mode-popup-buffer-on-failure nil
  "If true, pops out the proctor buffer with failure output."
  :type 'bool
  :group 'proctor-mode)


(defface proctor-mode-failure-face
  '((((class color) (background light))
     :background "orange red") ;; TODO: Hard to read strings over
    (((class color) (background dark))
     :background "firebrick"))
  "Face for failures in proctor-mode."
  :group 'proctor-mode)

;; (defface proctor-mode-error-face
;;     '((((class color) (background light))
;;             :background "orange1")
;;           (((class color) (background dark))
;;                 :background "orange4"))
;;       "Face for errors in proctor-mode."
;;         :group 'proctor-mode)

(defface proctor-mode-success-face
  '((((class color) (background light))
     :foreground "black"
     :background "green")
    (((class color) (background dark))
     :foreground "black"
     :background "green"))
  "Face for success in proctor-mode."
  :group 'proctor-mode)

;; Hooks ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun proctor/add-before-hook (fname)
  (interactive "sWhich function: ")
  (add-hook 'proctor-mode-before-test-hook fname t t))

(defun proctor/add-after-hook (fname)
  (interactive "sWhich function: ")
  (add-hook 'proctor-mode-after-test-hook fname t t))

(defun proctor/add-success-test-hook (fname)
  (interactive "sWhich function: ")
  (add-hook 'proctor-mode-success-test-hook fname t t))

(defun proctor/add-fail-test-hook (fname)
  (interactive "sWhich function: ")
  (add-hook 'proctor-mode-fail-test-hook fname t t))

;; Setting test command / function ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun proctor/set-function (fname)
  (interactive "aWhich function: ")
  (make-local-variable 'proctor-mode-command)
  (setq proctor-mode-command fname))

(defun proctor/set-command (command)
  (interactive
   (list
    (read-string "Command: " proctor-mode-command)))
  (make-local-variable 'proctor-mode-command)
  (setq proctor-mode-command command))

;; Navigation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun proctor/switch-to-buffer ()
  "Switch back and forth between proctor and the previous buffer"
  (interactive)
  (if (string= (format "*%s*" proctor-mode-buffer-name) (buffer-name))
      (switch-to-buffer (other-buffer))
    (switch-to-buffer (format "*%s*" proctor-mode-buffer-name))))

;; Notifications ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun -proctor-mode-restore-modeline (orig-modeline-fg)
  (set-face-background 'modeline orig-modeline-fg)
  (setq -proctor-mode-modeline-in-use nil))

(defun -proctor-mode-flash-modeline (&optional time color)
  (interactive)
  (if (not -proctor-mode-modeline-in-use)
      (let ((orig-modeline-fg (face-background 'modeline))
            (time (or time 2))
            (color (or color "#d70000")))
        (setq -proctor-mode-modeline-in-use t)
        (set-face-background 'modeline color)
        (run-with-timer time nil
                        '-proctor-mode-restore-modeline
                        orig-modeline-fg))
    (run-with-timer -proctor-mode-modeline-usage-delay nil
                    '-proctor-mode-flash-modeline
                    time
                    color)))

(defun proctor/notify (header msg)
  (when (fboundp 'notify)
    (notify header msg))
  (message msg))

(defun proctor/begin-notification ()
  (interactive)
  (-proctor-mode-flash-modeline 0.3 "purple")
  (proctor/notify "proctor-mode"
          (format "TESTING: %s"
                  proctor-mode-command)))
(defun proctor/succeed ()
  (interactive)
  (-proctor-mode-flash-modeline 0.6 "green")
  (proctor/notify "proctor-mode"
                   (or proctor-mode-success-message
                       (propertize (format "SUCCESS: %s"
                                           proctor-mode-command)
                                   'face
                                   'proctor-mode-success-face))))

(defun proctor/warning (&optional msg)
  (interactive)
  (-proctor-mode-flash-modeline 0.6 "yellow")
  (when msg
    (run-with-timer 0.5 nil 'notify "proctor-mode" msg)))

(defun proctor/fail (&optional buffername)
  (interactive)
  (-proctor-mode-flash-modeline 0.6 "red")
  (proctor/notify "proctor-mode"
                   (or proctor-mode-failure-message
                       (propertize (format "FAILURE: %s"
                                           proctor-mode-command)
                                   'face
                                   'proctor-mode-failure-face))))

;; Process Filter setup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun -proctor-mode-decorate-process-filter (term-proc)
  "Decorates the existing process filter that exists on the term
buffer with the cljsbuild-mode one"
  (set-process-filter term-proc
                      (-proctor-mode-process-filter (process-filter term-proc))))

(defun -proctor-mode-process-filter (orig-filter)
  "This process filter tracks the key output of cljsbuild to do
various things (show cljsbuild buffer on errors, hide it on
success, display messages to the minibuffer, etc.)"
  `(lambda (term-proc output)
     (cond
      ((string-match "proctor_mode_test_success" output)
       (progn
         (proctor/succeed)
         (run-hooks 'proctor-mode-after-success-hook)))
      ;;
      ((string-match "proctor_mode_test_fail" output)
       (progn
         (proctor/fail)
         (when proctor-mode-popup-buffer-on-failure
           (pop-to-buffer proctor-mode-buffer))
         (run-hooks 'proctor-mode-after-fail-hook))))
     ;; call lower level process filter
     (run-hooks 'proctor-mode-after-test-hook)
     (if (fboundp #',orig-filter) (funcall ',orig-filter term-proc output))))

;; Buffer management ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun -proctor-mode-lookup-buffer (l)
  (when l
    (if (and (eq 'term-mode (with-current-buffer (car l) major-mode))
             (string=  (format "*%s*" proctor-mode-buffer-name)
                       (with-current-buffer (car l) (buffer-name))))
        (car l)
      (-proctor-mode-lookup-buffer (cdr l)))))

(defun -proctor-mode-remote-info ()
  (when (tramp-tramp-file-p default-directory)
    (let* ((tramp-info  (tramp-dissect-file-name default-directory))
           (remote-port (number-to-string (tramp-file-name-port tramp-info)))
           (remote-host (format "%s@%s"
                                (tramp-file-name-user tramp-info)
                                (tramp-file-name-real-host tramp-info))))
      `(("remote-port" . ,remote-port)
        ("remote-host" . ,remote-host)))))

(defun -proctor-mode-create-buffer ()
  (or (-proctor-mode-lookup-buffer (buffer-list))
      (let* ((remote-info (-proctor-mode-remote-info))
             (multi-term-ext-remote-ssh-port (cdr (assoc "remote-port" remote-info)))
             (multi-term-ext-remote-host (cdr (assoc "remote-host" remote-info)))
             (multi-term-ext-screen-session-name proctor-screen-session-name)
             (multi-term-buffer-name proctor-mode-buffer-name)
             (term-buffer (save-window-excursion
                            (multi-term-open-terminal))))
        (with-current-buffer term-buffer
          (term-send-raw-string "
proctor_prefix=\"proctor_mode\"
function proctor_mode_check_test_result {
  if [[ $? == 0 ]]; then
    echo \"${proctor_prefix}_test_success\"
  else
    echo \"${proctor_prefix}_test_fail\"
  fi
  return $?
}\n"))
        ;; making buffer and command local to this buffer only
        (-proctor-mode-decorate-process-filter (get-buffer-process term-buffer))
        (setq proctor-mode-buffer term-buffer)
        (display-buffer term-buffer)
        term-buffer)))

;; Main functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun -proctor-mode-get-working-directory (directory-path)
  ;; if the file is remote, get the dir path without the
  ;; tramp remote protocol crap
  (if (tramp-tramp-file-p directory-path)
      (tramp-file-name-localname
       (tramp-dissect-file-name directory-path))
    directory-path))

(defun -proctor-mode-cd-to-working-directory ()
  (let* ((proctor-mode-working-directory (-proctor-mode-get-working-directory
                                           (or proctor-mode-working-directory
                                               default-directory))))
    (with-current-buffer proctor-mode-buffer
      (term-send-raw-string (format "cd %s\n" proctor-mode-working-directory)))))

(defun -proctor-mode-execute-command (command)
  (proctor/begin-notification)
  (cond
   ;; this is a function
   ((symbolp command)
    (funcall command))
   ((stringp command)
    (with-current-buffer proctor-mode-buffer
      (term-send-raw-string (format "{ %s; proctor_mode_check_test_result; }\n"
                                    command))))))

(defun proctor/kill-buffer ()
  (interactive)
  (with-current-buffer proctor-mode-buffer
    (term-send-raw-string "exit\n"))
  (setq proctor-mode-buffer nil)
  (proctor/disable-after-save))

(defun proctor/run-tests ()
  (interactive)
  (-proctor-mode-create-buffer)
  (-proctor-mode-cd-to-working-directory)
  (-proctor-mode-execute-command proctor-mode-command))

;; After save utilities ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun proctor/enable-after-save ()
  (interactive)
  (add-hook 'after-save-hook 'proctor/run-tests t t))

(defun proctor/disable-after-save ()
  (interactive)
  (remove-hook 'after-save-hook 'proctor/run-tests t))

(defalias 'proctor/on 'proctor/enable-after-save)
(defalias 'proctor/off 'proctor/disable-after-save)


(provide 'proctor-mode)
;;; proctor-mode.el ends here
