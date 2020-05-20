;;; ~/.doom.d/org-pomodoro-config.el -*- lexical-binding: t; -*-

;;;** Org-pomodoro

(require 's)

;; Complice.co Less Wrong study hall
;; (https://complice.co/room/lesswrong/interstitial). Reference:
;; https://www.lesswrong.com/posts/hyeDFbg8ahYAu4ZJu/#586GQr5xjWBzXWda6
(setq! org-pomodoro-length 32)
(setq! org-pomodoro-short-break-length 8)
(setq! org-pomodoro-long-break-length 30)

;; Simulate
;;
;; (setq! org-pomodoro-ticking-sound-p t)
;; (setq! org-pomodoro-ticking-sound-states '(:pomodoro))
;;
;; but use \"play\" from the SoX package so that playback is smoother and
;; takes less CPU.
(defvar org-pomodoro-ticking-process nil)
(setq! org-pomodoro-ticking-sound-p nil)
(defun my-org-pomodoro-start-tick ()
  "Start ticks for org-pomodoro-mode.

Requires the \"play\" executable from the SoX package
\(http://sox.sourceforge.net/sox.html)."
  (let ((cmd
         ;; Pad with 0.79 seconds of silence because tick.wav included with
         ;; ‘org-pomodoro’ is 0.21 seconds long, to get a 1-second tick.
         (format "play %s pad 0 0.79 repeat - </dev/null"
                 (shell-quote-argument org-pomodoro-ticking-sound))))
    (setq org-pomodoro-ticking-process
          (start-process
           "*org-pomodoro-ticking-process*"
           "*org-pomodoro-ticking-process*"
           "python3"
           "-c"
           (format
            "\
import os, signal, subprocess, sys, time
x = subprocess.Popen(r'''%s''', shell=True)
while True:
    # Kill process tree if parent process exits.
    if os.getppid() == 1:
        os.killpg(os.getpgid(os.getpid()), signal.SIGKILL)
    # Exit if child exits.
    if x.poll() is not None:
        sys.exit(x.returncode)
    time.sleep(1)
"
            cmd)))))
(defun my-org-pomodoro-stop-tick ()
  "Stop ticks for org-pomodoro-mode."
  (when org-pomodoro-ticking-process
    (signal-process org-pomodoro-ticking-process 15) ; SIGTERM
    (setq org-pomodoro-ticking-process nil)))
(add-hook 'org-pomodoro-started-hook #'my-org-pomodoro-start-tick)
(add-hook 'org-pomodoro-finished-hook #'my-org-pomodoro-stop-tick)
(add-hook 'org-pomodoro-killed-hook #'my-org-pomodoro-stop-tick)

(defun org-pomodoro-end-in (minutes)
  "Force the current Pomodoro to end in MINUTES minutes."
  (interactive "nMinutes: ")
  (setq my-org-pomodoro-current-task-reminder-next-time nil)
  (setq org-pomodoro-end-time
        (time-add (current-time) (* minutes 60))))

(defcustom my-org-pomodoro-break-id nil
  "Task ID of task to clock into during Pomodoro breaks. Must specify manually."
  :type 'string)
(defun my-org-pomodoro-finished-lock-screen ()
  "Lock screen at the end of each Pomodoro work session."
  (message "Locking screen in 15 seconds - post calendar event from *scratch*")
  (cond
   ((eq system-type 'darwin)
    (start-process "lock" nil "bash" "-c" "sleep 15; pmset displaysleepnow"))
   ((and (executable-find "xset")
         (not (s-blank-str? (getenv "DISPLAY"))))
    (shell-command "xdotool search 'Chrome' key --window '%@' XF86AudioPlay")
    (start-process "lock" nil "bash" "-c" "sleep 15; xset s activate"))
   (t
    (display-warning
         'my-org-pomodoro-finished-lock-screen
         "Can't lock screen"))))
(defun my-org-pomodoro-finished-caffeinate ()
  "Prevent system from idle sleeping during Pomodoro breaks."
  (let ((countdown
         (cl-case org-pomodoro-state
           (:short-break (* 60 org-pomodoro-short-break-length))
           (:long-break (* 60 org-pomodoro-long-break-length))
           (t 0))))
    (when (> countdown 0)
      (cond
       ((eq system-type 'darwin)
        (async-start-process "my-org-pomodoro-finished-caffeinate"
                             "caffeinate" 'ignore
                             "-t" (number-to-string countdown)))
       (t
        (display-warning
         'my-org-pomodoro-finished-caffeinate
         "Can't prevent system from sleeping"))))))
(defun my-org-pomodoro-started-notify-hook ()
  (org-notify "Pomodoro started - snooze notifications in Hangouts Chat."))
(defun my-org-pomodoro-finished-notify-hook ()
  (org-notify "Pomodoro phase finished"))
(defvar my-org-pomodoro-clock-idle-time nil
  "Variable in which ‘org-clock-idle-time’ is saved.")
(defun my-org-pomodoro-start-break ()
  "Start break - clock into task with ID my-org-pomodoro-break-id."
  (interactive)
  ;; Set org-clock-idle-time to nil to disable it during Pomodoro breaks -
  ;; sometimes Emacs will hang after the break.
  (setq my-org-pomodoro-clock-idle-time org-clock-idle-time)
  (setq org-clock-idle-time nil)
  (save-excursion
    (org-id-goto my-org-pomodoro-break-id)
    (org-clock-in)))
(defun my-org-pomodoro-start-lunch ()
  (interactive)
  (org-pomodoro-notify "Going to lunch now" "")
  (setq org-pomodoro-count 0)
  (org-pomodoro-start :long-break)
  (my-org-pomodoro-start-break)
  (my-org-pomodoro-finished-lock-screen))
(defun my-org-pomodoro-finished-clock-in-break-hook ()
  "Clock into task with ID my-org-pomodoro-break-id during breaks if set."
  (require 'call-log)
  (clog/msg "%s %s" my-org-pomodoro-break-id org-pomodoro-state)
  (when my-org-pomodoro-break-id
    (clog/msg "About to start clock")
    (my-org-pomodoro-start-break)))
(defun my-org-pomodoro-break-finished-notify-hook ()
  (let ((msg "Pomodoro break finished -- get back to work!"))
    (if (fboundp 'terminal-notifier-notify)
        ;; Try to ensure timeout is very high by skipping org-notify.
        (terminal-notifier-notify "Org Pomodoro" msg 84000)
      (org-notify msg))))
(defun my-org-pomodoro-short-break-finished-punch-in ()
  "Run bh/punch-in when Pomodoro short breaks end."
  (setq org-clock-idle-time my-org-pomodoro-clock-idle-time)
  (message-box "Break finished - please run bh/punch-in"))
(defun my-org-pomodoro-long-break-finished-punch-out ()
  "Run bh/punch-out when Pomodoro long breaks end."
  (bh/punch-out))

(defcustom my-org-pomodoro-alarm-gcal-calendar-id nil
  "The Google Calendar ID on which to create alarms."
  :type 'string)
(defcustom my-org-pomodoro-current-task-reminder-interval 60
  "Number of seconds between being notified of the current task. Set to nil to disable notifications"
  :type 'number)

;; Update agenda to log count and time of pomodoros elapsed today.
(defvar my-org-pomodoro-start-time nil
  "Start time of current pomodoro.")
(defvar my-org-pomodoro-time-today-var 0
  "Amount of time spent in pomodoro today.
DO NOT USE - contains only time logged outside of the current pomodoro.
Call ‘my-org-pomodoro-time-today' instead.")
(defun my-org-pomodoro-time-today ()
  "Return amount of time spent in pomodoro today, as a floating-point
number of seconds."
  (+ my-org-pomodoro-time-today-var
     (if (eq org-pomodoro-state :pomodoro)
         (float-time (time-subtract (current-time)
                                    my-org-pomodoro-start-time))
       0)))
(defun my-org-pomodoro-time-today-set ()
  "Manually prompt for elapsed pomodoro time for today to set."
  (interactive)
  (require 'call-log)
  (let* ((input
          (read-from-minibuffer "Org Pomodoro Time Elapsed Today: ")))
    (clog/msg "Setting elapsed time to %s" input)
    (setq my-org-pomodoro-time-today-var
          (* 60 (org-duration-to-minutes input)))))
(defun my-org-pomodoro-reset-today (&optional arg)
  "Resets daily org-pomodoro variables every day"
  (if (null org-pomodoro-last-clock-in)
      (setq my-org-pomodoro-time-today-var 0)
    (let* ((effective-midnight
            `(0 0 0 . ,(nthcdr 3 (decode-time (current-time))))))
      (when
          (and (<= 0
                   (float-time
                    (time-subtract (current-time)
                                   effective-midnight)))
               (>= 0
                   (float-time
                    (time-subtract org-pomodoro-last-clock-in
                                   effective-midnight))))
        (setq my-org-pomodoro-time-today-var 0)))))
(advice-add #'org-pomodoro :before #'my-org-pomodoro-reset-today)
(defun my-org-pomodoro-set-start-time ()
  "Sets start time for use by my-org-pomodoro-time-today."
  (setq my-org-pomodoro-start-time (current-time)))
(add-hook 'org-pomodoro-started-hook #'my-org-pomodoro-set-start-time)
(defun my-org-pomodoro-finished-update-time-today ()
  "Updates stored variable for my-org-pomodoro-time-today."
  (setq my-org-pomodoro-time-today-var
        (+ my-org-pomodoro-time-today-var
           (float-time (time-subtract (current-time)
                                      my-org-pomodoro-start-time)))))
(add-hook 'org-pomodoro-finished-hook
          #'my-org-pomodoro-finished-update-time-today)
(defun my-org-agenda-pomodoro-info ()
  "Add Org Pomodoro Count and Time to agenda."
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-min))
      (let* ((search-for "Org Pomodoro - Count:")
             (match (search-forward search-for nil 'noerror)))
        (if match
          (let ((start-match (- (point) (length search-for))))
              (goto-char start-match)
              (delete-region start-match (point-max)))
          (progn
            (end-of-line)
            (newline)))
        (insert
         (format "Org Pomodoro - Count: %2d, Time: %s"
                 org-pomodoro-count
                 (org-timer-secs-to-hms
                         (round (my-org-pomodoro-time-today)))))
        (newline)
        ;; Add spaces to align with line above
        (insert "Try to get above                3:30:00")))))
(add-hook 'org-agenda-finalize-hook 'my-org-agenda-pomodoro-info 'append)

(defun my-org-pomodoro-today-tick-hook ())
(defvar my-org-pomodoro-current-task-reminder-next-time nil)
(defun my-org-pomodoro-tick-current-task-reminder ()
  "Prod me with reminders of my current task to stop me from being distracted."
  (when (or (null my-org-pomodoro-current-task-reminder-next-time)
            (> (float-time) my-org-pomodoro-current-task-reminder-next-time))
    (let* ((x (cl-floor
               (float-time (time-subtract org-pomodoro-end-time (current-time)))
               60))
           (quotient (car x))
           (remainder (car (cl-floor (cadr x)))))
      (when (and (eql org-pomodoro-state :pomodoro)
                 (not (null my-org-pomodoro-current-task-reminder-interval)))
        (cond
         ((> quotient 0)
          ;; Rate limit reminders in last minute to once every
          ;; ‘my-org-pomodoro-current-task-reminder-interval’ seconds.
          (setq my-org-pomodoro-current-task-reminder-next-time
                (min (car (cl-floor (+ (float-time)
                                       my-org-pomodoro-current-task-reminder-interval)))
                     (float-time (time-subtract org-pomodoro-end-time 60))))
          (org-pomodoro-notify "Pomodoro in progress" org-clock-heading))
         (t
          ;; Rate limit reminders in last minute to once every 5 seconds.
          (setq my-org-pomodoro-current-task-reminder-next-time
                (car (cl-floor (+ (float-time) 5))))
          (org-pomodoro-notify (format "Pomodoro in progress - %ds to break"
                                       remainder)
                               org-clock-heading)))))))
(defun my-org-pomodoro-finished-create-break-end-alarm ()
  "Create Google Calendar alarm for end of ‘org-pomodoro' break."
  (interactive)
  (when (and (or (eq org-pomodoro-state :short-break)
                 (eq org-pomodoro-state :long-break))
             ;; Current break has not ended yet.
             (> (float-time (time-subtract org-pomodoro-end-time (current-time)))
                0)
             my-org-pomodoro-alarm-gcal-calendar-id)
    (my-org-pomodoro--create-alarm-event
     my-org-pomodoro-alarm-gcal-calendar-id
     org-pomodoro-end-time)))
(defun my-org-pomodoro--create-alarm-event (calendar-id time)
  (call-process
   (expand-file-name "pomodoro_schedule_alarm.py" doom-private-dir)
   nil (get-buffer-create "*pomodoro_schedule_alarm.py*") nil
   "--calendar_id" calendar-id
   "--timestamp" (format-time-string "%FT%T%z" time)
   "--title" "org-pomodoro break end -- get back to work!"))

(add-hook 'org-pomodoro-started-hook #'my-org-pomodoro-started-notify-hook)
(add-hook 'org-pomodoro-finished-hook #'my-org-pomodoro-finished-notify-hook)
(add-hook 'org-pomodoro-finished-hook #'my-org-pomodoro-finished-lock-screen)
(add-hook 'org-pomodoro-finished-hook #'my-org-pomodoro-finished-caffeinate)
(add-hook 'org-pomodoro-finished-hook #'my-org-pomodoro-finished-clock-in-break-hook)
(add-hook 'org-pomodoro-finished-hook #'my-org-pomodoro-finished-create-break-end-alarm)
(add-hook 'org-pomodoro-tick-hook #'my-org-pomodoro-tick-current-task-reminder)
(add-hook 'org-pomodoro-break-finished-hook #'my-org-pomodoro-break-finished-notify-hook)
(add-hook 'org-pomodoro-short-break-finished-hook #'my-org-pomodoro-short-break-finished-punch-in)
(add-hook 'org-pomodoro-long-break-finished-hook #'my-org-pomodoro-long-break-finished-punch-out)
