;;; ~/.doom.d/org-pomodoro-config.el -*- lexical-binding: t; -*-

;;;** Org-pomodoro

(require 'async)
(require 's)

;; ;; Complice.co Less Wrong study hall
;; ;; (https://complice.co/room/lesswrong/interstitial). Reference:
;; ;; https://www.lesswrong.com/posts/hyeDFbg8ahYAu4ZJu/#586GQr5xjWBzXWda6
;; (setq! org-pomodoro-length 32)
;; (setq! org-pomodoro-short-break-length 8)
;; (setq! org-pomodoro-long-break-length 30)

;; Guzey schedule
;; https://guzey.com/productivity/#how-i-work-and-rest-how-my-system-is-different-from-all-the-others-and-why-i-like-it-so-much
;; Adopt a Pomodoro schedule strictly by the clock - working from :00-:25 and
;; :30-:55 and taking 5-minute breaks, with 35 minute breaks every 3 hours.
(setq! org-pomodoro-length 25)
(setq! org-pomodoro-short-break-length 5)
(setq! org-pomodoro-long-break-length 35)
(setq! org-pomodoro-long-break-frequency 5)

;; Simulate
;;
;; (setq! org-pomodoro-ticking-sound-p t)
;; (setq! org-pomodoro-ticking-sound-states '(:pomodoro))
;;
;; but use \"play\" from the SoX package so that playback is smoother and
;; takes less CPU.
(defvar org-pomodoro-ticking-process nil)
(setq! org-pomodoro-ticking-sound-p nil)
(defvar org-pomodoro-ticking-volume 1.0
  "Volume for ‘my-org-pomodoro-start-tick’. Should be in range 0.0-1.0.")
(defun my-org-pomodoro-start-tick ()
  "Start ticks for org-pomodoro-mode.

Requires the \"play\" executable from the SoX package
\(http://sox.sourceforge.net/sox.html)."
  (let ((cmd
         ;; Pad with 0.79 seconds of silence because tick.wav included with
         ;; ‘org-pomodoro’ is 0.21 seconds long, to get a 1-second tick.
         (format "play --volume %f %s pad 0 0.79 repeat - </dev/null"
                 org-pomodoro-ticking-volume
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
(defun my-org-pomodoro-change-ticking-volume (volume)
  "Change ticking volume for Pomodoro to VOLUME"
  (interactive "nVolume (0.0-1.0): ")
  (setq org-pomodoro-ticking-volume volume)
  (my-org-pomodoro-stop-tick)
  (my-org-pomodoro-start-tick))
(add-hook 'org-pomodoro-started-hook #'my-org-pomodoro-start-tick)
(add-hook 'org-pomodoro-finished-hook #'my-org-pomodoro-stop-tick)
(add-hook 'org-pomodoro-killed-hook #'my-org-pomodoro-stop-tick)

(defun org-pomodoro-end-at ()
  "Force the current Pomodoro to end at a time prompted from the user."
  (interactive)
  (unless (org-pomodoro-active-p)
    (org-pomodoro))
  (setq my-org-pomodoro-current-task-reminder-next-time nil)
  (setq org-pomodoro-end-time
        (org-read-date 'with-time 'to-time))
  (my-org-pomodoro-update-log-event org-pomodoro-end-time)
  (my-org-pomodoro-reschedule-alarm))

(defun org-pomodoro-end-in (minutes)
  "Force the current Pomodoro to end in MINUTES minutes."
  (interactive "nMinutes: ")
  (unless (org-pomodoro-active-p)
    (org-pomodoro))
  (setq my-org-pomodoro-current-task-reminder-next-time nil)
  (setq org-pomodoro-end-time
        (time-add (current-time) (* minutes 60)))
  (my-org-pomodoro-update-log-event org-pomodoro-end-time)
  (my-org-pomodoro-reschedule-alarm))

(defun org-pomodoro-start-long-break ()
  "Start a long break immediately."
  (interactive)
  (my-org-pomodoro-remove-alarm)
  (org-pomodoro-set :pomodoro)
  (setq org-pomodoro-count -1)
  (org-pomodoro-end-in 0))

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
  (message "%s %s" my-org-pomodoro-break-id org-pomodoro-state)
  (when my-org-pomodoro-break-id
    (message "About to start clock")
    (my-org-pomodoro-start-break)))
(defun my-org-pomodoro-clear-break-end-alarm-id ()
  "Clear ‘my-org-pomodoro-break-end-alarm-event-id’."
  (setq my-org-pomodoro-break-end-alarm-event-id nil))
(defun my-org-pomodoro-break-finished-notify-hook ()
  (let ((msg "Pomodoro break finished -- get back to work!"))
    (if (fboundp 'terminal-notifier-notify)
        ;; Try to ensure timeout is very high by skipping org-notify.
        (terminal-notifier-notify "Org Pomodoro" msg 84000)
      (org-notify msg))))
(defun my-org-pomodoro-short-break-finished-punch-in ()
  "Run bh/punch-in when Pomodoro short breaks end."
  (setq org-clock-idle-time my-org-pomodoro-clock-idle-time)
  (if (executable-find "osascript")
      ;; Use osascript on macOS because I suspect ‘message-box’ of sometimes
      ;; hanging Emacs if it’s not dismissed after a while.
      (start-process "org-pomodoro-notification" "*notify*"
                     "osascript" "-e" "\
tell application \"SystemUIServer\" \
to display dialog \"Break finished - please run bh/punch-in\" \
    with title \"Org Pomodoro\" \
    default button 1 \
    buttons {\"OK\"}
activate application (path to frontmost application as text)
")
    (message-box "Break finished - please run bh/punch-in")))
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
    (message "Setting elapsed time to %s" input)
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
(defvar my-org-pomodoro-break-end-alarm-event-id nil
  "The event ID of the break-end alarm created by
‘my-org-pomodoro-finished-create-break-end-alarm’.")
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
     nil
     org-pomodoro-end-time
     nil)))
(defun my-org-pomodoro-break-finished-clear-alarm-event-id ()
  "Clear ‘my-org-pomodoro-break-end-alarm-event-id’ at end of break."
  (setq my-org-pomodoro-break-end-alarm-event-id nil))
(defun my-org-pomodoro-reschedule-alarm ()
  "Reschedule alarm created with ‘my-org-pomodoro--create-alarm-event’ to
current ‘org-pomodoro-end-time’."
  (when my-org-pomodoro-break-end-alarm-event-id
    (my-org-pomodoro--create-alarm-event
     my-org-pomodoro-alarm-gcal-calendar-id
     my-org-pomodoro-break-end-alarm-event-id
     org-pomodoro-end-time
     nil)))
(defun my-org-pomodoro-remove-alarm ()
  "Remove alarm created with ‘my-org-pomodoro--create-alarm-event’ when Pomodoro
killed."
  (when my-org-pomodoro-break-end-alarm-event-id
    (my-org-pomodoro--create-alarm-event
     my-org-pomodoro-alarm-gcal-calendar-id
     my-org-pomodoro-break-end-alarm-event-id
     org-pomodoro-end-time
     t)))


(defun my-org-pomodoro--create-alarm-event (calendar-id event-id time remove?)
  (apply
   #'async-start-process
   "pomodoro_schedule_alarm.py"
   (expand-file-name "pomodoro_schedule_alarm.py" doom-private-dir)
   ;; Store event ID in ‘my-org-pomodoro-break-end-alarm-event-id'.
   (cond
    (remove? (lambda (proc)
               (setq my-org-pomodoro-break-end-alarm-event-id nil)))
    (event-id
     (lambda (proc)))
    (t
     (lambda (proc)
       (when (= 0 (process-exit-status proc))
         (with-current-buffer (process-buffer proc)
           (setq my-org-pomodoro-break-end-alarm-event-id
                 (string-trim (buffer-substring-no-properties
                               (point-min) (point-max)))))))))
   (append
    (list
     "--calendar_id" calendar-id
     "--timestamp" (format-time-string "%FT%T%z" time)
     "--title" "org-pomodoro break end -- get back to work!")
    (when event-id
      (list "--event_id" event-id))
    (when remove?
      (list "--remove")))))



(defcustom my-org-pomodoro-log-gcal-calendar-id nil
  "The Google Calendar ID on which to create Pomodoro logs."
  :type 'string)
(defvar my-org-pomodoro-log-event-id nil
  "The event ID to update when org-pomodoro ends.")
(defvar my-org-pomodoro-log-state nil
  "The value of ‘org-pomodoro-state’ when pomodoro was logged.")
(defvar my-org-pomodoro-log-event-titles nil
  "Titles of events clocked during a pomodoro, from newest to oldest.")
(defvar my-org-pomodoro-log-event-start-time nil
  "The start time of the event ID to update when org-pomodoro ends.")
(defun my-org-pomodoro-started-create-log-event ()
  "Create Google Calendar event to log start of ‘org-pomodoro' session."
  (setq my-org-pomodoro-log-event-start-time (current-time))
  (setq my-org-pomodoro-log-event-titles (list org-clock-heading))
  (setq my-org-pomodoro-log-state org-pomodoro-state)
  (my-org-pomodoro--create-log-event
   my-org-pomodoro-log-gcal-calendar-id
   my-org-pomodoro-log-state
   my-org-pomodoro-log-event-titles
   nil
   my-org-pomodoro-log-event-start-time
   org-pomodoro-end-time))
(defun my-org-pomodoro-update-log-event (end-time)
  "Update Event with ‘my-org-pomodoro-log-event-id’ with END-TIME as well as
current ‘my-org-pomodoro-log-event-titles'."
  (when my-org-pomodoro-log-event-id
    (my-org-pomodoro--create-log-event
     my-org-pomodoro-log-gcal-calendar-id
     my-org-pomodoro-log-state
     (reverse my-org-pomodoro-log-event-titles)
     my-org-pomodoro-log-event-id
     my-org-pomodoro-log-event-start-time
     end-time)))
(defun my-org-pomodoro-ended-update-log-event ()
  "Update Google Calendar event to log end of ‘org-pomodoro' session."
  (my-org-pomodoro-update-log-event (current-time))
  (setq my-org-pomodoro-log-event-id nil
        my-org-pomodoro-log-event-start-time nil
        my-org-pomodoro-log-event-titles nil))
(defun my-org-pomodoro-update-log-event-titles ()
  "Update ‘my-org-pomodoro-log-event-titles’ with currently clocked task."
  (when (not (string= org-clock-heading
                      (car my-org-pomodoro-log-event-titles)))
    (push org-clock-heading my-org-pomodoro-log-event-titles))
  ;; Output current value of variable for easier debugging.
  my-org-pomodoro-log-event-titles)

(defun my-org-pomodoro--create-log-event
    (calendar-id state clocked-events event-id start-time end-time)
  (apply
   #'async-start-process
   "org_pomodoro_calendar_export.py"
   (expand-file-name "org_pomodoro_calendar_export.py" doom-private-dir)
   ;; Store event ID in ‘my-org-pomodoro-log-event-id'.
   (if event-id
       (lambda (proc))
     (lambda (proc)
       (when (= 0 (process-exit-status proc))
         (with-current-buffer (process-buffer proc)
           (setq my-org-pomodoro-log-event-id
                 (string-trim (buffer-substring-no-properties
                               (point-min) (point-max))))))))
   (append
    (list
     "--calendar_id" calendar-id
     "--state" (format "%s" state)
     "--start_timestamp" (format-time-string "%FT%T%z" start-time)
     "--end_timestamp" (format-time-string "%FT%T%z" end-time))
    (mapcar (lambda (x) (concat "--clocked_event=" x))
            clocked-events)
    (when event-id
      (list "--event_id" event-id)))))

(add-hook 'org-pomodoro-started-hook #'my-org-pomodoro-clear-break-end-alarm-id)
(add-hook 'org-pomodoro-started-hook #'my-org-pomodoro-started-notify-hook)
(add-hook 'org-pomodoro-started-hook #'my-org-pomodoro-started-create-log-event)
(add-hook 'org-clock-in-hook #'my-org-pomodoro-update-log-event-titles)
(add-hook 'org-pomodoro-killed-hook #'my-org-pomodoro-ended-update-log-event)
(add-hook 'org-pomodoro-finished-hook #'my-org-pomodoro-ended-update-log-event)
(add-hook 'org-pomodoro-finished-hook #'my-org-pomodoro-finished-notify-hook)
(add-hook 'org-pomodoro-finished-hook #'my-org-pomodoro-finished-lock-screen)
(add-hook 'org-pomodoro-finished-hook #'my-org-pomodoro-finished-caffeinate)
(add-hook 'org-pomodoro-finished-hook #'my-org-pomodoro-finished-clock-in-break-hook)
(add-hook 'org-pomodoro-finished-hook #'my-org-pomodoro-finished-create-break-end-alarm)
(add-hook 'org-pomodoro-tick-hook #'my-org-pomodoro-tick-current-task-reminder)
(add-hook 'org-pomodoro-break-finished-hook #'my-org-pomodoro-break-finished-notify-hook)
(add-hook 'org-pomodoro-short-break-finished-hook #'my-org-pomodoro-short-break-finished-punch-in)
(add-hook 'org-pomodoro-long-break-finished-hook #'my-org-pomodoro-long-break-finished-punch-out)
