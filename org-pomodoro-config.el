;;; ~/.doom.d/org-pomodoro-config.el -*- lexical-binding: t; -*-

;;;** Org-pomodoro

(require 'async)
(require 'deferred)
(require 'org-pomodoro)
(require 'org-pomodoro-third-time)
(require 's)

(defun my-org-pomodoro-complice-config ()
  "Set ‘org-pomodoro’ settings for Complice.co Less Wrong study hall.

https://complice.co/room/lesswrong/interstitial). Reference:
https://www.lesswrong.com/posts/hyeDFbg8ahYAu4ZJu/#586GQr5xjWBzXWda6"
  (interactive)
  (setq! org-pomodoro-length 32)
  (setq! org-pomodoro-short-break-length 8)
  (setq! org-pomodoro-long-break-length 30)
  (setq! org-pomodoro-long-break-frequency 4))

(defun my-org-pomodoro-guzey-config ()
  "Set ‘org-pomodoro’ settings for Guzey schedule.

Guzey schedule
 https://guzey.com/productivity/#how-i-work-and-rest-how-my-system-is-different-from-all-the-others-and-why-i-like-it-so-much
 Adopt a Pomodoro schedule strictly by the clock - working from :00-:25 and
 :30-:55 and taking 5-minute breaks, with 35 minute breaks every 3 hours."
  (interactive)
  (setq! org-pomodoro-length 25)
  (setq! org-pomodoro-short-break-length 5)
  (setq! org-pomodoro-long-break-length 35)
  (setq! org-pomodoro-long-break-frequency 5))
(my-org-pomodoro-complice-config)

(defun my-org-pomodoro-terminal-notifier-notify (fn title message &rest r)
  "Override ‘org-pomodoro-notify’ to use ‘terminal-notifier-notify’."
  (if (fboundp #'terminal-notifier-notify)
      (terminal-notifier-notify title message "org-pomodoro")
    (apply fn title message r)))
(advice-add #'org-pomodoro-notify
            :around #'my-org-pomodoro-terminal-notifier-notify)

(defun my-org-pomodoro-start-half ()
  "Start or set time for ‘org-pomodoro' for half of ‘org-pomodoro-length’."
  (interactive)
  (org-pomodoro-third-time-end-in (/ org-pomodoro-length 2)))
(defun my-org-pomodoro-half-on-expiry (fn &rest r)
  "Start a half-pomodoro when ‘org-pomodoro’ would prompt to reset count."
  (let ((half-p
         (and org-pomodoro-last-clock-in
              org-pomodoro-expiry-time
              (org-pomodoro-expires-p))))
    (apply fn r)
    (when half-p
      (my-org-pomodoro-start-half))))
(advice-add #'org-pomodoro :around #'my-org-pomodoro-half-on-expiry)

;; Simulate
;;
;; (setq! org-pomodoro-ticking-sound-p t)
;; (setq! org-pomodoro-ticking-sound-states '(:pomodoro))
;;
;; but use \"play\" from the SoX package so that playback is smoother and
;; takes less CPU.
(setq! org-pomodoro-ticking-sound-p nil)
(defvar org-pomodoro-ticking-volume 1.0
  "Volume for ‘my-org-pomodoro-start-tick’. Should be in range 0.0-1.0.")
(setq! org-pomodoro-ticking-volume 0.2)
(defconst org-pomodoro-ticking-process-name "*org-pomodoro-ticking-process*"
  "Name of process spawned by ‘my-org-pomodoro-start-tick’")
(defun my-org-pomodoro-start-tick ()
  "Start ticks for org-pomodoro-mode.

Requires the \"play\" executable from the SoX package
\(http://sox.sourceforge.net/sox.html)."
  (interactive)
  (when (not (and (executable-find "play")
                  (executable-find "python3")))
    (user-error "my-org-pomodoro-start-tick: python3 and play (from SoX) must be on PATH"))
  (let ((cmd
         ;; Pad with 0.79 seconds of silence because tick.wav included with
         ;; ‘org-pomodoro’ is 0.21 seconds long, to get a 1-second tick.
         (format "play --volume %f %s pad 0 0.79 repeat - </dev/null >/dev/null 2>&1"
                 org-pomodoro-ticking-volume
                 (shell-quote-argument org-pomodoro-ticking-sound))))
    (start-process
     org-pomodoro-ticking-process-name
     org-pomodoro-ticking-process-name
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
      cmd))))
(defun my-org-pomodoro-stop-tick ()
  (interactive)
  "Stop ticks for org-pomodoro-mode."
  (dolist (proc (process-list))
    (when (and (string-prefix-p org-pomodoro-ticking-process-name
                                (process-name proc))
               (process-live-p proc))
      (signal-process proc 15))))       ; SIGTERM
(defun my-org-pomodoro-change-ticking-volume (volume)
  "Change ticking volume for Pomodoro to VOLUME"
  (interactive "nVolume (0.0-1.0): ")
  (setq org-pomodoro-ticking-volume volume)
  (my-org-pomodoro-stop-tick)
  (my-org-pomodoro-start-tick))

(defun my-org-pomodoro-modify-end-time-hook ()
  "Hook to reschedule alarms when end time changed."
  (setq my-org-pomodoro-current-task-reminder-next-time nil)
  (my-org-pomodoro-update-log-event org-pomodoro-end-time)
  (my-org-pomodoro-reschedule-break-end-alarm)
  (my-org-pomodoro-reschedule-break-reminder-alarm))

(defun org-pomodoro-start-short-break (&optional no-lock)
  "Start a short break immediately.

If NO-LOCK is non-nil, don’t lock screen."
  (interactive "P")
  (my-org-pomodoro-remove-break-end-alarm)
  (org-pomodoro-set :pomodoro)
  (setq org-pomodoro-count 0)
  (when no-lock
    (setq my-org-pomodoro-inhibit-lock t)
    (run-at-time 5 nil (lambda () (setq my-org-pomodoro-inhibit-lock nil))))
  (org-pomodoro-third-time-end-now))

(defun org-pomodoro-start-long-break (&optional no-lock)
  "Start a long break immediately.

If NO-LOCK is non-nil, don’t lock screen."
  (interactive "P")
  (my-org-pomodoro-remove-break-end-alarm)
  (org-pomodoro-set :pomodoro)
  (setq org-pomodoro-count -1)
  (when no-lock
    (setq my-org-pomodoro-inhibit-lock t)
    (run-at-time 5 nil (lambda () (setq my-org-pomodoro-inhibit-lock nil))))
  (org-pomodoro-third-time-end-now))

(defcustom my-org-pomodoro-break-id nil
  "Task ID of task to clock into during Pomodoro breaks. Must specify manually."
  :type 'string)
(defun my-org-agenda-skip-pomodoro-break ()
  "Skip break in agenda."
  (when (and my-org-pomodoro-break-id
             (string= my-org-pomodoro-break-id
                      (org-entry-get (point) "ID")))
    (save-excursion
      (or
       (ignore-errors (org-forward-element)
                      (point))
       (point-max)))))
(setq org-agenda-skip-function-global #'my-org-agenda-skip-pomodoro-break)

(defvar my-org-pomodoro-inhibit-lock nil)
(cl-defun my-org-pomodoro-finished-lock-screen ()
  "Lock screen at the end of each Pomodoro work session."
  (when my-org-pomodoro-inhibit-lock
    (cl-return-from my-org-pomodoro-finished-lock-screen))
  (message "Locking screen in 15 seconds - post calendar event from *scratch*")
  (let ((current-state org-pomodoro-state))
    (run-at-time 15 nil
                 (lambda ()
                   (when (and (not my-org-pomodoro-inhibit-lock)
                              (eq org-pomodoro-state current-state))
                     (cond
                      ((eq system-type 'darwin)
                       (start-process "lock" nil "bash" "-c" "pmset displaysleepnow"))
                      ((and (executable-find "xset")
                            (not (s-blank-str? (getenv "DISPLAY"))))
                       (start-process "lock" nil "bash" "-c" "xset s activate"))
                      (t
                       (display-warning
                        'my-org-pomodoro-finished-lock-screen
                        "Can't lock screen"))))))))
(defun my-org-pomodoro-finished-caffeinate ()
  "Prevent system from idle sleeping during Pomodoro breaks."
  (let ((countdown
         (cl-case org-pomodoro-state
           (:short-break (* 60 org-pomodoro-short-break-length))
           (:long-break (* 60 org-pomodoro-long-break-length))
           (t 0))))
    (when (> countdown 0)
      (cond
       ((executable-find "caffeinate")
        (async-start-process "my-org-pomodoro-finished-caffeinate" ;
                             "caffeinate" 'ignore
                             "sleep" (number-to-string countdown)))
       (t
        (display-warning
         'my-org-pomodoro-finished-caffeinate
         "Can't prevent system from sleeping"))))))
(defun my-org-pomodoro-finished-pause-music ()
  "Pause music when Pomodoro is finished."
  (interactive)
  (cond ((executable-find "playerctl")
         (async-start-process "my-org-pomodoro-finished-pause-music"
                              "playerctl" 'ignore
                              "--all-players" "pause"))
        ((eq system-type 'darwin)
         ;; I have disabled this more general script, macos_mediakeys.py, for
         ;; now, in favor of pause-chrome-music, because I usually play music
         ;; through Chrome somehow, and this general script will start YouTube
         ;; Music if it’s paused.
         ;; ;; For whatever reason (probably because it involves the GUI session),
         ;; ;; I need to invoke macos_mediakeys.py via a terminal spawned from the
         ;; ;; GUI, rather than just executing a command. That’s why I’m telling
         ;; ;; the Terminal app via Applescript to run the command.
         ;; (async-start-process "macos_mediakeys.py"
         ;;                      "osascript"
         ;;                      'ignore
         ;;                      "-e"
         ;;                      (concat
         ;;                       "tell application \"Terminal\" to do script \""
         ;;                       (shell-quote-argument
         ;;                        (expand-file-name "macos_mediakeys.py" doom-private-dir))
         ;;                       " playpause; exit\""))
         (async-start-process "pause-chrome-music"
          "osascript"
          'ignore
          (expand-file-name "pause-chrome-music.applescript" doom-private-dir))
         nil)
        (t
         (display-warning
          'my-org-pomodoro-finished-pause-music
          "Can’t pause music"))))
(defun my-org-pomodoro-finished-sync-anki ()
  "Sync Anki - used at end of Pomodoro."
  (interactive)
  (async-start
   `(lambda ()
      ,(async-inject-variables "^\\(load-path\\|my-org-roam-directories\\)$")
      (require 'anki-editor)
      (condition-case err
          (progn
            (anki-editor--anki-connect-invoke-result 'sync)
            nil)
        (t (format "my-org-pomodoro-finished-sync-anki: %S" err))))
   (lambda (s)
     (when s
       (display-warning 'org-pomodoro-config s)))))
(defvar my-org-pomodoro-finished-org-gcal-fetch-timer nil)
(defun my-org-pomodoro-finished-org-gcal-fetch ()
  "Schedule ‘org-gcal-fetch’ one minute after pomodoro finishes.

Schedule one minute later to ensure that various tasks run at finish have had a
chance to run, since this temporarily blocks Emacs."
  (when (>= org-pomodoro-short-break-length 5)
    (setq my-org-pomodoro-finished-org-gcal-fetch-timer
          (run-at-time (* 1 60) nil #'org-gcal-fetch))))
(defvar my-org-pomodoro-finished-agenda-list-timer nil)
(defun my-org-pomodoro-finished-agenda-list ()
  "Schedule ‘org-agenda-list’ five minutes after pomodoro finishes.

Schedule five minutes later to ensure that various tasks run at finish have had a
chance to run, since refreshing the agenda blocks Emacs. Also, this allows
‘my-org-pomodoro-finished-org-gcal-fetch’ to complete."
  (setq my-org-pomodoro-finished-agenda-list-timer
        (run-at-time (* 5 60) nil #'my-org-pomodoro-agenda-list)))
(defun my-org-pomodoro-break-finished-cancel-timers ()
  "Cancel timers upon end of break."
  (when my-org-pomodoro-finished-org-gcal-fetch-timer
    (cancel-timer my-org-pomodoro-finished-org-gcal-fetch-timer)
    (setq my-org-pomodoro-finished-org-gcal-fetch-timer nil))
  (when my-org-pomodoro-finished-agenda-list-timer
    (cancel-timer my-org-pomodoro-finished-agenda-list-timer)
    (setq my-org-pomodoro-finished-agenda-list-timer nil)))
(defun my-org-pomodoro-agenda-list ()
  "Pop up ‘org-agenda-list’ buffer and refresh it."
  (org-agenda-list)
  (when org-agenda-buffer-name
    (pop-to-buffer org-agenda-buffer-name))
  (org-agenda-redo 'all))
(defun my-org-pomodoro-started-punch-in ()
  "Punch in on Pomodoro start.

Effectively the same as ‘bh/punch-in’, but since ‘org-pomodoro’ starts the
clock, we just need to set the proper variable and the bh clocking functions
will work as designed."
  (setq bh/keep-clock-running t))
(defun my-org-pomodoro-finished-notify-hook ()
  (org-pomodoro-notify "Pomodoro phase finished"
                       (format "%S" org-pomodoro-state)))
(defvar my-org-pomodoro-clock-idle-time nil
  "Variable in which ‘org-clock-idle-time’ is saved.")
(defun my-org-pomodoro-start-break ()
  "Start break - clock into task with ID my-org-pomodoro-break-id."
  (interactive)
  ;; Set org-clock-idle-time to nil to disable it during Pomodoro breaks -
  ;; sometimes Emacs will hang after the break.
  (setq my-org-pomodoro-clock-idle-time org-clock-idle-time)
  (setq org-clock-idle-time nil)
  (when my-org-pomodoro-break-id
    (if-let ((m (org-id-find my-org-pomodoro-break-id 'marker)))
        (org-with-point-at m
         (org-clock-in))
      (user-error "Could not find location of ID %S" my-org-pomodoro-break-id))))
(defun my-org-pomodoro-start-lunch ()
  (interactive)
  (org-pomodoro-notify "Going to lunch now" "")
  (setq org-pomodoro-count -1)
  (org-pomodoro-start :pomodoro)
  (org-pomodoro-third-time-end-now))
(defun my-org-pomodoro-clear-break-end-alarm-id ()
  "Clear ‘my-org-pomodoro-break-end-alarm-event-id’."
  (setq my-org-pomodoro-break-end-alarm-event-id nil))
(defun my-org-pomodoro-break-finished-notify-hook ()
  (org-pomodoro-notify
   "Break finished!" "Pomodoro break finished -- get back to work!"))

(defcustom my-org-pomodoro-alarm-gcal-calendar-id nil
  "The Google Calendar ID on which to create alarms."
  :type 'string)
(defcustom my-org-pomodoro-current-task-reminder-interval 60
  "Number of seconds between being notified of the current task. Set to nil to disable notifications"
  :type 'number)
(defcustom my-org-pomodoro-browser nil
 "If set, the browser for Org Pomodoro to use for OAuth2 requests.")

;; Produce report for count and time of total Pomodoros today.
(defvar my-org-pomodoro-count-today-var 0
  "Number of pomodoros today.")
(defvar my-org-pomodoro-time-today-var 0
  "Amount of time spent in pomodoro today, in seconds.")

(defun my-org-pomodoro-info-today ()
  "Show count of pomodoros and time spent within today."
  (interactive)
  (message "Updating info...")
  (let ((process-environment (copy-sequence process-environment)))
    (when my-org-pomodoro-browser
      (setenv "BROWSER" my-org-pomodoro-browser))
    (with-current-buffer (generate-new-buffer "*org_pomodoro_calendar_log_sum.py*")
      (let*
          ((proc
            (apply
             #'start-process
             "org_pomodoro_calendar_log_sum.py"
             (current-buffer)
             (expand-file-name "org_pomodoro_calendar_log_sum.py" doom-private-dir)
             (let* ((today-start
                     (append `(0 0 ,(or org-extend-today-until 0))
                             (nthcdr 3 (decode-time (org-current-effective-time))))))
               (append
                (list
                 "--calendar_id" my-org-pomodoro-log-gcal-calendar-id
                 "--state" ":pomodoro"
                 "--start_timestamp" (format-time-string "%FT%T%z"
                                                         (encode-time today-start))
                 "--end_timestamp" (format-time-string "%FT%T%z" (current-time))))))))
        ;; This process timeout code from
        ;; https://emacs.stackexchange.com/a/10295/17182. Not using CLI
        ;; ‘timeout’ program because it doesn’t exist on macOS.
        (with-timeout (5.0
                       (kill-process proc)
                       (error "my-org-pomodoro-info-today: process timeout after 5.0 seconds"))
          (while (process-live-p proc)
            (sit-for 0.05))
          (when (= 0 (process-exit-status proc))
            (with-current-buffer (process-buffer proc)
              (let* ((out
                      (string-trim (buffer-substring-no-properties
                                    (point-min) (point-max))))
                     (result-list (s-split "," out)))
                (setq my-org-pomodoro-count-today-var
                      (string-to-number (nth 0 result-list))
                      my-org-pomodoro-time-today-var
                      (string-to-number (nth 1 result-list)))
                (message "Org Pomodoro - Count: %2d, Time: %s"
                         my-org-pomodoro-count-today-var
                         (org-timer-secs-to-hms
                          (round my-org-pomodoro-time-today-var)))))))))))
(defun my-org-pomodoro-finished-info-today ()
  "Run ‘my-org-pomodoro-info-today’ when Pomodoro finishes."
  (display-warning
   'org-pomodoro-config
   (condition-case-unless-debug err
      (my-org-pomodoro-info-today)
     (t (format "Error: %S" err)))))
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
          (org-pomodoro-notify
           (format "Pomodoro in progress - %ds to break" remainder)
           (format "%s%s"
                   org-clock-heading
                   (if (null my-org-pomodoro-started-break-reminder-prompt)
                       ""
                     (format "\nBreak reminder: %s"
                             my-org-pomodoro-started-break-reminder-prompt))))))))))

(defvar my-org-pomodoro-break-end-alarm-event-id nil
  "The event ID of the break-end alarm created by
‘my-org-pomodoro-finished-create-break-end-alarm’.")
(defconst my-org-pomodoro-break-end-alarm-title
  "org-pomodoro break end -- get back to work!"
  "The title of the break-end alarm created by
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
     my-org-pomodoro-break-end-alarm-event-id
     'my-org-pomodoro-break-end-alarm-event-id
     my-org-pomodoro-break-end-alarm-title
     org-pomodoro-end-time
     nil)))
(defun my-org-pomodoro-break-finished-clear-alarm-event-id ()
  "Clear ‘my-org-pomodoro-break-end-alarm-event-id’ at end of break."
  (setq my-org-pomodoro-break-end-alarm-event-id nil))
(defun my-org-pomodoro-reschedule-break-end-alarm ()
  "Reschedule alarm created with ‘my-org-pomodoro--create-alarm-event’ to
current ‘org-pomodoro-end-time’."
  (when my-org-pomodoro-break-end-alarm-event-id
    (my-org-pomodoro--create-alarm-event
     my-org-pomodoro-alarm-gcal-calendar-id
     my-org-pomodoro-break-end-alarm-event-id
     'my-org-pomodoro-break-end-alarm-event-id
     my-org-pomodoro-break-end-alarm-title
     org-pomodoro-end-time
     nil)))
(defun my-org-pomodoro-remove-break-end-alarm ()
  "Remove alarm from ‘my-org-pomodoro-break-end-alarm-event-id' when Pomodoro
killed."
  (when my-org-pomodoro-break-end-alarm-event-id
    (my-org-pomodoro--create-alarm-event
     my-org-pomodoro-alarm-gcal-calendar-id
     my-org-pomodoro-break-end-alarm-event-id
     'my-org-pomodoro-break-end-alarm-event-id
     my-org-pomodoro-break-end-alarm-title
     org-pomodoro-end-time
     t)))

(defun my-org-pomodoro--create-alarm-event (calendar-id event-id event-id-var title time remove?)
  (deferred:$
    (let ((process-environment (copy-sequence process-environment)))
      (when my-org-pomodoro-browser
        (setenv "BROWSER" my-org-pomodoro-browser))
      (apply
       #'deferred:process
       (expand-file-name "org_pomodoro_schedule_alarm.py" doom-private-dir)
       (append
        (list
         "--calendar_id" calendar-id
         "--timestamp" (format-time-string "%FT%T%z" time))
        (when title
           (list "--title" title))
        (when event-id
          (list "--event_id" event-id))
        (when remove?
          (list "--remove")))))
    (deferred:nextc it
      (lambda (output)
        (cond
         (remove?
          (set event-id-var nil))
         (event-id nil)
         (t
          (set event-id-var
               (string-trim output))))))))

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
  (deferred:$
    (let ((process-environment (copy-sequence process-environment)))
      (when my-org-pomodoro-browser
        (setenv "BROWSER" my-org-pomodoro-browser))
      (apply
       #'deferred:process
       (expand-file-name "org_pomodoro_calendar_export.py" doom-private-dir)
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
    (deferred:nextc it
      (lambda (output)
        (cond
         (event-id nil)
         (t
          ;; Store event ID.
          (setq my-org-pomodoro-log-event-id (string-trim output))))))))

(defvar my-org-pomodoro-break-reminder-event-id nil
  "The event ID to update when org-pomodoro ends.")
(defvar my-org-pomodoro-started-break-reminder-prompt nil
  "Prompt for break reminders.")
(defun my-org-pomodoro-started-break-reminder-prompt (prompt)
  "Create Google Calendar event for break reminder after ‘org-pomodoro' session."
  (interactive "MBreak start reminder (leave empty for none): ")
  (let ((remove? (or (null prompt) (string-empty-p prompt))))
    (setq my-org-pomodoro-started-break-reminder-prompt
          (if remove? nil prompt))
    (unless (and remove? (null my-org-pomodoro-break-reminder-event-id))
         (my-org-pomodoro--create-alarm-event
          my-org-pomodoro-alarm-gcal-calendar-id
          my-org-pomodoro-break-reminder-event-id
          'my-org-pomodoro-break-reminder-event-id
          (if remove? "break start" prompt)
          org-pomodoro-end-time
          remove?))))
(defun my-org-pomodoro-started-break-reminder-prompt-hook ()
  "Adapt ‘my-org-pomodoro-started-break-reminder-prompt’ for hooks."
  ;; Use run-at-time to allow other hooks to run per
  ;; https://emacs.stackexchange.com/a/70970/17182.
  (run-at-time 0.1 nil
                 (lambda ()
                   (condition-case-unless-debug err
                       (progn
                         (call-interactively #'my-org-pomodoro-started-break-reminder-prompt)
                         (my-org-pomodoro-start-tick))
                     (t . (message "my-org-pomodoro-started-break-reminder-hook: error: %S" err))))))
(defun my-org-pomodoro-remove-break-reminder-alarm ()
 "Remove alarm from ‘my-org-pomodoro-break-reminder-event-id’ when Pomodoro
killed."
 (when my-org-pomodoro-break-reminder-event-id
   (my-org-pomodoro-started-break-reminder-prompt "")))

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

(defun my-org-pomodoro-reschedule-break-reminder-alarm ()
 "Reschedule alarm created with ‘my-org-pomodoro--create-alarm-event’ to
current ‘org-pomodoro-end-time’."
 (when my-org-pomodoro-break-reminder-event-id
   (my-org-pomodoro--create-alarm-event
    my-org-pomodoro-alarm-gcal-calendar-id
    my-org-pomodoro-break-reminder-event-id
    'my-org-pomodoro-break-reminder-event-id
    nil
    org-pomodoro-end-time
    nil)))

(defun org-pomodoro-kill-running ()
  "Kill the running Pomodoro."
  (interactive)
  (when (org-pomodoro-active-p)
    (org-pomodoro-kill))
  (setq org-pomodoro-count 0))
(defun my-org-pomodoro-pomodoro-light-on ()
  "Turn on my Pomodoro light lamp when Pomodoro starts."
  (interactive)
  (start-process
   "*pomodoro-light-on*"
   "*pomodoro-light-on*"
   "chronic"  ;; Don’t show output unless command fails
   "kasa"
   "--alias=Pomodoro light"
   "on"))
(defun my-org-pomodoro-pomodoro-light-off ()
 "Turn off my Pomodoro light lamp when Pomodoro starts."
 (interactive)
 (start-process
  "*pomodoro-light-off*"
  "*pomodoro-light-off*"
  "chronic"  ;; Don’t show output unless command fails
  "kasa"
  "--alias=Pomodoro light"
  "off"))

(add-hook 'org-pomodoro-started-hook #'my-org-pomodoro-clear-break-end-alarm-id)
(add-hook 'org-pomodoro-started-hook #'my-org-pomodoro-started-create-log-event)
(add-hook 'org-pomodoro-started-hook #'my-org-pomodoro-started-break-reminder-prompt-hook)
(add-hook 'org-pomodoro-started-hook #'my-org-pomodoro-started-punch-in)
(add-hook 'org-pomodoro-started-hook #'my-org-pomodoro-pomodoro-light-on)
(remove-hook 'org-pomodoro-started-hook #'my-org-pomodoro-start-tick)
(add-hook 'org-clock-in-hook #'my-org-pomodoro-update-log-event-titles)
(add-hook 'org-pomodoro-killed-hook #'my-org-pomodoro-ended-update-log-event)
(add-hook 'org-pomodoro-killed-hook #'my-org-pomodoro-remove-break-end-alarm)
(add-hook 'org-pomodoro-killed-hook #'my-org-pomodoro-remove-break-reminder-alarm)
(add-hook 'org-pomodoro-killed-hook #'my-org-pomodoro-finished-sync-anki)
(add-hook 'org-pomodoro-killed-hook #'my-org-pomodoro-finished-info-today)
(add-hook 'org-pomodoro-killed-hook #'my-org-pomodoro-pomodoro-light-off)
(add-hook 'org-pomodoro-killed-hook #'my-org-pomodoro-stop-tick)
(add-hook 'org-pomodoro-finished-hook #'my-org-pomodoro-stop-tick)
(add-hook 'org-pomodoro-finished-hook #'my-org-pomodoro-pomodoro-light-off)
(add-hook 'org-pomodoro-finished-hook #'my-org-pomodoro-finished-info-today)
(add-hook 'org-pomodoro-finished-hook #'my-org-pomodoro-ended-update-log-event)
(add-hook 'org-pomodoro-finished-hook #'my-org-pomodoro-finished-notify-hook)
(add-hook 'org-pomodoro-finished-hook #'my-org-pomodoro-finished-lock-screen)
(add-hook 'org-pomodoro-finished-hook #'my-org-pomodoro-finished-caffeinate)
(add-hook 'org-pomodoro-finished-hook #'my-org-pomodoro-finished-pause-music)
(add-hook 'org-pomodoro-finished-hook #'my-org-pomodoro-finished-sync-anki)
(add-hook 'org-pomodoro-finished-hook #'bh/punch-out)
(add-hook 'org-pomodoro-finished-hook #'my-org-pomodoro-finished-create-break-end-alarm)
(add-hook 'org-pomodoro-finished-hook #'my-org-pomodoro-finished-org-gcal-fetch)
(add-hook 'org-pomodoro-finished-hook #'my-org-pomodoro-finished-agenda-list)
(add-hook 'org-pomodoro-tick-hook #'my-org-pomodoro-tick-current-task-reminder)
(add-hook 'org-pomodoro-break-finished-hook #'my-org-pomodoro-break-finished-notify-hook)
(add-hook 'org-pomodoro-break-finished-hook #'my-org-pomodoro-break-finished-cancel-timers)
(add-hook 'org-pomodoro-third-time-modify-end-time-hook
          #'my-org-pomodoro-modify-end-time-hook)

;; Patch org-pomodoro to remove calls to ‘org-agenda-maybe-redo’.
(el-patch-feature org-pomodoro)
(after! org-pomodoro
  (el-patch-defun org-pomodoro-start (&optional state)
    "Start the `org-pomodoro` timer.
The argument STATE is optional.  The default state is `:pomodoro`."
    (when org-pomodoro-timer (cancel-timer org-pomodoro-timer))

    ;; add the org-pomodoro-mode-line to the global-mode-string
    (unless global-mode-string (setq global-mode-string '("")))
    (unless (memq 'org-pomodoro-mode-line global-mode-string)
     (setq global-mode-string (append global-mode-string
                                   '(org-pomodoro-mode-line))))

    (org-pomodoro-set (or state :pomodoro))

    (when (eq org-pomodoro-state :pomodoro)
     (org-pomodoro-maybe-play-sound :start)
     (run-hooks 'org-pomodoro-started-hook))
    (org-pomodoro-update-mode-line)
    (el-patch-remove (org-agenda-maybe-redo)))

  (el-patch-defun org-pomodoro-reset ()
    "Reset the org-pomodoro state."
    (when org-pomodoro-timer
      (cancel-timer org-pomodoro-timer))
    (setq org-pomodoro-state :none
          org-pomodoro-end-time nil)
    (org-pomodoro-update-mode-line)
    (el-patch-remove (org-agenda-maybe-redo)))

  (el-patch-defun org-pomodoro-finished ()
    "Is invoked when a pomodoro was finished successfully.
This may send a notification, play a sound and start a pomodoro break."
    (unless org-pomodoro-clock-break
       (org-clock-out nil t))
    (org-pomodoro-maybe-play-sound :pomodoro)
    (setq org-pomodoro-count (+ org-pomodoro-count 1))
    (if (zerop (mod org-pomodoro-count org-pomodoro-long-break-frequency))
        (org-pomodoro-start :long-break)
      (org-pomodoro-start :short-break))
    (org-pomodoro-notify "Pomodoro completed!" "Time for a break.")
    (org-pomodoro-update-mode-line)
    (el-patch-remove (org-agenda-maybe-redo))
    (run-hooks 'org-pomodoro-finished-hook)))

(unless (and (boundp 'org-pomodoro-third-time-mode)
             org-pomodoro-third-time-mode)
  (org-pomodoro-third-time-mode +1))
(provide 'org-pomodoro-config)
