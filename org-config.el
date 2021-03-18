;;; ~/.doom.d/org-config.el -*- lexical-binding: t; -*-

;;; Configuration for Org mode

(map! "C-c a" #'org-agenda
      "C-c b" #'org-switchb
      "C-c c" #'org-capture
      "C-c l" #'org-store-link
      "S-<f11>" #'org-clock-goto
      "C-<f11>" #'my-org-clock-in
      "C-S-<f11>" #'my-org-goto-heading
      :mode org-mode
      "C-c C-x C-i" #'org-clock-in
      "C-c C-x <C-i>" #'org-clock-in
      "C-c C-x i" #'org-clock-in
      "C-c C-x C-o" #'org-clock-out
      "C-c C-x o" #'org-clock-in)
(use-package! evil-org
  :hook (org-mode . evil-org-mode))
(after! evil-org
  (map! :map evil-org-mode-map
        (:when IS-MAC
         :mi "<s-up>" #'org-backward-element
         :mi "<s-down>" #'org-forward-element))
  (map! :m "<C-i>" #'better-jumper-jump-forward)
  (evil-define-key 'motion evil-org-mode-map
    (kbd (concat "g" (alist-get 'up evil-org-movement-bindings))) nil
    (kbd (concat "g" (alist-get 'down evil-org-movement-bindings))) nil))
(defun my-org-clock-in ()
  "Select a recently clock-in task to clock into.  See `org-clock-in'."
  (interactive) (org-clock-in '(4)))
(defun my-org-goto-heading ()
  "Run C-u org-refile to list all headings."
  (interactive)
  ;; org-file doesn't work unless it's run from within an Org buffer, so find
  ;; an arbitrary one.
  (with-current-buffer
    (save-excursion
      (catch 'aaa
        (dolist (buffer (buffer-list))
          (with-current-buffer buffer
            (when (derived-mode-p 'org-mode)
              (throw 'aaa buffer))))))
    (org-refile '(4))))

;;; Org agenda
(setq!
 org-agenda-files-name (expand-file-name "agenda_files" doom-private-dir))
(defun org-agenda-expand-files-name ()
  (let ((org-agenda-files org-agenda-files-name)) (org-agenda-files)))
(setq!
 org-agenda-files (org-agenda-expand-files-name)
 org-agenda-span 'day
 org-agenda-start-day "."
 org-agenda-start-on-weekday nil
 org-agenda-skip-deadline-prewarning-if-scheduled t)

(defun transform-square-brackets-to-curly-ones (string-to-transform)
  "Transforms [ into ( and ] into ) in STRING-TO-TRANSFORM.

Other chars left unchanged."
  (concat
   (mapcar #'(lambda (c)
               (cond ((equal c ?\[) ?\{)
                     ((equal c ?\]) ?\})
                     (t c)))
           string-to-transform)))
(defun org-clock-report-buffer (&optional no-narrow)
  "Evaluate all the clocktables in the buffer.

By default, evaluate only the clocktables in the current Org subtree, in order
to avoid recomputing all the clock tables in the buffer, which will take a
while in my daily-log.org file.  With a prefix arg NO-NARROW, evaluate all the
clocktables in the currently visible portion of the buffer."
  (interactive "P")
  (save-restriction
    (unless no-narrow
      (org-narrow-to-subtree))
    (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "#\\\+BEGIN: clocktable" nil t)
            (org-clock-report)
            (forward-line 1)))))
(defun org-timestamp-add-days (ts days)
  "Return timestamp TS shifted by a number of days.

TS is a string timestamp understandable by ‘org-parse-time-string’. DAYS is an
integer number of days.

Examples:
  (string= (org-timestamp-add-days \"2020-10-25\" 2) \"2020-10-27\")
  (string= (org-timestamp-add-days \"2020-10-25\" -2) \"2020-10-23\")
"
   (format-time-string
    "%F"
    (encode-time (decoded-time-add (org-parse-time-string ts)
                                   (make-decoded-time :day days)))))

(defcustom org-daily-log-file
  (concat org-directory "/daily-log.org")
  "The path to Org file in which daily log entries are captured."
  :type 'file)

(setq! org-capture-templates
      `(("t" "Task" entry (file (lambda () (concat org-directory "/inbox.org")))
         "
* TODO %?%^{Title}
%u
" :clock-in t :clock-resume t :jump-to-captured t)
        ("n" "Note" entry (file (lambda () (concat org-directory "/inbox.org")))
         "
* %u %?
" :jump-to-captured t)
        ("i" "Idea" entry (file (lambda () (concat org-directory "/inbox.org")))
         "
* %u %?REPLACE_ME                      :IDEA:
" :clock-in t :clock-resume t)
        ("j" "Journal" plain (file+weektree (lambda () (concat org-directory "/journal.org")))
         "
* %U %^{Title}                 :journal:
:PROPERTIES:
:Effort: 9999:00
:END:

%?
" :clock-in t :clock-resume t)
        ("d" "Drill" entry (file+headline org-default-notes-file "Drill")
         "
* Drill entry        :drill:
:PROPERTIES:
:DRILL_CARD_TYPE: hide1cloze
:Effort: 0:02
:END:
%?!|2 + 2|! equals !|4|!.
" :clock-in t :clock-resume t :jump-to-captured t)
        ("D" "Daily Log" entry
         (file (lambda ()
                 (require 'org-journal)
                 (org-journal--get-entry-path org-overriding-default-time)))
         "
* %u Daily log
:PROPERTIES:
:Effort: 0:05
:END:
** Summary
** Problem
** Insight
** NEXT [#A] Three things for the day <%(org-timestamp-add-days \"%<%F>\" +1) 06:00>
** Clocktables

#+BEGIN: clocktable :maxlevel 9 :emphasize nil :scope agenda :stepskip0 t :fileskip0 t :block %<%F> :link t :match \"Google/-MEETING\" :narrow 60!
#+END: clocktable

#+BEGIN: clocktable :maxlevel 9 :emphasize nil :scope agenda :stepskip0 t :fileskip0 t :block %<%F> :link t :match \"Google/MEETING\" :narrow 60!
#+END: clocktable

#+BEGIN: clocktable :maxlevel 9 :emphasize nil :scope agenda :stepskip0 t :fileskip0 t :block %<%F> :link t :match \"-Google\" :narrow 60!
#+END: clocktable
" :time-prompt t :clock-in t :clock-resume t :jump-to-captured t)
        ("W" "GTD weekly review" entry
         (file (lambda ()
                 (require 'org-journal)
                 (org-journal--get-entry-path org-overriding-default-time)))
         "
* NEXT %u GTD weekly review
SCHEDULED: <%<%Y-%m-%d %a 13:00-14:00>>
:PROPERTIES:
:Effort:   1:00
:END:
Follow:

- [[https://gettingthingsdone.com/wp-content/uploads/2014/10/Weekly_Review_Checklist.pdf][Weekly Review Checklist]]
- \"Weekly Review\" section in Getting Things Done.

  In 2015 edition: Chapter 8: \"Reflecting: Keeping It All Fresh and
  Functional\", section \"The Power of the Weekly Review\".

Checklist:

- GET CLEAR
  - [ ] Collect Loose Papers and Materials \\\\
    Gather all accumulated business cards, receipts, and miscellaneous
    paper-based materials into your in-tray.
  - [ ] Get “IN” to Zero \\\\
    Process completely all outstanding paper materials, journal and meeting
    notes, voicemails, dictation, and emails.
    - [ ] inbox.org files
    - [ ] Personal email
    - [ ] Personal tasks (use ~sync-tasks~)
    - [ ] Corp email
    - [ ] Corp tasks (use ~sync-tasks~)
    - [ ] [[https://b.corp.google.com/savedsearches/5024171][Buganizer: Org-mode assigned but not captured]]
    - [ ] [[https://critique.corp.google.com/#search/&q=reviewer:me+-is:submitted+-starred:me][Critique: CLs to review]]
    - [ ] Close all browser tabs (use org-capture-extension to capture)
    - [ ] Chats
  - [ ] Empty Your Head \\\\
    Put in writing and process any uncaptured new projects, action items,
    waiting for’s, someday maybe’s, etc.
- GET CURRENT
  - [ ] Review Action Lists \\\\
    Mark off completed actions. Review for reminders of further action steps to
    record.
  - [ ] Review Clock Data \\\\
    Run `org-clock-csv-calendar-export' and examine Org Clock calendar.
  - [ ] Review Previous Calendar Data \\\\
    Review past calendar in detail for remaining action items, reference data,
    etc., and transfer into the active system.
  - [ ] Review Upcoming Calendar \\\\
    Review upcoming calendar events–long and short term. Capture actions
    triggered.
  - [ ] Review Waiting For List \\\\
    Record appropriate actions for any needed follow-up. Check off received
    ones.
  - [ ] Review Project (and Larger Outcome) Lists \\\\
    Evaluate status of projects, goals, and outcomes, one by one, ensuring at
    least one current action item on each.  Browse through project plans,
    support material, and any other work-in-progress material to trigger new
    actions, completions, waiting for’s, etc.
  - [ ] Review Any Relevant Checklists \\\\
    Use as a trigger for any new actions.
- GET CREATIVE
  - [ ] Review Someday Maybe List \\\\
    Review for any projects which may now have become active, and transfer to
    “Projects.” Delete items no longer of interest.
  - [ ] Be Creative and Courageous \\\\
    Any new, wonderful, hare-brained, creative, thought-provoking, risk-taking
    ideas to add into your system???

#+BEGIN: clocktable :maxlevel 9 :emphasize nil :scope agenda :stepskip0 t :fileskip0 t :tstart \"%(org-timestamp-add-days \"%<%F>\" -6)\" :tend \"%<%F>\" :link t :match \"Google/-MEETING\" :narrow 60!
#+END: clocktable

#+BEGIN: clocktable :maxlevel 9 :emphasize nil :scope agenda :stepskip0 t :fileskip0 t :tstart \"%(org-timestamp-add-days \"%<%F>\" -6)\" :tend \"%<%F>\" :link t :match \"Google/MEETING\" :narrow 60!
#+END: clocktable

#+BEGIN: clocktable :maxlevel 9 :emphasize nil :scope agenda :stepskip0 t :fileskip0 t :tstart \"%(org-timestamp-add-days \"%<%F>\" -6)\" :tend \"%<%F>\" :link t :match \"-Google\" :narrow 60!
#+END: clocktable
" :time-prompt t :tree-type week :clock-in t :clock-resume t :jump-to-captured t)
        ("p" "Link and Text" entry (file+headline org-default-notes-file "Links")
         "
* %?REPLACE_ME
Source: [[%:link][%:description]]
#+BEGIN_SRC html
<blockquote>
%i
</blockquote>
#+END_SRC

%U
")
        ("L" "Link" entry (file+headline org-default-notes-file "Links")
         "
* %?[[%:link][%(transform-square-brackets-to-curly-ones \"%:description\")]]
  %U
" :jump-to-captured t)))

;; Create ‘C-u 2 M-x org-capture’ command to refile org-capture template under
;; headline at point.
(defvar my-org-capture-rfloc nil
  "Holds the RFLOC argument to pass to ‘org-refile’.")
(defun my-org-capture-under-headline (&optional goto keys)
  "Capture a headline using ‘org-capture’, according to the template you \
select, and then immediately refile that headline under the headline at the \
current point.  GOTO and KEYS are passed to ‘org-capture’."
  (interactive "P")
  (unwind-protect
      (progn
        (unless (eq major-mode 'org-mode)
          (user-error "Must be called from an Org-mode buffer"))
        ;; Capture refile target at point. For format see
        ;; ‘org-refile-target-table’.
        (setq my-org-capture-rfloc
              (list
               (org-display-outline-path t t nil t)
               (buffer-file-name (buffer-base-buffer))
               nil
               (org-with-wide-buffer
                (org-back-to-heading t)
                (point-marker))))
        (funcall #'org-capture nil keys)
        (when my-org-capture-rfloc
          (org-capture-goto-last-stored)
          ;; Refile last-captured target under the headline stored earlier.
          (org-refile nil nil my-org-capture-rfloc)
          ;; Ensure point is at the newly-captured and refiled headline.
          (org-refile-goto-last-stored)))
    ;; Ensure ‘my-org-capture-rfloc’ is reset.
    (setq my-org-capture-rfloc nil)))
(defun my-org-capture-under-headline-prefix (_orig-fn &rest _args)
  "Provide an additional “C-u 2” prefix arg to `org-capture'.

When ‘org-capture’ is called with this prefix argument, it will capture a
headline according to the template you select, and then immediately refile that
headline under the headline at the current point."
  (let ((goto (nth 0 _args)))
    (if (equal goto 2)
        (apply #'my-org-capture-under-headline _args)
      (apply _orig-fn _args))))
(advice-add 'org-capture :around #'my-org-capture-under-headline-prefix)

(defun org-today-overridden ()
  (if (null org-overriding-default-time)
      (org-today)
    (let* ((ot
            (decode-time org-overriding-default-time))
           (cgreg (list (nth 4 ot) (nth 3 ot) (nth 5 ot))))
      (calendar-absolute-from-gregorian cgreg))))

(defun my-org-daily-log--find-daily-log ()
   (re-search-forward
    (rx-to-string
     `(and
       line-start
       (repeat 4 "*")
       " "
       (0+ not-newline)
       ,(let ((d (calendar-gregorian-from-absolute (org-today-overridden))))
         (format "[%04d-%02d-%02d "
                 (calendar-extract-year d)
                 (calendar-extract-month d)
                 (calendar-extract-day d)))
       (0+ not-newline)
       "] Daily log"))))

(defun my-org-daily-log--find-today ()
   (re-search-forward
    (rx-to-string
     `(and
       line-start
       (repeat 3 "*")
       ,(let ((d (calendar-gregorian-from-absolute (org-today-overridden))))
         (format " %04d-%02d-%02d "
                 (calendar-extract-year d)
                 (calendar-extract-month d)
                 (calendar-extract-day d)))))))

(defun my-org-daily-log--goto-daily-log-headline ()
  (condition-case nil
    (save-excursion
      (with-current-buffer (get-file-buffer org-daily-log-file)
        (save-restriction
          (widen)
          (goto-char (point-min))
          (my-org-daily-log--find-today)
          (org-narrow-to-subtree)
          (my-org-daily-log--find-daily-log)
          (point-marker))))
    (error nil)))

(defun my-org-daily-log-goto-today ()
  "Go to today's default log, or create it if not created yet."
  (interactive)
  (require 'call-log)
  (let ((daily-log-marker (my-org-daily-log--goto-daily-log-headline)))
    (if daily-log-marker
        (progn
          (clog/msg "in marker branch")
          (switch-to-buffer (marker-buffer daily-log-marker))
          (widen)
          (goto-char (marker-position daily-log-marker))
          (org-narrow-to-subtree)
          (set-marker daily-log-marker nil))
      (progn
        (let ((org-overriding-default-time
               (or org-overriding-default-time (current-time))))
          (org-capture nil "D")
          (org-capture-finalize 'stay-with-capture)
          (org-narrow-to-subtree))))))

(setq! org-refile-targets '((org-agenda-files . (:maxlevel . 6))))
(setq! org-refile-use-outline-path t)
(setq! org-refile-use-cache t)
(run-with-idle-timer 300 t (lambda ()
                             (org-refile-cache-clear)
                             (org-refile-get-targets)))
(setq! org-alphabetical-lists t)
;; Override Doom Emacs default. I've already written too many files with my
;; value for this setting, and I don't write much code in Org-mode files, so
;; I'll live with the consequences outlined in
;; https://github.com/hlissner/doom-emacs/issues/1049#issuecomment-446837455
(setq! org-src-preserve-indentation nil)
(setq! org-src-fontify-natively t)
(setq! org-pretty-entities t)
(setq! org-use-sub-superscripts '{})
(setq! org-hide-macro-markers t)
(defun org-toggle-hide-macro-markers ()
  "Toggle the literal or descriptive display of macros."
  (interactive)
  (setq org-hide-macro-markers (not org-hide-macro-markers))
  (org-restart-font-lock))


;;;** Todo settings
(setq! org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n@/!)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))
(setq! org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("MEETING" :foreground "forest green" :weight bold)
              ("PHONE" :foreground "forest green" :weight bold))))
(setq! org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING") ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))
;;;** Agenda

;; Don't show tasks in agenda that are done
(setq! org-agenda-skip-scheduled-if-done t)
(setq! org-agenda-skip-deadline-if-done t)

;; Skip tasks in the global TODO list that are done or scheduled, because
;; either of these means the tasks has been considered. Tasks marked with a
;; deadline still need to be scheduled before I've truly considered them, so
;; leave them in.
(setq! org-agenda-todo-ignore-scheduled 'future)

(defcustom my-org-agenda-active-days 14
  "Number of days in the past to search for active projects.")
(cl-defun my-org-agenda-someday-maybe (&optional buffer)
  "Show agenda for Someday/Maybe tasks.

Use `org-ql-search' to search."
  (interactive)
  (org-ql-search
    (org-agenda-files)
    `(and
      (todo)
      (tags "HOLD")
      (not (tags "CANCELLED" "ARCHIVED"))
      (not (scheduled :from 1)))
    :super-groups '((:auto-map my-org-super-agenda-group-by-project-or-task-group))
    :buffer (or buffer org-ql-view-buffer)
    :title "Someday/Maybe tasks"))
(cl-defun my-org-agenda-waiting (&optional buffer)
  "Show agenda for NEXT steps in org-mode projects

Use `org-ql-search' to search for all WAITING tasks."
  (interactive)
  (org-ql-search
    (org-agenda-files)
    `(and
      (todo)
      (tags "WAITING")
      (not (tags "HOLD" "CANCELLED" "ARCHIVED"))
      (not (scheduled :from 1)))
    :super-groups '((:auto-map my-org-super-agenda-group-by-project-or-task-group))
    :buffer (or buffer org-ql-view-buffer)
    :sort 'date
    :title "WAITING for tasks"))
(cl-defun my-org-agenda-ready-projects (&optional buffer)
  "Show agenda for ready projects (those with all children done)

Use `org-ql-search' to search for all loose TODOs."
  (interactive)
  (org-ql-search
    (org-agenda-files)
    `(and
      (todo "TODO")
      (children)
      (not (children (todo)))
      (not (tags "HOLD" "CANCELLED" "ARCHIVED"))
      (not (scheduled :from 1))
      (not (bh/skip-subprojects)))
    :super-groups '((:auto-map my-org-super-agenda-group-by-project-or-task-group))
    :title "Ready projects (those with all children done)"
    :buffer (or buffer org-ql-view-buffer)))
(cl-defun my-org-agenda-loose-todos (&optional buffer)
  "Show agenda for Loose TODOs (those not part of projects)

Use `org-ql-search' to search for all loose TODOs."
  (interactive)
  (org-ql-search
    (org-agenda-files)
    `(and
      (todo "TODO")
      (not (children))
      (not (tags "HOLD" "CANCELLED" "ARCHIVED"))
      (not (scheduled :from 1))
      (not (bh/skip-subprojects)))
    :super-groups '((:auto-map my-org-super-agenda-group-by-project-or-task-group))
    :title "Loose TODOs (not part of projects)"
    :buffer (or buffer org-ql-view-buffer)))
(cl-defun my-org-agenda-stuck-projects (&optional buffer)
  "Show agenda for projects with stuck tasks

Use `org-ql-search' to search."
  (interactive)
  (org-ql-search
    (org-agenda-files)
    `(and
      (not (done))
      (not (todo "NEXT"))
      (not (tags "HOLD" "CANCELLED" "ARCHIVED"))
      (not (scheduled :from 1))
      (not (bh/skip-non-subprojects)))
    :super-groups '((:auto-map my-org-super-agenda-group-by-project-or-task-group))
    :title "Tasks making project stuck"
    :buffer (or buffer org-ql-view-buffer)))
(cl-defun my-org-agenda-next-tasks (&optional buffer)
  "Show agenda for NEXT steps in org-mode projects

Use `org-ql-search' to search for all NEXT steps for projects.  Show only the
NEXT steps that have a timestamp within the last `my-org-agenda-active-days'
days."
  (interactive)
  (org-ql-search
    (org-agenda-files)
    `(and
      (todo "NEXT")
      (not (tags "HOLD" "CANCELLED" "ARCHIVED"))
      (not (scheduled :from 1))
      (or (not (children))
          (children (not (todo))))
      (ts :from ,(- my-org-agenda-active-days)))
    :buffer (or buffer org-ql-view-buffer)
    :super-groups '((:auto-map my-org-super-agenda-group-by-project-or-task-group))
    :sort 'date
    :title (format
            "NEXT (grouped by parent, except scheduled for future, %d-day active)"
            my-org-agenda-active-days)))
(cl-defun my-org-agenda-archivable-tasks (&optional buffer)
  "Show agenda for Archivable tasks

Consider these types of headlines for archiving:

- Headlines with a *done* todo keyword.
- Headlines with *no* todo keyword tagged with \"gcal\" - these are
  entries created by org-gcal. If I'm actively managing such a task,
  I'll always add a todo keyword of some kind to the heading, so these
  tasks will be saved from archiving unless they're marked done.

Only consider top-level tasks in project trees - don't individually archive
tasks that are part of an ongoing project. Only archive projects that have been
done for at least 30 days.

Daily log entries (marked by the \"dailylog\" tag) should never be
archived.

Use `org-ql-search' to search."
  (interactive)
  (org-ql-search
    (org-agenda-files)
    `(and
      (not (tags "REFILE" "dailylog" "ARCHIVE"))
      (not (ts :from -30))
      (not (property "recurrence"))     ; Exclude parents of recurring events
      (or (done)
          (and (tags "gcal")
               (not (todo))))
      (or (not (parent))
          (parent (and (not (todo)) (not (done)))))
      (or (not (children))
          (descendants
           (and (not (todo))
                (not (ts :from -30))))))
    :buffer (or buffer org-ql-view-buffer)
    :super-groups '((:auto-map my-org-super-agenda-group-by-project-or-task-group))
    :sort 'date
    :title "Archivable tasks"))
(defcustom my-org-agenda-old-gcal-archive-files
  '("~/Documents/org/home-org/gcal.org_archive")
  "List of files for ‘my-org-agenda-old-gcal-tasks' to search.")
(cl-defun my-org-agenda-old-gcal-tasks (&optional buffer)
  "Show agenda for old GCal tasks that can be deleted.

Look for tasks with property ‘org-gcal-calendar-id-property' that are older than
90 days, have never been clocked, have no children, and are not parents of
recurring events. These tasks are merely cached versions of events on Google
Calendar that I’ve never interacted with, and can thus be deleted without any
ill effect.

Use ‘org-ql-search' to search."
  (interactive)
  (org-ql-search
    (org-agenda-files t t)
    `(and
      (not (ts :from -90))
      (property ,org-gcal-calendar-id-property)
      (not (property "recurrence"))     ; Exclude parents of recurring events
      (not (property "ID"))             ; Exclude events that might be linked to
      (not (property "CUSTOM_ID"))
      (not (clocked))
      (not (children)))
    :buffer (or buffer org-ql-view-buffer)
    :super-groups '((:auto-ts))
    :sort 'date
    :title "Old GCal tasks to delete."))
(defmacro my-org-agenda-ql-wrapper (wrapper-name wrapped-func-name)
  "Defines a wrapper for use in `org-agenda-custom-commands'.

Calling this macro will define a function named WRAPPER-NAME that wraps
WRAPPED-FUNC-NAME in order to be called by
`org-agenda-custom-commands'.  WRAPPED-FUNC-NAME must be passed an unused
argument when called in `org-agenda-custom-commands'."
  `(cl-defun ,wrapper-name (unused)
     ,(format "Wrap `%s' for `org-agenda'" wrapped-func-name)
     (with-current-buffer org-agenda-buffer-name
       (,wrapped-func-name (current-buffer)))))
(my-org-agenda-ql-wrapper my-org-agenda-someday-maybe-agenda-command
                          my-org-agenda-someday-maybe)
(my-org-agenda-ql-wrapper my-org-agenda-waiting-agenda-command
                          my-org-agenda-waiting)
(my-org-agenda-ql-wrapper my-org-agenda-ready-projects-agenda-command
                          my-org-agenda-ready-projects)
(my-org-agenda-ql-wrapper my-org-agenda-loose-todos-agenda-command
                          my-org-agenda-loose-todos)
(my-org-agenda-ql-wrapper my-org-agenda-stuck-projects-agenda-command
                          my-org-agenda-stuck-projects)
(my-org-agenda-ql-wrapper my-org-agenda-next-tasks-agenda-command
                          my-org-agenda-next-tasks)
(my-org-agenda-ql-wrapper my-org-agenda-archivable-tasks-agenda-command
                          my-org-agenda-archivable-tasks)
(my-org-agenda-ql-wrapper my-org-agenda-old-gcal-tasks-agenda-command
                          my-org-agenda-old-gcal-tasks)

(defun org-agenda-archive-default-or-down ()
  "Call ‘org-agenda-archive-default’, or ‘org-agenda-next-line' if not on a headline."
  (interactive)
  (condition-case err
      (funcall-interactively #'org-agenda-archive-default)
    (t
     (pcase-let ((`(user-error ,msg) err))
       (if (string= msg "Command not allowed in this line")
           (funcall-interactively #'org-agenda-next-line)
         (user-error msg))))))
(defun org-agenda-kill-or-down ()
  "Call ‘org-agenda-kill’, or ‘org-agenda-next-line' if not on a headline."
  (interactive)
  (condition-case err
      (funcall-interactively #'org-agenda-kill)
    (t
     (pcase-let ((`(user-error ,msg) err))
       (if (string= msg "Command not allowed in this line")
           (funcall-interactively #'org-agenda-next-line)
         (user-error msg))))))
(after! org-agenda
  (setq! org-agenda-confirm-kill nil)
  (org-defkey org-agenda-mode-map "\C-k" #'org-agenda-kill-or-down)
  (org-defkey org-agenda-mode-map "$" #'org-agenda-archive-default-or-down))
(after! org-super-agenda
  (org-defkey org-super-agenda-header-map "$"
              #'org-agenda-archive-default-or-down))


(setq! org-agenda-span 1)
(setq my-org-agenda-export-options
      ;; Use defaults for now, but leave available for future customization
      '())
(setq! org-agenda-custom-commands '())
(add-to-list 'org-agenda-custom-commands
             '("H" "Someday/Maybe"
               ((my-org-agenda-someday-maybe-agenda-command ""))))
(add-to-list 'org-agenda-custom-commands
             `("W" "WAITING"
               ((my-org-agenda-waiting-agenda-command ""))))
(add-to-list 'org-agenda-custom-commands
             `("P" "Ready projects (those with all children done)"
               ((my-org-agenda-ready-projects-agenda-command ""))
               ((org-agenda-write-buffer-name
                 "Ready projects (those with all children done)")
                (org-agenda-exporter-settings
                  my-org-agenda-export-options))
               "~/Downloads/agenda-P-export.pdf"))
(add-to-list 'org-agenda-custom-commands
             `("U" "Loose TODOs (not part of projects)"
               ((my-org-agenda-loose-todos-agenda-command ""))
               ((org-agenda-write-buffer-name
                 "Loose TODOs (not part of projects)")
                (org-agenda-exporter-settings
                  my-org-agenda-export-options))
               "~/Downloads/agenda-U-export.pdf"))
(add-to-list 'org-agenda-custom-commands
             '("u" "Tasks making projects stuck"
               ((my-org-agenda-stuck-projects-agenda-command ""))
               ((org-agenda-write-buffer-name
                 "Tasks making projects stuck")
                (org-agenda-exporter-settings
                  my-org-agenda-export-options))
               "~/Downloads/agenda-u-export.pdf"))
(add-to-list 'org-agenda-custom-commands
             `("n" "NEXT (active, grouped by parent, except scheduled for future)"
               ((my-org-agenda-next-tasks-agenda-command ""))
               ((org-agenda-write-buffer-name
                 "NEXT (active, grouped by parent, except scheduled for future)")
                (org-agenda-exporter-settings
                  my-org-agenda-export-options))
               "~/Downloads/agenda-n-export.pdf"))
(add-to-list 'org-agenda-custom-commands
             '("A" "Archivable tasks"
               ((my-org-agenda-archivable-tasks-agenda-command ""))
               ((org-tags-match-list-sublevels nil)
                (org-agenda-archives-mode nil))))
(add-to-list 'org-agenda-custom-commands
             '("c" "Old GCal tasks"
               ((my-org-agenda-old-gcal-tasks-agenda-command ""))
               ((org-tags-match-list-sublevels nil)
                (org-agenda-archives-mode nil)
                (org-agenda-confirm-kill nil))))
(add-to-list 'org-agenda-custom-commands
             '("Q" . "Custom queries"))
(add-to-list 'org-agenda-custom-commands
             '("Q/" "Archive occur"
               ;; Dynamically bind `org-agenda-text-search-extra-files' with the
               ;; symbol `agenda-archives' prepended to search archive files when
               ;; calling `org-occur-in-agenda-files'.
               (lambda (unused)
                 (let* ((tmp (if (boundp 'org-agenda-text-search-extra-files)
                                 org-agenda-text-search-extra-files
                               '()))
                        (org-agenda-text-search-extra-files
                         (cond ((null tmp) '(agenda-archives))
                               ((equal (car tmp) 'agenda-archives) tmp)
                               (t (cons 'agenda-archives tmp)))))
                   (call-interactively 'org-occur-in-agenda-files)))
               ""))
(setq! org-agenda-custom-commands org-agenda-custom-commands)
(defvar my-org-agenda-combined-output-file
  "~/Downloads/agenda-export.pdf"
  "Output PDF of ‘my-org-agenda-write-combined’.")
(defun my-org-agenda--get-export-file (key)
  (nth 3 (alist-get key org-agenda-custom-commands nil nil #'string=)))
(defun my-org-agenda-write-combined ()
  "Combine several agenda views into one PDF suitable for printing"
  (interactive)
  (require 'call-log)
  ;; (org-store-agenda-views)
  (call-process "pdfnup" nil (get-buffer-create "*Async Shell Command*") nil
                "--nup" "2x2" "--no-landscape"
                "-o" (expand-file-name my-org-agenda-combined-output-file)
                (expand-file-name (my-org-agenda--get-export-file "n"))
                (expand-file-name (my-org-agenda--get-export-file "u"))
                (expand-file-name (my-org-agenda--get-export-file "U")))
  (clog/msg "Wrote %s" my-org-agenda-combined-output-file))

(setq! org-stuck-projects
      '("TODO={TODO\\|NEXT}-HOLD-CANCELLED-REFILE" ("NEXT" "HOLD") nil ""))

(setq! org-columns-default-format "%60ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM %10CLOCKSUM_T")
(setq! org-global-properties
      (quote (("Effort_ALL" . "0:05 0:10 0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 8:00")
              ("SYTLE_ALL" . "habit"))))

;;; Keybindings to jump to org agenda entries and narrow buffer.
(defun org-agenda-switch-to-and-narrow (&optional delete-other-windows)
  (interactive)
  (org-agenda-switch-to delete-other-windows)
  (org-narrow-to-subtree))
(defun org-agenda-goto-and-narrow (&optional highlight)
  (interactive)
  (org-agenda-goto highlight)
  (org-narrow-to-subtree))
(after! org-agenda
  (org-defkey org-agenda-mode-map (kbd "<C-tab>") #'org-agenda-goto-and-narrow)
  (org-defkey org-agenda-mode-map (kbd "<C-return>") #'org-agenda-switch-to-and-narrow))

;; Easier-to-use alias of C-c C-^
(org-defkey org-mode-map (kbd "C-c C-6") 'org-up-element)

(org-defkey org-mode-map (kbd "C-c C-1") 'org-time-stamp-inactive)

;;;** Idle time

;;; Make idle time more accurate on Linux (X idle time rather than just Emacs
;;; idle time)
(setq! org-clock-idle-time 15)
(when (eq system-type 'gnu/linux)
  (let ((xprintidle (executable-find "xprintidle")))
    (if xprintidle
        (setq! org-clock-x11idle-program-name xprintidle)
      (display-warning
       'environment
       "xprintidle should be installed for accurate idle time on Linux."))))

;;; Enable notifications on OS X using the terminal-notifier program.
(defcustom terminal-notifier-command
  (executable-find "terminal-notifier")
  "The path to terminal-notifier."
  :type 'file)
(defun terminal-notifier-notify (title message &optional timeout)
  "Show a message with `terminal-notifier-command'."
  (apply
   #'start-process
   "terminal-notifier"
   "*terminal-notifier*"
   terminal-notifier-command
   "-title" title
   "-message" message
   "-sender" "org.gnu.Emacs"
   (append
    (when timeout (list "-timeout" timeout)))))
(when terminal-notifier-command
  (setq! org-show-notification-handler
        (lambda (message) (terminal-notifier-notify "Org Mode" message))))

;; Notify when Emacs wants to read from minibuffer.
(setq! minibuffer-auto-raise t)
(defun my-read-from-minibuffer-alert (fn prompt &rest r)
  "Prompt when ‘read-from-minibuffer’ is called and frame is not focused."
  (require 'alert)
  (unless (or noninteractive
              (frame-focus-state (selected-frame)))
    (terminal-notifier-notify "read-from-minibuffer" prompt))
  (apply fn prompt r))
(advice-add #'read-from-minibuffer :around #'my-read-from-minibuffer-alert)

;;; Ask for effort estimate when clocking in, but only when an effort estimate
;;; is not present. Based on http://orgmode.org/worg/org-hacks.html#sec-1-9-10
;;; but simplified by using the org-set-effort function, which is called
;;; interactively here and so provides a prompt with autocompletion.
(add-hook 'org-clock-in-prepare-hook
          'my-org-mode-ask-effort)
(defun my-org-mode-ask-effort ()
  "Ask for an effort estimate when clocking in."
  (let ((todo (nth 2 (org-heading-components))))
    (message "todo %s" todo)
    (when (and (null (org-entry-get-multivalued-property (point) "Effort"))
               (not org-capture-mode))
      (org-set-effort))))

;;; Show current task in frame title
(setq! org-clock-clocked-in-display 'frame-title)
(setq! org-clock-frame-title-format '("" "%b - " org-mode-line-string))

;;; Play sound when effort has expired.
(setq! org-clock-sound
       (expand-file-name
        ;; Sound source:
        ;; http://soundbible.com/1496-Japanese-Temple-Bell-Small.html
        "Japanese Temple Bell Small-SoundBible.com-113624364.wav"
        doom-private-dir))

;;;; org-refile settings:
;;;;
;;;; Based on http://doc.norang.ca/org-mode.html#Refiling and
;;;; https://blog.aaronbieber.com/2017/03/19/organizing-notes-with-refile.html
(setq! org-refile-targets '((nil :maxlevel . 9)
                            (org-agenda-files :maxlevel . 9)))
(setq! org-refile-use-outline-path 'buffer-name)
;;; Targets complete directly with Ivy, so no need to complete in steps.
(setq! org-outline-path-complete-in-steps nil)
;;; Allow refile to create parent tasks with confirmation
(setq! org-refile-allow-creating-parent-nodes 'confirm)
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))
(setq! org-refile-target-verify-function 'bh/verify-refile-target)

;; Display images inline, but not too wide by default.
(setq! org-startup-with-inline-images t)
;; Override ‘org--create-inline-image’ to set ‘:max-width’ rather than ‘:width’.
;; This will resize image widths up to a max of their original width, rather
;; than taking up the whole buffer, which prevents smaller images from becoming
;; pixelated due to being resized larger than their original size.
(defun org--create-inline-image (file width)
  "Create image located at FILE, or return nil.
WIDTH is the width of the image.  The image may not be created
according to the value of `org-display-remote-inline-images'.

NOTE: Local override of internal function."
  (let* ((remote? (file-remote-p file))
         (file-or-data
          (pcase org-display-remote-inline-images
            ((guard (not remote?)) file)
            (`download (with-temp-buffer
                         (set-buffer-multibyte nil)
                         (insert-file-contents-literally file)
                         (buffer-string)))
            (`cache (let ((revert-without-query '(".")))
                      (with-current-buffer (find-file-noselect file)
                        (buffer-string))))
            (`skip nil)
            (other
             (message "Invalid value of `org-display-remote-inline-images': %S"
                      other)
             nil))))
    (when file-or-data
      (create-image file-or-data
                    (and (image-type-available-p 'imagemagick)
                         width
                         'imagemagick)
                    remote?
                    :max-width width))))
(defun org-resize-inline-images-hook (frame)
  "Hook to update Org-mode image width in resized Org-mode windows.

Iterates over all buffers in FRAME."
  (let ((buffers (delete-dups (mapcar #'window-buffer (window-list frame)))))
    (dolist (buf buffers)
      (with-current-buffer buf
        (org-resize-inline-images)))))
(defvar-local org-resize-inline-images--timer nil)
(defun org-redisplay-inline-images-in-buffer (buffer)
  "Redisplay inline images in Org-mode buffer BUFFER."
  (setq org-resize-inline-images--timer nil)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
        (org-redisplay-inline-images))))
(defun org-resize-inline-images ()
  "Update Org-mode image size in current buffer after window is resized."
  (when
      (and (eq major-mode 'org-mode)
           (not (= (window-pixel-width)
                   (window-pixel-width-before-size-change))))
    (when org-resize-inline-images--timer
      (cancel-timer org-resize-inline-images--timer))
    (setq org-resize-inline-images--timer
          (run-at-time
           1 nil #'org-redisplay-inline-images-in-buffer (current-buffer)))
    (setq-local org-image-actual-width
                (list (floor (* 0.95 (window-pixel-width)))))))
(add-hook 'window-size-change-functions #'org-resize-inline-images-hook)
(add-hook 'org-mode-hook #'org-resize-inline-images)
(setq! org-image-actual-width '(800))

(after! org-pomodoro
  (load! "org-pomodoro-config.el"))

(add-to-list 'org-modules 'org-id)      ; Needed for ID links to work
(after! org-id
  (setq! org-id-locations-file
         (expand-file-name ".org-id-locations.el"
                           user-emacs-directory)))
(defcustom distraction-id nil
  "Task ID of task to clock into for distracting tasks (Hacker News, Reddit, etc.). Must specify manually."
  :type 'string)
(defun distraction-clock-in ()
  "Start distracted time."
  (interactive)
  (save-excursion
    (org-id-goto distraction-id)
    (org-clock-in)))

;;;; Make tag selection more intuitive
;;;; See https://blog.aaronbieber.com/2016/03/05/playing-tag-in-org-mode.html
(defun air--org-swap-tags (tags)
  "Replace any tags on the current headline with TAGS.

The assumption is that TAGS will be a string conforming to Org Mode's
tag format specifications, or nil to remove all tags."
  (let* ((old-tags (org-get-tags-string))
         (tags (if tags
                   (concat " " tags)
                 "")))
    (save-excursion
      (beginning-of-line)
      (re-search-forward
       (concat "[ \t]*" (regexp-quote old-tags) "[ \t]*$")
       (line-end-position)
       t)
      (replace-match tags)
      (org-set-tags tags))))
(defun air-org-set-tags (tag)
  "Add TAG if it is not in the list of tags, remove it otherwise.

TAG is chosen interactively from the global tags completion table."
  (interactive
   (list (let ((org-last-tags-completion-table
                (if (derived-mode-p 'org-mode)
                    (org-uniquify
                     (delq nil (append (org-get-buffer-tags)
                                       (org-global-tags-completion-table))))
                  (org-global-tags-completion-table))))
           (org-icompleting-read
            "Tag: " 'org-tags-completion-function nil nil nil
            'org-tags-history))))
  (let* ((cur-list (org-get-tags nil t))
         (new-tags (mapconcat 'identity
                              (if (member tag cur-list)
                                  (delete tag cur-list)
                                (append cur-list (list tag)))
                              ":"))
         (new (if (> (length new-tags) 1) (concat " :" new-tags ":")
                nil)))
    (air--org-swap-tags new)))
(defun air-org-set-tags-ctrl-c-ctrl-c-hook ()
  (let* ((context (org-element-context))
         (type (org-element-type context)))
    (if (or (eq type 'headline)
            (eq type 'inlinetask))
        (save-excursion (goto-char (org-element-property :begin context))
                        (call-interactively #'air-org-set-tags)
                        t)
      nil)))
(add-hook 'org-ctrl-c-ctrl-c-hook #'air-org-set-tags-ctrl-c-ctrl-c-hook)
(org-defkey org-mode-map (kbd "C-c C-q") #'air-org-set-tags)


;;;** Org-gcal
(after! alert
  (setq! alert-default-style
         (cond ((executable-find "notify-send")
                'libnotify)
               ((eq system-type 'darwin)
                'notifier)
               (t 'message))))
(after! org-gcal
  (setq! org-gcal-config-file (expand-file-name "org-gcal-config.el" doom-private-dir))
  ;; Disable Auto Archive - my gcal.org_archive is so big that this majorly
  ;; slows down every fetch. Instead, I'll just archive old entries once a
  ;; month along with the rest of the entries to be archived.
  (setq! org-gcal-auto-archive nil)
  (setq! org-gcal-remove-api-cancelled-events nil)
  (setq! org-gcal-recurring-events-mode 'nested)
  (when (file-exists-p org-gcal-config-file)
    (load org-gcal-config-file)))

(defvar org-gcal-debug nil)
(defun org-gcal-toggle-debug ()
  "Toggle debugging flags for ‘org-gcal'."
  (interactive)
  (cond
   (org-gcal-debug
    (setq
     debug-on-error (cdr (assq 'debug-on-error org-gcal-debug))
     debug-ignored-errors (cdr (assq 'debug-ignored-errors org-gcal-debug))
     deferred:debug (cdr (assq 'deferred:debug org-gcal-debug))
     deferred:debug-on-signal
     (cdr (assq 'deferred:debug-on-signal org-gcal-debug))
     org-gcal-debug nil)
    (message "org-gcal-debug DISABLED"))
   (t
    (setq
     org-gcal-debug
     `((debug-on-error . ,debug-on-error)
       (debug-ignored-errors . ,debug-ignored-errors)
       (deferred:debug . ,deferred:debug)
       (deferred:debug-on-signal . ,deferred:debug-on-signal))
     debug-on-error '(error)
     ;; These are errors that are thrown by various pieces of code that
     ;; don’t mean anything.
     debug-ignored-errors (append debug-ignored-errors
                                  '(scan-error file-already-exists))
     deferred:debug t
     deferred:debug-on-signal t)
    (message "org-gcal-debug ENABLED"))))


(defun my-org-gcal-schedule ()
  "Suggest a default schedule time for the event at point and create/update it \
using ‘org-gcal-post-at-point’.

Default suggestions (in the absence of existing data in the entry):

- Calendar ID: first entry in ‘org-gcal-file-alist’
- Start time: tomorrow at 10 AM
- End time: start time plus effort. Prompt for effort if not already present.
"
  (interactive)
  (require 'org-gcal)
  (save-excursion
    (org-back-to-heading)
    (let* ((elem (org-element-at-point))
           (tobj (org-element-property :scheduled elem))
           (duration (org-element-property :EFFORT elem))
           (calendar-id
            (org-entry-get (point) "calendar-id")))
      (unless calendar-id
        (setq calendar-id
              (read-from-minibuffer "Calendar ID: "
                                    (caar org-gcal-file-alist)))
        (org-entry-put (point) org-gcal-calendar-id-property calendar-id))
      ;; Set SCHEDULED time if not already present.
      (unless (plist-get (cadr tobj) :hour-start)
        (org-schedule nil "+1d 10:00")
        (org-schedule nil))
      (unless duration
        (org-set-effort))
      (setq elem (org-element-at-point))
      (setq tobj (org-element-property :scheduled elem)
            ;; By default, set duration to effort minus clocked time with
            ;; adjustments.
            duration
            (let ((min-duration 5)      ; Minimum event duration
                  (resolution 5))       ; Event resolution
              (org-duration-from-minutes
               (max
                min-duration
                ;; Round up to the nearest multiple of ‘resolution’ minutes.
                (* resolution
                   (ceiling
                    (/ (- (org-duration-to-minutes (org-element-property
                                                    :EFFORT elem))
                          (org-clock-sum-current-item))
                       resolution)))))))
      (when (and (= (plist-get (cadr tobj) :hour-start)
                    (plist-get (cadr tobj) :hour-end))
                 (= (plist-get (cadr tobj) :minute-start)
                    (plist-get (cadr tobj) :minute-end)))
        (let* ((duration (read-from-minibuffer "Duration: " duration))
               (duration-minutes (org-duration-to-minutes duration))
               (duration-seconds (* 60 duration-minutes))
               (end-time (org-timestamp-from-time
                          (time-add (org-timestamp-to-time tobj)
                                    duration-seconds)
                          'with-time)))
          ;; Add SCHEDULED time with start and end times filled out.
          (org-add-planning-info
           'scheduled
           (concat
            (org-timestamp-format tobj "%Y-%m-%d %a %H:%M")
            "-"
            (org-timestamp-format end-time "%H:%M")))))
      ;; Finally, create/update event with information added to entry.
      (org-gcal-post-at-point 'skip-import))))
(defun my-org-gcal-schedule-now ()
  "Schedule event at point to current time, then call ‘my-org-gcal-schedule'."
  (interactive)
  (org-schedule nil (format-time-string ". %H:%M"))
  (call-interactively #'my-org-gcal-schedule))

;;;** Org-drill
(use-package! org-drill
  :commands (org-drill)
  :config
  (setq! org-drill-scope 'agenda-with-archives)
  (setq! org-drill-match "-ARCHIVE")
  (setq! org-drill-left-cloze-delimiter "!|")
  (setq! org-drill-right-cloze-delimiter "|!")
  (setq! org-drill-add-random-noise-to-intervals-p t)
  (setq! org-drill-adjust-intervals-for-early-and-late-repetitions-p t)
  (setq! org-drill-learn-fraction 0.3)
  (defun my-org-drill-global-visual-line-mode (org-drill &rest r)
    "Wrap `org-drill' to turn on `global-visual-line-mode' during drills.

This is needed because `org-drill-table' only works on `org-mode' tables, which
don't support wrapping."
    (let (old-global-visual-line-mode global-visual-line-mode)
      (unwind-protect
          (progn
            (global-visual-line-mode t)
            (apply org-drill r))
        (global-visual-line-mode old-global-visual-line-mode))))
  (defun my-org-drill-maximize-frame (fn &rest r)
    "Maximize frame when ‘org-drill' is run."
    (let* ((frame (selected-frame))
           (maximized? (eq 'maximized (frame-parameter frame 'fullscreen))))
      (when (not maximized?)
        (toggle-frame-maximized frame))
      (apply fn r)
      (when (not maximized?)
        (toggle-frame-maximized frame))))
  (advice-add #'org-drill :around #'my-org-drill-maximize-frame)
  (after! (org-capture org-drill)
    (map! :map org-mode-map
          "C-c d" #'org-drill-type-inbox-init
          "C-c D" #'org-drill-type-inbox-remove)
    (map! :map org-capture-mode-map
          "C-c d" #'my-org-capture-defer-task
          "C-c D" #'org-drill-type-inbox-remove)))
(defun org-drill-type-inbox-init ()
  "Mark headline as card of the inbox type."
  (interactive)
  (org-with-point-at (point)
    ;; Set properties to skip first few steps of review.
    (org-entry-put (point) "DRILL_LAST_INTERVAL" "4.5234")
    (org-entry-put (point) "DRILL_REPEATS_SINCE_FAIL" "2")
    (org-entry-put (point) "DRILL_TOTAL_REPEATS" "1")
    (org-entry-put (point) "DRILL_FAILURE_COUNT" "0")
    (org-entry-put (point) "DRILL_AVERAGE_QUALITY" "5.0")
    (org-entry-put (point) "DRILL_EASE" "2.6")
    (org-entry-put (point) "DRILL_LAST_QUALITY" "5")
    ;; Add inbox tag
    (org-set-tags
     (cl-remove-duplicates
      (cons "inbox" (org-get-tags nil 'local))
      :test #'string=))))
(defun org-drill-type-inbox-remove ()
  "Remove all inbox-related data from headline at point."
  (interactive)
  (org-with-point-at (point)
    (when (member "inbox" (org-get-tags nil 'local))
      (org-drill-strip-entry-data)
      ;; Remove inbox tag
      (org-set-tags
       (seq-filter (lambda (tag) (not (equal tag "inbox")))
                   (org-get-tags nil 'local))))))
(defun org-drill-review-inbox ()
  "Review inbox cards."
  (interactive)
  (require 'org-drill)
  (let ((org-drill-question-tag "inbox"))
    (call-interactively #'org-drill)))
;; Need to eagerly load because my Org files call functions declared by this
;; file in their local variables.
(require 'org-drill)


;;;* org-fc
(use-package! org-fc
  :custom
  (org-fc-directories '("~/Documents/org"))
  (org-fc-custom-contexts
   '((inbox . (:filter (tag "inbox")))
     (reviews . (:filter (not (tag "inbox")))))))
(after! (evil org-fc)
  (add-to-list 'evil-emacs-state-modes 'org-fc-dashboard-mode)
  (evil-define-minor-mode-key '(normal insert emacs) 'org-fc-review-flip-mode
    (kbd "RET") 'org-fc-review-flip
    (kbd "p") 'org-fc-review-edit
    (kbd "D") 'org-fc-type-inbox-remove
    (kbd "E") 'org-fc-review-edit
    (kbd "n") 'org-fc-review-flip
    (kbd "s") 'org-fc-review-suspend-card
    (kbd "q") 'org-fc-review-quit)
  (evil-define-minor-mode-key '(normal insert emacs) 'org-fc-review-rate-mode
    (kbd "a") 'org-fc-review-rate-again
    (kbd "p") 'org-fc-review-edit
    (kbd "D") 'org-fc-type-inbox-remove
    (kbd "E") 'org-fc-review-edit
    (kbd "h") 'org-fc-review-rate-hard
    (kbd "g") 'org-fc-review-rate-good
    (kbd "e") 'org-fc-review-rate-easy
    (kbd "s") 'org-fc-review-suspend-card
    (kbd "q") 'org-fc-review-quit))
(defun my-org-capture-defer-task ()
  "Defer the task at point to a later time."
  (interactive)
  (org-drill-type-inbox-init)
  (org-capture-finalize))
(defun org-fc-review-inbox ()
  "Run ‘org-fc’ review on the ‘inbox’ context (see ‘org-fc-custom-contexts')."
  (interactive)
  (org-fc-review (alist-get 'inbox (org-fc-contexts))))
(defun org-fc-review-reviews ()
  "Run ‘org-fc’ review on the ‘reviews’ context (see ‘org-fc-custom-contexts')."
  (interactive)
  (org-fc-review (alist-get 'reviews (org-fc-contexts))))
(defun org-fc-type-inbox-init ()
  "Mark headline as card of the inbox type."
  (interactive)
  (org-with-point-at (point)
    (org-fc-type-normal-init)
    (org-fc--add-tag "inbox")
    (org-fc-set-review-data
     `(("front" "2.5" "2" "2.0"
        ,(format-time-string
          org-fc-timestamp-format
          (encode-time (decoded-time-add (decode-time nil "UTC")
                                         (make-decoded-time :day 2)))
          "UTC"))))))

(defcustom org-fc-review-schedule-alist nil
  "Alist mapping review contexts to IDs of org-mode headlines.

These headlines will be rescheduled by ‘org-fc-review-schedule’. If a context
is not present or is mapped to nil, that context won’t have a schedule task
associated with it.")
(defun org-fc-review-schedule ()
  "Schedule review tasks for ‘org-fc’.

For each context present in ‘org-fc-review-schedule-alist’, find the associated
task and reschedule it."
  (interactive)
  (dolist (entry org-fc-review-schedule-alist)
    (pcase-let* ((`(,context-name . ,task-id) entry)
                 (context (alist-get context-name (org-fc-contexts)))
                 (max-time '(999999 999999)) ; Actually 4046-10-13
                 (next-due
                  (seq-reduce
                   (lambda (x entry)
                     (let ((min-due
                            (seq-reduce
                             (lambda (x position)
                               (let ((due (plist-get position :due)))
                                 (if (time-less-p x due) x due)))
                             (plist-get entry :positions)
                             max-time)))
                       (if (time-less-p x min-due) x min-due)))
                   (org-fc-index context)
                   max-time)))
      (org-with-point-at (org-id-find task-id 'marker)
        (let ((inhibit-message t))
          (org-schedule
           nil (format-time-string (org-time-stamp-format) next-due)))))))

(defun org-fc-review-remove ()
  "Remove all ‘org-fc’ data from the note at the current point."
  (interactive)
  (org-fc-with-point-at-entry
   (atomic-change-group
     (org-fc--remove-tag org-fc-suspended-tag)
     (org-fc--remove-tag org-fc-flashcard-tag)
     (org-delete-property org-fc-created-property)
     (org-delete-property org-fc-type-property)
     (org-delete-property org-fc-type-cloze-type-property)
     (org-delete-property org-fc-type-cloze-max-hole-property)
     (pcase-let* ((`(,start . ,end) (org-fc-review-data-position t))
                  (outside-drawer
                   (save-excursion
                     (goto-char start)
                     (forward-line -1)
                     (point))))
       (delete-region start end)
       (save-excursion
         (goto-char outside-drawer)
         (org-remove-empty-drawer-at outside-drawer)))))
  (org-fc-review-reset)
  (org-fc-review-next-card))
(defun org-fc-type-inbox-remove ()
  "Remove item at point from ‘org-fc' inbox."
  (interactive)
  (org-fc-with-point-at-entry
    (org-fc--remove-tag "inbox")
    (org-fc-review-remove)))

;;;* Org-roam
(use-package! org-roam
  :hook (after-init . org-roam-mode)
  :bind
  (:map org-roam-mode-map
   (("C-c n l" . org-roam)
    ("C-c n t" . org-roam-dailies-today)
    ("C-c n f" . org-roam-find-file)
    ("C-c n g" . org-roam-show-graph))
   (:map org-mode-map
    ("C-c n i" . org-roam-insert)
    ("C-c n u" . org-roam-unlinked-references))))
(after! org-roam
  (require 'org-roam-compat)
  (setq! org-roam-directory "~/Documents/org/home-org/roam")
  (setq! org-roam-link-title-format "§%s")
  (setq! org-roam-completion-system 'ivy)
  (defun my-org-roam-capture-split-window (&rest _args)
    "Split current window and select new window."
    (unless (eq org-roam-capture--context 'ref)
      (select-window (split-window))))
  (advice-add 'org-roam--capture :before #'my-org-roam-capture-split-window)
  (defun my-org-roam-set-buffer-name-hook ()
    "Set buffer name of org-roam files."
    (when-let (((org-roam--org-roam-file-p))
               (name (car (org-roam--extract-titles))))
      (rename-buffer name)))
  (add-hook 'find-file-hook #'my-org-roam-set-buffer-name-hook)
  (require 'org-roam-protocol)
  (require 'org-roam-capture)
  (nconc (assoc "d" org-roam-capture-templates)
         '(:immediate-finish t :jump-to-captured t))
  (nconc (assoc "r" org-roam-capture-ref-templates)
         '(:immediate-finish t :jump-to-captured t
           ;; Use headline to populate title for org-roam bookmark instead of
           ;; #+title file-level property so that I can easily run
           ;; ‘org-drill-type-inbox-init’ to defer the task.
           :head "#+roam_key: ${ref}\n\n* ${title}\n:PROPERTIES:\n:link: [[${ref}][${title}]]\n:END:")))


(defun org-roam-create-note-from-headline ()
  "Create an Org-roam note from the current headline and jump to it.

Normally, insert the headline’s title using the ’#title:’ file-level property
and delete the Org-mode headline. However, if the current headline has a
Org-mode properties drawer already, keep the headline and don’t insert
‘#+title:'. Org-roam can extract the title from both kinds of notes, but using
‘#+title:’ is a bit cleaner for a short note, which Org-roam encourages."
  (interactive)
  (let ((title (nth 4 (org-heading-components)))
        (has-properties (org-get-property-block)))
    (org-cut-subtree)
    (org-roam-find-file title nil nil 'no-confirm)
    (org-paste-subtree)
    (unless has-properties
      (kill-line)
      (while (outline-next-heading)
        (org-promote)))
    (goto-char (point-min))
    (when has-properties
      (kill-line)
      (kill-line))))

(defcustom my-org-roam-directories (list org-roam-directory)
  "List of org-roam directories to examine in ‘my-org-roam-agenda-file-hook’.")
(defun my-org-roam-agenda-file-hook ()
  "Add recently-modified org-roam files to ‘org-agenda-files’."
  (interactive)
  (let* ((since
          (format-time-string
           "%Y-%m-%d"
           (encode-time (decoded-time-add (decode-time nil)
                                          (make-decoded-time :day -60))))))
    (async-start-process
     "org-roam-agenda-update"
     "sh"
     (lambda (proc)
       (let ((lines
              (split-string
               (with-current-buffer (process-buffer proc)
                 (buffer-string))
               "\n" t)))
         (setq!
          org-agenda-files
          (append (org-agenda-expand-files-name) lines))))
     "-c"
     (string-join
      `("for dir in"
        ,(string-join (mapcar (lambda (x)
                                (shell-quote-argument (expand-file-name x)))
                              my-org-roam-directories)
                      " ")
        "; do cd \"$dir\";"
        "git log --after" ,since "--oneline | awk '{print $1}' |"
        "xargs -n1 sh -c 'git diff --name-only \"$0\" \"${0}~1\"' |"
        "sort | uniq | grep ^roam/ | grep '\.org$' |"
        "grep -v ^roam/andy-matuschak-notes | tr '\\n' '\\0' |"
        "xargs -0 -n1 sh -c"
        "'printf \"%s\\n\" \"$(git rev-parse --show-toplevel)/$0\"';"
        "done")
      " "))))
(run-with-idle-timer 5 nil #'my-org-roam-agenda-file-hook)
(run-with-idle-timer (* 60 3) t #'my-org-roam-agenda-file-hook)

(use-package org-journal
  :bind
  ("C-c n j" . org-journal-new-entry)
  (:map org-journal-mode-map
    ("C-c n p" . org-journal-previous-entry)
    ("C-c n n" . org-journal-next-entry))
  :custom
  (org-journal-date-prefix "#+title: ")
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-date-format "%A, %d %B %Y")
  (org-journal-time-prefix "* ")
  (org-journal-carryover-items ""))
(after! (org-journal org-roam)
  (setq! org-journal-dir org-roam-directory))
(defun org-journal-add-agenda-link ()
  "Add link to agenda to current file."
  (cl-assert (string= org-journal-file-format
                      "%Y-%m-%d.org"))
  (require 'f)
  (newline)
  (org-insert-link
   nil
   (format "elisp:(let ((org-agenda-sticky nil) (org-agenda-window-setup 'reorganize-frame)) (org-agenda-list nil \"%s\"))"
           (f-base (buffer-file-name)))
   "(agenda)"))
(add-hook 'org-journal-after-header-create-hook #'org-journal-add-agenda-link)



;;;* Useful packages suggested by
;;;* https://blog.jethro.dev/posts/zettelkasten_with_org/.
(after! org-attach
  (setq! org-attach-id-dir "data/"))
(use-package! org-download
  :hook (dired-mode . org-download-enable)
  :custom
  (org-download-method 'attach)
  (org-download-backend "curl \"%s\" -o \"%s\""))

;;;** norang configuration

;; Stolen from http://doc.norang.ca/org-mode.html#Clocking
;; bh/organization-task-id changed.

;; Show lot of clocking history so it's easy to pick items off the C-F11 list
(setq! org-clock-history-length 23)
;; Resume clocking task on clock-in if the clock is open
(setq! org-clock-in-resume t)
;; Change tasks to NEXT when clocking in
(setq! org-clock-in-switch-to-state 'bh/clock-in-to-next)
;; Separate drawers for clocking and logs
(setq! org-drawers (quote ("PROPERTIES" "LOGBOOK")))
;; Save clock data and state changes and notes in the LOGBOOK drawer
(setq! org-clock-into-drawer t)
;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq! org-clock-out-remove-zero-time-clocks t)
;; Clock out when moving task to a done state
(setq! org-clock-out-when-done t)
;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq! org-clock-persist t)
;; Do not prompt to resume an active clock
(setq! org-clock-persist-query-resume nil)
;; Enable auto clock resolution for finding open clocks
(setq! org-clock-auto-clock-resolution (quote when-no-clock-is-running))
;; Include current clocking task in clock reports
(setq! org-clock-report-include-clocking-task t)
;; Create globally unique entry IDs when needed
(setq! org-id-link-to-org-use-id t)

(defcustom org-clock-csv-calendar-export-id nil
  "Calendar to export clock data to.  Used by ‘org-clock-csv-calendar-export'.")
(defun org-clock-csv-calendar-export ()
  "Export Org-clock data to Google Calendar with ID \
‘org-clock-csv-calendar-export-id'."
  (interactive)
  (let ((csv-buffer (org-clock-csv nil 'no-switch)))
    (save-excursion
      (with-current-buffer csv-buffer
        (call-process-region
         nil nil
         (expand-file-name "org_clock_csv_calendar_export.py" doom-private-dir)
         nil (get-buffer-create "*org_clock_csv_calendar_export.py*") nil
         "--org_clock_csv" "/dev/stdin"
         "--calendar_id" org-clock-csv-calendar-export-id
         "--logging_level" "DEBUG")))))

;; Reset day at 4 AM, just like Anki.
(setq! org-extend-today-until 4)

(setq! org-html-htmlize-output-type 'css)

;;; Recompute effort of a parent headline from the efforts of the children if
;;; they sum to a higher value.
(defun my-org-update-heading-effort-from-children (marker)
  "Compute the sum of efforts for each child of the heading at MARKER.

If the sum is greater than the current effort for this heading, offer to update
it.  This function is called recursively on each child, so the entire tree's
efforts may be updated by this function."
  (require 'call-log)                   ; For clog/msg
  (let*
      ((abort-at-marker)
       (ret
        (catch
            'break
          (org-with-point-at marker
            (clog/msg "At %S (%s)" (point-marker) (org-get-heading))
            (org-narrow-to-subtree)
            (outline-show-all)
            (let*
                ((current-effort
                  (org-duration-to-minutes
                   (or (org-entry-get marker org-effort-property) 0)))
                 (children-effort 0))
              (save-excursion
                (save-restriction
                  (when (org-goto-first-child)
                    ;; Use while loop with empty body to simulate a C do-while
                    ;; loop - in other words, we test at the end of the loop
                    ;; "body" whether a next sibling exists.
                    (while
                        (let ((x (my-org-update-heading-effort-from-children (point-marker))))
                         (clog/msg "x = %S" x)
                         (setq children-effort (+ children-effort (nth 0 x)))
                         (org-get-next-sibling))))))
              (let ((children-effort-duration
                     (org-duration-from-minutes children-effort)))
                (when (< current-effort children-effort)
                  (pcase (read-char-choice
                          (format
                           "Update effort in \"%s\" to children's sum (%s)? (y,n,j) "
                           (org-get-heading 'no-tags 'no-todo 'no-priority 'no-comment)
                           children-effort-duration)
                          '(?y ?n ?j))
                    (?n nil)
                    (?y
                     (org-entry-put
                      marker org-effort-property children-effort-duration)
                     (setq current-effort children-effort))
                    (?j
                     (setq abort-at-marker marker)
                     (throw 'break 'abort-at-marker)))))
              (list current-effort (point-max-marker)))))))
    (pcase ret
      ('abort-at-marker
       (clog/msg "%S" abort-at-marker)
       (pop-to-buffer-same-window (marker-buffer abort-at-marker))
       (set-buffer (marker-buffer abort-at-marker))
       (goto-char (marker-position abort-at-marker))
       'abort)
      ('abort 'abort)
      (_ ret))))
(defun my-org-effort-from-children-hook ()
  "Update effort of a heading from its children before clocking in."
  (pcase (my-org-update-heading-effort-from-children (point-marker))
    ('abort 'abort)
    (_ nil)))
(add-hook 'org-clock-in-prepare-hook 'my-org-effort-from-children-hook)

(defun my-org-update-heading-effort-from-children-all ()
  "Run over all projects, updating their efforts from their children.

Pressing ‘j’ will abort the run, leaving the point at the heading we were at
when ‘j’ was pressed."
  (interactive)
  (require 'call-log)
  (org-map-entries
   (lambda ()
     (display-buffer (current-buffer) '(display-buffer-same-window))
     (recenter nil)
     (pcase (my-org-effort-from-children-hook)
       ('abort
        (clog/msg "'abort")
        (setq org-map-continue-from (point))
        (let ((debug-on-quit nil))
          (signal 'quit nil)))
       (x x)))
   nil 'agenda #'bh/skip-tasks)
  (clog/msg "Updating efforts complete."))

;;; bh clocking functions
(setq! bh/keep-clock-running nil)

(defun bh/clock-in-to-next (kw)
  "Switch a task from TODO to NEXT when clocking in.
Skips capture tasks, projects, and subprojects.
Switch projects and subprojects from NEXT back to TODO"
  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
    (cond
     ((and (member (org-get-todo-state) (list "TODO"))
           (bh/is-task-p))
      "NEXT")
     ((and (member (org-get-todo-state) (list "NEXT"))
           (bh/is-project-with-active-tasks-p))
      "TODO"))))

(defun bh/find-project-task ()
  "Move point to the parent (project) task if any."
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(defun bh/punch-in (arg)
  "Start continuous clocking and set the default task to the
selected task.  If no task is selected set the Organization task
as the default task."
  (interactive "p")
  (setq bh/keep-clock-running t)
  (if (equal major-mode 'org-agenda-mode)
      ;;
      ;; We're in the agenda
      ;;
      (let* ((marker (org-get-at-bol 'org-hd-marker))
             (tags (org-with-point-at marker (org-get-tags-at))))
        (if (and (eq arg 4) tags)
            (org-agenda-clock-in '(16))
          (bh/clock-in-organization-task-as-default)))
    ;;
    ;; We are not in the agenda
    ;;
    (save-restriction
      (widen)
      ;; Find the tags on the current task
      (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
          (org-clock-in '(16))
        (bh/clock-in-organization-task-as-default)))))

(defun bh/punch-out ()
  (interactive)
  (setq bh/keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out))
  (org-agenda-remove-restriction-lock))

(defun bh/clock-in-default-task ()
  (save-excursion
    (org-with-point-at org-clock-default-task
      (org-clock-in))))

(defun bh/clock-in-parent-task ()
  "Move point to the parent (project) task if any and clock in"
  (let ((parent-task))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (not parent-task) (org-up-heading-safe))
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (if parent-task
            (org-with-point-at parent-task
              (org-clock-in))
          (when bh/keep-clock-running
            (bh/clock-in-default-task)))))))

(defcustom bh/organization-task-id nil
  "Task ID of default Organization task (for use with bh/clock-in-organization-task-as-default. Must specify manually."
  :type 'string)

(defun bh/clock-in-organization-task-as-default ()
  (interactive)
  (org-with-point-at (org-id-find bh/organization-task-id 'marker)
    (org-clock-in '(16))))

(defun bh/clock-out-maybe ()
  (when (and bh/keep-clock-running
             (not org-clock-clocking-in)
             (marker-buffer org-clock-default-task)
             (not org-clock-resolving-clocks-due-to-idleness))
    (bh/clock-in-parent-task)))

(add-hook 'org-clock-out-hook 'bh/clock-out-maybe 'append)

;; Needed for clocking functions: http://doc.norang.ca/org-mode.html#Projects
(defun bh/is-project-p ()
  "Any task with a todo keyword subtask."
  (save-restriction
    (widen)
    (let ((is-a-task
           (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (and is-a-task (not (bh/is-task-p))))))

(defun bh/is-project-with-active-tasks-p ()
  "Any task with a non-done todo keyword subtask."
  (save-restriction
    (widen)
    (let ((is-a-task
           (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (and is-a-task
           (let ((has-non-done-subtask))
             (save-excursion
               (when (org-goto-first-child)
                 (while (and (not has-non-done-subtask)
                             (org-goto-sibling))
                   (when (member (org-get-todo-state) org-not-done-keywords)
                     (setq has-non-done-subtask t)))))
             has-non-done-subtask)))))

(defun bh/is-subproject-p ()
  "Any task which is a subtask of another project"
  (let ((is-subproject)
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (while (and (not is-subproject) (org-up-heading-safe))
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq is-subproject t))))
    (and is-a-task is-subproject)))

(defun bh/is-task-p ()
  "Any task with a todo keyword and no todo keyword subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1))
          (top-level (org-current-level)))
      (save-excursion
        (while (and
                ;; is-a-task never changes - use it for early exit.
                is-a-task
                (not has-subtask)
                (outline-next-heading)
                (> (org-current-level) top-level))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))

(defun bh/skip-subprojects ()
  "Skip subprojects (including both projects and leaf tasks)."
  (save-restriction
    (widen)
    (cond
     ((not (bh/is-subproject-p)) nil)
     (t
      (let ((next-headline
             (save-excursion (or (outline-next-heading) (point-max)))))
        next-headline)))))

(defun bh/skip-non-tasks ()
  "Show leaf tasks.  Skip projects (including subprojects)."
  (save-restriction
    (widen)
    (let ((is-a-task
           (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (cond
       ((and is-a-task (bh/is-task-p)) nil)
       (t
        (let ((next-headline
               (save-excursion (or (outline-next-heading) (point-max)))))
          next-headline))))))

(defun bh/skip-tasks ()
  "Show projects (including subprojects).  Skip leaf tasks."
  (save-restriction
    (widen)
    (let ((is-a-task
           (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (cond
       ((and is-a-task (bh/is-project-p)) nil)
       (t
        (let ((next-headline
               (save-excursion (or (outline-next-heading) (point-max)))))
          next-headline))))))


(defun bh/skip-non-subprojects ()
  "Show subprojects (including both projects and leaf tasks)."
  (save-restriction
    (widen)
    (cond
     ((bh/is-subproject-p) nil)
     (t
      (let ((next-headline
             (save-excursion (or (outline-next-heading) (point-max)))))
        next-headline)))))

(defun my-org-super-agenda-group-by-project-or-task-group (item)
  "Output the name of the parent headline of the current headline.

In order to ensure that tasks that are part of projects are sorted before loose
tasks (tasks not part of projects), the name of the parent headline is prefixed
with “P: ” if it contains a TODO keyword and “TG: ” (for “task group”)
otherwise."
  (org-super-agenda--when-with-marker-buffer
    (org-super-agenda--get-marker item)
    (let* ((parent-title)
           (parent-has-todo)
           (parent-outline-path))
      (save-excursion
        (when (org-up-heading-safe)
          (setq parent-title (org-get-heading 'notags 'notodo))
          (setq parent-outline-path
                (let ((p (org-get-outline-path)))
                  (if p
                      (format " | %s" (string-join p "/"))
                    "")))
          (setq parent-has-todo
                (member (nth 2 (org-heading-components)) org-todo-keywords-1))))
      (when parent-title
        (if parent-has-todo
            (format "P: %s%s" parent-title parent-outline-path)
          (format "TG: %s%s" parent-title parent-outline-path))))))
;;;;


(defun my-org-archive-dwim (&optional find-done)
  "Tag heading with ARCHIVE tag if it's not the top-level of a project, or it’s
located in an Org-roam file. Otherwise, archive the subtree to a file.

FIND-DONE has the same meaning "
  (interactive "P")
  (cond
   ((bh/is-subproject-p)
    (org-toggle-archive-tag find-done))
   ((string-match-p "/roam/" buffer-file-name)
    (org-toggle-archive-tag find-done))
   (t
    (org-archive-subtree find-done))))
(setq! org-archive-default-command #'my-org-archive-dwim)
(after! org-agenda
  (org-defkey org-agenda-mode-map "$" #'org-agenda-archive-default))

;;; Find all inactive timestamps in tree, buffer, or all org buffers
;;; https://lists.gnu.org/archive/html/emacs-orgmode/2011-07/msg01228.html
(defun org-find-timestamps ()
  "Find inactive timestamps within a date-range and maybe sort them.

This function can help to bring the notes, that you take within `org-mode',
into a chronological order, even if they are scattered among many different
nodes.  The result is somewhat like a diary, listing your notes for each
successive day.  Please be aware however: This intended usage requires, that
you routinely insert inactive timestamps into the notes that you write.

`org-find-timstamps' works by creating a regular expression to match a given
range of dates, doing a search for it and displaying the results either as a
sparse tree or with the help of occur.  The original buffer is not modified.
"
  (interactive)
  (require 'call-log)
  (let ((occur-buffer-name "*Occur*")
        (occur-header-regex "^[0-9]+ match\\(es\\)?") ;; regexp to match for header-lines in *Occur* buffer
        first-date
        last-date
        pretty-dates
        swap-dates
        (days 0)
        date-regex
        position-before-year
        collect-method
        buff
        org-buffers)
    (save-window-excursion
      ;; temporary buffer for date-manipulations
      (with-temp-buffer
        ;; ask user for date-range
        (setq last-date (org-read-date nil nil nil "End date (or start): " nil nil))
        (setq first-date (org-read-date nil nil nil "Start date (or end): " nil nil))
        ;; swap dates, if required
        (when (string< last-date first-date)
          (setq swap-dates last-date)
          (setq last-date first-date)
          (setq first-date swap-dates))
        (setq pretty-dates (concat "from " first-date " to " last-date))
        ;; construct list of dates in working buffer
        ;; loop as long we did not reach end-date
        (while (not (looking-at-p last-date))
          (end-of-buffer)
          ;; only look for inactive timestamps
          (insert "[")
          (setq position-before-year (point))
          ;; Monday is probably wrong, will be corrected below
          (insert first-date " Mo]\n")
          (goto-char position-before-year)
          ;; advance number of days and correct day of week
          (org-timestamp-change days 'day)
          (setq days (1+ days)))

        (end-of-buffer)
        ;; transform constructed list of dates into a single, optimized regex
        (setq date-regex (regexp-opt (split-string (buffer-string) "\n" t)))))


    ;; ask user, which buffers to search and how to present results
    (setq collect-method
          (car (split-string (org-icompleting-read "Please choose, which buffers to search and how to present the matches: "
                                                   '("multi-occur -- all org-buffers, list" "org-occur -- this-buffer, sparse tree") nil t nil nil "occur -- this buffer, list"))))

    ;; Perform the actual search
    (save-window-excursion
      (cond ((string= collect-method "occur")
             (occur date-regex))

            ((string= collect-method "org-occur")
             (if (string= major-mode "org-mode")
                 (org-occur date-regex)
               (error "Buffer not in org-mode")))

            ((string= collect-method "multi-occur")
             ;; construct list of all org-buffers
             (dolist (buff (buffer-list))
               (set-buffer buff)
               (if (string= major-mode "org-mode")
                   (setq org-buffers (cons buff org-buffers))))
             (multi-occur org-buffers date-regex))))

    ;; Postprocessing: Optionally sort buffer with results
    ;; org-occur operates on the current buffer, so we cannot modify its results afterwards
    (if (string= collect-method "org-occur")
        (clog/msg (concat "Sparse tree with matches " pretty-dates))
      ;; switch to occur-buffer and modify it
      (if (not (get-buffer occur-buffer-name))
          (clog/msg (concat "Did not find any matches " pretty-dates))
        (let ((original-inhibit-read-only inhibit-read-only))
          (unwind-protect
              (progn
                ;; next line might be risky, so we unwind-protect it
                (setq inhibit-read-only t)
                (set-buffer occur-buffer-name)
                (goto-char (point-min))
                ;; beautify the occur-buffer by replacing the potentially long original regexp
                (while (search-forward (concat " for \"" date-regex "\"") nil t)
                  (replace-match "" nil t))
                (goto-char (point-min))
                ;; Sort results by matching date ?
                (when (y-or-n-p "Sort results by date ? ")
                  (when (string= collect-method "multi-occur")
                    ;; bring all header lines ('xx matches for ..') to top of buffer, all lines with matches to bottom
                    (sort-subr t
                               'forward-line
                               'end-of-line
                               ;; search-key for this sort only differentiates between header-lines and matche-lines
                               (lambda () (if (looking-at-p occur-header-regex) 2 1))
                               nil))

                  ;; goto first line of matches
                  (goto-char (point-max))
                  (search-backward-regexp occur-header-regex)
                  (forward-line)
                  ;; sort all matches according to date, that matched the regex
                  (sort-subr t
                             'forward-line
                             'end-of-line
                             ;; search-key for this sort is date
                             (lambda () (search-forward-regexp date-regex) (match-string 0))
                             nil
                             'string<))
                ;; pretend, that we did not modify the occur-buffer
                (insert "Searched " pretty-dates "\n")
                (goto-char (point-min))
                (set-buffer-modified-p nil)
                (clog/msg (concat "occur-buffer with matches " pretty-dates "(`C-h m' for help)")))

            (setq inhibit-read-only original-inhibit-read-only))))



      ;; switch to occur-buffer
      (if (get-buffer occur-buffer-name)
          (switch-to-buffer occur-buffer-name)))))

;;; Don't insert hard spaces to indent text with heading in Org mode
(setq! org-adapt-indentation nil)
(setq! org-startup-indented nil)

;;; Turn off some org-mode eye candy for performance (see
;;; https://github.com/hlissner/doom-emacs/blob/develop/docs/faq.org#why-is-scrolling-slow-in-emacsdoom)
(remove-hook 'org-mode-hook #'org-superstar-mode)
(setq org-fontify-quote-and-verse-blocks nil
      org-fontify-whole-heading-line nil
      org-hide-leading-stars nil)

(require 'org-inlinetask)

(use-package! org-randomnote
  :bind ("C-c r" . org-randomnote)
  :config
  (setq! org-randomnote-candidates
         (remove-if
          (lambda (x)
            (string-match-p "/gcal.org$" x))
          (org-agenda-files))))

(setq! org-enforce-todo-dependencies t)
(setq! org-enforce-todo-checkbox-dependencies t)

(setq! org-log-done (quote time))
(setq! org-log-redeadline (quote time))
(setq! org-log-reschedule (quote time))
(defun my-org-log-next-action ()
  "Prompt for the very next action to take when changing heading status to NEXT.

Include a checkbox to force acknowledging the action provided before marking
the item done."
  (when (string= org-log-note-state "NEXT")
      (goto-char (point-max))
      (insert "- [ ] The *very next* action to take")
      (search-backward "The"))
  (when (string= org-log-note-state "WAITING")
      (goto-char (point-max))
      (insert "- [ ] What *exactly* am I waiting on?")
      (search-backward "What")))
(add-hook 'org-log-buffer-setup-hook #'my-org-log-next-action)

;;; Week in review (https://emacs.stackexchange.com/a/7864)
(defcustom org-timeline-files nil
  "The files to be included in `org-timeline-all-files'.

Follows the same rules as `org-agenda-files'"
  :type 'sexp)

(setq! org-timeline-files org-agenda-files)

(add-to-list 'org-agenda-custom-commands
             '("R" "Week in review"
               agenda ""
               ;; agenda settings
               ((org-agenda-span 'week)
                (org-agenda-start-on-weekday 0) ;; start on Sunday
                (org-agenda-overriding-header "Week in Review (no archives)")
                (org-agenda-files
                 (let ((org-agenda-files org-timeline-files))
                   (org-agenda-files nil 'ifmode)))
                (org-agenda-log-mode-items '(clock state closed))
                ;; Ignore scheduled and deadline tasks, showing only log entries
                (org-agenda-start-with-log-mode 'only)
                ;; Don't include archive files - I won't have archived items in
                ;; the past week.
                (org-agenda-archives-mode nil))))

(use-package! org-super-agenda
  :config
  (setq! org-super-agenda-groups
         '(;; Each group has an implicit boolean OR operator between its selectors.
           (:name "Today"  ; Optionally specify section name
            :time-grid t  ; Items that appear on the time grid
            :todo "TODAY")  ; Items that have this TODO keyword
           (:name "Important" :priority "A")
           ;; Set order of multiple groups at once
           (:name "Habits" :habit t)
           (:name "Inbox" :tag "inbox")
           (:name "Drill" :tag "drill")
           (:todo "WAITING")
           (:name "Today" :scheduled today :deadline today)
           (:name "Overdue" :scheduled past :deadline past)
           (:todo ("SOMEDAY" "TO-READ" "CHECK" "TO-WATCH" "WATCHING")
            ;; Show this group at the end of the agenda (since it has the
            ;; highest number). If you specified this group last, items
            ;; with these todo keywords that e.g. have priority A would be
            ;; displayed in that group instead, because items are grouped
            ;; out in the order the groups are listed.
            :order 9)
           (:priority<= "B"
            ;; Show this section after "Today" and "Important", because
            ;; their order is unspecified, defaulting to 0. Sections
            ;; are displayed lowest-number-first.
            :order 1)))
  ;; After the last group, the agenda will display items that didn't
  ;; match any of these groups, with the default order position of 99
  (org-super-agenda-mode 1))

;; Use sticky agenda's so they persist
(setq! org-agenda-sticky t)

(defadvice! my-org-agenda-no-dim-checkboxes (_orig-fn &rest _args)
  :around #'org-agenda--mark-blocked-entry
  "Don't dim tasks with empty checkboxes in org-agenda.

This lets me enable ‘org-agenda-dim-blocked-tasks' and
‘org-enforce-todo-checkbox-dependencies’ simultaneously without dimming tasks
with empty todo checkboxes."
  (let ((org-blocker-hook (remove #'org-block-todo-from-checkboxes
                                  org-blocker-hook)))
    (apply _orig-fn _args)))

(setq! org-list-allow-alphabetical t)

;; See https://github.com/hlissner/doom-emacs/issues/3185
(defadvice! no-errors/+org-inline-image-data-fn (_protocol link _description)
  :override #'+org-inline-image-data-fn
  "Interpret LINK as base64-encoded image data. Ignore all errors."
  (ignore-errors
    (base64-decode-string link)))

;;; Local Variables:
;;; outline-regexp: ";;;\\*+\\|\\`"
;;; End:
