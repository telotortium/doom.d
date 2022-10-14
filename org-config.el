;;; ~/.doom.d/org-config.el -*- lexical-binding: t; -*-

;;; Configuration for Org mode

(map! "C-c a" #'org-agenda
      "C-c b" #'org-switchb
      "C-c c" #'org-capture
      "C-c l" #'org-store-link
      "S-<f11>" #'org-clock-goto
      "C-<f11>" #'my-org-clock-in
      "C-S-<f11>" #'my-org-goto-heading
      :n "zs" #'org-save-all-org-buffers
      :mode org-mode
      "C-c C-x C-i" #'org-clock-in
      "C-c C-x <C-i>" #'org-clock-in
      "C-c C-x i" #'org-clock-in
      "C-c C-x C-o" #'org-clock-out
      "C-c C-x o" #'org-clock-in)
(use-package! evil-org
  :hook (org-mode . evil-org-mode))
(after! (evil-core evil-org)
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
(add-hook 'org-mode-hook #'electric-quote-local-mode)


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
        (forward-line 1))))
  (alert "org-clock-report-buffer complete")
  nil)
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

(require 'f)
(setq! org-directory "~/Documents/org")
(defcustom org-daily-log-file
  (f-join org-directory "home-org" "daily-log.org")
  "The path to Org file in which daily log entries are captured."
  :type 'file)

(defmacro org-capture-templates-put-entry (templates entry)
  "Put ENTRY into TEMPLATES, a variable in the format of ‘org-capture-templates’.

ENTRY is a list with its first element the keys used to invoke the templates. If
an entry with those keys exists, replace it with the contents of ENTRY (which
are copied so that TEMPLATES can be mutated later without affecting ENTRY).
Otherwise, add ENTRY to TEMPLATE."
  `(progn
    (require 'cl-lib)
    (setf (alist-get (car ,entry) ,templates nil nil #'equal)
          (cl-copy-list (cdr ,entry)))))
(after! org-capture
 (org-capture-templates-put-entry
  org-capture-templates
  `("t" "Task" entry (file (lambda () (f-join org-directory "home-org" "inbox.org")))
    "
* TODO %?%^{Title}
%u
" :clock-in t :clock-resume t :jump-to-captured t))
 (org-capture-templates-put-entry
  org-capture-templates
  `("p" "Phone (interrupt)" entry (file (lambda () (f-join org-directory "home-org" "inbox.org")))
    "
* PHONE %?%^{Title}
%U
" :clock-in t :clock-resume t :jump-to-captured t))
 (org-capture-templates-put-entry
  org-capture-templates
  `("n" "Note" entry (file (lambda () (f-join org-directory "home-org" "inbox.org")))
    "
* %u %?
" :jump-to-captured t))
 (org-capture-templates-put-entry
  org-capture-templates
  `("i" "Idea" entry (file (lambda () (f-join org-directory "home-org" "inbox.org")))
    "
* %u %?REPLACE_ME                      :IDEA:
" :clock-in t :clock-resume t))
 (org-capture-templates-put-entry
  org-capture-templates
  `("j" "Journal" plain (file+weektree (lambda () (f-join org-directory "home-org" "journal.org")))
    "
* %U %^{Title}                 :journal:
:PROPERTIES:
:Effort: 9999:00
:END:

%?
" :clock-in t :clock-resume t))
 (org-capture-templates-put-entry
  org-capture-templates
  `("d" "Drill" entry (file+headline org-default-notes-file "Drill")
    "
* Drill entry        :drill:
:PROPERTIES:
:DRILL_CARD_TYPE: hide1cloze
:Effort: 0:02
:END:
%?!|2 + 2|! equals !|4|!.
" :clock-in t :clock-resume t :jump-to-captured t))
 (org-capture-templates-put-entry
  org-capture-templates
  `("D" "Daily Log" plain
    (file my-org-roam-daily-for-overriding-time)
    "\
* %u Daily log
:PROPERTIES:
:Effort: 0:05
:END:
** Summary
** Problem
** Insight
** TODO [#A] Three things for the day <%(org-timestamp-add-days \"%<%F>\" +1) 06:00>
Advance to NEXT once filled out
** Clocktables

#+BEGIN: clocktable :maxlevel 9 :emphasize nil :scope agenda :stepskip0 t :fileskip0 t :block %<%F> :link t :match \"TikTok/-MEETING\" :narrow 60!
#+END: clocktable

#+BEGIN: clocktable :maxlevel 9 :emphasize nil :scope agenda :stepskip0 t :fileskip0 t :block %<%F> :link t :match \"TikTok/MEETING\" :narrow 60!
#+END: clocktable

#+BEGIN: clocktable :maxlevel 9 :emphasize nil :scope agenda :stepskip0 t :fileskip0 t :block %<%F> :link t :match \"-TikTok-break\" :narrow 60!
#+END: clocktable
" :time-prompt t :clock-in t :clock-resume t :jump-to-captured t))
 (org-capture-templates-put-entry
  org-capture-templates
  `("W" "GTD weekly review" plain
    (file my-org-roam-daily-for-overriding-time)
    "\
* NEXT %u GTD weekly review
SCHEDULED: <%<%Y-%m-%d %a 13:00-14:00>>
:PROPERTIES:
:Effort:   2:00
:END:
Follow:

- [[https://gettingthingsdone.com/wp-content/uploads/2014/10/Weekly_Review_Checklist.pdf][Weekly Review Checklist]]
- \"Weekly Review\" section in Getting Things Done.

  In 2015 edition: Chapter 8: \"Reflecting: Keeping It All Fresh and
  Functional\", section \"The Power of the Weekly Review\".

If it’s been more than a month since you’ve done the weekly review, follow [[id:682a1b3a-9335-4d53-b596-0775543e5b21][this catchup checklist]] first.

Checklist:

- GET CLEAR
  - [ ] Collect Loose Papers and Materials \\\\
    Gather all accumulated business cards, receipts, and miscellaneous
    paper-based materials into your in-tray.
  - [ ] Get “IN” to Zero \\\\
    Process completely all outstanding paper materials, journal and meeting
    notes, voicemails, dictation, and emails.
    - [ ] Personal email (use [[https://mail.google.com/mail/u/0/#search/is%3Ainbox+-label%3Anewsletters+-label%3Anewsletters%2Ffavorite][Inbox without newsletters]])
    - [ ] Personal tasks (use src_sh{cd ~/Documents/org/home-org && ./sync-tasks --pull})
    - [ ] Corp email
    - [ ] Corp tasks
    - [ ] Close all browser tabs (use org-capture-extension to capture)
    - [ ] Chats
    - [ ] Inbox (use [[elisp:(my-org-agenda-inbox-recent)]])
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
  - [ ] Run [[elisp:(org-drill-review-inbox)]] \\
    Look at stuff I've captured over the past week, including just now.
  - [ ] Review Any Relevant Checklists \\\\
    Use as a trigger for any new actions.
- GET CREATIVE
  - [ ] Review Someday Maybe List \\\\
    Review for any projects which may now have become active, and transfer to
    “Projects.” Delete items no longer of interest.
  - [ ] Be Creative and Courageous \\\\
    Any new, wonderful, hare-brained, creative, thought-provoking, risk-taking
    ideas to add into your system???

#+BEGIN: clocktable :maxlevel 9 :emphasize nil :scope agenda :stepskip0 t :fileskip0 t :tstart \"%(org-timestamp-add-days \"%<%F>\" -6)\" :tend \"%<%F>\" :link t :match \"TikTok/-MEETING\" :narrow 60!
#+END: clocktable

#+BEGIN: clocktable :maxlevel 9 :emphasize nil :scope agenda :stepskip0 t :fileskip0 t :tstart \"%(org-timestamp-add-days \"%<%F>\" -6)\" :tend \"%<%F>\" :link t :match \"TikTok/MEETING\" :narrow 60!
#+END: clocktable

#+BEGIN: clocktable :maxlevel 9 :emphasize nil :scope agenda :stepskip0 t :fileskip0 t :tstart \"%(org-timestamp-add-days \"%<%F>\" -6)\" :tend \"%<%F>\" :link t :match \"-TikTok-break\" :narrow 60!
#+END: clocktable
" :time-prompt t :immediate-finish t))
 (org-capture-templates-put-entry
  org-capture-templates
  `("P" "org-protocol"))
 (org-capture-templates-put-entry
   org-capture-templates
   `("Pp" "Link and Text" entry (file+headline org-default-notes-file "Links")
     "
* %?REPLACE_ME
Source: [[%:link][%:description]]
#+BEGIN_SRC html
<blockquote>
%i
</blockquote>
#+END_SRC

%U
"))
 (org-capture-templates-put-entry
  org-capture-templates
  `("PL" "Link" entry (file+headline org-default-notes-file "Links")
    "\
* %?%:description
:PROPERTIES:
:ID: %(org-id-new)
:ROAM_REFS: \"%(org-link-make-string \"%:link\" \"%:description\")\"
:END:
%U" :jump-to-captured t)))
(defun my-org-roam-daily-for-overriding-time ()
  "Called by ‘org-capture’ templates."
  (require 'org-roam)
  (require 'org-roam-capture)
  (require 'org-roam-dailies)
  (let* ((org-roam-directory
          (default-value 'org-roam-directory))
         (time
          (or org-overriding-default-time (current-time)))
         (daily-file
          (expand-file-name (format-time-string "%Y-%m-%d.org" time)
                            (expand-file-name
                             org-roam-dailies-directory
                             org-roam-directory))))
     (when (org-roam-capture--new-file-p daily-file)
      (save-excursion
        (save-restriction
          (org-roam-dailies--capture time 'goto))))
     daily-file))

;; Create ‘C-u 2 M-x org-capture’ command to refile org-capture template under
;; headline at point.
(defun my-org-capture-under-headline (&optional _goto keys)
  "Capture under the headline at current point.

This finds an appropriate point at the end of the subtree to capture the new
entry to, then call ‘org-capture’ with \"C-u 0\" to capture at this new point,
and finally fixes the level of the newly-captured entry to be a child of the
headline at which this function was called.

KEYS are passed to ‘org-capture’ in its KEYS argument.  _GOTO corresponds to the
GOTO argument of ‘org-capture’, but is is ignored because we intentionally place
the point where we want it."
  (interactive "P")
  (unwind-protect
      (progn
        (unless (eq major-mode 'org-mode)
          (user-error "Must be called from an Org-mode buffer"))
        (org-with-point-at (point)
          (org-back-to-heading t)
          (let* ((headline-marker (point-marker))
                 (headline-level (org-current-level)))
            (org-end-of-subtree t)
            (unless (org--line-empty-p 1)
              (end-of-line)
              (open-line 1)
              (forward-line 1))
            (cl-letf*
                (((symbol-function 'my-org-capture-under-headline--fixup-level)
                  (lambda ()
                    ;; If :begin-marker doesn’t point to a buffer, the capture
                    ;; has already been finalized, so don’t try to do any work
                    ;; in that case.
                    (when-let* ((begin-marker (org-capture-get :begin-marker))
                                ((marker-buffer begin-marker)))
                      ;; Go to the capture buffer.
                      (org-goto-marker-or-bmk begin-marker)
                      ;; Make the captured headline one level below the original headline.
                      (while (< (org-current-level) (+ 1 headline-level))
                        (save-excursion (org-demote-subtree)))
                      (while (> (org-current-level) (+ 1 headline-level))
                        (save-excursion (org-promote-subtree))))))
                 ;; Store the original ‘org-capture-finalize’ for the override before.
                 ((symbol-function 'org-capture-finalize-orig) (symbol-function 'org-capture-finalize))
                 ;; Need to fixup level in case ‘:immediate-finish’ set in ‘org-capture-templates’
                 ((symbol-function 'org-capture-finalize)
                  (lambda (&optional stay-with-capture)
                    (my-org-capture-under-headline--fixup-level)
                    (org-capture-finalize-orig stay-with-capture))))
              (apply #'org-capture 0 keys)
              ;; If the template didn’t contain ‘:immediate-finish’, we fix up
              ;; levels now.
              (my-org-capture-under-headline--fixup-level)))))))
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
(defun my-org-git-sync ()
  "Run org-syncup-full script to save Org buffers and then org-git-sync."
  (interactive)
  (let ((minibuffer-auto-raise nil))
    (let ((c (current-window-configuration)))
      (message "my-org-git-sync: launching - will restore window configuration afterward.")
      (deferred:try
        (let ((minibuffer-auto-raise nil))
          (deferred:process-shell
            "~/bin/org-syncup-full -n -g -P"))
        :finally
        (lambda (_)
          (let ((minibuffer-auto-raise nil))
            (set-window-configuration c)
            (message "my-org-git-sync: restored window configuration")
            (start-process-shell-command
             "promesia-index" nil
             (format "%s promnesia index --source home-org tiktok-org"
                     (cond
                      (IS-MAC "nice -n19 taskpolicy -d throttle")
                      (IS-LINUX "nice -n19 ionice -c idle")
                      (t ""))))))))))
(run-with-idle-timer 300 t #'my-org-git-sync)

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
(defun my-org-list-all-org-files-under (directory &optional archives)
  "List all Org files under DIRECTORY, recursively.
If ARCHIVES set, include archives too."
  (unless (executable-find "fd")
    (user-error "‘fd’ must be installed and on PATH"))
  (s-split "\0"
           (shell-command-to-string (format "fd -a -0 '\\.(?:org%s)$' %s"
                                            (if archives "|org_archive" "")
                                            (shell-quote-argument
                                             (expand-file-name
                                              directory))))
           'omit-nulls))
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
      (not (parent (tags "HOLD")))
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
(defconst my-org-agenda-inbox-query
  `(and
     (tags-local "inbox" "REFILE")
     (not (scheduled :from 1))
     (not (tags "HOLD" "CANCELLED" "ARCHIVED"))))
(cl-defun my-org-agenda-inbox (&optional buffer)
 "Show agenda for inbox entries not scheduled for future."
 (interactive)
 (org-ql-search
   (org-agenda-files)
   my-org-agenda-inbox-query
   :super-groups '((:auto-ts t))
   :title "Inbox entries"
   :buffer (or buffer org-ql-view-buffer)))
(cl-defun my-org-agenda-inbox-recent (&optional buffer)
 "Show agenda for recent inbox entries.
Like ‘my-org-agenda-inbox’, but only for entries from the past
‘my-org-agenda-active-days'."
 (interactive)
 (org-ql-search
   (org-agenda-files)
   (append
    my-org-agenda-inbox-query
    `((ts :from ,(- my-org-agenda-active-days))))
   :super-groups '((:auto-ts t))
   :title "Inbox entries"
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
      (ts :to -30)
      (not (ts :from -30))
      (not (path ".*/roam/.*"))         ; Roam files don’t have archive files
      (not (property "recurrence"))     ; Exclude parents of recurring events
      (or (done)
          (and (tags "gcal")
               (not (todo))))
      (or (not (parent))
          (parent (and (not (todo)) (not (done)))))
      (or (not (children))
          (descendants
           (and (not (todo))
                (ts :to -30)
                (not (ts :from -30)))))
      t)
    :buffer (or buffer org-ql-view-buffer)
    :super-groups '((:auto-map my-org-super-agenda-group-by-project-or-task-group))
    :sort 'date
    :title "Archivable tasks"))
(cl-defun my-org-agenda-stale-tasks (&optional buffer)
  "Show agenda for stale tasks

Consider these types of headlines for archiving:

- Headlines with a *done* todo keyword.
- Headlines with *no* todo keyword tagged with \"gcal\" - these are
  entries created by org-gcal. If I'm actively managing such a task,
  I'll always add a todo keyword of some kind to the heading, so these
  tasks will be saved from archiving unless they're marked done.

Only consider top-level tasks in project trees - don't individually archive
tasks that are part of an ongoing project. Only archive stale that have been
done for at least 30 days.

Daily log entries (marked by the \"dailylog\" tag) should never be
archived.

Use `org-ql-search' to search."
  (interactive)
  (org-ql-search
    (org-agenda-files)
    `(and
      (not (tags "REFILE" "dailylog" "ARCHIVE"))
      (ts :to -30)
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
                (ts :to -30)
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
  (require 'org-gcal)
  (org-ql-search
    (org-agenda-files t t)
    `(and
      (ts :to -90)
      (not (ts :from -90))
      (property ,org-gcal-calendar-id-property)
      (not (property "recurrence"))     ; Exclude parents of recurring events
      (not (property "ID"))             ; Exclude events that might be linked to
      (not (property "CUSTOM_ID"))
      (or
       ;; The following two lines correspond to most newly-fetched tasks.
       (not (todo))
       (todo "MEETING")
       ;; This will catch tasks that were pulled into recurring.org by
       ;; ‘my-org-gcal-set-next’.
       (tags "recurring"))
      (not (clocked))
      (not (closed))
      (not (regexp org-logbook-drawer-re))
      (not (children)))
    :buffer (or buffer org-ql-view-buffer)
    :super-groups '((:auto-ts))
    :sort 'date
    :title "Old GCal tasks to delete."))
(cl-defun my-org-agenda-ancient-tasks (&optional buffer)
 "Show agenda for ancient tasks.

Look for tasks with property ‘org-gcal-calendar-id-property' that are older than
90 days, have never been clocked, have no children, and are not parents of
recurring events. These tasks are merely cached versions of events on Google
Calendar that I’ve never interacted with, and can thus be deleted without any
ill effect.

Use ‘org-ql-search' to search."
 (interactive)
 (require 'org-gcal)
 (org-ql-search
   (my-org-list-all-org-files-under org-directory)
   `(and
     (ts :to -365)
     (not (ts :from -365))
     (or
      (todo)
      (tags-local "REFILE" "inbox"))
     (not (parent (tags "REFILE" "inbox" "HOLD" "WAITING"))))
   :buffer (or buffer org-ql-view-buffer)
   :super-groups '((:auto-ts))
   :sort 'date
   :title "Ancient tasks, to re-triage or archive"))
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
(my-org-agenda-ql-wrapper my-org-agenda-inbox-agenda-command
                          my-org-agenda-inbox)
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
  (org-defkey org-agenda-mode-map "$" #'org-agenda-archive-default-or-down)
  ;; My normal binding for "Q", #’kill-this-buffer, will sometimes cause a
  ;; "Selecting deleted buffer" error - see
  ;; https://github.com/hlissner/doom-emacs/issues/4468
  (org-defkey org-agenda-mode-map "Q" #'org-agenda-Quit))
;; Copy keybindings from above.
(after! org-super-agenda
  (org-defkey org-super-agenda-header-map "$"
              #'org-agenda-archive-default-or-down)
  (org-defkey org-super-agenda-header-map "\C-k" #'org-agenda-kill-or-down)
  (org-defkey org-super-agenda-header-map "Q" #'org-agenda-Quit))


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
 `("I" "Inbox"
   ((my-org-agenda-inbox-agenda-command ""))))
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

;; Highlight lines of marked Org-Agenda items
;; See https://emacs.stackexchange.com/a/70380/17182
(defun my-org-agenda-highlight-marked (oldfun &rest r)
  ;; ‘advice--cd*r’ finds the underlying advised function so that we can
  ;; properly compare functions.
  (let* ((oldfun-base (advice--cd*r oldfun))
         ;; Functions that examine the whole buffer and are not implemented
         ;; in terms of more primitive functions - currently just
         ;; ‘org-agenda-bulk-unmark-all'.
         (all-buffer-funcs
                   (mapcar (lambda (sym)
                             (advice--cd*r (symbol-function sym)))
                           '(org-agenda-bulk-unmark-all)))
         (begin
          (cond
           ((member oldfun all-buffer-funcs) (point-min))
           ((use-region-p) (region-beginning))
           (t (point-at-bol))))
         (end
          (cond
           ((member oldfun all-buffer-funcs) (point-max))
           ((use-region-p) (region-end))
           (t (point-at-eol)))))
    (apply oldfun r)
    (save-excursion
      (goto-char begin)
      (forward-line 0)
      (while (< (point) end)
        (let* ((ovs (overlays-at (point)))
               (marked-entry-overlay
                (nth 0
                     (cl-remove-if-not
                      (lambda (ov)
                        (eq (overlay-get ov 'type) 'org-marked-entry-overlay))
                      ovs)))
               (highlight-marked-overlay
                (nth 0
                     (cl-remove-if-not
                      (lambda (ov)
                        (eq (overlay-get ov 'type) 'my-org-agenda-highlight-marked))
                      ovs))))
          (cond
           ((and highlight-marked-overlay (not marked-entry-overlay))
            (delete-overlay highlight-marked-overlay))
           ((and marked-entry-overlay (not highlight-marked-overlay))
            (let ((ov (make-overlay (point-at-bol) (point-at-eol))))
              (overlay-put ov 'type 'my-org-agenda-highlight-marked)
              (overlay-put ov 'face '(:background "yellow"))))))
        (forward-line 1)))))
;; ‘org-agenda-bulk-mark-all’, ‘org-agenda-bulk-mark-regexp’, and
;; ‘org-agenda-bulk-toggle-all’ are all implemented using
;; ‘org-agenda-bulk-mark’, and ‘org-agend-bulk-toggle’, so
;; they don't need to be advised.
(dolist (cmd '(org-agenda-bulk-mark org-agenda-bulk-toggle
                    org-agenda-bulk-unmark org-agenda-bulk-unmark-all))
  (advice-add cmd :around #'my-org-agenda-highlight-marked))

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
(defun terminal-notifier-notify (title message &optional group)
  "Show a message with `terminal-notifier-command'.

TITLE and MESSAGE are self-explanatory. GROUP, if present, is passed to
‘terminal-notifier-command’ to dismiss previous notifications with that GROUP."
  (apply
   #'start-process
   "terminal-notifier"
   "*terminal-notifier*"
   terminal-notifier-command
   "-title" (format "%s%s%s"
                    (if group group "")
                    (if group ": " "")
                    title)
   "-message" message
   ;; Disabled because macOS won’t display a notification if Emacs is focused:
   ;; https://github.com/julienXX/terminal-notifier/issues/216#issuecomment-616864111
   ;;"-sender" "org.gnu.Emacs"
   "-activate" "org.gnu.Emacs"
   (when group
     (list "-group" group))))
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

(defun org-clock-notify-once-if-expired ()
  "Show notification if we spent more time than we estimated before.
Notification is shown only once.

Overrides built-in version in order to also show clocked time."
  (when (org-clocking-p)
    (let ((effort-in-minutes (org-duration-to-minutes org-clock-effort))
          (clocked-time (org-clock-get-clocked-time)))
      (if (setq org-clock-task-overrun
                (if (or (null effort-in-minutes) (zerop effort-in-minutes))
                    nil
                 (>= clocked-time effort-in-minutes)))
          (unless org-clock-notification-was-shown
               (setq org-clock-notification-was-shown t)
               (org-notify
                 (format-message "Task `%s' should be finished by now. (%s/%s)"
                                org-clock-heading
                                (apply #'format "%d:%d"
                                       (cl-floor clocked-time 60))
                                org-clock-effort)
                 org-clock-sound))
        (setq org-clock-notification-was-shown nil)))))
(defvar my-org-clock-nag-after-expiry--timer nil
  "Used by ‘my-org-clock-nag-after-expiry’, for which see.")
(defun my-org-clock-nag-after-expiry (&rest _r)
  "Nag org-clock notifications repeated after expiry.

Normally, ‘org-clock-notify-once-if-expired’ ensures that notifications for
clocking more than the effort on the task will fire only once. For certain tasks
(distractions, for exgmple), I want the clock to nag me repeatedly. This advice
will implement that functionality.

To use this, set the property ‘org-clock-nag-after-expiry’ on the headline in
question to a number of minutes after which the notification should re-fire.
This property is inherited - you can set it to the string “nil” to turn this
off if the property is inherited."
  (when-let* (((and org-clock-notification-was-shown
                    org-clock-marker
                    (marker-buffer org-clock-marker)
                    (null my-org-clock-nag-after-expiry--timer)))
              (nag-prop (org-entry-get org-clock-marker
                                       "org-clock-nag-after-expiry"
                                       'inherit))
              ((string-match "^[0-9][0-9]*$" nag-prop))
              (nag-minutes (string-to-number nag-prop)))
    (setq my-org-clock-nag-after-expiry--timer
          (run-at-time
                ;; Subtract 1 from nag-minutes because the clock time is checked
                ;; by default every 60 seconds (see ‘org-clock-update-period’).
                (* 60 (max 0 (- nag-minutes 1)))
                nil
                #'my-org-clock-nag-after-expiry--reset
                (copy-marker org-clock-marker)))))
(defun my-org-clock-nag-after-expiry--reset (clock-marker)
  "Function called by timer set up in ‘my-org-clock-nag-after-expiry'.

CLOCK-MARKER was the value of ‘org-clock-marker’ when the timer was set."
  (cancel-timer my-org-clock-nag-after-expiry--timer)
  (setq my-org-clock-nag-after-expiry--timer nil)
  (when (and
         org-clock-marker
         (eq (marker-buffer clock-marker)
             (marker-buffer org-clock-marker))
         (= (marker-position clock-marker)
            (marker-position org-clock-marker)))
    (setq org-clock-notification-was-shown nil)))
(advice-add #'org-clock-notify-once-if-expired
            :after #'my-org-clock-nag-after-expiry)



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

(load! "org-pomodoro-config.el")

(add-to-list 'org-modules 'org-id)      ; Needed for ID links to work
(after! org-id
  (setq! org-id-locations-file
         (expand-file-name ".org-id-locations.el"
                           user-emacs-directory)
         org-id-locations-file-relative nil))
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
                                       (air-org-global-tags-completion-table))))
                  (air-org-global-tags-completion-table))))
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
(defun air-org-global-tags-completion-table (&optional files)
  "Like ‘org-global-tags-completion-table’, but faster.

Call ‘find-file-noselect' only if the file is not already being visited.  This
prevents calling potentially expensive operations, especially reverting the
file, unless the file is not being visited at all - I sync my files fairly often
with Git, so they tend to have their mtime updated, but my set of tags doesn’t
change often enough that I want to revert all the time. I always have
‘org-revert-all-org-buffers’ if needed.

Optional FILES argument is a list of files which can be used
instead of the agenda files."
  (save-excursion
    (org-uniquify
     (delq nil
           (apply #'append
                  (mapcar
                   (lambda (file)
                     (set-buffer
                      (or (org-find-base-buffer-visiting file)
                          (find-file-noselect file)))
                     (org--tag-add-to-alist
                      (org-get-buffer-tags)
                      (mapcar (lambda (x)
                                (and (stringp (car-safe x))
                                     (list (car-safe x))))
                              org-current-tag-alist)))
                   (if (car-safe files) files
                     (org-agenda-files))))))))
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
(setq! org-gcal-config-file (expand-file-name "org-gcal-config.el" doom-private-dir))
(when (file-exists-p org-gcal-config-file)
 (load org-gcal-config-file))
;; Ensure plstore doesn’t keep prompting for password.
(setq! plstore-cache-passphrase-for-symmetric-encryption t)
(after! org-gcal
  ;; Disable Auto Archive - my gcal.org_archive is so big that this majorly
  ;; slows down every fetch. Instead, I'll just archive old entries once a
  ;; month along with the rest of the entries to be archived.
  (setq! org-gcal-auto-archive nil)
  (setq! org-gcal-remove-api-cancelled-events nil)
  (setq! org-gcal-recurring-events-mode 'nested)
  (defun my-org-gcal-set-effort (_calendar-id event _update-mode)
    "Set Effort property based on EVENT if not already set."
    (when-let* ((stime (plist-get (plist-get event :start)
                             :dateTime))
                (etime (plist-get (plist-get event :end)
                                  :dateTime))
                (diff (float-time
                       (time-subtract (org-gcal--parse-calendar-time-string etime)
                                      (org-gcal--parse-calendar-time-string stime))))
                (minutes (floor (/ diff 60))))
      (let ((effort (org-entry-get (point) org-effort-property)))
        (unless effort
          (message "need to set effort - minutes %S" minutes)
          (org-entry-put (point)
                         org-effort-property
                         (apply #'format "%d:%02d" (cl-floor minutes 60)))))))
  (add-hook 'org-gcal-after-update-entry-functions #'my-org-gcal-set-effort)

  (defun my-org-gcal-set-next (_calendar-id event _update-mode)
    "Set NEXT on certain headlines."
    (when-let* ((title (plist-get event :summary))
                ((or (string= title "Plan tasks for workday")
                     (string= title "Check inbox"))))
      (let ((org-inhibit-logging t))
        (org-todo "NEXT"))))
  (add-hook 'org-gcal-after-update-entry-functions #'my-org-gcal-set-next)

  (defun my-org-gcal-default-todo-meeting (_calendar-id event _update-mode)
    "Set all events with no TODO heading to be MEETING by default.
Applies only for files in ‘org-gcal-fetch-file-alist’."
    (when-let* ((title (plist-get event :summary))
                ((member (abbreviate-file-name buffer-file-name)
                         (mapcar #'cdr org-gcal-fetch-file-alist)))
                ((not (string= "" (org-get-todo-state))))
                ((not (string= "transparent"
                               (org-entry-get (point) "TRANSPARENCY")))))
      (let ((org-inhibit-logging t))
        (org-todo "MEETING"))))
  (add-hook 'org-gcal-after-update-entry-functions
            #'my-org-gcal-default-todo-meeting)

  (defun my-org-gcal-recurrence-no-update-effort-from-children
      (_calendar-id _event _update-mode)
    "Set Org property “effort-no-update-from-children” on all events with “recurrence” property."
    (when (org-entry-get (point) "recurrence")
      (org-entry-put (point) "effort-no-update-from-children" "t")))
  (add-hook 'org-gcal-after-update-entry-functions
            #'my-org-gcal-recurrence-no-update-effort-from-children)

  (defun my-org-gcal-exclude-corp-personal-events (event)
    "Remove events from corp calendar with summary \"*Personal*\."
    (let* ((summary (or (plist-get event :summary)
                        "busy"))
           (include (not (string= summary "*Personal*"))))
       include))
  (add-hook 'org-gcal-fetch-event-filters
            #'my-org-gcal-exclude-corp-personal-events))

;; Run ‘org-gcal-sync’ regularly not at startup, but at 8 AM every day,
;; starting the next time 8 AM arrives.
(run-at-time
 (let* ((now-decoded (decode-time))
        (today-8am-decoded
         (append '(0 0 8) (nthcdr 3 now-decoded)))
        (now (encode-time now-decoded))
        (today-8am (encode-time today-8am-decoded)))
       (if (time-less-p now today-8am)
           today-8am
         (time-add today-8am (* 24 60 60))))
 (* 24 60 60)
 (defun my-org-gcal-sync-clear-token ()
   "Sync calendar, clearing tokens first."
   (require 'org-gcal)
   (org-gcal-sync-tokens-clear)
   (org-gcal-sync)
   (org-agenda nil "a")
   (switch-to-buffer org-agenda-buffer)
   (org-agenda-redo)
   (org-agenda-goto-today)))

;;; FIXME: remove this once I’ve debugged why plstore secret entries are being
;;; inserted into my Org mode buffers.  This is somehow caused by ‘oauth2-auto’,
;;; which I’ve added as a dependency for ‘org-gcal’.
(defadvice! insert-break-on-secret-entries (&rest args)
  "Break if trying to insert secret entries outside of plstore buffer."
  :before #'insert
  (when
    (and
      (stringp (buffer-file-name))
      (not (string-match-p "oauth2-auto.plist" (buffer-file-name)))
      (equal ";;; secret entries\n" (nth 0 args)))
    (debug)))

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
           (duration (org-element-property :EFFORT elem)))
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

;; Functions to move org-gcal timestamp between SCHEDULED propeerty and org-gcal drawer
(defun my-org-gcal-toggle-timestamp (&optional point)
  "Move org-gcal timestamp between SCHEDULED property and org-gcal drawer."
  (interactive)
  (let ((pt (or point (point))))
    (cond ((org-entry-get pt "SCHEDULED")
           (my-org-gcal-unschedule-timestamp pt))
          ((my-org-gcal--has-unscheduled-timestamp pt)
           (my-org-gcal-schedule-timestamp pt))
          (t
           (org-with-point-at pt
             (user-error "Not an org-gcal entry at %S")
             (point-marker))))))
(defun my-org-gcal-unschedule-timestamp (&optional point)
  "Move org-gcal timestamp from SCHEDULED property to org-gcal drawer."
  (interactive)
  (require 'org-gcal)
  (org-with-point-at (or point (point))
    (org-back-to-heading)
    (org-narrow-to-element)
    (if-let* ((scheduled-timestamp (org-entry-get (point) "SCHEDULED"))
              (drawer-point
               (re-search-forward
                (format "^[ \t]*:%s:[ \t]*$" org-gcal-drawer-name)
                (point-max)
                'noerror)))
        (progn
          (goto-char drawer-point)
          (newline)
          (insert scheduled-timestamp)
          (forward-line 1)
          (message "%s" (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
          (unless
              (re-search-forward
               "^[ \t]*:END:[ \t]*$"
               (point-at-eol)
               'noerror)
            (forward-line 0)
            (newline))
          (org-schedule '(4)))           ; Unschedule - do this last to preserve drawer-point
      (org-with-point-at (point)
        (user-error "Not an org-gcal entry with a SCHEDULED timestamp at %S"
                    (point-marker))))))
(defun my-org-gcal-schedule-timestamp (&optional point)
  "Move org-gcal timestamp from org-gcal drawer to SCHEDULED property."
  (interactive)
  (require 'org-gcal)
  (org-with-point-at (or point (point))
    (org-back-to-heading)
    (org-narrow-to-element)
    (if-let* ((pt (my-org-gcal--has-unscheduled-timestamp (point)))
              (md (match-data 'integer))
              (ts (buffer-substring-no-properties (nth 2 md) (nth 3 md))))
        (progn
          ;; Delete timestamp line
          (goto-char pt)
          (forward-line 0)
          (kill-whole-line)
          ;; Delete following blank line if present
          (when (eolp) (kill-whole-line))
          (org-schedule nil ts))
       (org-with-point-at (point)
         (user-error "Not an org-gcal entry with a timestamp in org-gcal drawer at %S"
                     (point-marker))))))
(defun my-org-gcal--has-unscheduled-timestamp (&optional point)
  "Is entry an org-gcal entry with an unscheduled timestamp?

If it is, returns the point at the *end* of the timestamp, and sets match
data for the timestamp according to ‘org-element--timestamp-regexp’.
In general, does *not* preserve point or match data."
  (require 'org-gcal)
  (org-with-point-at (or point (point))
    (org-back-to-heading)
    (org-narrow-to-element)
    (when-let* ((drawer-point
                 (re-search-forward
                  (format "^[ \t]*:%s:[ \t]*$" org-gcal-drawer-name)
                  (point-max)
                  'noerror)))
      (forward-line 1)
      (re-search-forward org-element--timestamp-regexp (point-at-eol) 'noerror))))
(defun my-org-gcal-unscheduled-toggle-timestamp (fn arg &rest r)
  "Move timestamp to org-gcal drawer when unscheduling a org-gcal event."
  (if-let* ((interactive? (called-interactively-p 'any))
            (unscheduling? (equal arg '(4)))
            (scheduled? (org-entry-get (point) "SCHEDULED"))
            (drawer-point
             (org-with-point-at (point)
               (org-back-to-heading)
               (org-narrow-to-element)
               (re-search-forward
                    (format "^[ \t]*:%s:[ \t]*$" org-gcal-drawer-name)
                    (point-max)
                    'noerror))))
      (my-org-gcal-unschedule-timestamp)
    (apply fn arg r)))
(advice-add #'org-schedule :around #'my-org-gcal-unscheduled-toggle-timestamp)

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
  (defvar my-org-drill-clock-heading-id nil
    "ID of org-mode heading ")
  (defun my-org-drill-clock-time (fn &rest r)
    "Clock time on ‘my-org-drill-clock-heading-id' when ‘org-drill' is run."
    (require 'org-clock)
    (unless my-org-drill-clock-heading-id
      (display-warning
       'org-config "‘my-org-drill-clock-heading-id’ not set"))
    (let ((clocking? (org-clocking-p)))
      (unwind-protect
          (progn
            (when my-org-drill-clock-heading-id
              (org-with-point-at (org-id-find
                                  my-org-drill-clock-heading-id t)
                (org-clock-in)))
            (apply fn r))
        (when my-org-drill-clock-heading-id
          (if clocking?
               (org-with-point-at org-clock-interrupted-task
                     (org-clock-in))
             (org-clock-out))))))
  (advice-add #'org-drill :around #'my-org-drill-maximize-frame)
  (advice-add #'org-drill :around #'my-org-drill-clock-time)
  (defun my-org-drill-rebind-keys (fn &rest r)
    "Rebind some key sequences at ‘org-drill’ prompt.

Needs to be done this way because ‘org-drill’ has no keymap."
    (let ((continue t)
          result)
      (while continue
        (setq result (apply fn r))
        (cond
         ((and (stringp result) (string= result "\C-c\C-o"))
          (call-interactively #'org-open-at-point))
         (t
          (setq continue nil))))
      result))
  (advice-add #'org-drill--read-key-sequence :around #'my-org-drill-rebind-keys)
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
    ;; Log org-drill reschedules into drawer
    (org-entry-put (point) "LOG_INTO_DRAWER" "t")
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
(defun my-org-capture-defer-task ()
  "Defer the task at point to a later time."
  (interactive)
  (org-drill-type-inbox-init)
  (org-capture-finalize))
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
(defun org-drill-review-inbox (&optional scope drill-match resume-p cram)
  "Review inbox cards.
SCOPE, DRILL-MATCH, RESUME-P, and CRAM passed to ‘org-drill'."
  (interactive)
  (require 'org-drill)
  (let ((org-drill-question-tag "inbox"))
    (if (called-interactively-p 'any)
        (call-interactively #'org-drill)
      (funcall #'org-drill scope drill-match resume-p cram))))
(defun org-drill-resume-inbox ()
 "Resume reviewing inbox cards."
 (interactive)
 (require 'org-drill)
 (let ((org-drill-question-tag "inbox"))
   (call-interactively #'org-drill-resume)))
;; Need to eagerly load because my Org files call functions declared by this
;; file in their local variables.
(require 'org-drill)

;;;* Anki-editor
;;;
;;; Configuration mostly taken from https://yiufung.net/post/anki-org/
(use-package! anki-editor
  :after org
  :bind (:map org-mode-map
              ("<f12>" . anki-editor-cloze-region-auto-incr)
              ("<f11>" . anki-editor-cloze-region-dont-incr)
              ("<f10>" . anki-editor-reset-cloze-number)
              ("<f9>"  . anki-editor-push-tree)
              ("<f8>"  . anki-editor-open-note-in-anki))
  :hook (org-capture-after-finalize . anki-editor-reset-cloze-number) ; Reset cloze-number after each capture.
  :init
  (require 'org-capture)
  (org-capture-templates-put-entry
    org-capture-templates
   `("a" "anki-editor"))
  (org-capture-templates-put-entry
    org-capture-templates
   `("ab" "Anki basic"
     entry
     (file+headline org-default-notes-file "Drill")
     "
* %(format-time-string \"%H:%M\")   %^g
:PROPERTIES:
:ANKI_TAGS: org-anki-editor
:ANKI_NOTE_TYPE: Basic
:ANKI_DECK: /
:END:
** Front
%?
** Back
%x
"
     :jump-to-captured t))
  (org-capture-templates-put-entry
    org-capture-templates
   `("ac" "Anki cloze"
     entry
     (file+headline org-default-notes-file "Drill")
     "
* %(format-time-string \"%H:%M\")   %^g
:PROPERTIES:
:ANKI_TAGS: org-anki-editor
:ANKI_NOTE_TYPE: Cloze
:ANKI_DECK: /
:END:
** Text
%x
** Extra
"
     :jump-to-captured t))
 (setq anki-editor-create-decks t ;; Allow anki-editor to create a new deck if it doesn't exist
  anki-editor-org-tags-as-anki-tags t
  anki-editor-use-math-jax t)
 :config
 (add-to-list
  'anki-editor-ignored-org-tags
  "drill" 'append)
 (add-to-list
  'anki-editor-ignored-org-tags
  "inbox" 'append)

 (defun anki-editor-push-tree ()
   "Push the current tree using ‘anki-editor-push-notes’."
   (interactive)
   (funcall-interactively #'anki-editor-push-notes '(4)))
 (defun anki-editor-cloze-region-auto-incr (&optional arg)
   "Cloze region without hint and increase card number."
   (interactive)
   (anki-editor-cloze-region my-anki-editor-cloze-number "")
   (setq my-anki-editor-cloze-number (1+ my-anki-editor-cloze-number))
   (forward-sexp))
 (defun anki-editor-cloze-region-dont-incr (&optional arg)
   "Cloze region without hint using the previous card number."
   (interactive)
   (anki-editor-cloze-region (1- my-anki-editor-cloze-number) "")
   (forward-sexp))
 (defun anki-editor-reset-cloze-number (&optional arg)
   "Reset cloze number to ARG or 1"
   (interactive)
   (setq my-anki-editor-cloze-number (or arg 1)))
 (defun anki-editor-push-tree ()
   "Push all notes under a tree."
   (interactive)
   (anki-editor-push-notes '(4))
   (anki-editor-reset-cloze-number))
  ;; Initialize
 (anki-editor-reset-cloze-number))

;; TODO: replace with implementation from ‘org-generic-id-update-id-locations’ once
;; it’s merged upstream.
(require 'persist)
(require 'org-generic-id)
(persist-defvar anki-editor-push-agenda-fast--last-pushed-time nil
                "Time at which ‘anki-editor-push-agenda-fast’ last completed.")
(defun anki-editor-push-agenda-fast ()
  "Push all anki-files fast.

Grep to find only Org files that *could* contain Anki notes, and then push all the
notes in those files."
  (interactive)
  (require 's)
  (require 'anki-editor)
  (dolist
      (f
       (org-generic-id-files-modified-since-modtime
        anki-editor-push-agenda-fast--last-pushed-time
        (s-split "\n"
                 (shell-command-to-string "rg -l -e :ANKI_NOTE_TYPE: ~/Documents/org")
                 'omit-nulls)))
    (save-excursion
      (when-let* ((buf (org-find-base-buffer-visiting f)))
        (with-current-buffer buf
          (anki-editor-push-notes nil nil 'file)))))
  (setq anki-editor-push-agenda-fast--last-pushed-time (current-time))
  (org-save-all-org-buffers))
(defun anki-editor-open-note-in-anki ()
  "Open the note at the current point in Anki."
  (interactive)
  (if-let* ((nid (org-entry-get (point) anki-editor-prop-note-id)))
      (anki-editor--anki-connect-invoke-result
       'guiBrowse
       `((query . ,(format "nid:%s" nid))))
      t
    (user-error "Must be on an Org headline that contains the property ‘%s’"
                anki-editor-prop-note-id)))

;; org-protocol support for opening a file - needed for ‘my-anki-editor-backlink’.
(after! org-protocol
 (add-to-list
  'org-protocol-protocol-alist
  '("org-open-file" :protocol "open-file" :function org-protocol-open-file)))
(defun org-protocol-open-file (fname)
  "Process an org-protocol://open-file?file= style URL with FNAME.

The value for the “file” key contains a file name to open. This file name is
opened using ‘find-file’, so any expansions done by that function are performed
by this handler as well.

Returns the file name to open, or NIL if no file is to be opened."
  (let ((f (org-protocol-sanitize-uri
            (plist-get (org-protocol-parse-parameters fname nil '(:file))
                       :file))))
    f))
(after! org-protocol
 (add-to-list
  'org-protocol-protocol-alist
  '("org-open-org-link" :protocol "open-org-link" :function org-protocol-open-org-link)))
(defun org-protocol-open-org-link (fname)
  "Process an org-protocol://open-org-link?link= style URL with FNAME.

The value for the “link” key is an Org link, and is opened using
‘org-link-open-from-string’ after first focusing and raising Emacs. In
particular, remember that non-HTTP links will generally require opening and
closing square brackets.

This function returns NIL, because no file is directly opened here. In
particular, that means Emacsclient will return immediately."
  (when-let ((f (org-protocol-sanitize-uri
                 (plist-get (org-protocol-parse-parameters fname nil '(:link))
                            :link))))
    (x-focus-frame nil)
    (org-link-open-from-string f)
    nil))
(defadvice! my-anki-editor-backlink (fn &rest r)
  "Add links from Anki cards back to the file that generated them."
  :around #'anki-editor--build-fields
  (require 'url-util)
  (let ((fields (apply fn r)))
    (when-let*
        (((and fields))
         (note-type (org-entry-get nil anki-editor-prop-note-type))
         (current-file
          (when-let ((f (buffer-file-name)))
            (abbreviate-file-name f)))
         (title (or (org-entry-get nil "ITEM") ""))
         (field-name
          (cond
           ((string= note-type "Basic") "Back")
           ((string= note-type "Basic (and reversed card)") "Note")
           ((string= note-type "Cloze") "Extra")
           ((string= note-type "Cloze with typed text") "Extra")
           (t nil)))
         (field (assoc field-name fields)))
      (setf (alist-get field-name fields nil nil #'equal)
            (concat (cdr field)
                    (format "<div><hr><p>Source: <a href=\"org-protocol://open-org-link?link=%%5B%%5Bfile:%s%s%%5D%%5D\">%s</p></div>"
                            (url-hexify-string (org-link-escape current-file))
                            (url-hexify-string (if (string= "" title)
                                                   ""
                                                 (org-link-escape (format "::*%s" title))))
                            (org-html-encode-plain-text current-file)))))
    fields))
(defadvice! my-anki-editor-fix-attach-dir (fn &rest r)
 "Make ‘org-attach-id-dir’ absolute in ‘anki-editor-note-at-point’."
 :around #'anki-editor-note-at-point
 (require 'org-attach)
 (let ((org-attach-id-dir (file-truename org-attach-id-dir)))
   (apply fn r)))

(use-package! od2ae
  :commands (od2ae-convert-entry-to-anki od2ae-convert-all-notes)
  :config
  (setq! od2ae-deck "/"))

;;;* Org-roam
;; Needed by ‘org-roam-setup' - for some reason this is not being loaded.
(require 'org-duration)
(use-package! org-roam
  :hook (after-init . org-roam-setup)
  :init
  (setq! org-roam-directory org-directory)
  (setq! org-roam-extract-new-file-path "home-org/roam/${slug}.org")
  (setq! org-roam-dailies-directory "home-org/roam/daily/")
  (setq! org-roam-v2-ack t)
  (setq! org-roam-link-title-format "§%s")
  (setq! org-roam-db-node-include-function (lambda () t))
  (setq! +org-roam-auto-backlinks-buffer t)
  ;; (setq! org-roam-db-node-include-function
  ;;        (lambda ()
  ;;         (not (member "drill" (org-get-tags)))))
 nil)
(after! org-roam
  (require 'org-roam-compat)
  ;; Copied from doom.emacs.d/modules/lang/org/contrib/roam2.el
  (map! (:prefix ("C-c n" . "org-roam")
         "D" #'org-roam-demote-entire-buffer
         "f" #'org-roam-node-find
         "F" #'org-roam-ref-find
         "g" #'org-roam-graph
         "i" #'org-roam-node-insert
         "I" #'org-id-get-create
         "m" #'org-roam-buffer-toggle
         "M" #'org-roam-buffer-display-dedicated
         "n" #'org-roam-capture
         "r" #'org-roam-refile
         "R" #'org-roam-link-replace-all
         (:prefix ("d" . "by date")
          :desc "Goto previous note" "b" #'org-roam-dailies-goto-previous-note
          :desc "Goto date"          "d" #'org-roam-dailies-goto-date
          :desc "Capture date"       "D" #'org-roam-dailies-capture-date
          :desc "Goto next note"     "f" #'org-roam-dailies-goto-next-note
          :desc "Goto tomorrow"      "m" #'org-roam-dailies-goto-tomorrow
          :desc "Capture tomorrow"   "M" #'org-roam-dailies-capture-tomorrow
          :desc "Capture today"      "n" #'org-roam-dailies-capture-today
          :desc "Goto today"         "t" #'org-roam-dailies-goto-today
          :desc "Capture today"      "T" #'org-roam-dailies-capture-today
          :desc "Goto yesterday"     "y" #'org-roam-dailies-goto-yesterday
          :desc "Capture yesterday"  "Y" #'org-roam-dailies-capture-yesterday
          :desc "Find directory"     "-" #'org-roam-dailies-find-directory)
         (:prefix ("o" . "node properties")
          "a" #'org-roam-alias-add
          "A" #'org-roam-alias-remove
          "t" #'org-roam-tag-add
          "T" #'org-roam-tag-remove
          "r" #'org-roam-ref-add
          "R" #'org-roam-ref-remove)
         (:prefix ("s" . "Promnesia search")
          :desc "Personal Promnesia search"     "p" #'my-promnesia-search-personal
          :desc "Corp Promnesia search"         "c" #'my-promnesia-search-corp
          :desc "All profiles Promnesia search" "a" #'my-promnesia-search-all)))
      ;; (defun my-org-roam-capture-split-window (&rest _args)
      ;;   "Split current window and select new window."
      ;;   (unless (eq org-roam-capture--context 'ref)
      ;;     (select-window (split-window))))
      ;; (advice-add 'org-roam--capture :before #'my-org-roam-capture-split-window)
  (defun my-org-roam-get-first-node-title ()
    "Get title from #+title, or from first node in Org-roam file."
    (require 'org-roam-node)
    (require 'org-roam)
    (unless (org-roam-file-p)
      (user-error "Must be called from inside an Org-roam file"))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward org-outline-regexp-bol nil t)
        (let* ((node (org-roam-node-at-point))
               (file-title (org-roam-get-keyword "TITLE")))
          (cond
            ((and file-title (not (string= "" file-title)))
             file-title)
            (node (org-roam-node-title node))
            (t nil))))))
  (defun my-org-roam-set-buffer-name-hook ()
    "Set buffer name of org-roam files."
    (with-demoted-errors "Error: %S"
     (when-let (((org-roam-file-p))
                ((s-contains?
                  "/roam/"
                  (buffer-file-name (buffer-base-buffer))))
                (title (my-org-roam-get-first-node-title)))
       (rename-buffer title))))
  (add-hook 'find-file-hook #'my-org-roam-set-buffer-name-hook)
  (defun my-org-hugo-get-roam-title (fn &optional backend subtreep ext-plist)
    "Set title for ‘org-export’ according to ‘my-org-roam-get-first-node-title’."
    (let ((info (funcall fn backend subtreep ext-plist)))
      (if-let* (((org-roam-file-p))
                ((null (plist-get info :title)))
                (title (my-org-roam-get-first-node-title)))
          (org-combine-plists info `(:title (,title)))
        info)))
  (advice-add #'org-export-get-environment :around #'my-org-hugo-get-roam-title)
  (cl-defun my-org-roam-relocate-property-drawer-after-capture ()
    "Relocate PROPERTIES drawer to first node after capture of a new file."
    (message "file %S" (buffer-file-name))
    (unless (org-roam-capture--get :new-file)
      (cl-return-from my-org-roam-relocate-property-drawer-after-capture nil))
    (message "in new file %S" (buffer-file-name))
    (save-excursion
      (goto-char (point-min))
      (when-let* (((org-at-property-drawer-p))
                  ((re-search-forward org-property-drawer-re nil t))
                  (start (match-beginning 0))
                  (end (match-end 0))
                  ((equal (point-min) start))
                  (drawer (buffer-substring start (+ end 1))))
        (message "drawer %S" drawer)
        (with-undo-collapse
          (when (save-excursion
                  (re-search-forward org-outline-regexp-bol nil t))
           (kill-region start (+ end 1))
           (re-search-forward org-outline-regexp-bol)
           (end-of-line)
           (newline)
           (insert drawer))))))
  (cl-defun my-org-roam-dailies-today-id ()
    "Return the ID of today’s org-roam dailies entry.
Create the entry if it does not exist."
    ;; ‘save-window-excursion’ to avoid closing the current capture window if
    ;; used from ‘org-capture’.
    (save-window-excursion
     (save-excursion
      (let ((org-roam-dailies-capture-templates
             (cl-copy-list org-roam-dailies-capture-templates)))
        (plist-put! (nthcdr 4 (assoc "D" org-roam-dailies-capture-templates))
                    :immediate-finish t :jump-to-captured t)
        (org-roam-dailies--capture (org-read-date nil t "%<%y-%m-%d>") t "D"))
      (goto-char (point-min))
      (org-next-visible-heading 1)
      (org-id-get-create))))
  (cl-defun my-org-roam-dailies-today-link ()
    "Return a link to today’s ‘org-roam' dailies entry."
    (org-link-make-string
     (concat "id:" (my-org-roam-dailies-today-id))
     (format-time-string "%Y-%m-%d" org-overriding-default-time)))
  (cl-defun my-org-roam-dailies-link-to-today ()
    "Link the current headline to ‘org-roam’ today’s daily headline."
    (interactive)
    (save-excursion
      ;; Go to first heading.
      (goto-char (point-min))
      (outline-next-heading)
      ;; Go to end of current heading (before any subheadings) and insert the date there.)
      (outline-next-heading)
      (if (eobp)
          (progn
            (end-of-line)
            (open-line 1)
            (forward-line 1))
        (forward-line -1)
        (unless (org--line-empty-p 1)
          (end-of-line)
          (open-line 1)
          (forward-line 1)))
      (insert (my-org-roam-dailies-today-link))
      nil))
  (cl-defun my-org-roam-add-to-inbox ()
    "Run ‘org-drill-type-inbox-init’ on the current top-level ‘org-roam’ headline."
    (interactive)
    (save-excursion
      ;; Go to first heading.
      (goto-char (point-min))
      (outline-next-heading)
      ;; And add to inbox
      (org-drill-type-inbox-init)
      nil))
  (add-hook 'org-roam-capture-new-node-hook
            #'my-org-roam-relocate-property-drawer-after-capture
            100)
  (require 'org-roam-protocol)
  (require 'org-roam-dailies)
  (require 'org-roam-capture)
  (require 'cl-lib)
  (setq! org-roam-capture-templates
         (cl-copy-list org-roam-capture-templates))
  (plist-put! (nthcdr 4 (assoc "d" org-roam-capture-templates))
              :immediate-finish t :jump-to-captured t
              :target
              '(file+head
                "home-org/roam/${slug}.org"
                "\
#+setupfile: common.setup
#+date: %U

* ${title}"))
  (setf (alist-get "D" org-roam-capture-templates nil nil #'equal)
        (cons
         "TikTok"
         (cdr (cl-copy-list
               (alist-get "d" org-roam-capture-templates nil nil #'equal)))))
  (let* ((rval (nthcdr 4 (assoc "D" org-roam-capture-templates)))
         (target (cl-copy-list (plist-get rval :target))))
    (plist-put! rval
                :target
                (list
                 (nth 0 target)
                 "tiktok-org/roam/${slug}.org"
                 (nth 2 target))))
 (defun org-capture-plist-dump ()
   "Dump variables for org-roam-capture plists for debugging."
  (message "org-capture-plist: %s"
           (pp-to-string org-capture-plist))
  (message "org-capture-current-plist: %s"
   (pp-to-string org-capture-current-plist))
  (message "org-roam-capture--info: %s"
   (pp-to-string org-roam-capture--info))
  (message "org-roam-capture--node: %s"
   (pp-to-string org-roam-capture--node))
  (message "org-capture-templates: %s"
           (pp-to-string org-capture-templates))
  (message "org-roam-capture-templates: %s"
           (pp-to-string org-roam-capture-templates)))
 (cl-defun my-org-roam-after-finalize-link-to-today ()
   (when-let*
       (
        ;; First 2 checks confirm that we’re capturing for ‘org-roam-capture’.
        ((and org-roam-capture--node))
        ((org-capture-get :org-roam))
        (key (org-capture-get :key))
        (buffer (org-capture-get :buffer)))
     (save-excursion
       (with-current-buffer buffer
         ;; Skip D from ‘org-roam-dailies-capture-templates’.
         (when (equal "D" key)
           (cl-return-from my-org-roam-after-finalize-link-to-today nil))
         (my-org-roam-dailies-link-to-today)
         (when (member key '("R" "G"))
           (my-org-roam-add-to-inbox))))))
 (add-hook 'org-capture-after-finalize-hook
  #'my-org-roam-after-finalize-link-to-today)
 (setq! org-roam-capture-ref-templates
  (cl-copy-list org-roam-capture-ref-templates))
 (plist-put! (nthcdr 4 (assoc "r" org-roam-capture-ref-templates))
             :immediate-finish t :jump-to-captured t
             :target
             '(file+head
               "home-org/roam/${slug}.org"
               "\
#+setupfile: common.setup
#+date: %U

* ${title}
%(when-let* ((body (plist-get org-roam-capture--info :body))
             ((not (string-empty-p body))))
    (format \"\
#+BEGIN_SRC html
<blockquote>
%s
</blockquote>
#+END_SRC
\" body))"))
 (setq! org-roam-dailies-capture-templates
  '(("D" "default" entry "* %?\n%U" :target
     (file+head "%<%Y-%m-%d>.org" "\
#+setupfile: common.setup
* %<%Y-%m-%d>
[[elisp:(let ((org-agenda-sticky nil) (org-agenda-include-inactive-timestamps t) (org-agenda-window-setup 'reorganize-frame)) (message \"org-roam-dailes-capture-templates: overriding time %S\" org-default-overriding-time) (org-agenda-list nil \"%<%Y-%m-%d>\"))][(agenda)]]
#+comment: If this file is blank after capturing daily log or weekly review, try that command again.
"))))
  ;; "R" is like "r" but also runs ‘org-drill-type-inbox-init' - the templates
  ;; are exactly the same, but the behavior is controlled by
  ;; ‘my-org-roam-after-finalize-link-to-today’.
 (setf (alist-get "R" org-roam-capture-ref-templates nil nil #'equal)
       (cl-copy-list (alist-get "r" org-roam-capture-ref-templates nil nil #'equal)))
  ;; "g" and "G" are exactly like "r" and "R", but they put the captured node in
  ;; the tiktok-org repo instead.
 (progn
   (setf (alist-get "g" org-roam-capture-ref-templates nil nil #'equal)
         (cl-copy-list (alist-get "r" org-roam-capture-ref-templates nil nil #'equal)))
   (let* ((rval (nthcdr 4 (assoc "g" org-roam-capture-ref-templates)))
          (target (cl-copy-list (plist-get rval :target))))
     (plist-put! rval
                 :target
                 (list
                  (nth 0 target)
                  "tiktok-org/roam/${slug}.org"
                  (nth 2 target))))
   (setf (alist-get "G" org-roam-capture-ref-templates nil nil #'equal)
         (cl-copy-list (alist-get "R" org-roam-capture-ref-templates nil nil #'equal)))
   (let* ((rval (nthcdr 4 (assoc "G" org-roam-capture-ref-templates)))
          (target (cl-copy-list (plist-get rval :target))))
     (plist-put! rval
                 :target
                 (list
                  (nth 0 target)
                  "tiktok-org/roam/${slug}.org"
                  (nth 2 target)))))
 (el-patch-defun org-roam-protocol--insert-captured-ref-h ()
   "Insert the ref if any."
   (message "org-roam-capture--info: %S" org-roam-capture--info)
   (message "org-roam-capture--node: %S" org-roam-capture--node)
   (message "title: %S" (and (org-roam-node-p org-roam-capture--node)
                             (org-roam-node-title org-roam-capture--node)))
   (when-let ((ref (plist-get org-roam-capture--info :ref)))
    (el-patch-swap
     (org-roam-ref-add ref)
     (org-roam-ref-add
        (if-let ((title
                  (when (org-roam-node-p org-roam-capture--node)
                    (org-roam-node-title org-roam-capture--node))))
         (org-link-make-string ref title)
         ref)))))
 nil)                                  ; To make eval-region on previous block easier


(defun my-org-roam-capture-daily-from-file (input)
  "Run ‘org-roam-dailies-capture-today’ using input from file INPUT.
File INPUT has the heading on the first line and content on the following lines.
Contents are inserted literally, so make sure they are valid Org-mode syntax."
  (let* ((input-buf (find-file-noselect input)))
    (save-window-excursion
      (save-excursion
        (let (title body marker)
          (org-roam-dailies-capture-today)
          (save-excursion
            (with-current-buffer input-buf
              (setq-local buffer-read-only t)
              (goto-char (point-min))
              (setq title (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
              (forward-line 1)
              (setq body (buffer-substring-no-properties (point-at-bol) (point-max)))))
          ;; Ensure point is at end of line so that I can insert the title.
          (end-of-line)
          (unless (equal ?\s (char-before))
            (insert " "))
          (insert (string-trim title))
          (newline)
          (insert (string-trim body))
          (newline)
          (when org-capture-mode
            (org-capture-finalize)))))
    (kill-buffer input-buf)))

(defconst my-promnesia-extension-url-release
  "chrome-extension://kdmegllpofldcpaclldkopnnjjljoiio")
(defconst my-promnesia-extension-url-dev
  "chrome-extension://epkkfejibedemmekobfclekmekmdmeep")
(defcustom my-promnesia-extension-url my-promnesia-extension-url-dev
  "URL to open Promnesia extension in Chrome.")
(defun my-promnesia-search-url-with-query (query)
  "Format URL for Promnesia Search, with QUERY if non-nil and non-empty."
  (let ((query-string
         (when (and (stringp query) (not (string-empty-p query)))
           (url-build-query-string `((q ,query))))))
    (format "%s/search.html%s" my-promnesia-extension-url
            (if query-string (concat "?" query-string) ""))))
(defun my-promnesia-search-personal (&optional query)
  "Search Promnesia in personal profile."
  (interactive "sPromesia search query: ")
  (start-process
   "promnesia-search-personal"
   "*promnesia-search-personal*"
   (expand-file-name "google-chrome-personal"
                     doom-private-dir)
   (my-promnesia-search-url-with-query query)))
(defun my-promnesia-search-corp (&optional query)
  "Search Promnesia in corp profile."
  (interactive "sPromesia search query: ")
  (start-process
   "promnesia-search-corp"
   "*promnesia-search-corp*"
   (expand-file-name "google-chrome-corp"
                     doom-private-dir)
   (my-promnesia-search-url-with-query query)))
(defun my-promnesia-search-all (&optional query)
 "Search Promnesia in corp profile."
 (interactive "sPromesia search query: ")
 (dolist (f '(my-promnesia-search-personal my-promnesia-search-corp))
   (funcall f query)))

(defun my-org-html-to-org-quote ()
  "Change an HTML blockquote to Org quote.
Changes this

#+BEGIN_SRC html
<blockquote>
[html ...]
</blockquote>
#+END_SRC html

to this:

#+BEGIN_QUOTE
[org ...]
#+END_QUOTE"
  (interactive)
  (let* ((begin (if (use-region-p) (region-beginning) (point-min)))
         (end (if (use-region-p) (region-end) (point-max)))
         (regexp
          (rx-to-string
           `(and
             line-start
             "#+BEGIN_SRC html"
             ?\n
             "<blockquote>"
             ?\n
             (group
              (*? anychar))
             ?\n
             "</blockquote>"
             ?\n
             "#+END_SRC"
             line-end))))
    (save-excursion
      (save-match-data
        (goto-char begin)
        (while (re-search-forward regexp end t)
          (let ((m (match-string-no-properties 1)))
            (replace-match
             (format
              "\
#+BEGIN_QUOTE
%s#+END_QUOTE"                          ; Pandoc output has final newline
              ;; Must save match data so that ‘replace-match’ can work, because
              ;; the manipulations in the temp buffer can change it.
              (save-match-data
               (with-temp-buffer
                 (insert m)
                 (call-process-region
                  (point-min) (point-max)
                  "pandoc" t t t
                  "-f" "html" "-t" "org" "--wrap=preserve")
                 (buffer-string))))
             nil t)))))))

(after! org-roam-node
 (el-patch-defun org-roam-extract-subtree ()
  "Convert current subtree at point to a node, and extract it into a new file."
  (interactive)
  (save-excursion
    (org-back-to-heading-or-point-min t)
    (when (bobp) (user-error "Already a top-level node"))
    (org-id-get-create)
    (save-buffer)
    (org-roam-db-update-file)
    (let* ((template-info nil)
           (node (org-roam-node-at-point))
           (template (org-roam-format-template
                      (string-trim (org-capture-fill-template org-roam-extract-new-file-path))
                      (lambda (key default-val)
                        (let ((fn (intern key))
                              (node-fn (intern (concat "org-roam-node-" key)))
                              (ksym (intern (concat ":" key))))
                          (cond
                           ((fboundp fn)
                            (funcall fn node))
                           ((fboundp node-fn)
                            (funcall node-fn node))
                           (t (let ((r (read-from-minibuffer (format "%s: " key) default-val)))
                                (plist-put template-info ksym r)
                                r)))))))
           (file-path (read-file-name "Extract node to: "
                                      (file-name-as-directory org-roam-directory) template nil template)))
      (when (file-exists-p file-path)
        (user-error "%s exists. Aborting" file-path))
      (org-cut-subtree)
      (save-buffer)
      (with-current-buffer (find-file-noselect file-path)
        (org-paste-subtree)
        ;; I don’t know why this doesn’t just use my
        ;; ‘org-roam-capture-templates’, so I have to replicate this on my own.
        (el-patch-swap
          (org-roam-promote-entire-buffer)
          (progn
            (save-excursion
              (goto-char (point-min))
              (insert
               (format "\
#+setupfile: common.setup
#+date: %s

"
                       (format-time-string (org-time-stamp-format t t)))))
            (save-excursion
               (my-org-roam-dailies-link-to-today))))
        (save-buffer)))))
 (defalias 'org-roam-create-note-from-headline #'org-roam-extract-subtree))

(defcustom my-org-roam-directories (list org-roam-directory)
  "List of org-roam directories to examine in ‘my-org-roam-agenda-file-hook’.")
(defun my-org-roam-agenda-file-hook ()
  "Add recently-modified org-roam files to ‘org-agenda-files’."
  (interactive)
  (async-start
   `(lambda ()
      ,(async-inject-variables "^\\(load-path\\|my-org-roam-directories\\)$")
      (require 'cl-lib)
      (require 'org-ql)
      (require 'f)
      (require 's)
      (let* (text-search agenda)
        (dolist (dir my-org-roam-directories)
          (let
              ((files
                (f-files dir
                         (lambda (f) (s-ends-with? ".org" f))
                         'recursive)))
            (push files text-search)
            (push
             (org-ql-select
                files
                `(or
                  (ts :from -60)
                  (tags-local "inbox" "drill"))
                :action
                (lambda () (buffer-file-name (current-buffer))))
             agenda)))
        (delete-dups agenda)
        (delete-dups text-search)
        (list agenda text-search)))
   (lambda (res)
     (setq!
      org-agenda-files
      (apply #'append (org-agenda-expand-files-name) (nth 0 res)))
     (setq!
      org-agenda-text-search-extra-files
      (apply #'append org-agenda-text-search-extra-files (nth 1 res)))
     (delete-dups org-agenda-files)
     (delete-dups org-agenda-text-search-extra-files))))

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
(setq! org-attach-id-dir "data/")
(setq! org-attach-use-inheritance t)
(use-package! org-download
  :hook (dired-mode . org-download-enable)
  :custom
  (org-download-method 'attach)
  (org-download-annotate-function #'org-download-annotate-default)
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
  (require 's)
  (require 'org)
  (require 'org-clock-csv)
  (org-save-all-org-buffers)
  ;; Execute this in a separate process to avoid blocking Emacs - since
  ;; ‘org-clock-csv’ uses ‘org-element’ to read the Org files, it is quite slow
  ;; - I’ve had it take over 30 minutes with my Agenda.
  (deferred:try
    (deferred:process-shell
      (format (concat "nice %s %s --batch --eval \"(progn (require 'org-clock-csv) (org-clock-csv-batch-and-exit))\" %s"
                      "| nice %s --org_clock_csv /dev/stdin --calendar_id %s --logging_level DEBUG")
              (shell-quote-argument (file-truename
                                     (expand-file-name invocation-name
                                                       invocation-directory)))
              (s-join " "
                      (mapcar
                       (lambda (lib)
                         (format "-L %s"
                                 (shell-quote-argument
                                  (file-name-directory
                                   (locate-library lib)))))
                       '("s" "org" "org-clock-csv")))
              (s-join " "
                      (mapcar #'shell-quote-argument
                              (org-agenda-files)))
              (shell-quote-argument
               (expand-file-name "org_clock_csv_calendar_export.py"
                                 doom-private-dir))
              (shell-quote-argument
               org-clock-csv-calendar-export-id)))
    :finally
    (lambda (_)
      (org-notify "org-clock-csv-calendar-export finished"))))


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
                 (no-update (org-entry-get marker "effort-no-update-from-children"))
                 (children-effort 0))
              (when no-update
                (clog/msg "Not updating effort at %S due to set property “effort-no-update-from-children”"
                          marker)
                (setq abort-at-marker marker)
                (throw 'break 'abort-at-marker))
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

(setq! org-log-state-notes-insert-after-drawers t)
(defun org-state-notes-convert-to-headings ()
  "Convert all state notes in current entry to headings.

Works best when ‘org-log-state-notes-insert-after-drawers’ is non-nil, so that
you can run this command over and over again as you insert state notes."
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (when (re-search-forward (org-item-beginning-re))
      ;; Search for last top-level list item.
      (while
          (condition-case nil
              (progn (org-next-item) t)
            (error nil)))
      (let ((inhibit-redisplay t))
        (with-undo-collapse
          (while
                (condition-case nil
                      (progn (org-previous-item) t)
                    (error nil))
              (save-excursion
                  (org-next-item)
                  (org-toggle-heading)
                  ;; Replace item fold characters \\
                  (replace-regexp
                     (rx-to-string '(seq (1+ " ") "\\\\" eol)) "" nil
                     (point-at-bol) (+ 1 (point-at-eol)))))
          (org-toggle-heading)
          ;; Replace item fold characters \\
          (replace-regexp
             (rx-to-string '(seq (1+ " ") "\\\\" eol)) "" nil
             (point-at-bol) (+ 1 (point-at-eol))))))))
;; From https://emacs.stackexchange.com/a/54411/17182
(defmacro with-undo-collapse (&rest body)
  "Like `progn' but perform BODY with undo collapsed."
  (declare (indent 0) (debug t))
  (let ((handle (make-symbol "--change-group-handle--"))
        (success (make-symbol "--change-group-success--")))
    `(let ((,handle (prepare-change-group))
            ;; Don't truncate any undo data in the middle of this.
           (undo-outer-limit nil)
           (undo-limit most-positive-fixnum)
           (undo-strong-limit most-positive-fixnum)
           (,success nil))
       (unwind-protect
         (progn
           (activate-change-group ,handle)
           (prog1 ,(macroexp-progn body)
             (setq ,success t)))
         (if ,success
           (progn
             (accept-change-group ,handle)
             (undo-amalgamate-change-group ,handle))
           (cancel-change-group ,handle))))))

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
         ;; Each group has an implicit boolean OR operator between its selectors.
         '(
           ;; Discard top-level recurring events fetched by org-gcal unless I’ve
           ;; done something to indicate I’m using them.
           (:discard
            (:tag "ARCHIVE"
             :and
             (:todo "CANCELLED")
             ;; TODO: re-enable once
             ;; https://github.com/alphapapa/org-super-agenda/issues/227 is
             ;; fixed and the fix present in the installed version of
             ;; ‘org-super-agenda’.
             ;; :and
             ;; (
             ;;   ;; Not using ~:property "recurrence"~ because the properties
             ;;   ;; there are inherited, which means it will match all the
             ;;   ;; children too.
             ;;   :regexp "^[ \t]*:recurrence:"
             ;;   :not (:log t)
             ;;   :not (:habit t))
             :and
             (:tag "inbox")))
           (:name "Today"  ; Optionally specify section name
            :time-grid t  ; Items that appear on the time grid
            :todo "TODAY")  ; Items that have this TODO keyword
           (:name "Important (max in-flight should be 3)" :priority "A")
           ;; Set order of multiple groups at once
           (:todo "WAITING")
           (:name "Drill" :tag "drill")
           (:name "Today" :scheduled today :deadline today)
           (:name "Overdue"
            :and
            (:scheduled past :deadline past))
           (:name "Habits" :habit t)
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

(defadvice! my-org-agenda-goto-today-refresh (_orig-fn &rest _args)
  "Refresh agenda if ‘org-agenda-goto-today’ doesn’t go to today’s agenda.

There is a bug that appears when ‘org-agenda-sticky’ is enabled where, sometimes,
calling ‘org-agenda-goto-today’ in the agenda goes to the line containing the
agenda date, but doesn’t change to whatever today’s date is.  The workaround
is to refresh the agenda and call ‘org-agenda-goto-today’ again."
  :around #'org-agenda-goto-today
  (apply _orig-fn _args)
  (forward-line 0)
  (if (looking-at "^\\w+ +\\([0-9]+ \\w+ [0-9]+\\) ")
    (let* ((agenda-date-string (match-string-no-properties 1))
           (agenda-date
            (cl-subseq
             (timezone-parse-date (format "%s 00:00" agenda-date-string))
             0 3))
           (current-date
            (cl-subseq
             (timezone-parse-date
              (format-time-string
               (concat org-super-agenda-date-format " %H:%M")))
             0 3)))
      (when (not (equal agenda-date current-date))
        (clog/msg "agenda-date != current-date, refreshing: %S %S"
                  agenda-date current-date)
        (org-agenda-redo)
        (apply _orig-fn _args)))
    (message "Did not find date on current agenda line: %S %s"
             (point-marker)
             (buffer-substring-no-properties
              (point-at-bol) (point-at-eol)))
    (apply _orig-fn _args)))


(setq! org-list-allow-alphabetical t)

;; See https://github.com/hlissner/doom-emacs/issues/3185
(defadvice! no-errors/+org-inline-image-data-fn (_protocol link _description)
  :override #'+org-inline-image-data-fn
  "Interpret LINK as base64-encoded image data. Ignore all errors."
  (ignore-errors
    (base64-decode-string link)))

(defun my-disable-after-save-hook-for-counsel-rg-org (fn &rest _args)
  "Disable the after-save hook named by FN when save comes from ‘counsel-rg-org'."
  (unless (memq real-this-command '(rg-org counsel-rg-org consult-ripgrep-org))
    (apply fn _args)))
(advice-add #'org-hugo-export-wim-to-md-after-save
            :around #'my-disable-after-save-hook-for-counsel-rg-org)
(advice-add #'gac-after-save-func
            :around #'my-disable-after-save-hook-for-counsel-rg-org)


;; Override org-refresh-category-properties to use the fixed version from
;; Org-mode upstream. The old version is breaking anki-editor.
(when-let*
    ;; Only execute this code when using yantar92/org fork.
    ((package (alist-get 'org doom-packages))
     (recipe (plist-get package :recipe))
     ((string= "yantar92/org"
               (or (plist-get (plist-get recipe :fork) :repo)
                   (plist-get recipe :repo)))))
  (cond
   ((string= (org-git-version) "9.5-ec36031b4")
    (defadvice! my-org-refresh-category-properties-override ()
      "Refresh category text properties in the buffer."
      :override #'org-refresh-category-properties
      (let ((case-fold-search t)
            (inhibit-read-only t)
            (default-category
              (cond ((null org-category)
                     (if buffer-file-name
                         (file-name-sans-extension
                          (file-name-nondirectory buffer-file-name))
                       "???"))
                    ((symbolp org-category) (symbol-name org-category))
                    (t org-category))))
        (with-silent-modifications
          (org-with-wide-buffer
           ;; Set buffer-wide property from keyword.  Search last #+CATEGORY
           ;; keyword.  If none is found, fall-back to `org-category' or
           ;; buffer file name, or set it by the document property drawer.
           (put-text-property
            (point-min) (point-max)
            'org-category
            (catch 'buffer-category
              (goto-char (point-max))
              (while (re-search-backward "^[ \t]*#\\+CATEGORY:" (point-min) t)
                (let ((element (org-element-at-point)))
                  (when (eq (org-element-type element) 'keyword)
                    (throw 'buffer-category
                           (org-element-property :value element)))))
              default-category))
           ;; Set categories from the document property drawer or
           ;; property drawers in the outline.  If category is found in
           ;; the property drawer for the whole buffer that value
           ;; overrides the keyword-based value set above.
           (goto-char (point-min))
           (let ((regexp (org-re-property "CATEGORY")))
             (while (re-search-forward regexp nil t)
               (let ((value (match-string-no-properties 3)))
                 (when (org-at-property-p)
                   (put-text-property
                    (save-excursion (org-back-to-heading-or-point-min t))
                    (save-excursion (if (org-before-first-heading-p)
                                        (point-max)
                                      (org-end-of-subtree t t)))
                    'org-category
                    value))))))))))
   (t
    (display-warning
     'org-config "org-refresh-categories-properties: Check if version in current code is like https://github.com/emacs-straight/org/blob/38362699d175339330af63a57d40a47b4f748f5a/lisp/org.el#L8607-L8650. Remove this override if so."))))

;; Override org-hugo-auto-export-mode to export asynchronously (why on earth is
;; this not the default?)
(el-patch-feature org-hugo-auto-export-mode)
(after! org-hugo-auto-export-mode
  (el-patch-defun org-hugo-export-wim-to-md-after-save ()
    "Function for `after-save-hook' to run `org-hugo-export-wim-to-md'.

The exporting happens only when Org Capture is not in progress."
    (unless (eq real-this-command 'org-capture-finalize)
      (save-excursion
        (el-patch-swap
          (org-hugo-export-wim-to-md)
          (org-hugo-export-wim-to-md nil 'async))))))

;; Link to commit by commit message rather than commit (which isn’t stable when
;; I rebase.)
(defadvice! my-orgit-rev-store-by-commit-message ()
  :override #'orgit-rev-store
  "Store a link to a Magit-Revision mode buffer.
This advice overrides ‘orgit-rev-store’ to refer to the commit using the commit
message rather than the commit hash."
  (cond ((eq major-mode 'magit-revision-mode)
         (my-orgit-rev-store-by-commit-message-1 magit-buffer-revision))
        ((derived-mode-p 'magit-mode)
         (when-let ((revs (magit-region-values 'commit)))
           (mapc 'my-orgit-rev-store-by-commit-message-1 revs)
           t))))
(defun my-orgit-rev-store-by-commit-message-1 (rev)
  (let* ((repo (orgit--current-repository))
         (summary (magit-git-str "log" "--pretty=%s" "-n1" rev))
         (ref (format ":/^%s"
                      (replace-regexp-in-string
                       "[][\\/^.$|()*+?{}]"
                       "[\\1]"
                       summary))))
    (org-link-store-props
     :type        "orgit-rev"
     :link        (format "orgit-rev:%s::%s" repo
                          (or ref (magit-rev-parse rev)))
     :description (format-spec
                   (magit-rev-format "%%N - magit-rev %s" ref)
                   `((?N . ,repo))))))

(defun cancel-and-move-to-next (&optional count)
  "Cancel current heading and move to next one."
  (interactive "p")
  (let ((macro [109 98 47 111 114 103 45 103 99 97 108 58 return 106 94 108 118 116 62 34 97 121 32 117 21 21 3 20 99 3 11 39 98 106 94 102 91 108 118 105 93 34 97 112 70 45 100 116 93 3 14]))
    (org-back-to-heading)
    (org-fold-show-all)
    (evil-execute-macro (or count 1) macro)))

;;; Local Variables:
;;; outline-regexp: ";;;\\*+\\|\\`"
;;; End:
