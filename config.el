;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(setq! custom-file (expand-file-name "custom.el" doom-private-dir))
(load custom-file)

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq! user-full-name "Robert Irelan"
       user-mail-address "rirelan@gmail.com")

(add-to-list 'debug-ignored-errors 'search-failed)
(setq! debug-on-message nil)
(defun dbgmsg (format-string &rest args)
  "Log a debug message using ‘message’.
Primarily exists to easily find and remove after code is written."
  (apply #'message (concat "[dbgmsg] " format-string) args))

;; Add timestamps to log messages in the "*Messages*" buffer. Override
;; ‘doom--timestamped-message-a’ to make timestamps millisecond precision and
;; highlight the timestamp.
(el-patch-defun doom--timestamped-message-a (format-string &rest args)
  "Advice to run before `message' that prepends a timestamp to each message.

Activate this advice with:
(advice-add 'message :before 'doom--timestamped-message-a)"
  (when (and (stringp format-string)
             message-log-max
             (not (string-equal format-string "%s%s")))
    (with-current-buffer "*Messages*"
      (let ((timestamp (format-time-string
                        (el-patch-swap "[%F %T] " "[%F %T.%3N]")
                        (current-time)))
            (deactivate-mark nil))
        (el-patch-swap
          (with-silent-modifications
            (goto-char (point-max))
            (if (not (bolp))
                (newline))
            (insert timestamp))
          (with-silent-modifications
            (let (begin end)
              (goto-char (point-max))
              (if (not (bolp))
                  (newline))
              (setq begin (point))
              (insert timestamp)
              (setq end (point))
              ;; Adding the space *before* setting text property is important to
              ;; ensure the text of the message isn’t colored.
              (insert " ")
              (put-text-property begin end 'face 'header-line))))))
    (let ((window (get-buffer-window "*Messages*")))
      (when (and window (not (equal (selected-window) window)))
        (with-current-buffer "*Messages*"
          (goto-char (point-max))
          (set-window-point window (point-max)))))))
(advice-add 'message :before 'doom--timestamped-message-a)
(setq message-log-max 20000)

;; Disabled by Doom Emacs, but I want it.
(setq! auto-save-default t)

;; reset these values to ‘gcmh’ defaults. Doom sets them too low for my usage.
(after! gcmh
  (setq! gcmh-high-cons-threshold #x40000000)
  (setq! gcmh-idle-delay 15))

(setq! custom-file (expand-file-name "custom.el" doom-private-dir))

;; By default, C-i is equivalent to TAB. Remove this.
(define-key key-translation-map [?\C-i]
  (λ! (if (and (not (cl-position 'tab    (this-single-command-raw-keys)))
               (not (cl-position 'kp-tab (this-single-command-raw-keys)))
               (display-graphic-p))
          [C-i] [?\C-i])))

(after! evil
  (add-to-list 'evil-emacs-state-modes 'image-mode)
  (add-to-list 'evil-emacs-state-modes 'magit-mode)
  (add-to-list 'evil-emacs-state-modes 'Custom-mode)
  (add-to-list 'evil-emacs-state-modes 'profiler-report-mode)
  (add-to-list 'evil-emacs-state-modes 'pocket-reader-mode)
  (add-to-list 'evil-emacs-state-modes 'rmail-mode))
(after! evil-collection
  (setq! evil-collection-setup-minibuffer t)
  (add-to-list '+evil-collection-disabled-list 'magit)
  (add-to-list '+evil-collection-disabled-list 'outline))
(use-package! evil-collection
  :custom (evil-collection-setup-minibuffer t))
(after! evil-visual-mark-mode
  (evil-visual-mark-mode t))

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "monospace" :size 12))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one-light)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;;; Use auto-compile to recompile bytecode whenever files are loaded or saved.
(use-package! auto-compile
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(defun server-process-matches-server-file ()
  "Check that the server process in ‘server-auth-dir’ matches the currently running server process in this instance of Emacs."
  (when server-process
    (with-temp-buffer
      (let ((file (expand-file-name server-name server-auth-dir)))
        (insert-file-contents-literally file)
        (when-let* (((looking-at "127\\.0\\.0\\.1:\\([0-9]+\\) \\([0-9]+\\)"))
                    (match1 (match-string 1))
                    (port (string-to-number match1))
                    (match2 (match-string 2))
                    (pid (string-to-number match2))
                    (server-port (nth 1 (process-contact server-process)))
                    (server-file (plist-get (process-plist server-process)
                                            :server-file)))
          (and (= port server-port)
               (= pid (emacs-pid))
               (string= server-file file)))))))
(after! server
  :config
  (setq! server-name "server")
  (setq! server-use-tcp t)
  (defun my-server-warn-if-stopped ()
    (unless (eq t (server-running-p))
      (display-warning
       'config "Emacs server stopped running - run ‘+default/restart-server'")))
  (run-at-time 30 60 #'my-server-warn-if-stopped)
  (if (eq t (server-running-p))
      (display-warning
       'config "Not starting server - server with name \"%s\" already running%s"
       server-name
       (if (server-process-matches-server-file)
           " in this process"
         (format " (in another Emacs process - check %s)"
                 (expand-file-name server-name server-auth-dir))))
    (+default/restart-server)))

;; Work around https://github.com/hlissner/doom-emacs/issues/5692
(when (fboundp '+vertico/consult-fd)
  (require 'consult)
  (require 'vertico-directory))
(after! vertico
  (map! :map vertico-map
        "<prior>" #'vertico-scroll-down
        "<next>" #'vertico-scroll-up))

(defun which-key-show-local-map ()
  "Show contents of ‘current-local-map’."
  (interactive)
  (load "which-key")                    ; Force autoload
  (which-key--show-keymap 'current-local-map (current-local-map) nil 'all))
(after! which-key
  (define-key! help-map
    "bl"    #'which-key-show-local-map))

(after! (counsel org)
  ;; Make counsel-rg work correctly - see
  ;; https://github.com/hlissner/doom-emacs/issues/3038#issuecomment-624165004.
  ;;
  ;; I override the exit status in a separate script (in this repository)
  ;; because attempting to use ‘cl-letf’ to override ‘process-exit-status’ is
  ;; failing for me, at least on a machine with Emacs 28.0.50 with native-comp.
  (setf (elt counsel-rg-base-command 0) "~/.doom.d/counsel-rg")
  (defun counsel-rg-org (search-archives)
    "Specialize ‘counsel-rg’ for Org-mode files.

  Unless ‘\\[universal-argument]’ prefix ARG is used, don’t include archives in
  the search. Saves all Org buffers beforehand so that ‘counsel-rg’ sees the
  contents of all Org-mode buffers."
    (interactive "P")
    (org-save-all-org-buffers)
    (let* ((extra-rg-args (concat "--smart-case"
                                  " --type-add org:*.org"
                                  " --type-add org:*.org_archive"
                                  " --type org")))
      (when (not search-archives)
        (setq extra-rg-args (concat extra-rg-args " -g!*.org_archive")))
      (counsel-rg nil "~/Documents/org/" extra-rg-args nil)))
  (map! "C-c q" #'counsel-rg-org))

(after! (consult org)
  (defun consult-ripgrep-org (&optional search-archives initial)
    "Search for regexp with rg in my Org directory with INITIAL input.

See `consult-grep' for more details."
    (interactive "P")
    (org-save-all-org-buffers)
    (let ((consult-ripgrep-args consult-ripgrep-args))
      (setq consult-ripgrep-args
            (concat consult-ripgrep-args
                    (concat " --smart-case"
                            " --type-add org:*.org"
                            " --type-add org:*.org_archive"
                            " --type org")))
      (when (not search-archives)
        (setq consult-ripgrep-args
              (concat consult-ripgrep-args
                      " -g!*.org_archive")))
      (consult--grep "Ripgrep Org" #'consult--ripgrep-builder
                     "~/Documents/org" initial)))
  (map! "C-c q" #'consult-ripgrep-org))

(after! fd-dired
  (when (eq system-type 'darwin)
    (setq! fd-dired-ls-option
           '("| xargs -0 gls -ld --quoting-style=literal" . "-ld"))))


;; Kill current buffer but keep its frame around
(map! :n "Q" #'kill-this-buffer)

;; Swap gj and j, gk and k
(map!
 :m "j" #'evil-next-visual-line
 :m "gj" #'evil-next-line
 :m "k" #'evil-previous-visual-line
 :m "gk" #'evil-previous-line)
;; Disable evil-lion keybindings
(after! evil-lion
  (map! :map evil-normal-state-map "gl" nil
        :map evil-visual-state-map "gl" nil
        :map evil-normal-state-map "gL" nil
        :map evil-visual-state-map "gL" nil))

(defadvice! my-doom-disable-enlargen (&rest _r)
  :override #'doom/window-enlargen
  "Disable ‘doom/window-enlargen’ in favor of ‘doom/window-maximize-buffer’.

This appears similar to an old Emacs bug #32351:
https://lists.gnu.org/archive/html/bug-gnu-emacs/2018-08/msg00097.html. While
this bug should have long been fixed, I saw a stack trace that was very similar
when Emacs was hanging on macOS. ‘doom/window-enlargen’ creates narrow buffers
near the edge of the frame, so it may be a culprit. Work around this by using
‘doom/window-maximize-buffer’, which has one window take up the whole frame."
  (call-interactively #'doom/window-maximize-buffer))

;; Remap keys for tag/xref navigation
(map! :n "C-]" #'+lookup/definition
      :n "C-t" #'better-jumper-jump-backward
      "C-<mouse-1>" #'+lookup/definition
      ;; Doom binds this to C-t by default
      "M-t" #'+workspace/new)

;;; Line and column numbers
(setq! display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(line-number-mode 1)
(column-number-mode 1)

(require 'visual-fill-column)
(setq! visual-fill-column-width 120)
(add-hook 'text-mode-hook #'visual-fill-column-mode)

(require 'rg)
(setq! rg-custom-type-aliases
       '(("org" . "*.org *.org_archive")))
(rg-define-search rg-org
  "Run rg on my Org files"
  :query ask
  :files "org"
  :dir "~/Documents/org"
  :confirm prefix
  :flags '("--smart-case")             ; Emacs default search is smart case
  :menu '("Custom" "o" "Org-mode files"))
(defun rg-org-save-files (&rest _unused)
  "Run ‘org-save-all-org-buffers’ so ‘rg-org’ searches all file contents."
  (org-save-all-org-buffers))
(advice-add 'rg-org :before #'rg-org-save-files)
(map! "C-c q" #'rg-org)

;; Allow creating org-roam files that are a prefix of existing file names
;; (see https://www.orgroam.com/manual/How-do-I-create-a-note-whose-title-already-matches-one-of-the-candidates_003f.html#How-do-I-create-a-note-whose-title-already-matches-one-of-the-candidates_003f)
(after! ivy
  (setq! ivy-use-selectable-prompt t))

;; Disable fuzzy features that are enabled merely by Ivy finding Flx in the
;; load-path.
(after! (ivy flx)
  (unless (featurep! +fuzzy)
    (setq ivy-flx-limit 0
          ivy--flx-cache nil
          ivy--flx-featurep nil)))

(setq!
 flycheck-emacs-lisp-load-path 'inherit
 ;; Don't re-run Flycheck syntax checkers on inserting new lines, to save
 ;; performance.
 flycheck-check-syntax-automatically '(save idle-buffer-switch idle-change mode-enabled)
 flycheck-idle-buffer-switch-delay 4
 flycheck-idle-change-delay 4)
(add-hook!
 'emacs-lisp-mode-hook
 :depth 100
  (defun flycheck-emacs-lisp-load-path-inherit ()
    "Inherit Emacs load-path from current session.
Prevents annoying errors from custom packages.

This overrides the changes made by ‘flycheck-cask-setup’, since
Cask isn’t working correctly on my machine right now."
    (setq-local flycheck-emacs-lisp-load-path 'inherit)))

;; Doesn’t work with Doom because we don’t use ‘package-initialize’.
(after! flycheck
  (add-to-list 'flycheck-disabled-checkers 'emacs-lisp-package))

;; Enable for quotes in docstrings.
(add-hook 'emacs-lisp-mode-hook #'electric-quote-local-mode)

;;; syntax highlighting for vimscript files
(use-package! vimrc-mode
  :mode ".vim\\(rc\\)?$")

;; Give buffers editing files with the same basename more distinctive names
;; based on directory.
(use-package! uniquify
  :init
  (setq! uniquify-buffer-name-style 'post-forward-angle-brackets))

(setq! history-length 5000)

(use-package! rainbow-identifiers
  :hook (prog-mode . rainbow-identifiers-mode)
  :config
  :custom
  (rainbow-identifiers-choose-face-function
   #'rainbow-identifiers-cie-l*a*b*-choose-face)
  (rainbow-identifiers-cie-l*a*b*-saturation 30)
  (rainbow-identifiers-cie-l*a*b*-color-count 64)
  (rainbow-identifiers-face-count 64)
  ;; Disable distinctive variable name face so that rainbow-identifiers-mode
  ;; highlights the variable at its declaration the same way as its use.
  (rainbow-identifiers-faces-to-override '(font-lock-variable-name-face)))


;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq! org-directory "~/Documents/org/google-org/")
(after! org
  (load! "org-config.el"))

;; Pocket
(after! pocket-reader
  (setq! pocket-reader-open-url-default-function #'browse-url-default-browser)
  (defun archive-is-browse-url-default-browser (url)
    (browse-url-default-browser (concat "https://archive.is/" url)))
  (setq! pocket-reader-url-open-fn-map nil)
  (add-to-list 'pocket-reader-url-open-fn-map
               '(archive-is-browse-url-default-browser
                 "\\(.*\\.\\)?medium.com" "bloomberg.com" "nytimes.com"))
  (defun pocket-reader-roam-capture ()
    "Open URL of current item with default function."
    (interactive)
    (require 'org-roam-protocol)
    (pocket-reader--at-marked-or-current-items
     (let* ((id (tabulated-list-get-id))
            (item (ht-get pocket-reader-items id))
            (url (pocket-reader--get-url item))
            (title (pocket-reader--not-empty-string
                    (pocket-reader--or-string-not-blank
                     (ht-get item 'resolved_title)
                     (ht-get item 'given_title)
                     "[untitled]"))))
       (org-roam-protocol-open-ref (list :template "r" :ref url :title title)))))
  (map! :mode pocket-reader-mode "x" #'pocket-reader-roam-capture))

;;; Enable commands disabled by default
(put 'narrow-to-region 'disabled nil)
(put 'list-timers 'disabled nil)

;; Attempt to fix slow scrolling by delaying highlighting of current line -
;; https://github.com/hlissner/doom-emacs/issues/2217#issuecomment-615088393
(use-package! hl-line+
  :config
  (hl-line-when-idle-interval 0.3)
  (toggle-hl-line-when-idle 1))

;;; Enable pixel-scrolling
(when (require 'pixel-scroll nil 'noerror)
  (let ((mode (if (fboundp 'pixel-scroll-precision-mode)
                  'pixel-scroll-precision-mode
                'pixel-scroll-mode)))
    (funcall mode +1)
    (add-hook!
     'minibuffer-setup-hook
     (defun minibuffer-setup-disable-pixel-scroll ()
       (funcall mode -1)))
    (add-hook!
     'minibuffer-exit-hook
     (defun minibuffer-exit-disable-pixel-scroll ()
       (funcall mode +1)))))

(setq! profiler-max-stack-depth 64)
(setq! adaptive-wrap-extra-indent 2)
(add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode)
(global-visual-line-mode +1)

;; https://superuser.com/a/132347
(defun switch-to-minibuffer-window ()
  "switch to minibuffer window (if active)"
  (interactive)
  (when (active-minibuffer-window)
    (select-frame-set-input-focus (window-frame (active-minibuffer-window)))
    (select-window (active-minibuffer-window))))
;; Bind to SPC b .
(map! :leader
      :desc "switch to minibuffer"
      "b ." #'switch-to-minibuffer-window)

(defun my-format-iso8601-time-string (time)
  "Format encoded TIME to ISO8601 time string (with local time zone)."
  (format-time-string "%FT%T%z" time))

(defadvice! my-anki-editor-fix-attach-dir (fn &rest r)
 "Make ‘org-attach-id-dir’ absolute in ‘anki-editor-note-at-point’."
 :around #'anki-editor-note-at-point
 (require 'org-attach)
 (let ((org-attach-id-dir (file-truename org-attach-id-dir)))
   (apply fn r)))

;; Git commits should use Github-flavored Markdown
(setq! git-commit-major-mode 'gfm-mode)
;; Stop ‘magit-previous-line' from throwing errors about undefined variable
;; ‘project-switch-commands’ by forcing an autoload.
(after! magit
  (require 'magit-extras))
(after! magit-extras
  (load "project")
  nil)

(setq! parinfer-rust-preferred-mode "paren")
(defcustom my-parinfer-rust-git-repo-local-dir "~/misc/build/parinfer-rust"
  "Location of parinfer-rust Git checkout for use in patched ‘parinfer-rust--download-from-github’.")
(when (string-match-p "^aarch64-apple-darwin.*" system-configuration)
  (after! parinfer-rust-helper
    (defadvice! my-parinfer-rust-compile-dylib (parinfer-rust-version
                                                library-location
                                                lib-name)
      :override #'parinfer-rust--download-from-github
      "Compiles parinfer-rust from ‘my-parinfer-rust-git-repo-local-dir', installs it in LIBRARY-LOCATION.

PARINFER-RUST-VERSION and LIB-NAME currently ignored."
      (shell-command
       (concat (format "set -xv; cd %s && "
                       (shell-quote-argument
                        (expand-file-name my-parinfer-rust-git-repo-local-dir)))
               "cargo build --release --features emacs && "
               (format
                "install -m 755 ./target/release/libparinfer_rust.dylib %s"
                (shell-quote-argument
                 (expand-file-name parinfer-rust-library))))))))

(use-package! follow
  :defer t
  ;; Remove all this once
  ;; https://github.com/seagle0128/doom-modeline/commit/36fed6d1a1614f72d425073d7c9e1529f622fe7a
  ;; is gotten from upstream.
  :config
  (after! doom-modeline
    (doom-modeline-def-segment follow
      (when follow-mode
        (let* ((windows (follow-all-followers))
               (nwindows (length windows))
               (nfollowing (- (length (memq (selected-window) windows))
                              1)))
         (concat
          (doom-modeline-spc)
          (propertize (format "Follow %d/%d" (- nwindows nfollowing) nwindows)
                      'face 'doom-modeline-buffer-minor-mode)))))
   ;; Based on the main modeline (see ‘doom-modeline-set-main-modeline').
   (doom-modeline-def-modeline 'follow
    '(bar workspace-name window-number modals matches follow buffer-info remote-host buffer-position word-count parrot selection-info)
    '(objed-state misc-info persp-name battery grip irc mu4e gnus github debug repl lsp minor-modes input-method indent-info buffer-encoding major-mode process vcs checker))
   (add-hook! 'follow-mode-hook
     (defun +follow-set-modeline ()
       (doom-modeline-set-modeline 'follow)))))

(when
    (and (not (fboundp 'play-sound-internal))
         (executable-find "play"))
  (defadvice! play-sound-sox (sound)
    :override #'play-sound
    "Implement ‘play-sound’ using ‘play’ binary from SoX.

This replaces the use of ‘play-sound-internal’ when Emacs is compiled without
sound support.  Currently supports only :file and :volume entries in ‘sound’."
    (when-let* (((eq (car-safe sound) 'sound))
                (args (cdr-safe sound)))
      (let* ((file (plist-get args :file))
             (volume (plist-get args :volume)))
        (apply #'start-process "play-sound-sox" nil
              (append
               (list "play" file)
               (when volume
                (list "vol" volume))))))))

(atomic-chrome-start-server)

;; Fix ‘ask-user-about-supersession-threat’ for indirect buffers.
(defadvice! fix-supersession-for-indirect-buffers (origfn &rest args)
  :around #'userlock--ask-user-about-supersession-threat
  "Fix ‘userlock--ask-user-about-supersession-threat’ for indirect buffers.
Original function ORIGFN is called with ARGS.

Needs to be fixed because ‘visited-file-modtime’ returns 0 for indirect buffers,
and also the FILENAME passed to ‘userlock--ask-user-about-supersession-threat’
ends up as nil in this case. Indirect buffers are often used by Org mode for
capture, so I run into this situation a lot."
  (let* ((filename (car-safe args))
         (buf (or (buffer-base-buffer (current-buffer))
                  (current-buffer))))
    (cond
     ;; This means that (current-buffer) is not an indirect buffer, so just pass
     ;; arguments unchanged.
     ((eq buf (current-buffer))
      (apply origfn args))
     ;; This means that (current-buffer) is an indirect buffer. Change to the
     ;; base buffer and change FILENAME to the file name of the base buffer.
     (t
      (with-current-buffer buf
        (apply origfn buffer-file-name (cdr-safe args)))))))

;;;* Local configuration

;;; Allow users to provide an optional "config-local" containing personal settings
(load! "config-local.el")

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

;;; Local Variables:
;;; outline-regexp: ";;;\\*+\\|\\`"
;;; End:
