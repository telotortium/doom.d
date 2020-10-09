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
(setq! debug-on-quit t)

;; Disabled by Doom Emacs, but I want it.
(setq! auto-save-default t)

;; reset these values to ‘gcmh’ defaults. Doom sets them too low for my usage.
(after! gcmh
  (setq! gcmh-high-cons-threshold #x40000000)
  (setq! gcmh-idle-delay 15))

(after! evil
  (add-to-list 'evil-emacs-state-modes 'image-mode)
  (add-to-list 'evil-emacs-state-modes 'magit-mode)
  (add-to-list 'evil-emacs-state-modes 'Custom-mode)
  (add-to-list 'evil-emacs-state-modes 'profiler-report-mode)
  (add-to-list 'evil-emacs-state-modes 'pocket-reader-mode))
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

(after! server
  :config
  (setq! server-name "server")
  (setq! server-use-tcp t)
  (defun warn-server-name-changed (original-server-name)
    (when (not (string= server-name original-server-name))
      (warn "server-name = \"%s\", should be \"%s\""
            server-name original-server-name)))
  (run-at-time nil 30 #'warn-server-name-changed server-name)
  (if (server-running-p)
      (warn "Not starting server - server with name \"%s\" already running" server-name)
    (server-force-stop)
    (server-force-delete)
    (server-start)))

(after! (counsel org)
  (defun counsel-rg-org (search-archives)
    "Specialize ‘counsel-rg’ for Org-mode files.

  Unless ‘\\[universal-argument]’ prefix ARG is used, don’t include archives in
  the search. Saves all Org buffers beforehand so that ‘counsel-rg’ sees the
  contents of all Org-mode buffers."
    (interactive "P")
    (org-save-all-org-buffers)
    (let* ((extra-rg-args (concat "--smart-case"
                                  " --type-add 'org:*.org'"
                                  " --type-add 'org:*.org_archive'"
                                  " --type org")))
      (when (not search-archives)
        (setq extra-rg-args (concat extra-rg-args " '-g!*.org_archive'")))
      (counsel-rg nil "~/Documents/org/" extra-rg-args nil)))
  (map! "C-c q" #'counsel-rg-org))

;; Kill current buffer but keep its frame around
(map! :n "Q" #'kill-this-buffer)

;; Swap gj and j, gk and k
(map!
 :m "j" #'evil-next-visual-line
 :m "gj" #'evil-next-line
 :m "k" #'evil-previous-visual-line
 :m "gk" #'evil-previous-line)

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

(use-package! rg
  :commands (rg rg-org rg-project rg-dwim)
  :config
  (setq! rg-custom-type-aliases
         '(("org" . "*.org *.org_archive")))
  (rg-define-search rg-org
    "Run rg on my Org files"
    :query ask
    :files "org"
    :dir "~/Documents/org"
    :confirm prefix
    :flags ("--smart-case")             ; Emacs default search is smart case
    :menu ("Custom" "o" "Org-mode files"))
  (defun rg-org-save-files (&rest unused)
    "Run ‘org-save-all-org-buffers’ so ‘rg-org’ searches all file contents."
    (org-save-all-org-buffers))
  (advice-add 'rg-org :before #'rg-org-save-files))

(setq!
 ;; Inherit Emacs load-path from current session - prevents annoying errors
 ;; from custom packages.
 flycheck-emacs-lisp-load-path 'inherit
 ;; Don't re-run Flycheck syntax checkers on inserting new lines, to save
 ;; performance.
 flycheck-check-syntax-automatically '(save idle-buffer-switch idle-change mode-enabled)
 flycheck-idle-buffer-switch-delay 4
 flycheck-idle-change-delay 4)

;;; syntax highlighting for vimscript files
(use-package! vimrc-mode
  :mode ".vim\\(rc\\)?$")

;; Give buffers editing files with the same basename more distinctive names
;; based on directory.
(use-package! uniquify
  :custom
  (uniquify-buffer-name-style 'post-forward-angle-brackets))

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
  (cl-defun pocket-reader-roam-capture ()
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

(setq! profiler-max-stack-depth 64)
(use-package! explain-pause-mode
  :config
  (explain-pause-mode t)
  (explain-pause-profiles-ignore-command
   '(gcmh-idle-garbage-collect idle-timer)))

(setq! adaptive-wrap-extra-indent 2)
(add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode)
(global-visual-line-mode +1)

;;;* Local configuration

;;; Allow users to provide an optional "init-local" containing personal settings
(load! "init-local.el")

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
