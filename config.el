;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Robert Irelan"
      user-mail-address "rirelan@gmail.com")

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
(setq doom-font (font-spec :family "monospace" :size 14))

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

(use-package! server
  :config
  (setq! server-name "server")
  (setq! server-socket-dir "~/.emacs.d/server")
  (setq! server-use-tcp t)
  (defun warn-server-name-changed (original-server-name)
    (when (not (string= server-name original-server-name))
      (warn "server-name = \"%s\", should be \"%s\""
            server-name original-server-name)))
  (run-at-time nil 30 #'warn-server-name-changed server-name)
  (if (server-running-p)
      (warn "Not starting server - server with name \"%s\" already running" server-name)
    (server-start)))

;; Kill current buffer but keep its frame around
(map! :n "Q" #'kill-this-buffer)

;; Evil magic search
(setq! evil-magic 'very-magic)

(defun prog-mode-wrap-hook ()
  "Set auto-fill for comments only in `prog-mode'."
  (setq-local comment-auto-fill-only-comments t)
  (auto-fill-mode t))
(add-hook 'prog-mode-hook 'prog-mode-wrap-hook)

(use-package! rg
  :commands (rg rg-project rg-dwim)
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

(map! "C-c a" #'org-agenda
      "C-c b" #'org-switchb
      "C-c c" #'org-capture
      "C-c l" #'org-store-link
      "S-<f11>" #'org-clock-goto
      "C-<f11>" #'my-org-clock-in
      "C-S-<f11>" #'my-org-goto-heading)
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
 org-agenda-files (expand-file-name "agenda_files" user-emacs-directory)
 org-agenda-span 'day
 org-agenda-start-on-weekday nil
 org-agenda-skip-deadline-prewarning-if-scheduled t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq! org-directory "~/Documents/org/google-org/")

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
