;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here, run 'doom sync' on
;; the command line, then restart Emacs for the changes to take effect.
;; Alternatively, use M-x doom/reload.
;;
;; WARNING: Disabling core packages listed in ~/.emacs.d/core/packages.el may
;; have nasty side-effects and is not recommended.


;; All of Doom's packages are pinned to a specific commit, and updated from
;; release to release. To un-pin all packages and live on the edge, do:
;(unpin! t)

;; ...but to unpin a single package:
;(unpin! pinned-package)
;; Use it to unpin multiple packages
;(unpin! pinned-package another-pinned-package)


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a particular repo, you'll need to specify
;; a `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, for whatever reason,
;; you can do so here with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;;; For logging messages prefixed with function name.
(package! call-log
  :recipe (:host github :repo "jordonbiondo/call-log"
           :fork (:host nil :repo "git@github.com:telotortium/call-log")
           :depth full))

(package! el-mock)
(package! auto-compile)
(package! rg)
(package! vimrc-mode)
(package! rainbow-identifiers)

(package! so-long :built-in 'prefer)

(disable-packages! evil-org-agenda)

;; (unpin! org)                            ; To avoid picking up built-in version
;; (package! org
;;   :recipe (:host github :repo "emacs-straight/org"
;;            :fork (:host github :repo "yantar92/org"
;;                   :branch "feature/org-fold-universal-core")
;;            :files (:defaults "etc")
;;            ;; HACK A necessary hack because org requires a compilation step
;;            ;;      after being cloned, and during that compilation a
;;            ;;      org-version.el is generated with these two functions, which
;;            ;;      return the output of a 'git describe ...'  call in the repo's
;;            ;;      root. Of course, this command won't work in a sparse clone,
;;            ;;      and more than that, initiating these compilation step is a
;;            ;;      hassle, so...
;;            :build t
;;            :pre-build
;;            (progn
;;              (require 'straight)
;;              ;; Prevent ‘doom sync -p’ from removing org-version.el with .git/info/exclude
;;              (with-temp-file (doom-path (straight--repos-dir "org") ".git" "info" "exclude")
;;                (insert "/org-version.el\n"))
;;              (with-temp-file (doom-path (straight--repos-dir "org") "org-version.el")
;;                (insert "(defun org-release () \"9.5\")\n"
;;                        (format "(defun org-git-version (&rest _) \"9.5-%s\")\n"
;;                                (cdr (doom-call-process "git" "rev-parse" "--short" "HEAD")))
;;                        "(provide 'org-version)\n")))))
(package! org-clock-csv)
(package! org-pomodoro
  :recipe (:host github :repo "marcinkoziej/org-pomodoro"
           :fork (:host nil :repo "git@github.com:telotortium/org-pomodoro")
           :depth full))
(package! org-pomodoro-third-time
  :recipe (:host github :repo "telotortium/org-pomodoro-third-time"
           :fork (:host nil :repo "git@github.com:telotortium/org-pomodoro-third-time")
           :depth full))
(when (package! alert)
  (package! org-gcal
    :recipe (:host github :repo "kidd/org-gcal.el"
             :fork (:host nil :repo "git@github.com:telotortium/org-gcal.el")
             :depth full)))
(package! org-clock-convenience)
(package! org-drill)
(package! org-drill-table)
(package! orgit)
(package! org-journal)
(package! org-ql)
(package! org-randomnote)
(package! org-super-agenda)
(package! org-download)
(package! org-cliplink)
(package! ox-hugo)
(package! org-roam)
(package! org-roam-ui)                  ; TODO: should figure out commit from org-roam
(package! pocket-reader)

(package! hl-line :disable t)
(package! hl-line+)
(package! evil-visual-mark-mode)

(package! visual-fill-column)

;; Testing with async-await
(package! async-await)
(package! aio)

;; Testing org-gcal
(package! load-relative)

;; Anki support
(package! anki-editor)
(package! anki-connect)
(package! od2ae
  :recipe (:host github :repo "telotortium/emacs-od2ae"
           :fork (:host nil :repo "git@github.com:telotortium/emacs-od2ae")
           :depth full))


(package! el-patch)

(package! atomic-chrome)

(package! elnode)

(package! khoj
  :recipe (:host github :repo "debanjum/khoj"
           :files (:defaults "src/interface/emacs/khoj.el")
           ;; :fork (:host nil :repo "git@github.com:telotortium/khoj"
           ;;        :branch "method"
           ;;        :files (:defaults "src/interface/emacs/khoj.el"))
           :depth full))

(package! gptel)
