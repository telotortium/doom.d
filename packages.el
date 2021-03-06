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
           :fork (:host nil :repo "git@github.com:telotortium/call-log")))

(package! el-mock)
(package! auto-compile)
(package! rg)
(package! vimrc-mode)
(package! rainbow-identifiers)
(package! git-auto-commit-mode
  :recipe (:host github :repo "ryuslash/git-auto-commit-mode"
           :fork (:repo "git@github.com:telotortium/git-auto-commit-mode"
                  :host nil :branch "gac-merge")))

(package! so-long :built-in 'prefer)

(disable-packages! evil-org-agenda)

(package! org-mode
  :recipe (:host github :repo "emacs-straight/org-mode"
           :fork (:repo "https://github.com/yantar92/org"
                  :host nil :branch "feature/org-fold-universal-core")
           :files ("*.el" "lisp/*.el" "contrib/lisp/*.el")
           ;; HACK A necessary hack because org requires a compilation step
           ;;      after being cloned, and during that compilation a
           ;;      org-version.el is generated with these two functions, which
           ;;      return the output of a 'git describe ...'  call in the repo's
           ;;      root. Of course, this command won't work in a sparse clone,
           ;;      and more than that, initiating these compilation step is a
           ;;      hassle, so...
           :pre-build
           (with-temp-file (doom-path (straight--repos-dir "org-mode") "org-version.el")
             (insert "(fset 'org-release (lambda () \"9.4\"))\n"
                     "(fset 'org-git-version #'ignore)\n"
                     "(provide 'org-version)\n"))))
(package! org-clock-csv)
(package! org-pomodoro
  :recipe (:host github :repo "marcinkoziej/org-pomodoro"
           :fork (:host nil :repo "git@github.com:telotortium/org-pomodoro")))
(package! org-fc
  :recipe (:host nil :repo "https://git.sr.ht/~l3kn/org-fc"
           :branch "main"
           :files (:defaults "awk" "demo.org")))
(when (package! alert)
  (package! org-gcal
    :recipe (:host github :repo "kidd/org-gcal.el"
             :fork (:host nil :repo "git@github.com:telotortium/org-gcal.el"))))
(package! org-drill)
(package! org-drill-table)
(package! org-journal)
(package! org-ql)
(package! org-randomnote)
(package! org-super-agenda)
(package! org-download)
(package! org-cliplink)
(package! pocket-reader)

(package! hl-line :disable t)
(package! hl-line+)
(package! evil-visual-mark-mode)

(package! parinfer-mode :recipe (:host github :repo "Wagk/parinfer-mode" :branch "patch-1"))

(package! explain-pause-mode
  :recipe (:host github :repo "lastquestion/explain-pause-mode"))
