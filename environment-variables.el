;;; environment-variables.el -*- lexical-binding: t; -*-

(require 'seq)

;; Environment variables tied to one shell, terminal, or login session.  These
;; must not be persisted by `doom env' or copied into launchd's GUI environment.
(defconst rmi-transient-environment-variables
  '("LaunchInstanceID"
    "LaunchInstanceId"
    "SECURITYSESSIONID"
    "XPC_SERVICE_NAME"
    "XPC_FLAGS"
    "TERM_SESSION_ID"
    "TMPDIR"
    "ITERM_SESSION_ID"
    "ITERM_PROFILE"
    "ITERM_ORIG_PS1"
    "ITERM_PREV_PS1"
    "BASH_LOAD_STATE"
    "TMUX"
    "TMUX_PANE"
    ;; VS Code/Electron process state accidentally copied into launchd by an
    ;; older exporter.  Do not deny the prefixes wholesale: some variables in
    ;; these namespaces are intentional user configuration.
    "ELECTRON_NO_ATTACH_CONSOLE"
    "ELECTRON_RUN_AS_NODE"
    "VSCODE_CODE_CACHE_PATH"
    "VSCODE_CRASH_REPORTER_PROCESS_TYPE"
    "VSCODE_CWD"
    "VSCODE_ESM_ENTRYPOINT"
    "VSCODE_IPC_HOOK"
    "VSCODE_NLS_CONFIG"
    "VSCODE_PID"
    "VSCODE_RESOLVING_ENVIRONMENT"
    ;; A malformed key produced when VSCODE_NLS_CONFIG was split on spaces.
    "Studio"))

(defconst rmi-transient-environment-variable-patterns
  '("\\`CODEX_"))

(defun rmi-transient-environment-variable-p (variable)
  "Return non-nil when VARIABLE should not be persisted."
  (or (member variable rmi-transient-environment-variables)
      (seq-some (lambda (pattern) (string-match-p pattern variable))
                rmi-transient-environment-variable-patterns)))

(defun rmi-check-call-process (program &rest args)
  "Run PROGRAM with ARGS and signal an error if it fails."
  (let ((exit-status (apply #'call-process program nil nil nil args)))
    (unless (and (integerp exit-status) (= exit-status 0))
      (error "%s exited unsuccessfully: status %S"
             program exit-status))))

(defun macos-launchctl-setenv-launchagent (&optional env-file)
  "Write and apply a LaunchAgent from Doom ENV-FILE.

ENV-FILE defaults to `doom-env-file' when available, otherwise to the default
Doom environment snapshot under ~/doom.emacs.d/.local/env."
  (let*
      ((env-file (or env-file
                     (and (boundp 'doom-env-file) doom-env-file)
                     (expand-file-name "~/doom.emacs.d/.local/env")))
       (doom-env (with-temp-buffer
                   (insert-file-contents env-file)
                   (read (current-buffer))))
       (launch-agent
        (expand-file-name
         "~/Library/LaunchAgents/io.github.telotortium.environment.plist")))
    (make-directory (file-name-directory launch-agent) t)
    (with-temp-file launch-agent
      (insert "{}\n"))
    (rmi-check-call-process "plutil" "-convert" "xml1" launch-agent)
    (rmi-check-call-process
     "plutil" "-replace" "Label" "-string"
     "io.github.telotortium.environment" launch-agent)
    (rmi-check-call-process
     "plutil" "-replace" "RunAtLoad" "-bool" "true" launch-agent)
    (rmi-check-call-process
     "plutil" "-replace" "ProgramArguments" "-array" launch-agent)
    (dolist (argument
             '("sh" "-c"
               "while [ $# -ge 2 ]; do launchctl setenv \"$1\" \"$2\"; shift 2; done"
               "__dollar_sign_0__"))
      (rmi-check-call-process
       "plutil" "-insert" "ProgramArguments" "-string" argument
       "-append" launch-agent))
    ;; Remove transient values left by older runs from the live launchd
    ;; environment as well as from the newly generated LaunchAgent.
    (dolist (variable rmi-transient-environment-variables)
      (call-process "launchctl" nil nil nil "unsetenv" variable))
    (dolist (entry (sort doom-env #'string-lessp))
      (when (string-match "\\([^=]+\\)=\\(.*\\)" entry)
        (let ((variable (match-string 1 entry))
              (value (match-string 2 entry)))
          (unless (rmi-transient-environment-variable-p variable)
            (rmi-check-call-process
             "plutil" "-insert" "ProgramArguments" "-string" variable
             "-append" launch-agent)
            (rmi-check-call-process
             "plutil" "-insert" "ProgramArguments" "-string" value
             "-append" launch-agent)
            (rmi-check-call-process "launchctl" "setenv" variable value)))))))

(provide 'environment-variables)
