
(* Script to do menu bar search in Emacs using macOS menu bar.
*)
tell application "Emacs" to activate
tell application "System Events" to tell process "Emacs"
    click menu bar item "Help" of menu bar 1
end tell
