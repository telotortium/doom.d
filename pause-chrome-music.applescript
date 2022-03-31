tell application "Google Chrome"
	repeat with w in windows
		tell w
			repeat with t in tabs
				set tabURL to (URL of t)
				try
					if tabURL starts with "https://somafm.com" then
						with timeout of 2 seconds
							tell t
								execute javascript "var x = document.querySelector('[ng-if=\"$root.playingStation.playing\"]'); if (x !== null) { x.click(); }"
							end tell
						end timeout
					end if
					if tabURL starts with "https://music.youtube.com" then
						with timeout of 2 seconds
							tell t
								execute javascript "var x = document.getElementById('play-pause-button'); if (x !== null && x.getAttribute('aria-label').match(/Pause/i)) { x.click(); }"
							end tell
						end timeout
					end if
					if tabURL starts with "https://youtube.com" or URL of t starts with "https://www.youtube.com" then
						with timeout of 2 seconds
							tell t
								execute javascript "var x = document.querySelector('.ytp-play-button'); var y = document.getElementById('movie_player'); if (x !== null && x.getAttribute('aria-label').match(/Pause/i) && (y === null || y.classList.contains('playing-mode'))) { x.click(); }"
							end tell
						end timeout
					end if
				on error number -1712
					display dialog "Timeout when trying to pause " & tabURL
				end try
			end repeat
		end tell
	end repeat
end tell
