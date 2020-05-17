;;;; Provide a command to hide and show org-drill cloze delimiters.
;;;;
;;;; From
;;;; http://www.giovannicarmantini.com/2015/07/putting-some-make-up-on-my-org-mode-flashcards.
(require 'org-drill)

;; remove clozes when exporting ;;
(defun gsc/drill-compute-cloze-regexp ()
  "Same regular expression as in org-drill-cloze-regexp,
but adding a group for the first delimiter, so that it can be
distinguished easily in a match."
  (concat "\\("
          (regexp-quote org-drill-left-cloze-delimiter)
          "\\)\\([[:cntrl:][:graph:][:space:]]+?\\)\\(\\|"
          (regexp-quote org-drill-hint-separator)
          ".+?\\)\\("
          (regexp-quote org-drill-right-cloze-delimiter)
          "\\)"))

;; Old function removed at some point from `org-drill'.
(defun org-pos-in-regexp (pos regexp &optional nlines)
  (save-excursion
    (goto-char pos)
    (org-in-regexp regexp nlines)))

(defun gsc/drill-cloze-removal (backend)
  "Remove drill clozes in the current buffer.
BACKEND is the export back-end being used, as a symbol."
  (while (re-search-forward (gsc/drill-compute-cloze-regexp) nil t)
    ;; (Copy-pasted this from org-drill-el)
    ;; Don't delete:
    ;; - org links, partly because they might contain inline
    ;;   images which we want to keep visible.
    ;; - LaTeX math fragments
    ;; - the contents of SRC blocks
    (unless (save-match-data
              (or (org-pos-in-regexp (match-beginning 0)
                                     org-bracket-link-regexp 1)
                  (org-in-src-block-p)
                  (org-inside-LaTeX-fragment-p)))
      (replace-match "\\2" nil nil))))

(add-hook 'org-export-before-processing-hook 'gsc/drill-cloze-removal)

;; hide clozes in text ;;
(defvar gsc/drill-groups-to-hide '(1 3 4)
"Group 1 and 4 are the left and right delimiters respectively,
group 3 is the cloze hint.")

(setplist 'gsc/inv-cloze '(invisible t))

(defun gsc/hide-clozes-groups ()
  (save-excursion
    (goto-char (point-min))
    (let ((cloze-regexp (gsc/drill-compute-cloze-regexp)))
    (while (re-search-forward cloze-regexp nil t)
      (loop for group in gsc/drill-groups-to-hide do
            (overlay-put
             (make-overlay (match-beginning group) (match-end group))
             'category 'gsc/inv-cloze))))))

(defun gsc/show-clozes-all ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward (gsc/drill-compute-cloze-regexp) nil t)
      (remove-overlays
       (match-beginning 1) (match-end 4) 'category 'gsc/inv-cloze))))

(defun gsc/hide-show-clozes (arg)
"If called with no argument, hides delimiters and hints
for org-drill clozes.
If called with the C-u universal argument, it shows them."
(interactive "p")
(case arg
  (1 (gsc/hide-clozes-groups))
  (4 (gsc/show-clozes-all))))

(provide 'org-drill-cloze-enhancement)
