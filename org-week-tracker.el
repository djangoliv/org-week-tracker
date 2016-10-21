;;; org-week-tracker.el --- Simple week review

;; Copyright (C) 2015 Djangoliv'

;; Author: Djangoliv <djangoliv@mailoo.com>
;; URL:  https://github.com/djangoliv/org-week-tracker
;; Version: 0.1
;; Keywords: org, week

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; org-week-tracker is a simple week review tool

;; To use org-week-tracker, make sure that this file is in Emacs load-path
;; (add-to-list 'load-path "/path/to/directory/")
;;
;; Then require org-week-tracker
;; (require 'org-week-tracker)

;; To start org-week-tracker
;; M-x org-week-tracker-go-to-current-entry
;;

;; You can exclude some days with the org-week-tracker-day-list
;; (setq org-week-tracker-exclude-day-list '(0)) ;; exclude sunday
;; (setq org-week-tracker-exclude-day-list '(0 6)) ;; exclude sturday and sunday

;;; Code:

;; create the list for font-lock.
;; each category of keyword is given a particular face
;; (setq org-week-tracker-font-lock-keywords
;;       `(
;;         (,org-week-tracker-keywords-regexp . font-lock-keyword-face)
;;         (,org-week-tracker-days-regexp . font-lock-builtin-face)
;;         (,org-week-tracker-months-regexp . font-lock-builtin-face)
;;         (,header-regexp . font-lock-function-name-face)
;;         ;;(,header-2-regexp . font-lock-constant-face)
;;         ))

(defvar org-week-tracker-file "~/.org-week-tracker.org" "org week tracker File")
(defvar org-week-tracker-exclude-day-list "org week tracker day list to exclude")
(defvar org-week-tracker-week (cond ((equal current-language-environment "French") "Semaine" )
                                    ((equal current-language-environment "Deutch") "Woche")
                                    (t  "Week")) "Week in current language")
(require 'timezone)

;;;###autoload
(define-derived-mode org-week-tracker org-mode
  "org-week-tracker mode: Major mode for tracking time"
  ;; code for syntax highlighting
  (setq font-lock-defaults '((org-week-tracker-font-lock-keywords)))
  ;; read-only
  (setq buffer-read-only t))

(defun org-week-tracker-goto-current-entry (&optional ask)
  " create or goto entry
    create month by month and go to the good day"
  (interactive "P")
  (let ((year (format-time-string "%Y")) (month (format-time-string "%m")) (day (format-time-string "%d"))  (week-line) (time))
    (if ask
        (progn
          (set 'year (read-from-minibuffer (format "Year (default %s): " (format-time-string "%Y")) (format-time-string "%Y")))
          (set 'month (read-from-minibuffer (format "Month (default %s): " (format-time-string "%m")) (format-time-string "%m")))
          (set 'day (read-from-minibuffer (format "Day (default %s): " (format-time-string "%d")) (format-time-string "%d")))))
    (let* ((filename org-week-tracker-file)
           (year (string-to-number year))
           (month (string-to-number month))
           (day (string-to-number day))
           (buffer (or (org-find-base-buffer-visiting filename)
                       (find-file-noselect filename)
                       (error "Unable to find buffer for file: %s" filename))))
      (switch-to-buffer buffer)
      (org-set-startup-visibility)
      ;; insert year if no exist
      (org-week-tracker-find-insert "^\\*+[ \t]+\\([12][0-9]\\{3\\}\\)\\(\\s-*?\\\([ \t]:[[:alnum:]:_@#%%]+:\\)?\\s-*$\\)" year nil (format "%s\n" year))
      ;; insert month if no exist
      (org-week-tracker-find-insert "^\\*+[ \t]+\\([0-3][0-9]\\) [a-z]+$" year month
                                (format-time-string "%m %B\n" (encode-time 0 0 0 day month year)))
      ;; insert tree month if needed
      (if (equal (char-after) 10)
          (org-week-tracker-insert-tables-dates-for-month month year))
      ;; align all tables
      (org-table-map-tables 'org-table-align)
      ;; gotoWeek
      (goto-char (point-min))
      (setq time (encode-time 1 1 0 day month year))
      (setq week-line (format "%s %s" org-week-tracker-week (format-time-string "%W" time)))
      (search-forward week-line)
      ;; show all buffer
      (widen)
      ;; open the good week
      (org-set-startup-visibility)
      (outline-show-branches)
      (org-reveal)
      (org-cycle)
      ;; go to day
      (search-forward (capitalize (format-time-string "%a" time)))
      (org-table-next-field)
      ;; save
      (save-buffer))))

(defun org-week-tracker-find-insert (regex year &optional month toInsert)
  "find or insert"
  (when month (org-narrow-to-subtree))
  (let ((re (format regex year month)) match)
    (goto-char (point-min))
    (while (and (setq match (re-search-forward re nil t))
                (goto-char (match-beginning 1))
                (< (string-to-number (match-string 1)) (or month year))))
    (cond
     ((not match)
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (org-week-tracker-insert-line year month toInsert)
      )
     ((= (string-to-number (match-string 1)) (or month year))
      (beginning-of-line))
     (t
      (beginning-of-line)
      (org-week-tracker-insert-line year month toInsert)))))

(defun org-week-tracker-insert-line (year &optional month text)
  (delete-region (save-excursion (skip-chars-backward " \t\n") (point)) (point))
  (insert "\n" (make-string 1 ?*) " \n")
  (backward-char)
  (when month (org-do-demote))
  (insert text)
  (beginning-of-line))

;; insert month dates
(defun org-week-tracker-insert-tables-dates-for-month (month year)
  "insert a bunch of tables dates by week"
  (let* ((day 1) (last_week) (last_day_week) (week) (day_name) (beg_time) (end_time)
         (time (encode-time 1 1 0 day month year)))
    (setq last_week 0)
    (while (= (nth 4 (decode-time time)) month)
      (setq week (format-time-string "%W\n" time))
      ;; exclude days
      (if (member (calendar-day-of-week (list month day year)) org-week-tracker-exclude-day-list)
          (setq day (1+ day))
        (progn
          ;; write table
          (if (equal week last_week)
              (progn
                ;; days
                (setq day_name (format-time-string "%a" time))
                (insert (format "\t|%s %02d| | | \n" (capitalize day_name) day))
                (insert (format "\t|--+--+--+--\n"))
                (setq day (1+ day)))
            (progn
              ;; new week
              (insert (format "*** %s %s " org-week-tracker-week (format-time-string "%W" time)))
              (setq beg_time (encode-time 1 1 0 day month year))
              (insert (format-time-string "(%d/%m/%y ..." beg_time))
              ;; get next sunday (end of week)
              (setq last_day_week day)
              (while (not (= (calendar-day-of-week (list month last_day_week year)) 0))
                (setq last_day_week (1+ last_day_week)))
              (setq end_time (encode-time 1 1 0 last_day_week month year))
              (insert (format-time-string " %d/%m/%y)\n" end_time))
              ;; table head
              (insert "\t|<8>|<30>|<90>| \n")
              (insert "\t|--|--|--\n")
              (setq last_week week)))))
      (setq time (encode-time 1 1 0 day month year))))
  (delete-blank-lines))

;; clear memory. no longer needed
(setq org-week-tracker-keywords nil)

;; clear memory. no longer needed
(setq org-week-tracker-keywords-regexp nil)

;; add the mode to the `features' list
(provide 'org-week-tracker)

;; TODO
;; * read-only (avec indirect-buffer ?)
;; * coherence langues
;; * syntax higlight
;; * trop de string-to-number

;; Local Variables:
;; coding: utf-8
;; End:

;;; org-week-tracker.el ends here
