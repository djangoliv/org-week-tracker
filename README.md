# org-week-tracker

**org-week-tracker** is a simple week review tool

To use org-week-tracker, make sure that this file is in Emacs load-path

(add-to-list 'load-path "/path/to/directory/")


Then require org-week-tracker

(require 'org-week-tracker)


To start org-week-tracker

M-x org-week-tracker-go-to-current-entry


You can exclude some days with the org-week-tracker-day-list

(setq org-week-tracker-exclude-day-list '(0)) ;; exclude sunday
(setq org-week-tracker-exclude-day-list '(0 6)) ;; exclude sturday and sunday
