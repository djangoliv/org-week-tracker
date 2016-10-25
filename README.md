# Org Week Tracker

**org-week-tracker** is a simple week review tool

To use org-week-tracker, make sure that this file is in Emacs load-path  
``` emacs-lisp
  (add-to-list 'load-path "/path/to/directory/")
```

Then require org-week-tracker  
``` emacs-lisp
  (require 'org-week-tracker)
```

To start org-week-tracker  
``` emacs-lisp
  M-x org-week-tracker-go-to-current-entry
```

## Configuration

You can exclude some days with the org-week-tracker-day-list  
``` emacs-lisp
  (setq org-week-tracker-exclude-day-list '(0)) ;; exclude sunday  
  (setq org-week-tracker-exclude-day-list '(0 6)) ;; exclude sturday and sunday
```
You can specify the org-week-tracker file
``` emacs-lisp
   (setq org-week-tracker-file "~/.emacs.d/time-tracker.org") ;; for example
```
You can also define a shortcut
``` emacs-lisp
   (global-set-key (kbd "s-<end>") 'org-week-tracker-goto-current-entry)
```
You can exclude some days with the org-week-tracker-day-list
``` emacs-lisp
   (setq org-week-tracker-exclude-day-list '(0)) ;; exclude sunday
   (setq org-week-tracker-exclude-day-list '(0 6)) ;; exclude sturday and sunday
```

## shortcuts
in org-week-tracker buffer:
 - C-c . : visualize current month  
 - C-c <up> : previous month  
 - C-c <down> : next month


contributions are welcome !
