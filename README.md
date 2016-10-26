# Org Week Tracker

**org-week-tracker** is a simple week review tool

To use org-week-tracker, make sure that this file is in Emacs load-path  
``` emacs-lisp
  (add-to-list 'load-path "/path/to/directory/")
```

Then require org-week-tracker  
``` emacs-lisp
  (autoload 'org-week-tracker-goto-current-entry "org-week-tracker" nil t)
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

| Shortcut       | cmmand                              | description                                 |
| -------------- | ----------------------------------- | ------------------------------------------- |
| C-c .          | org-week-tracker-open-current-month | visualize current month                     |
| C-u C-c .      | org-week-tracker-open-current-month | visualize current month in indirect buffer  |
| C \<up\>       | org-week-tracker-open-prev-month    | visualize previous month                    |
| C-u C \<up\>   | org-week-tracker-open-prev-month    | visualize previous month in indirect buffer |
| C \<down\>     | org-week-tracker-open-next-month    | visualize next month                        |
| C-u C \<down\> | org-week-tracker-open-next-month    | visualize next month in indirect buffer     |


contributions are welcome !
