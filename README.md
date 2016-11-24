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
You can also define a shortcut
``` emacs-lisp
   (global-set-key (kbd "C-c w") 'org-week-tracker-goto-current-entry)
```

## Configuration

You can specify the org-week-tracker file
``` emacs-lisp
   (setq org-week-tracker-file "~/.emacs.d/time-tracker.org") ;; for example
```
You can exclude some days with the org-week-tracker-exclude-day-list
``` emacs-lisp
   (setq org-week-tracker-exclude-day-list '(0)) ;; exclude sunday
   (setq org-week-tracker-exclude-day-list '(0 6)) ;; exclude saturday and sunday
```
You can change de tables size
``` emacs-lisp
   (setq org-week-tracker-table-size '(7 20 90)) ;; Default
```

## shortcuts
in org-week-tracker buffer:

| Shortcut     | cmmand                                | description                                 |
| ------------ | ------------------------------------- | ------------------------------------------- |
| C-.          | org-week-tracker-open-current-month   | visualize current month                     |
| C-u C-.      | org-week-tracker-open-current-month   | visualize current month in indirect buffer  |
| C-\<up\>     | org-week-tracker-open-prev-month      | visualize previous month                    |
| C-S-\<up\>   | org-week-tracker-open-prev-month      | visualize previous month in indirect buffer |
| C-\<down\>   | org-week-tracker-open-next-month      | visualize next month                        |
| C-S-\<down\> | org-week-tracker-open-next-month      | visualize next month in indirect buffer     |
| C-:          | org-week-tracker-run-calendar         | open calendar at curent date                |
| C-c k        | org-week-tracker-kill-current-subtree | delete subtree                              |

## License

Copyright (C) 2016 Djangoliv'

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.  
This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.  
You should have received a copy of the GNU General Public License along with this program. If not, see http://www.gnu.org/licenses/.  

contributions are welcome !
