;;; packages.el --- paruka-org layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: paruka <paruka.me@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `paruka-org-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `paruka-org/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `paruka-org/pre-init-PACKAGE' and/or
;;   `paruka-org/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst paruka-org-packages
  '(
    (org :location built-in)
    (org-mac-link :toggle (spacemacs/system-is-mac))
    (org-mac-iCal :toggle (spacemacs/system-is-mac))
    org-pomodoro
    org-cliplink
    org-fstree
    org-dashboard
    plantuml-mode
    deft
    )
  "The list of Lisp packages required by the paruka-org layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")


(defun paruka-org/post-init-org-pomodoro ()
  (progn
    (add-hook 'org-pomodoro-finished-hook '(lambda () (paruka/notification "Pomodoro Finished" "☕️ Have a break!")))
    (add-hook 'org-pomodoro-short-break-finished-hook '(lambda () (paruka/notification "Short Break" "🐝 Ready to Go?")))
    (add-hook 'org-pomodoro-long-break-finished-hook '(lambda () (paruka/notification "Long Break" " 💪 Ready to Go?")))
    (add-hook 'org-pomodora-killed-hook '(lambda () (paruka/notification "Pomodora Killed" "☕️ Have a rest!")))

    (setq org-pomodoro-keep-killed-pomodoro-time t)

    (when (spacemacs/system-is-linux)
      (setq org-pomodoro-audio-player "/usr/bin/mpv")
      ;;  (play-sound-file (org-pomodoro-sound :long-break))
      )

    (when (spacemacs/system-is-mac)
      (setq org-pomodoro-play-sounds nil))))

(defun paruka-org/init-org-mac-link ()
  (use-package org-mac-link
    :commands org-mac-grab-link
    :init
    (progn
      (add-hook 'org-mode-hook
                (lambda ()
                  (define-key org-mode-map (kbd "C-c g") 'org-mac-grab-link))))
    :defer t))

(defun paruka-org/post-init-deft ()
  (progn
    (setq deft-use-filter-string-for-filename t)
    (setq deft-recursive t)
    (setq deft-extension "org")
    (setq deft-directory deft-dir)))

(defun paruka-org/init-org-fstree ()
  (use-package org-fstree :defer t))

(defun paruka-org/init-org-cliplink ()
  (use-package org-cliplink :defer t))

(defun paruka-org/init-org-dashboard ()
  (use-package org-dashboard :defer t))

(defun paruka-org/init-plantuml-mode ()
  (use-package plantuml-mode
    :defer t
    :init (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))))

(defun paruka-org/init-org-mac-iCal ()
  (use-package org-mac-iCal :defer t))

(defun paruka-org/post-init-org ()
  (with-eval-after-load 'org
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "," 'org-priority)
      (require 'org-compat)
      (require 'org)
      ;; (add-to-list 'org-modules "org-habit")
      (add-to-list 'org-modules 'org-habit)
      (require 'org-habit)
      (require 'org-fstree)
      ;; 加密文章
      ;; "http://coldnew.github.io/blog/2013/07/13_5b094.html"
      ;; org-mode 設定
      (require 'org-crypt)

      ;; 當被加密的部份要存入硬碟時，自動加密回去
      (org-crypt-use-before-save-magic)

      ;; 設定要加密的 tag 標籤為 secret
      (setq org-crypt-tag-matcher "secret")

      ;; 避免 secret 這個 tag 被子項目繼承 造成重複加密
      ;; (但是子項目還是會被加密喔)
      (setq org-tags-exclude-from-inheritance (quote ("secret")))

      ;; 用於加密的 GPG 金鑰
      ;; 可以設定任何 ID 或是設成 nil 來使用對稱式加密 (symmetric encryption)
      (setq org-crypt-key nil)

      (setq-default org-display-custom-times t)
      (setq org-time-stamp-custom-formats '("<%Y-%m-%d %a %R>" . "<%Y-%m-%d %a %R>"))

      (setq org-agenda-files org-agenda-dir)
      (define-key global-map (kbd "C-c l") 'org-store-link)
      (define-key global-map (kbd "C-c a") 'org-agenda)
      (define-key global-map (kbd "C-c b") 'org-iswitchb)

      ;; Various preferences
      (setq org-log-done t
            org-edit-timestamp-down-means-later t
            org-archive-mark-done nil
            org-hide-emphasis-markers t
            org-catch-invisible-edits 'show
            org-export-coding-system 'utf-8
            org-fast-tag-selection-single-key 'expert
            org-html-validation-link nil
            org-export-kill-product-buffer-when-displayed t
            org-tags-column 80)

      ;; org plugins
      (after-load 'ob-ditaa
        (unless (and (boundp 'org-ditaa-jar-path)
                     (file-exists-p org-ditaa-jar-path))
          (let ((jar-name "ditaa0_9.jar")
                (url "http://jaist.dl.sourceforge.net/project/ditaa/ditaa/0.9/ditaa0_9.zip"))
            ;;      (setq org-ditaa-jar-path (expand-file-name jar-name (file-name-directory user-init-file)))
            (setq org-ditaa-jar-path (expand-file-name jar-name paruka/plugin-path))
            (unless (file-exists-p org-ditaa-jar-path)
              (paruka/grab-ditaa url jar-name)))))

      (after-load 'ob-plantuml
        (when (string= "" org-plantuml-jar-path)
          (let ((jar-name "plantuml.1.2017.15.jar")
                (url "https://downloads.sourceforge.net/project/plantuml/1.2017.15/plantuml.1.2017.15.jar?r=&ts=1501092829&use_mirror=nchc"))
            (setq org-plantuml-jar-path (expand-file-name jar-name paruka/plugin-path))
            (setq plantuml-jar-path org-plantuml-jar-path)
            (unless (file-exists-p org-plantuml-jar-path)
              (url-copy-file url org-plantuml-jar-path)))))
      ;; org plugins end

      ;;; mirror mode here!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      (setq org-support-shift-select t)

      ;;; Capturing
      (global-set-key (kbd "C-c c") 'org-capture)

      (require 'org-protocol)

      ;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
      (setq org-capture-templates
            (quote (("t" "todo" entry (file "~/git/org/refile.org")
                     "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
                    ("r" "respond" entry (file "~/git/org/refile.org")
                     "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
                    ("n" "note" entry (file "~/git/org/refile.org")
                     "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
                    ("j" "Journal" entry (file+datetree "~/git/org/diary.org")
                     "* %?\n%U\n" :clock-in t :clock-resume t)
                    ("w" "org-protocol" entry (file "~/git/org/refile.org")
                     "* TODO Review %c\n%U\n" :immediate-finish t)
                    ("m" "Meeting" entry (file "~/git/org/refile.org")
                     "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
                    ("P" "Phone call" entry (file "~/git/org/refile.org")
                     "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
                    ("h" "Habit" entry (file "~/git/org/habit.org")
                     "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")
                    ("p" "Protocol" entry (file+headline "~/git/org/refile.org" "Inbox")
                     "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
                    ("L" "Protocol Link" entry (file+headline "~/git/org/refile.org" "Inbox")
                     "* %? [[%:link][%:description]] \nCaptured On: %U"))))

      (require 'org-dashboard)

      ;;; Refiling
      (setq org-refile-use-cache nil)

      ;; Targets include this file and any file contributing to the agenda - up to 5 levels deep
      (setq org-refile-targets '((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5)))

      (after-load 'org-agenda
        (add-to-list 'org-agenda-after-show-hook 'org-show-entry))

      ;; Exclude DONE state tasks from refile targets
      (setq org-refile-target-verify-function 'paruka/verify-refile-target)

      ;; Targets start with the file name - allows creating level 1 tasks
      ;;(setq org-refile-use-outline-path (quote file))
      (setq org-refile-use-outline-path t)
      (setq org-outline-path-complete-in-steps nil)

      ;; Allow refile to create parent tasks with confirmation
      (setq org-refile-allow-creating-parent-nodes 'confirm)

      
      ;;; To-do settings
      (setq org-todo-keywords
            (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
                    (sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
                    (sequence "WAITING(w@/!)" "DELEGATED(e!)" "HOLD(h)" "|" "CANCELLED(c@/!)")))
            org-todo-repeat-to-state "NEXT")

      (setq org-todo-keyword-faces
            (quote (("NEXT" :inherit warning)
                    ("PROJECT" :inherit font-lock-string-face))))
      
      ;;; Agenda views
      (setq-default org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3))

      (let ((active-project-match "-INBOX/PROJECT"))

        (setq org-stuck-projects
              `(,active-project-match ("NEXT")))

        (setq org-agenda-compact-blocks t
              org-agenda-sticky t
              org-agenda-start-on-weekday nil
              org-agenda-span 'day
              org-agenda-include-diary nil
              org-agenda-sorting-strategy
              '((agenda habit-down time-up user-defined-up effort-up category-keep)
                (todo category-up effort-up)
                (tags category-up effort-up)
                (search category-up))
              org-agenda-window-setup 'current-window
              org-agenda-custom-commands
              `(("N" "Notes" tags "NOTE"
                 ((org-agenda-overriding-header "Notes")
                  (org-tags-match-list-sublevels t)))
                ("g" "GTD"
                 ((agenda "" nil)
                  (tags "INBOX"
                        ((org-agenda-overriding-header "Inbox")
                         (org-tags-match-list-sublevels nil)))
                  (stuck ""
                         ((org-agenda-overriding-header "Stuck Projects")
                          (org-agenda-tags-todo-honor-ignore-options t)
                          (org-tags-match-list-sublevels t)
                          (org-agenda-todo-ignore-scheduled 'future)))
                  (tags-todo "-INBOX/NEXT"
                             ((org-agenda-overriding-header "Next Actions")
                              (org-agenda-tags-todo-honor-ignore-options t)
                              (org-agenda-todo-ignore-scheduled 'future)
                              ;; TODO: skip if a parent is WAITING or HOLD
                              (org-tags-match-list-sublevels t)
                              (org-agenda-sorting-strategy
                               '(todo-state-down effort-up category-keep))))
                  (tags-todo ,active-project-match
                             ((org-agenda-overriding-header "Projects")
                              (org-tags-match-list-sublevels t)
                              (org-agenda-sorting-strategy
                               '(category-keep))))
                  (tags-todo "-INBOX/-NEXT"
                             ((org-agenda-overriding-header "Orphaned Tasks")
                              (org-agenda-tags-todo-honor-ignore-options t)
                              (org-agenda-todo-ignore-scheduled 'future)
                              ;; TODO: skip if a parent is a project
                              (org-agenda-skip-function
                               '(lambda ()
                                  (or (org-agenda-skip-subtree-if 'todo '("PROJECT" "HOLD" "WAITING" "DELEGATED"))
                                      (org-agenda-skip-subtree-if 'nottododo '("TODO")))))
                              (org-tags-match-list-sublevels t)
                              (org-agenda-sorting-strategy
                               '(category-keep))))
                  (tags-todo "/WAITING"
                             ((org-agenda-overriding-header "Waiting")
                              (org-agenda-tags-todo-honor-ignore-options t)
                              (org-agenda-todo-ignore-scheduled 'future)
                              (org-agenda-sorting-strategy
                               '(category-keep))))
                  (tags-todo "/DELEGATED"
                             ((org-agenda-overriding-header "Delegated")
                              (org-agenda-tags-todo-honor-ignore-options t)
                              (org-agenda-todo-ignore-scheduled 'future)
                              (org-agenda-sorting-strategy
                               '(category-keep))))
                  (tags-todo "-INBOX/HOLD"
                             ((org-agenda-overriding-header "On Hold")
                              ;; TODO: skip if a parent is WAITING or HOLD
                              (org-tags-match-list-sublevels nil)
                              (org-agenda-sorting-strategy
                               '(category-keep))))
                  ;; (tags-todo "-NEXT"
                  ;;            ((org-agenda-overriding-header "All other TODOs")
                  ;;             (org-match-list-sublevels t)))
                  )))))


      (add-hook 'org-agenda-mode-hook 'hl-line-mode)

      
      ;;; Org clock
      ;; Save the running clock and all clock history when exiting Emacs, load it on startup
      (after-load 'org
        (org-clock-persistence-insinuate))

      (setq org-clock-persist t)
      (setq org-clock-in-resume t)

      ;; Save clock data and notes in the LOGBOOK drawer
      (setq org-clock-into-drawer t)
      ;; Save state changes in the LOGBOOK drawer
      (setq org-log-into-drawer t)
      ;; Removes clocked tasks with 0:00 duration
      (setq org-clock-out-remove-zero-time-clocks t)

      ;; Show clock sums as hours and minutes, not "n days" etc.
      (setq org-time-clocksum-format
            '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

      
      (add-hook 'org-clock-in-hook 'paruka/show-org-clock-in-header-line)
      (add-hook 'org-clock-out-hook 'paruka/hide-org-clock-from-header-line)
      (add-hook 'org-clock-cancel-hook 'paruka/hide-org-clock-from-header-line)

      (after-load 'org-clock
        (define-key org-clock-mode-line-map [header-line mouse-2] 'org-clock-goto)
        (define-key org-clock-mode-line-map [header-line mouse-1] 'org-clock-menu))

      
      (when (and (spacemacs/system-is-mac) (file-directory-p "/Applications/org-clock-statusbar.app"))
        (add-hook 'org-clock-in-hook
                  (lambda () (call-process "/usr/bin/osascript" nil 0 nil "-e"
                                           (concat "tell application \"org-clock-statusbar\" to clock in \"" org-clock-current-task "\""))))
        (add-hook 'org-clock-out-hook
                  (lambda () (call-process "/usr/bin/osascript" nil 0 nil "-e"
                                           "tell application \"org-clock-statusbar\" to clock out"))))

      
      ;; Remove empty LOGBOOK drawers on clock out

      (after-load 'org-clock
        (add-hook 'org-clock-out-hook 'paruka/remove-empty-drawer-on-clock-out 'append))

      ;; TODO: warn about inconsistent items, e.g. TODO inside non-PROJECT
      ;; TODO: nested projects!

      
;;; Archiving

      (setq org-archive-mark-done nil)
      (setq org-archive-location "%s_archive::* Archive")

      

      ;;(package 'org-pomodoro)
      (after-load 'org-agenda
        (define-key org-agenda-mode-map (kbd "P") 'org-pomodoro))

      ;; ;; Show iCal calendars in the org agenda
      ;; (when (and *is-a-mac* (require 'org-mac-iCal nil t))
      ;;   (setq org-agenda-include-diary t
      ;;         org-agenda-custom-commands
      ;;         '(("I" "Import diary from iCal" agenda ""
      ;;            ((org-agenda-mode-hook #'org-mac-iCal)))))

      ;;   (add-hook 'org-agenda-cleanup-fancy-diary-hook
      ;;             (lambda ()
      ;;               (goto-char (point-min))
      ;;               (save-excursion
      ;;                 (while (re-search-forward "^[a-z]" nil t)
      ;;                   (goto-char (match-beginning 0))
      ;;                   (insert "0:00-24:00 ")))
      ;;               (while (re-search-forward "^ [a-z]" nil t)
      ;;                 (goto-char (match-beginning 0))
      ;;                 (save-excursion
      ;;                   (re-search-backward "^[0-9]+:[0-9]+-[0-9]+:[0-9]+ " nil t))
      ;;                 (insert (match-string 0))))))


      (add-hook 'org-mode-hook '(lambda ()
                                  ;; keybinding for editing source code blocks
                                  ;; keybinding for inserting code blocks
                                  (local-set-key (kbd "C-c i s") 'paruka/org-insert-src-block)))


      ;; plantuml
      (require 'plantuml-mode)
      ;;(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))

      (after-load 'org
        (define-key org-mode-map (kbd "C-M-<up>") 'org-up-element)
          ;;(define-key org-mode-map (kbd "M-h") nil)
          ;;(define-key org-mode-map (kbd "C-c g") 'org-mac-grab-link))
      )

      (after-load 'org
        (org-babel-do-load-languages
         'org-babel-load-languages
         `(
           (R . t)
           (ditaa . t)
           (dot . t)
           (emacs-lisp . t)
           (gnuplot . t)
           (haskell . nil)
           (latex . t)
           (ledger . t)
           (ocaml . nil)
           (octave . t)
           (python . t)
           (ruby . t)
           (screen . nil)
           (,(if (locate-library "ob-sh") 'sh 'shell) . t)
           (sql . t)
           (perl . t)
           (js . t)
           (css . t)
           (sed . t)
           (awk . t)
           (org . t)
           (plantuml . t)
           (C . t)
           (lua . t)
           (sqlite . t))))

      ;;(require-package 'org-bullets)
      ;;(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

      ;; (add-hook 'org-babel-after-execute-hook
      ;;           (lambda ()
      ;;             (when org-inline-image-overlays
      ;;               (org-redisplay-inline-images))))

      ;;(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

      ;; (after-load 'org-mode
      ;;   (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
      ;;   (add-to-list 'org-src-lang-modes  '("dot" . graphviz-dot))
      ;;   )

      )))
;;; packages.el ends here
