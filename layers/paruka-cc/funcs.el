(defun paruka/add-to-mode (mode lst)
  (dolist (file lst)
    (add-to-list 'auto-mode-alist (cons file mode))))

(defun paruka/build-tab-stop-list (width)
  (let ((num-tab-stops (/ 80 width))
        (counter 1)
        (ls nil))
    (while (<= counter num-tab-stops)
      (setq ls (cons (* width counter) ls))
      (setq counter (1+ counter)))
    (set (make-local-variable 'tab-stop-list) (nreverse ls))))

(defun paruka/c++-looking-at-lambda_as_param ()
  "Return t if text after point matches '[...](' or '[...]{'"
  (looking-at ".*[,(][ \t]*\\[[^]]*\\][ \t]*[({][^}]*?[ \t]*[({][^}]*?$"))

(defun paruka/c++-looking-at-lambda_in_uniform_init ()
  "Return t if text after point matches '{[...](' or '{[...]{'"
  (looking-at ".*{[ \t]*\\[[^]]*\\][ \t]*[({][^}]*?[ \t]*[({][^}]*?$"))

(defun paruka/c++-indentation-examine (langelem looking-at-p)
  (and (equal major-mode 'c++-mode)
       (ignore-errors
         (save-excursion
           (goto-char (c-langelem-pos langelem))
           (funcall looking-at-p)))))

(defun paruka/c-common-style ()
  (google-set-c-style)
  (google-make-newline-indent)
  (c-set-style "Google")
  (setq tab-width 4)
  (paruka/build-tab-stop-list tab-width)
  (setq c-basic-offset tab-width)
  (setq indent-tabs-mode nil) ;; force only spaces for indentation
  (local-set-key "\C-o" 'ff-find-other-file)
  (c-set-offset 'substatement-open 0)
  ;;(c-set-offset 'arglist-intro 'c-lineup-arglist-intro-after-paren)
  (c-set-offset 'access-label '-)

  ;;https://www.gnu.org/software/emacs/manual/html_node/ccmode/Brace_002fParen-Line_002dUp.html
  (c-set-offset 'arglist-close 'c-lineup-close-paren)
  (c-set-offset 'stream-op 'c-lineup-streamop)

  (c-set-offset
   'block-close
   (lambda (langelem)
     (if (paruka/c++-indentation-examine
          langelem
          #'paruka/c++-looking-at-lambda_in_uniform_init)
         '-
       0)))

  (c-set-offset
   'statement-block-intro
   (lambda (langelem)
     (if (paruka/c++-indentation-examine
          langelem
          #'paruka/c++-looking-at-lambda_in_uniform_init)
         0
       '+)))

  ;; http://stackoverflow.com/questions/23553881/emacs-indenting-of-c11-lambda-functions-cc-mode
  (defadvice c-lineup-arglist (around my activate)
    "Improve indentation of continued C++11 lambda function opened as argument."
    (setq ad-return-value
          (if (paruka/c++-indentation-examine
               langelem
               #'paruka/c++-looking-at-lambda_as_param)
              0
            ad-do-it)))

  ;; flycheck
  (flycheck-select-checker 'c/c++-gcc)
  ;;(flycheck-select-checker 'c/c++-clang)
  (setq flycheck-gcc-language-standard "c++17")
  (setq flycheck-clang-language-standard "c++1z")

  ;; irony-mode
  ;;(setq irony-additional-clang-options '("-std=c++1z"))

  ;; flycheck clang include path
  (let ((root (ignore-errors (projectile-project-root))))
    (when root
      (add-to-list
       (make-variable-buffer-local 'flycheck-clang-include-path)
       root)))

  (setq cc-search-directories
        '("." "/usr/include" "/usr/local/include/*" "../*/include" "$WXWIN/include"))

  (paruka/add-to-mode 'c++-mode (list "\\.inc\\'" "\\.inl\\'"))
  )

(defun paruka/rtags-mode-hook ()
  (add-to-list 'exec-path paruka/rtags-path)

  ;; (require 'rtags)
  ;; (require 'helm-rtags)
  ;; (require 'company-rtags)
  ;; (require 'flycheck-rtags)

  ;;(require 'company)
  ;;(global-company-mode)

  (rtags-start-process-unless-running)
  (setq rtags-autostart-diagnostics t)
  (rtags-diagnostics)
  (setq rtags-completions-enabled t)
  (push 'company-rtags company-backends)

  ;; (flycheck-select-checker 'rtags)
  ;; (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
  ;; (setq-local flycheck-check-syntax-automatically nil)

  (setq rtags-display-result-backend 'helm)
  (setq rtags-use-helm t)

  ;; (mapc (lambda (x)
  ;;         (define-key c-mode-base-map
  ;;           (kbd (car x)) (cdr x)))
  ;;       '(("M-." . rtags-find-symbol-at-point)
  ;;         ("M-," . rtags-find-references-at-point)
  ;;         ("M-;" . rtags-find-file)
  ;;         ("C-." . rtags-find-symbol)
  ;;         ("C-," . rtags-find-references)
  ;;         ("C-<" . rtags-find-virtuals-at-point)
  ;;         ("M-i" . rtags-imenu)
  ;;         ("M-<left>" . rtags-location-stack-back)
  ;;         ("M-<right>" . rtags-location-stack-forward)))

  (mapc (lambda (x)
          (define-key c-mode-base-map
            (kbd (concat "C-c r " (car x))) (cdr x)))
        '(("." . rtags-find-symbol-at-point)
          ("," . rtags-find-references-at-point)
          ("v" . rtags-find-virtuals-at-point)
          ("V" . rtags-print-enum-value-at-point)
          ("/" . rtags-find-all-references-at-point)
          ("Y" . rtags-cycle-overlays-on-screen)
          (">" . rtags-find-symbol)
          ("<" . rtags-find-references)
          ("C-p" . rtags-location-stack-back)
          ("C-n" . rtags-location-stack-forward)
          ("D" . rtags-diagnostics)
          ("G" . rtags-guess-function-at-point)
          ("p" . rtags-set-current-project)
          ("P" . rtags-print-dependencies)
          ("e" . rtags-reparse-file)
          ("E" . rtags-preprocess-file)
          ("R" . rtags-rename-symbol)
          ("M" . rtags-symbol-info)
          ("s" . rtags-display-summary)
          ("O" . rtags-goto-offset)
          (";" . rtags-find-file)
          ("F" . rtags-fixit)
          ("X" . rtags-fix-fixit-at-point)
          ("B" . rtags-show-rtags-buffer)
          ("I" . rtags-imenu)
          ("T" . rtags-taglist)))
  )

(defun paruka/helm-gtags-define-key-for-mode (mode)
  (evil-leader/set-key-for-mode 'c++-mode
    "gc" 'helm-gtags-create-tags
    "gm" 'helm-imenu
    "gg" 'helm-gtags-dwim
    "gG" 'helm-gtags-dwim-other-window
    "gr" 'helm-gtags-find-rtag
    "gs" 'helm-gtags-find-symbol
    "gf" 'helm-gtags-find-files
    "gi" 'helm-gtags-tags-in-this-function
    "gu" 'helm-gtags-update-tags
    "gU" 'ggtags-update-tags
    "gS" 'helm-gtags-show-stack
    "gt" 'helm-gtags-select
    "gR" 'helm-gtags-resume
    )
  )
