;;; packages.el --- paruka-cc layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author:  paruka <paruka.me@gmail.com>
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
;; added to `paruka-cc-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `paruka-cc/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `paruka-cc/pre-init-PACKAGE' and/or
;;   `paruka-cc/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst paruka-cc-packages
  '(
    google-c-style
    modern-cpp-font-lock
    cc-mode
    ;;company-rtags
    ;;helm-rtags
    ;;flycheck-rtags
    ;;rtags
    flycheck
    company
    company-c-headers
    gdb-mi
    helm-make
    ;;ggtags
    ;;helm-gtags
    (lsp-mode :location (recipe
                         :fetcher github
                         :repo "emacs-lsp/lsp-mode"))
    (cquery :location (recipe
                       :fetcher github
                       :repo "cquery-project/emacs-cquery"))
    (helm-xref :location (recipe
                          :fetcher github
                          :repo "brotzeit/helm-xref"))
    (lsp-ui :location (recipe
                       :fetcher github
                       :repo "emacs-lsp/lsp-ui"))
    (company-lsp :location (recipe
                            :fetcher github
                            :repo "tigersoldier/company-lsp"))
    )
  "The list of Lisp packages required by the paruka-cc layer.

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

(defun paruka-cc/init-lsp-mode ()
  )

(defun paruka-cc/init-cquery ()
  (use-package cquery
    :commands lsp-cquery-enable
    :init
    (progn
      (add-hook 'c-mode-hook #'paruka/cquery-enable)
      (add-hook 'c++-mode-hook #'paruka/cquery-enable)
      (setq cquery-executable "/usr/bin/cquery")
      (setq cquery-extra-args '("--log-file=/tmp/cq.log"))
      (setq cquery-extra-init-params '(:index (:comments 2) :cacheFormat "msgpack"))
      (setq cquery-sem-highlight-method 'font-lock))))

(defun paruka-cc/post-init-cquery ()
  (progn
    (setq cquery-extra-init-params '(:completion (:detailedLabel t)))))

(defun paruka-cc/init-helm-xref ()
  (use-package helm-xref
    :init
    (progn
      (setq xref-show-xrefs-function 'helm-xref-show-xrefs))))

(defun paruka-cc/init-lsp-ui ()
  (use-package lsp-ui
    :init
    (progn
      (add-hook 'lsp-mode-hook 'lsp-ui-mode)
      (add-hook 'c-mode-hook 'flycheck-mode)
      (add-hook 'c++-mode-hook 'flycheck-mode))
    ))

(defun paruka-cc/post-init-lsp-ui ()
  (progn
    (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
    (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
    (define-key evil-normal-state-map (kbd "C-p") 'lsp-ui-peek-jump-forward)
    (define-key evil-normal-state-map (kbd "C-t") 'lsp-ui-peek-jump-backward)
    (with-eval-after-load 'projectile
      (setq projectile-project-root-files-top-down-recurring
            (append '("compile_commands.json"
                      ".cquery")
                    projectile-project-root-files-top-down-recurring)))
    ))

(defun paruka-cc/init-company-lsp ()
  (use-package company-lsp
    :init
    (progn
      (setq company-transformers nil company-lsp-async t company-lsp-cache-candidates nil))))

(defun paruka-cc/post-init-company-lsp ()
  (push 'company-lsp company-backends))

(defun paruka-cc/init-google-c-style ()
  ;; (use-package google-c-style
  ;;   :init (add-hook 'c++-mode-hook 'google-set-c-style))
  )

(defun paruka-cc/init-modern-cpp-font-lock ()
  (use-package modern-cpp-font-lock
    :ensure t
    :init (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode)))

(defun paruka-cc/init-cc-mode ()
  (use-package cc-mode
    :defer
    :mode ("\\.h\\'" . c++-mode)
    ;;:config
    ;;:init (progn
            ;; (add-to-list 'auto-mode-alist
            ;;              `("\\.h\\'" . ,c-c++-default-mode-for-headers))
      ;;      (add-hook 'c-mode-common-hook 'paruka/c-common-style)
        ;;    )
    ;; (progn
    ;;   (require 'complile)
    ;;   (add-hook 'c-mode-hook
    ;;             (lambda ()
    ;;               (unless (file-exists-p "Makefile")
    ;;                 A
    ;;                 (set (make-local-variable 'compile-command)
    ;;                      ;; emulate make's .c.o implicit pattern rule, but with
    ;;                      ;; different defaults for the CC, CPPFLAGS, and CFLAGS
    ;;                      ;; variables:
    ;;                      ;; $(CC) -c -o $@ $(CPPFLAGS) $(CFLAGS) $<
    ;;                      (let ((file (file-name-nondirectory buffer-file-name)))
    ;;                        (format "%s -o %s.o %s %s %s"
    ;;                                (or (getenv "CC") "clang")
    ;;                                (file-name-sans-extension file)
    ;;                                (or (getenv "CPPFLAGS") "-DDEBUG=9")
    ;;                                (or (getenv "CFLAGS") "-ansi -pedantic -Wall -g")
    ;;                                file))))))
    ;;   (add-hook 'c++-mode-hook
    ;;             (lambda ()
    ;;               (unless (file-exists-p "Makefile")
    ;;                 (set (make-local-variable 'compile-command)
    ;;                      ;; emulate make's .c.o implicit pattern rule, but with
    ;;                      ;; different defaults for the CC, CPPFLAGS, and CFLAGS
    ;;                      ;; variables:
    ;;                      ;; $(CC) -c -o $@ $(CPPFLAGS) $(CFLAGS) $<
    ;;                      (let ((file (file-name-nondirectory buffer-file-name)))
    ;;                        (format "%s -o %s.o %s %s %s"
    ;;                                (or (getenv "CC") "g++")
    ;;                                (file-name-sans-extension file)
    ;;                                (or (getenv "CPPFLAGS") " -DDEBUG=9")
    ;;                                (or (getenv "CFLAGS") "-ansi -pedantic -Wall -g -std=c++11")
    ;;                                file))))))
    ;;   )
    ))

(defun paruka-cc/post-init-cc-mode ()
  (add-hook 'c-mode-common-hook 'paruka/c-common-style)
  )

(defun paruka-cc/init-company-rtags ()
  (use-package company-rtags :defer t))

(defun paruka-cc/init-helm-rtags ()
  (use-package helm-rtags :defer t))

(defun paruka-cc/init-flycheck-rtags ()
  (use-package flycheck-rtags :defer t))

(defun paruka-cc/init-rtags ()
  (use-package rtags
    :init
    (progn
      (require 'company)
      (require 'company-rtags)
      (require 'helm-rtags)
      (require 'flycheck-rtags))
    :config))

(defun paruka-cc/post-init-ggtags ()
  (add-hook 'c-mode-local-vars-hook #'spacemacs/ggtags-mode-enable)
  (add-hook 'c++-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun paruka-cc/post-init-rtags()
  (add-hook 'c-mode-common-hook 'paruka/rtags-mode-hook))

(defun paruka-cc/post-init-flycheck ()
  (with-eval-after-load 'flycheck
    (progn
      ;; (setq flycheck-display-errors-delay 0.9
      ;;       flycheck-idle-change-delay 2.0
      ;;       flycheck-gcc-language-standard "c++17"
      ;;       flycheck-clang-language-standard "c++1z")
      (custom-set-variables
       '(flycheck-display-errors-delay 0.9)
       '(flycheck-idle-change-delay 2.0)
       '(flycheck-gcc-language-standard "c++17")
       '(flycheck-clang-language-standard "c++1z")
       '(flycheck-googlelint-verbose "3")
       '(flycheck-googlelint-linelength "80")
       '(flycheck-googlelint-filter "-whitespace,+whitespace/braces"))
      ))
  (spacemacs/add-to-hooks 'flycheck-mode '(c-mode-hook c++-mode-hook)))


(defun paruka-cc/post-init-company ()
  (add-hook 'c-mode-common-hook #'global-company-mode))

(defun paruka-cc/init-company-c-headers ()
  (use-package company-c-headers
    :if (configuration-layer/package-usedp 'company)
    :defer t
    :init (push 'company-c-headers company-backends-c-mode-common)))

(defun paruka-cc/init-gdb-mi ()
  (use-package gdb-mi
    :defer t
    :init
    (setq
     ;; use gdb-many-windows by default when `M-x gdb'
     gdb-many-windows t
     ;; Non-nil means display source file containing the main routine at startup
     gdb-show-main t)))

(defun paruka-cc/post-init-helm-make ()
)

(defun paruka-cc/post-init-helm-gtags ()
  (progn
    ;;(spacemacs/helm-gtags-define-keys-for-mode 'c++-mode)
    ;;(spacemacs/helm-gtags-define-keys-for-mode 'c-mode)

    ;;(add-hook 'c++-mode-hook #'spacemacs/ggtags-mode-enable)
    ;;(add-hook 'c-mode-hook #'spacemacs/ggtags-mode-enable)

    (paruka/helm-gtags-define-key-for-mode 'c++-mode)
    (paruka/helm-gtags-define-key-for-mode 'c-mode)
    (paruka/helm-gtags-define-key-for-mode 'python-mode)))

;;; packages.el ends here
