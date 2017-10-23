;;; packages.el --- Lua Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq paruka-lua-packages
      '(
        company
        flycheck
        ggtags
        helm-gtags
        lua-mode
        ))

(defun paruka-lua/post-init-flycheck ()
  (spacemacs/add-flycheck-hook 'lua-mode))

(defun paruka-lua/init-lua-mode ()
  (use-package lua-mode
    :defer t
    :mode ("\\.lua\\'" . lua-mode)
    :interpreter ("lua" . lua-mode)
    :init
    (progn
      (setq lua-indent-level paruka/lua-indent-level
            lua-indent-string-contents t)
      (spacemacs/set-leader-keys-for-major-mode 'lua-mode
        "d" 'lua-search-documentation
        "sb" 'lua-send-buffer
        "sf" 'lua-send-defun
        "sl" 'lua-send-current-line
        "sr" 'lua-send-region))))

(defun paruka-lua/post-init-lua-mode ()
  (progn
    (add-hook 'lua-mode-hook 'evil-matchit-mode)
    (add-hook 'lua-mode-hook 'smartparens-mode)
    (spacemacs|defvar-company-backends lua-mode)
    (spacemacs|add-company-hook lua-mode)
    (with-eval-after-load 'lua-mode
      (require 'company-keywords)
      (push '(lua-mode  "setmetatable" "local" "function" "and" "break" "do" "else" "elseif" "self" "resume" "yield"
                        "end" "false" "for" "function" "goto" "if" "nil" "not" "or" "repeat" "return" "then" "true"
                        "until" "while" "__index" "dofile" "getmetatable" "ipairs" "pairs" "print" "rawget" "status"
                        "rawset" "select" "_G" "assert" "collectgarbage" "error" "pcall" "coroutine"
                        "rawequal" "require" "load" "tostring" "tonumber" "xpcall" "gmatch" "gsub"
                        "rep" "reverse" "sub" "upper" "concat" "pack" "insert" "remove" "unpack" "sort"
                        "lower") company-keywords-alist))
    ))

(defun paruka-lua/post-init-company ()
  (add-hook 'lua-mode-hook 'company-mode))

(defun paruka-lua/post-init-ggtags ()
  (add-hook 'lua-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun paruka-lua/post-init-helm-gtags ()
  (spacemacs/helm-gtags-define-keys-for-mode 'lua-mode))
