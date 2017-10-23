;;; packages.el --- paruka-cc layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author:  paruka <paruka.me@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; variables

(spacemacs|defvar-company-backends c-mode-common)
(spacemacs|defvar-company-backends cmake-mode)
(spacemacs|define-jump-handlers c-mode)
(spacemacs|define-jump-handlers c++-mode)

(defvar c-c++-default-mode-for-headers 'c++-mode
  "Default mode to open header files. Can be `c-mode' or `c++-mode'.")
