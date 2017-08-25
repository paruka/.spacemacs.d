(when (spacemacs/system-is-mac)
 (defun paruka/notification (title message)
   (call-process "terminal-notifier"
                 nil 0 nil
                 "-group" "Emacs"
                 "-title" title
                 "-sender" "org.gnu.Emacs"
                 "-message" message
                 "-sound" "default"
                 "-activate" "org.gnu.Emacs")))

(when (spacemacs/system-is-linux)
  (defun paruka/notification (title message)
    (call-process "notify-send"
                  nil 0 nil
                  "-i" "/usr/share/icons/hicolor/48x48/apps/emacs.png"
                  title
                  message)))


(if (fboundp 'with-eval-after-load)
    (defalias 'after-load 'with-eval-after-load)
  (defmacro after-load (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))

;;; Show the clocked-in task - if any - in the header line
(defun sanityinc/show-org-clock-in-header-line ()
  (setq-default header-line-format '((" " org-mode-line-string " "))))

(defun sanityinc/hide-org-clock-from-header-line ()
  (setq-default header-line-format nil))

;; Lots of stuff from http://doc.norang.ca/org-mode.html
(defun sanityinc/grab-ditaa (url jar-name)
  "Download URL and extract JAR-NAME as `org-ditaa-jar-path'."
  ;; TODO: handle errors
  (message "Grabbing " jar-name " for org.")
  (let ((zip-temp (make-temp-name "emacs-ditaa")))
    (unwind-protect
        (progn
          (when (executable-find "unzip")
            (url-copy-file url zip-temp)
            (shell-command (concat "unzip -p " (shell-quote-argument zip-temp)
                                   " " (shell-quote-argument jar-name) " > "
                                   (shell-quote-argument org-ditaa-jar-path)))))
      (when (file-exists-p zip-temp)
        (delete-file zip-temp)))))

(define-minor-mode prose-mode
        "Set up a buffer for prose editing.
This enables or modifies a number of settings so that the
experience of editing prose is a little more like that of a
typical word processor."
        nil " Prose" nil
        (if prose-mode
            (progn
              (setq truncate-lines nil)
              (setq word-wrap t)
              (setq cursor-type 'bar)
              (when (eq major-mode 'org)
                (kill-local-variable 'buffer-face-mode-face))
              (buffer-face-mode 1)
              ;;(delete-selection-mode 1)
              (set (make-local-variable 'blink-cursor-interval) 0.6)
              (set (make-local-variable 'show-trailing-whitespace) nil)
              (flyspell-mode 1)
              (when (fboundp 'visual-line-mode)
                (visual-line-mode 1)))
          (kill-local-variable 'truncate-lines)
          (kill-local-variable 'word-wrap)
          (kill-local-variable 'cursor-type)
          (kill-local-variable 'show-trailing-whitespace)
          (buffer-face-mode -1)
          ;; (delete-selection-mode -1)
          (flyspell-mode -1)
          (when (fboundp 'visual-line-mode)
            (visual-line-mode -1))))

(defun sanityinc/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets."
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(defun sanityinc/org-refile-anywhere (&optional goto default-buffer rfloc msg)
  "A version of `org-refile' which allows refiling to any subtree."
  (interactive "P")
  (let ((org-refile-target-verify-function))
    (org-refile goto default-buffer rfloc msg)))

(defun sanityinc/org-agenda-refile-anywhere (&optional goto rfloc no-update)
  "A version of `org-agenda-refile' which allows refiling to any subtree."
  (interactive "P")
  (let ((org-refile-target-verify-function))
    (org-agenda-refile goto rfloc no-update)))

(defun sanityinc/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at (point))))

(defun paruka/org-insert-src-block (src-code-type)
  "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
  (interactive
   (let ((src-code-types
          '("emacs-lisp" "python" "C" "sh" "java" "js" "clojure" "C++" "css"
            "calc" "asymptote" "dot" "gnuplot" "ledger" "lilypond" "mscgen"
            "octave" "oz" "plantuml" "R" "sass" "screen" "sql" "awk" "ditaa"
            "haskell" "latex" "lisp" "matlab" "ocaml" "org" "perl" "ruby"
            "scheme" "sqlite")))
     (list (ido-completing-read "Source code type: " src-code-types))))
  (progn
    (newline-and-indent)
    (if (equal src-code-type "plantuml")
        (let ((picture-name (helm-read-string "Picture Name: ")))
          (insert (format "#+BEGIN_SRC plantuml :file %s.png :cmdline -charset UTF-8\n" picture-name))
          )
      (insert (format "#+BEGIN_SRC %s\n" src-code-type))
      )
    (newline-and-indent)
    (insert "#+END_SRC\n")
    (previous-line 2)
    (org-edit-src-code)))
