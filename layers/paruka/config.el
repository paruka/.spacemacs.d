(defvar org-agenda-dir ""
  "gtd org files location")

(defvar deft-dir ""
  "deft org files locaiton")

(defvar blog-admin-dir ""
  "blog-admin files location")

(defconst paruka/plugin-path
  (expand-file-name "plugins/" (file-name-directory dotspacemacs-directory)))

(when (not (file-directory-p paruka/plugin-path))
  (message "Creating %s" paruka/plugin-path)
  (make-directory paruka/plugin-path))

(defconst paruka/rtags-path
  (expand-file-name "rtags/" paruka/plugin-path))

(setq-default
 org-agenda-dir '("~/git/org"
                  "~/git/org/personal"
                  "~/git/org/work")
 deft-dir "~/git/org/"
 )
