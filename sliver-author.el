;;; sliver-author.el --- Authoring helpers -*- lexical-binding: t; -*-

(require 'sliver-core)

;;;###autoload
(defun sliver-open-module (&optional module)
  "Open MODULE file, prompting if needed."
  (interactive)
  (sliver-refresh)
  (unless module
    (setq module (completing-read "Open module: " sliver--all-modules)))
  (find-file (sliver--module-file module)))

;;;###autoload
(defun sliver-create-module (&optional module)
  "Create a new MODULE file."
  (interactive)
  (unless module
    (setq module (read-string "Create module: ")))
  (let ((file (sliver--module-file module)))
    (if (file-exists-p file)
	(message "Module '%s' already exists" module)
      (progn
	(find-file file)
	(insert ";;; name: %s\n;;; depends:\n;;; conflicts:\n;;; description:\n" module)
	))))

;;;###autoload
(defun sliver-insert-module (&optional module)
  "Insert load form for MODULE at point."
  (interactive)
  (sliver-refresh)
  (unless module
    (setq module (completing-read "Insert module: " sliver--all-modules)))
  (insert (format "(sliver-load \"%s\")" module)))

;;;###autoload
(defun sliver-init-module (&optional module)
  "Insert load form and create MODULE file."
  (interactive)
  (unless module
    (setq module (read-string "Create module: ")))
  (sliver-insert-module module)
  (save-buffer)
  (sliver-create-module module))

;;;###autoload
(defun sliver-open-init ()
  "Open the sliver init file."
  (interactive)
  (find-file sliver-init-file))

(provide 'sliver-author)
;;; sliver-author.el ends here
