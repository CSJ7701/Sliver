;;; sliver-metadata.el --- Metadata parsing and setting utilities -*- lexical-binding: t; -*-

(require 'sliver-core)

(defun sliver--scan-metadata (file)
  "Return plist of metadata for FILE."
  (with-temp-buffer
    (insert-file-contents file nil 0 2048)
    (let (deps conflicts desc)
      (goto-char (point-min))
      (while (and (not (eobp))
		  (looking-at "^;;; \\([^:]+\\):[ \t]*\\(.*\\)$"))
	(pcase (match-string 1)
	  ("depends"
	   (setq deps (split-string (match-string 2))))
	  ("conflicts"
	   (setq conflicts (split-string (match-string 2))))
	  ("description"
	   (setq desc (match-string 2))))
	(forward-line 1))
      (list :depends deps :conflicts conflicts :description desc))))

(defun sliver--scan-all-metadata ()
  "Scan all sliver files and populate hashmaps for dependencies and conflicts."
  (clrhash sliver--module-dependencies)
  (clrhash sliver--module-conflicts)
  (dolist (file (directory-files sliver-modules-dir t "-module\\.el$"))
    (let* ((metadata (sliver--scan-metadata file))
	   (module (file-name-base file))
	   (module-name (substring module 0 (- (length module)
					       (length "-module")))))
      (when-let ((deps (plist-get metadata :depends)))
	(puthash module-name deps sliver--module-dependencies))
      (when-let ((conflicts (plist-get metadata :conflicts)))
	(puthash module-name conflicts sliver--module-conflicts)))))

(defun sliver--validate-metadata ()
  (maphash
   (lambda (module deps)
     (dolist (dep deps)
       (unless (member dep sliver--all-modules)
	 (message "Sliver warning: %s depends on unknown module %s"
		  module dep))))
   sliver--module-dependencies)

  (maphash
   (lambda (module conflicts)
     (dolist (c conflicts)
       (unless (member c sliver--all-modules)
	 (message "Sliver warning: %s conflicts with unknown module %s"
		  module c))))
   sliver--module-conflicts))
	   

;;;###autoload
(defun sliver-declare-dependency (module dependency)
  "Declare DEPENDENCY for MODULE by editing its metadata."
  (interactive
   (let ((mod (or (sliver--current-sliver)
		  (completing-read "Sliver: " sliver--all-modules))))
     (list mod
	   (completing-read "Depends on: " sliver--all-modules))))
  (sliver--edit-metadata module 'depends dependency))

;;;###autoload
(defun sliver-declare-conflict (module conflict)
  "Declare CONFLICT for MODULE by editing its metadata."
  (interactive
   (let ((mod (or (sliver--current-sliver)
		  (completing-read "Sliver: " sliver--all-modules))))
     (list mod
	   (completing-read "Conflicts with: " sliver--all-modules))))
  (sliver--edit-metadata module 'conflicts dependency))

(defun sliver--edit-metadata (module key value)
  "Add VALUE to KEY metadata for MODULE."
  (let ((file (sliver--module-file module)))
    (unless (file-exists-p file)
      (error "Module file does not exist: %s" file))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
	(goto-char (point-min))
	(if (re-search-forward (format "^;;; %s:" key) nil t)
	    (let ((line-end (line-end-position)))
	      ;; Check if value already exists on this line
	      (unless (save-excursion
			(beginning-of-line)
			(re-search-forward (regexp-quote value) line-end t))
		(goto-char line-end)
		(insert " " value)))
	  ;; Create new metadata line
	  (goto-char (point-min))
	  (insert (format ";;; %s: %s\n" key value))))
      (save-buffer))))

(provide 'sliver-metadata)
