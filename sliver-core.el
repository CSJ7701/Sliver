;;; sliver-core.el --- Core state and registry -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'sliver-metadata)

(defgroup sliver nil
  "Modular Emacs configuration"
  :group 'initialization)

(defcustom sliver-modules-dir
  (expand-file-name "modules" user-emacs-directory)
  "Directory containing sliver module files."
  :type 'directory
  :group 'sliver)

(defcustom sliver-init-file
  (expand-file-name "init.el" user-emacs-directory)
  "Path to the main Emacs init file."
  :type 'file
  :group 'sliver)

;; Internal State
(defvar sliver--all-modules nil
  "List of all discovered sliver modules.")

(defvar sliver--loaded-modules nil
  "List of all loaded sliver modules.")

(defvar sliver--unloaded-modules nil
  "List of all unloaded sliver modules.")

(defvar sliver--module-conflicts
  (make-hash-table :test #'equal)
  "Hash table mapping module names to conflicting modules.")

(defvar sliver--module-dependencies
  (make-hash-table :test #'equal)
  "Hash table mapping module names to dependencies.")

(defun sliver--module-file (name)
  "Return absolute file path for module NAME."
  (expand-file-name (format "%s-module.el" name)
		    sliver-modules-dir))

(defun sliver--current-sliver ()
  "Return sliver name if current buffer is a sliver file."
  (when buffer-file-name
    (let* ((dir (file-name-directory buffer-file-name))
	   (name (file-name-nondirectory buffer-file-name)))
      (when (and (string-prefix-p (expand-file-name sliver-modules-dir)
				  (expand-file-name dir))
		 (string-suffix-p "-module.el" name))
	(let ((sliver-name (substring name 0 (- (length name)
						(length "-module.el")))))
	  (when (member sliver-name sliver--all-modules)
	    sliver-name))))))

(defun sliver-refresh ()
  "Refresh internal module lists."
  (setq sliver--all-modules
	(mapcar
	 (lambda (file)
	   (replace-regexp-in-string "-module\\.el$" "" file))
	 (directory-files sliver-modules-dir nil "-module\\.el$")))
  (setq sliver--unloaded-modules
	(seq-difference sliver--all-modules sliver--loaded-modules))
  (sliver--scan-all-metadata)
  (sliver--validate-metadata)
  )

(defun sliver-declare-conflict-XXX (module conflict)
  "Declare MODULE and CONFLICT as mutually exclusive."
  (when (and (member module sliver--all-modules)
	     (member conflict sliver--all-modules))
    (cl-labels ((add (a b)
		  (let ((lst (gethash a sliver--module-conflicts)))
		    (unless (member b lst)
		      (puthash a (cons b lst)
			       sliver--module-conflicts)))))
      (add module conflict)
      (add conflict module))))

(defun sliver-declare-dependency-XXX (module dependency)
  "Declare the MODULE depends on DEPENDENCY."
  (when (and (member module sliver--all-modules)
	     (member dependency sliver--all-modules))
    (let ((deps (gethash module sliver--module-dependencies)))
      (unless (member dependency deps)
	(puthash module (cons dependency deps)
		 sliver--module-dependencies)))))

(provide 'sliver-core)
;;; sliver-core.el ends here

  
