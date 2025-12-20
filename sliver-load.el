;;; sliver-load.el --- Module loading logic -*- lexical-binding: t; -*-

(require 'sliver-core)
(require 'sliver-conditions)

(defgroup sliver-load nil
  "Core module loading for Sliver."
  :group 'sliver)

(defcustom sliver-on-conflict-function #'error
  "Function called when a module conflict is detected."
  :type 'function
  :group 'sliver-load)

(defcustom sliver-after-load-hook nil
  "Hook run after a sliver module is loaded."
  :type 'hook
  :group 'sliver-load)

(defcustom sliver-conditional-verbose nil
  "If non-nil, emit messages when a module is skipped due to its conditions."
  :type 'boolean
  :group 'sliver-load)

;;;; Dependency/Conflict aware loading

(defun sliver--load-with-deps (module seen)
  "Load MODULE and its dependencies, tracking SEEN to detect cycles."
  (when (member module seen)
    (error "Cyclic dependency involving %s module"))
  (unless (member module sliver--loaded-modules)
    (let ((deps (gethash module sliver--module-dependencies)))
      (dolist (dep deps)
	(sliver--load-with-deps dep (cons module seen))))
    (sliver--load-single module)))

(defun sliver--load-single (module)
  "Load MODULE itself, checking conflicts but not dependencies."
  (let ((conflicts (gethash module sliver--module-conflicts)))
    (when (seq-some (lambda (m) (member m sliver--loaded-modules)) conflicts)
      (funcall sliver-on-conflict-function
	       "Module '%s' conflicts with loaded modules: %s"
	       module
	       (string-join (seq-filter (lambda (m) (member m sliver--loaded-modules)) conflicts)
			    ", "))))
  (let ((file (sliver--module-file module)))
    (when (file-exists-p file)
      (load file)
      (add-to-list 'sliver--loaded-modules module)
      (run-hooks 'sliver-after-load-hook))))

;;;; Public entry point

;;;###autoload
(defun sliver-load (&optional module &rest plist)
  "Load MODULE, prompting if not provided.
Optionally gated by conditional keywords.

When PLIST is empty, MODULE is loaded unconditionally.
When PLIST contains condition keywords, MODULE is loaded only if the constructed predicate evaluates to non-nil.
Recognized condition keywords are defined by 'sliver-build-condition'."
  (interactive)
  (sliver-refresh)
  (unless module
    (setq module (completing-read "Load module: " sliver--all-modules)))

  (let* ((predicate (apply #'sliver-build-condition plist))
	 (allowed (funcall predicate)))
    (cond
     (allowed
      (sliver--load-with-deps module nil))
     (sliver-conditional-verbose
      (message "[sliver] Skipping module %s (conditions not met)" module))
     (t nil))))

(provide 'sliver-load)
;;; sliver-load.el ends here
