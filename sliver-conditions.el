;;; sliver-conditions.el --- Machine facts and conditional loading for Sliver -*- lexical-binding: t; -*-

(require 'cl-lib)

(defgroup sliver-conditions nil
  "Machine context and conditional evaluation for Sliver."
  :group 'sliver)

(defun sliver-machine-hostname ()
  "Return the current system hostname as a string."
  (system-name))

(defun sliver-machine-os ()
  "Return the current OS type as a symbol.
Possible values include: 'gnu/linux', 'darwin', 'windows-nt'."
  system-type)

(defun sliver-machine-window-system ()
  "Return the current window system symbol, or nil."
  window-system)

(defun sliver-machine-fact (key)
  "Return the machine fact corresponding to KEY.
Valid keys:
   hostname
   os
   window-system"
  (pcase key
    ('hostname (sliver-machine-hostname))
    ('os (sliver-machine-os))
    ('window-system (sliver-machine-window-system))
    (_ nil)))

;;;;; Profiles

(defcustom sliver-machine-profiles nil
  "Alist of machine profiles.
Each entry has form:
   (PROFILE_NAME . CONDITIONS)

Where CONDITIONS is a plist suitable for 'sliver-build-condition', excluding :profile itself.
Example:
   '((\"Home-PC\" . (:hostname \"Jimmy\" :os gnu/linux))
     (\"Work-Laptop\" . (:os darwin)))"
  :type '(alist :key-type string :value-type plist)
  :group 'sliver-conditions)

(defun sliver-machine-profile-match-p (profile)
  "Return non-nil if PROFILE matches the current machine context."
  (let ((entry (assoc profile sliver-machine-profiles)))
    (when entry
      (let ((predicate (apply #'sliver-build-condition (cdr entry))))
	(funcall predicate)))))

;;;; Condition normalization
(defun sliver--normalize-conditions (plist)
  "Normalize PLIST into an alist of (KEY . VALUE)"
  (let (out)
    (while plist
      (let ((key (pop plist))
	    (val (pop plist)))
	(when (keywordp key)
	  (push (cons (intern (substring (symbol-name key) 1)) val)
		out))))
    (nreverse out)))

;;;; Condition matching
(defun sliver--condition-match-p (key expected)
  "Return non-nil if condition KEY matches EXPECTED."
  (pcase key
    ('profile
     (sliver-machine-profile-match-p expected))
    (_
     (equal (sliver-machine-fact key) expected))))

(defun sliver-condition-match-p (conditions &optional match)
  "Evaluate CONDITIONS using MATCH strategy.
CONDITIONS is an alist of (KEY . VALUE).
MATCH is either 'and (default) or 'or."
  (let ((mode (or match 'and)))
    (pcase mode
      ('or
       (cl-some (lambda (c)
		  (sliver--condition-match-p (car c) (cdr c)))
		conditions))
      (_
       (cl-every (lambda (c)
		   (sliver--condition-match-p (car c) (cdr c)))
		 conditions)))))

;;;; Public constructor
(defun sliver-build-condition (&rest plist)
  "Build and return a zero-argument predicate from PLIST.

Recognized keywords:
   :hostname
   :os
   :window-system
   :profile
   :match (symbol: 'and or 'or)

If no conditions are supplied, the returned predicate always succeeds."
  (let* ((match (plist-get plist :match))
	 (filtered (cl-loop for (k v) on plist by #'cddr
			    unless (eq k :match)
			    collect k
			    and collect v))
	 (conditions (sliver--normalize-conditions filtered)))
    (if (null conditions)
	(lambda () t)
      (lambda ()
	(sliver-condition-match-p conditions match)))))

(provide 'sliver-conditions)

;;; sliver-conditions.el ends here
   
