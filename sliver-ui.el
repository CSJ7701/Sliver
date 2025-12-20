;;; sliver-ui.el --- UI for sliver -*- lexical-binding: t; -*-

(require 'tabulated-list)
(require 'sliver-core)
(require 'sliver-load)
(require 'sliver-author)

;;;; Faces

(defface sliver-ui-module-face
  '((t :weight bold))
  "Face for module names.")

(defface sliver-ui-loaded-face
  '((t :foreground "green" :weight bold))
  "Face for loaded modules.")

(defface sliver-ui-unloaded-face
  '((t :foreground "gray"))
  "Face for unloaded modules.")

(defface sliver-ui-conflict-face
  '((t :foreground "red" :weight bold))
  "Face for conflicts.")

(defface sliver-ui-dependency-face
  '((t :foreground "cyan"))
  "Face for dependencies.")

(defface sliver-ui-section-header-face
  '((t :weight bold :underline t))
  "Face for section headers in the details sidebar.")

;;;; Keymaps

(defvar sliver-module-table-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'sliver-module-details-view)
    (define-key map (kbd "C-c C-c") #'sliver-module-table-load)
    (define-key map (kbd "o") #'sliver-open-module-at-point)
    (define-key map (kbd "g") #'sliver-module-table-refresh)
    (define-key map (kbd "q") #'sliver-module-table-quit)
    map))

(defvar sliver-module-details-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'kill-buffer-and-window)
    map))


;;;; Modes

(define-derived-mode sliver-module-table-mode tabulated-list-mode "Sliver Modules"
  "Major mode for viewing sliver modules."
  (setq tabulated-list-format
	[("Module" 24 t)
	 ("Loaded" 8 t)
	 ("Conflicts" 10 nil)
	 ("Dependencies" 15 nil)])
  (setq tabulated-list-padding 2)
  (use-local-map sliver-module-table-mode-map)
  (tabulated-list-init-header))

(define-derived-mode sliver-module-details-mode special-mode "Sliver Module Details"
  "Major mode for displaying details about a Sliver module."
  (read-only-mode 1))


;;;; Helpers

(defun sliver--bool-cell (value)
  (propertize
   (if value "✔" "✘")
   'face (if value 'sliver-ui-loaded-face 'sliver-ui-unloaded-face)))

(defun sliver--count-cell (count face)
  (propertize (number-to-string count) 'face face))

(defun sliver--insert-section (title items face)
  "Insert a titled section with ITEMS into the current buffer."
  (insert (propertize title 'face 'sliver-ui-section-header-face))
  (insert "\n")
  (if items
      (dolist (item items)
	(insert (format "  • %s\n" (propertize item 'face face))))
    (insert "  (none)\n"))
  (insert "\n"))


;;;; Table Logic
			  
(defun sliver-module-table-refresh ()
  "Refresh the Sliver module table."
  (interactive)
  (sliver-refresh)
  (setq tabulated-list-entries
	(mapcar
	 (lambda (module)
	   (let* ((loaded (member module sliver--loaded-modules))
		  (conflicts (gethash module sliver--module-conflicts))
		  (dependencies (gethash module sliver--module-dependencies)))
	     (list module
		   (vector
		    (propertize module 'face 'sliver-ui-module-face)
		    (sliver--bool-cell loaded)
		    (sliver--count-cell (length conflicts) 'sliver-ui-conflict-face)
		    (sliver--count-cell (length dependencies) 'sliver-ui-dependency-face)))))
	 sliver--all-modules))
  (tabulated-list-print t))

;;;###autoload
(defun sliver-module-table ()
  "Display the Sliver module table."
  (interactive)
  (let ((buf (get-buffer-create "*Sliver Modules*")))
    (with-current-buffer buf
      (sliver-module-table-mode)
      (sliver-module-table-refresh))
    (pop-to-buffer buf)))

(defun sliver-module-table-load ()
  "Load module at point."
  (interactive)
  (when-let ((module (tabulated-list-get-id)))
    (sliver-load module)
    (sliver-module-table-refresh)))

(defun sliver-open-module-at-point ()
  "Run 'sliver-open-module' on module at point."
  (interactive)
  (when-let* ((module (tabulated-list-get-id)))
    (sliver-open-module module)))


;;;; Details sidebar

(defun sliver-module-details-view ()
  "Show conflicts and dependencies for the module at point.
Used in Sliver's module table view."
  (interactive)
  (when-let* ((module (tabulated-list-get-id))
	      (deps (gethash module sliver--module-dependencies))
	      (conflicts (gethash module sliver--module-conflicts)))
    (let ((buf (get-buffer-create "*Sliver Module Details*")))
      (with-current-buffer buf
	(erase-buffer)
	(insert (propertize module 'face 'sliver-ui-module-face))
	(insert "\n\n")
	(sliver--insert-section "Dependencies" deps 'sliver-ui-dependency-face)
	(sliver--insert-section "Conflicts" conflicts 'sliver-ui-conflict-face)
	(goto-char (point-min))
	(sliver-module-details-mode))
      (display-buffer-in-side-window
       buf '((side . right) (window-width . 45))))))

(defun sliver-module-table-quit ()
  "Quit the module table buffer and close the conflict sidebar if open."
  (interactive)
  (when-let ((buf (get-buffer "*Sliver Module Details*")))
    (kill-buffer buf))
  (kill-buffer))
  
(provide 'sliver-ui)
;;; sliver-ui.el ends here
		 
