;;; org-buffers.el --- An Org-mode tool for buffer management

;; Copyright (C) 2010  Dan Davison

;; Author: Dan Davison <dandavison0 at gmail dot com>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 'org)
(require 'cl)

(defvar org-buffers-mode-map (make-sparse-keymap)
  "The keymap for `org-buffers-mode'.")

(define-key org-buffers-mode-map [(return)] 'org-buffers-follow-link)
(define-key org-buffers-mode-map "b" 'org-buffers-list:by)
(define-key org-buffers-mode-map "c" 'org-buffers-columns-view)
(define-key org-buffers-mode-map "d" 'org-buffers-tag-for-deletion)
(define-key org-buffers-mode-map "g" 'org-buffers-list:refresh)
(define-key org-buffers-mode-map "." 'org-buffers-switch-to-buffer)
(define-key org-buffers-mode-map "h" 'org-buffers-toggle-headings)
(define-key org-buffers-mode-map "o" 'org-buffers-switch-to-buffer-other-window)
(define-key org-buffers-mode-map "p" 'org-buffers-toggle-properties)
(define-key org-buffers-mode-map "u" 'org-buffers-remove-tags)
(define-key org-buffers-mode-map "x" 'org-buffers-execute-pending-operations)
(define-key org-buffers-mode-map "?" 'org-buffers-help)

(defvar org-buffers-mode-hook nil
  "Hook for functions to be called after buffer listing is
  created. Note that the buffer is read-only, so if the hook
  function is to modify the buffer it should use a let binding to
  temporarily bind buffer-read-only to nil.")

(defvar org-buffers-buffer-name
  "*Buffers*"
  "Name of buffer in which buffer list is displayed")

(defvar org-buffers-state
  '((:by . "major-mode") (:atom . heading) (:properties . nil))
  "Association list specifiying the current state of org-buffers.")

(defvar org-buffers-follow-link-method 'org-open-at-point
  "Method used to follow link with RET. Must be one of

'org-open-at-point :: use `org-open-at-point' to follow link.
'current-window    :: use switch-to-buffer
'other-window      :: use switch-to-buffer-other-window

Setting this variable to 'current-window makes the behaviour more
consistent with that of `Buffer-menu-mode' and `dired-mode'")

(defvar org-buffers-buffer-properties
  '(("buffer-name" . (buffer-name))
    ("major-mode" . (let ((mode (symbol-name major-mode)))
		      (if (string-match "-mode$" mode)
			  (replace-match "" nil t mode) mode)))
    ("buffer-file-name" . (buffer-file-name))
    ("default-directory" . default-directory)
    ("buffer-modified-p" . (format "%s" (buffer-modified-p))))
  "Association list specifying properties to be stored for each
buffer. The car of each element is the name of the property, and
the cdr is an expression which, when evaluated in the buffer,
yields the property value.")

(defcustom org-buffers-excluded-buffers
  `("*Completions*" ,org-buffers-buffer-name)
  "List of names of buffers that should not be listed by
  org-buffers-list."
  :group 'org-buffers)

(defcustom org-buffers-excluded-modes nil
  "List of names of major-modes (strings) that should not be listed
  by org-buffers-list."
  :group 'org-buffers)

(define-minor-mode org-buffers-mode
  "An Org-mode tool for buffer management.

  \\{org-buffers-mode-map}"
  nil " buffers" nil
  (org-set-local 'org-tag-alist '(("delete" . ?d)))
  (org-set-local'org-tags-column -50)
  (org-set-local 'org-columns-default-format "%25buffer-name(Buffer) %25major-mode(Mode) %25default-directory(Dir) %5buffer-modified-p(Modified)")
  (add-hook 'kill-buffer-hook 'org-buffers-reset-state nil 'local))

(defun org-buffers-help ()
  (interactive)
  (describe-function 'org-buffers-mode))

;;; Listing and view cycling

(defun org-buffers-list (&optional refresh frame)
  "Create an Org-mode listing of Emacs buffers.
By default, buffers are grouped by major mode. Optional
argument FRAME specifies the frame whose buffers should be
listed."
  (interactive)
  (pop-to-buffer
   (or
    (and (not refresh) (get-buffer org-buffers-buffer-name))
    (let ((org-buffers-p (equal (buffer-name) org-buffers-buffer-name))
	  (by (or (org-buffers-state-get :by) "major-mode"))
	  (atom (org-buffers-state-get :atom)) target)
      (when org-buffers-p
	(if (and (org-before-first-heading-p) (not (org-on-heading-p)))
	    (outline-next-heading))
	(setq target
	      (condition-case nil (org-make-org-heading-search-string) (error nil))))
      (with-current-buffer (get-buffer-create org-buffers-buffer-name)
	(setq buffer-read-only nil)
	(erase-buffer)
	(org-mode)
	(dolist
	    (buffer
	     (sort (remove-if 'org-buffers-exclude-p
			      (mapcar 'buffer-name (buffer-list frame))) 'string<))
	  (org-insert-heading t)
	  (insert
	   (org-make-link-string (concat "buffer:" buffer) buffer) "\n")
	  (dolist (pair (org-buffers-get-buffer-props buffer))
	    (org-set-property (car pair) (cdr pair))))
	(org-buffers-set-state '((:atom . heading)))
	(goto-char (point-min))
	(unless (equal by "NONE") (org-buffers-group-by by))
	(if target (condition-case nil (org-link-search target) (error nil)))
	(beginning-of-line)
	(if (equal by "NONE")
	    (org-overview)
	  (case atom
	    ('heading (progn (org-overview) (org-content)))
	    ('line (progn (show-all) (org-buffers-toggle-headings)))))
	(save-excursion
	  (mark-whole-buffer)
	  (indent-region (point-min) (point-max)))
	(org-buffers-mode)
	(setq buffer-read-only t)
	(current-buffer))))))

(defun org-buffers-list:refresh (&optional arg)
  "Refresh org-buffers listing."
  (interactive "P")
  (if arg (org-buffers-reset-state))
  (org-buffers-list 'refresh))

(defun org-buffers-list:by (&optional prop)
  "Group buffers according to value of property PROP."
  (interactive)
  (unless (org-buffers-state-get :properties)
    (org-buffers-toggle-properties))
  (let ((buffer-read-only nil))
    (org-buffers-set-state
     `((:by .
	    ,(or prop
		 (org-completing-read
		  "Property to group by: "
		  (cons "NONE" (mapcar 'car org-buffers-buffer-properties))))))))
  (org-buffers-list 'refresh))

(defun org-buffers-toggle-properties ()
  "Toggle entry properties in org-buffers listing buffer.
Removing properties may provide a less cluttered appearance for
browsing. However, in-buffer properties will be restored during
certain operations, such as `org-buffers-list:by'."
  (interactive)
  (if (org-buffers-state-get :properties)
      (progn (org-buffers-delete-properties)
	     (show-all)
	     (org-buffers-set-state '((:properties . nil))))
    (org-buffers-set-state
     '((:atom . heading) (:properties . t)))
    (org-buffers-list 'refresh)))

(defun org-buffers-toggle-headings ()
  "Toggle viewing of buffers as org headings.
Headings will be automatically restored during certain
operations, such as setting deletion tags."
  (interactive)
  (let ((buffer-read-only nil)
	(headings-p (org-buffers-state-eq :atom 'heading))
	(flat-p (org-buffers-state-eq :by "NONE")))
    (if (and headings-p (org-buffers-state-get :properties))
	(org-buffers-toggle-properties))
    (save-excursion
      (goto-char (point-min))
      (if (and (or headings-p (not flat-p))
	       (not (outline-on-heading-p)))
	  (outline-next-heading))
      (if flat-p
	  (progn 
	    (push-mark (point) 'nomsg 'activate)
	    (end-of-buffer)
	    (org-ctrl-c-star)
	    (pop-mark))
	(while (not (eobp))
	  (push-mark
	   (save-excursion (forward-line 1) (point)) 'nomsg 'activate)
	  (org-forward-same-level 1)	
	  (org-ctrl-c-star)
	  (pop-mark)))
      (mark-whole-buffer)
      (indent-region (point-min) (point-max)))
    (org-buffers-set-state
     `((:atom . ,(if headings-p 'line 'heading))))))

(defun org-buffers-delete-properties ()
  (let ((buffer-read-only nil))
    (save-excursion
      (goto-char (point-min))
      (org-buffers-delete-regions
       (nreverse
	(org-buffers-map-entries 'org-buffers-get-property-block))))))

(defun org-buffers-get-property-block ()
  "Return the (beg . end) range of the property drawer.
Unlike the org version the limits include the keywords delimiting
the drawer."
  (let ((beg (point))
	(end (progn (outline-next-heading) (point))))
    (goto-char beg)
    (if (re-search-forward org-property-drawer-re end t)
	(cons (match-beginning 1) (match-end 0)))))

(defun org-buffers-group-by (property)
  "Group top level headings according to the value of PROPERTY."
  (let ((atom (org-buffers-state-get :atom)))
    (save-excursion
      (goto-char (point-min))
      (mapc (lambda (subtree) ;; Create subtree for each value of `property'
	      (org-insert-heading t)
	      (if (> (org-buffers-outline-level) 1)
		  (org-promote))
	      (insert (car subtree) "\n")
	      (org-insert-subheading t)
	      (mapc 'org-buffers-insert-parsed-entry (cdr subtree)))
	    (prog1
		(mapcar (lambda (val) ;; Form list of parsed entries for each unique value of `property'
			  (cons val (org-buffers-parse-selected-entries property val)))
			(sort
			 (delete-dups (org-buffers-map-entries (lambda () (org-entry-get nil property nil))))
			 'string<))
	      (erase-buffer))))))

(defun org-buffers-exclude-p (buffer)
  "Return non-nil if BUFFER should not be listed."
  (or (member (with-current-buffer buffer major-mode)
	      org-buffers-excluded-modes)
      (member buffer org-buffers-excluded-buffers)
      (string= (substring buffer 0 1) " ")))

(defun org-buffers-reset-state ()
  (org-buffers-set-state
   '((:by . "major-mode") (:atom . heading) (:properties . nil))))

(defun org-buffers-columns-view ()
  "View buffers in Org-mode columns view.
This is currently experimental. RET can be used to follow links
in the first column, but certain other org-buffers keys conflict
with column-view or otherwise do not work correctly."
  (interactive)
  (unless (org-buffers-state-eq :by "NONE")
    (org-buffers-list:by "NONE"))
  (let ((buffer-read-only nil))
    (mark-whole-buffer)
    (org-columns)))

;;; Parsing and inserting entries
(defun org-buffers-parse-selected-entries (prop val)
  "Parse all entries with property PROP value VAL."
  (delq nil
	(org-buffers-map-entries
	 (lambda () (when (equal (org-entry-get nil prop) val)
		      (cons (org-get-heading) (org-get-entry)))))))

(defun org-buffers-insert-parsed-entry (entry)
  "Insert a parsed entry"
  (unless (org-at-heading-p) (org-insert-heading))
  (insert (car entry) "\n")
  (if (org-buffers-state-get :properties)
      (insert (cdr entry))))

(defun org-buffers-get-buffer-props (buffer)
  "Create alist of properties of BUFFER, as strings."
  (with-current-buffer buffer
    (mapcar 
     (lambda (pair) (cons (car pair) (eval (cdr pair))))
     org-buffers-buffer-properties)))

;;; Follow-link behaviour

(defun org-buffers-follow-link ()
  "Follow link to buffer on this line.
The buffer-switching behaviour of this function is determined by
the variable `org-buffers-follow-link-method'. See also
`org-buffers-switch-to-buffer' and
`org-buffers-switch-to-buffer-other-window', whose behaviour is
hard-wired."
  (interactive)
  (org-buffers-switch-to-buffer-generic org-buffers-follow-link-method))

(defun org-buffers-switch-to-buffer ()
"Switch to this entry's buffer in current window."
  (interactive)
  (org-buffers-switch-to-buffer-generic 'current-window))

(defun org-buffers-switch-to-buffer-other-window ()
  "Switch to this entry's buffer in other window."
  (interactive)
  (org-buffers-switch-to-buffer-generic 'other-window))

(defun org-buffers-switch-to-buffer-generic (method)
  (save-excursion
    (let ((atom (org-buffers-state-get :atom)) buffer)
      (cond
       ((eq atom 'heading) (org-back-to-heading))
       (t (beginning-of-line)))
      (setq buffer (org-buffers-get-buffer-name))
      (if (get-buffer buffer)
	  (case method
	    ('org-open-at-point (org-open-at-point))
	    ('current-window (switch-to-buffer buffer))
	    ('other-window (switch-to-buffer-other-window buffer)))
	(error "No such buffer: %s" buffer)))))

(defun org-buffers-get-buffer-name ()
  "Get buffer-name for current entry."
  (let ((headings-p (org-buffers-state-eq :atom 'heading)))
    (or (and headings-p (org-entry-get nil "buffer-name"))
	(and (save-excursion
	       (if headings-p (org-back-to-heading))
	       (re-search-forward "\\[\\[buffer:\\([^\]]*\\)" (point-at-eol) t))
	     (org-link-unescape (match-string 1))))))

;;; Setting tags and executing operations

(defun org-buffers-tag-for-deletion ()
  "Mark buffer for deletion.
If a region is selected, all buffers in the region are marked for
deletion. Buffers marked for deletion can be deleted using
`org-buffers-execute-pending-operations'."
  (interactive)
  (org-buffers-set-tags '("delete")))

(defun org-buffers-remove-tags ()
  "Remove deletion marks from buffers.
If a region is selected, marks are removed from all buffers in
the region."
  (interactive)
  (org-buffers-set-tags nil))

(defun org-buffers-set-tags (data)
  "Set tags to DATA at all non top-level headings in region.
DATA should be a list of strings. If DATA is nil, remove all tags
at such headings."
  (let* ((buffer-read-only nil)
	 (region-p (org-region-active-p))
	 (beg (if region-p (region-beginning) (point)))
	 (end (if region-p (region-end) (point))) beg-line end-line)
    (save-excursion
      (setq beg-line (progn (goto-char beg) (org-current-line))
	    end-line (progn (goto-char end) (org-current-line)))
      (if (org-buffers-state-eq :atom 'heading)
	  (setq
	   end (if (and region-p (not (eq end-line beg-line)) (not (eobp)))
		   (progn (goto-char end) (org-back-to-heading) (point))
		 (progn (outline-end-of-heading) (point)))
	   beg (progn (goto-char beg) (point-at-bol)))
	(org-buffers-toggle-headings) ;; doesn't alter line numbers
	(setq beg (progn (org-goto-line beg-line) (point-at-bol))
	      end (if (eq end-line beg-line) (point-at-eol)
		    (progn (org-goto-line end-line) (point-at-bol)))))
      (narrow-to-region beg end)
      (goto-char (point-min))
      (org-buffers-map-entries
       (lambda ()
	 (when (or (org-buffers-state-eq :by "NONE")
		   (> (org-outline-level) 1))
	   (org-set-tags-to
	    (if data (delete-duplicates (append data (org-get-tags)) :test 'string-equal))))))
      (widen)
      (org-content))
    (unless region-p
      (outline-next-heading)
      (unless (or (> (org-outline-level) 1) (org-buffers-state-eq :by "NONE"))
	(outline-next-heading)))))

(defun org-buffers-execute-pending-operations ()
  "Execute all pending operations.
Currently the only type of operation supported is
deletion. Buffers are tagged for deletion using
`org-buffers-tag-for-deletion'. Remove such tags from buffers
using `org-buffers-remove-tags'."
  (interactive)
  (if (org-buffers-state-eq :atom 'line) (org-buffers-toggle-headings))
  (let ((buffer-read-only nil) buffer)
    (org-buffers-delete-regions
     (nreverse
      (org-buffers-map-entries
       (lambda ()
	 (if (setq buffer (org-buffers-get-buffer-name))
	     (if (not (kill-buffer buffer))
		 (error "Failed to kill buffer %s" buffer)
	       (if (and (org-first-sibling-p)
			(not (save-excursion (org-goto-sibling))))
		   (org-up-heading-safe)) ;; Only child so delete parent also
	       (cons (point) (1+ (org-end-of-subtree))))))
       "+delete")))))

;;; Utilities

(defun org-buffers-map-entries (func &optional match)
  (org-scan-tags
   func (if match (cdr (org-make-tags-matcher match)) t)))
  
(defun org-buffers-set-state (state)
  "Add STATE to global state list.
New settings have precedence over existing ones."
  (mapc
   (lambda (pair) (unless (assoc (car pair) state)
		    (add-to-list 'state pair)))
   org-buffers-state)
  (setq org-buffers-state state))

(defmacro org-buffers-delete-regions (regions)
  "Delete regions in list.
REGIONS is a list of (beg . end) cons cells specifying buffer
regions."
  `(mapc (lambda (pair) (if pair (delete-region (car pair) (cdr pair))))
	 ,regions))

(defmacro org-buffers-state-get (key)
  `(cdr (assoc ,key org-buffers-state)))

(defmacro org-buffers-state-eq (key val)
  `(equal (org-buffers-state-get ,key) ,val))

(defmacro org-buffers-outline-level ()
  '(save-excursion (beginning-of-line) (org-outline-level)))

;;; Links to buffers

(org-add-link-type "buffer" 'display-buffer)
(add-hook 'org-store-link-functions 'org-buffers-store-link)

(defun org-buffers-store-link (&optional force)
  "Store a link to an Emacs buffer.
Returns nil by default, to avoid hijacking other link types."
  (if force
      (let* ((target (buffer-name))
	     (desc target) link)
	(org-store-link-props :type "buffer")
	(setq link (org-make-link "buffer:" target))
	(org-add-link-props :link link :description desc)
	link)))

(provide 'org-buffers)
;;; org-buffers.el ends here
