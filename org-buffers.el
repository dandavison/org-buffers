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
(define-key org-buffers-mode-map "d" 'org-buffers-mark-for-deletion)
(define-key org-buffers-mode-map "g" 'org-buffers-list:refresh)
(define-key org-buffers-mode-map "." 'org-buffers-switch-to-buffer)
(define-key org-buffers-mode-map "," 'org-buffers-cycle-presentation)
(define-key org-buffers-mode-map "o" 'org-buffers-switch-to-buffer-other-window)
(define-key org-buffers-mode-map "p" 'org-buffers-toggle-properties)
(define-key org-buffers-mode-map "u" 'org-buffers-remove-marks)
(define-key org-buffers-mode-map "x" 'org-buffers-execute-pending-operations)
(define-key org-buffers-mode-map "?" 'org-buffers-help)

(defvar org-buffers-mode-hook
  '(org-buffers-chomp-mode-from-modes)
  "Hook for functions to be called after buffer listing is
  created. Note that the buffer is read-only, so if the hook
  function is to modify the buffer it use a let binding to
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
    (let ((by (downcase (or (org-buffers-state-get :by) "major-mode")))
	  (atom (org-buffers-state-get :atom)) target)
      (if (org-before-first-heading-p) (outline-next-heading))
      (if (equal (buffer-name) org-buffers-buffer-name)
	  (setq target (org-make-org-heading-search-string)))
      (with-current-buffer (get-buffer-create org-buffers-buffer-name)
	(setq buffer-read-only nil)
	(erase-buffer)
	(org-mode)
	(mapc 'org-buffers-insert-entry
	      (sort (remove-if 'org-buffers-exclude-p
			       (mapcar 'buffer-name (buffer-list frame))) 'string<))
	(org-buffers-set-state '((:atom . heading)))
	(goto-char (point-min))
	(unless (equal by "none") (org-buffers-group-by by))
	(if target (condition-case nil (org-link-search target)))
	(beginning-of-line)
	(if (equal by "none")
	    (org-overview)
	  (case atom
	    ('heading (progn (org-overview) (org-content)))
	    ('line (progn (show-all) (org-buffers-cycle-presentation)))))
	(save-excursion
	  (mark-whole-buffer)
	  (indent-region (point-min) (point-max)))
	(org-buffers-mode)
	(setq buffer-read-only t)
	(current-buffer))))))

(defun org-buffers-list:refresh ()
  (interactive)
  (org-buffers-list 'refresh))

(defun org-buffers-list:by ()
  (interactive)
  (unless (org-buffers-state-get :properties)
    (org-buffers-toggle-properties))
  (let ((buffer-read-only nil))
    (org-buffers-set-state
     `((:by .
	    ,(org-completing-read
	      "Property to group by: "
	      (cons "NONE"
		    (set-difference
		     (delete-dups
		      (apply
		       'append
		       (org-buffers-map-entries (lambda () (mapcar 'car (org-entry-properties))))))
		     '("BLOCKED" "CATEGORY") :test 'string-equal)))))))
  (org-buffers-list 'refresh))

(defun org-buffers-toggle-properties ()
  (interactive)
  (if (org-buffers-state-get :properties)
      (progn (org-buffers-delete-properties)
	     (show-all)
	     (org-buffers-set-state '((:properties . nil))))
    (org-buffers-set-state
     '((:atom . heading) (:properties . t)))
    (let ((target (org-make-org-heading-search-string)))
      (org-buffers-list 'refresh)
      (org-link-search target)
      (beginning-of-line))))

(defun org-buffers-cycle-presentation ()
  (interactive)
  (let ((buffer-read-only nil))
    (if (and (org-buffers-state-eq :atom 'heading)
	     (org-buffers-state-get :properties))
	(org-buffers-toggle-properties))
    (save-excursion
      (goto-char (point-min))
      (unless (outline-on-heading-p)
	(outline-next-heading))
      (while (not (eobp))
	(push-mark
	 (save-excursion (forward-line 1) (point)) 'nomsg 'activate)
	(org-forward-same-level 1)
	(org-ctrl-c-star)
	(pop-mark))
      (mark-whole-buffer)
      (indent-region (point-min) (point-max))))
  (org-buffers-set-state
   `((:atom . ,(case (org-buffers-state-get :atom)
		 ('line 'heading)
		 ('heading 'line))))))

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

;;; Parsing and inserting entries
(defun org-buffers-parse-selected-entries (prop val)
  "Parse all entries with property PROP value VAL."
  (delq nil
	(org-buffers-map-entries
	 (lambda () (when (equal (org-entry-get nil prop) val)
		      (org-buffers-parse-entry))))))

(defun org-buffers-parse-entry ()
  "Parse a single entry"
  (cons (org-get-heading)
	(org-get-entry)))

(defun org-buffers-insert-parsed-entry (entry)
  "Insert a parsed entry"
  (unless (org-at-heading-p) (org-insert-heading))
  (insert (car entry) "\n")
  (if (org-buffers-state-get :properties)
      (insert (cdr entry))))

(defun org-buffers-insert-entry (buffer)
  "Create an entry for BUFFER.
The heading is a link to the buffer."
  (org-insert-heading t)
  (insert
   (org-make-link-string (concat "buffer:" buffer) buffer) "\n")
  (mapc (lambda (pair) (org-set-property (car pair) (format "%s" (cdr pair))))
	(org-buffers-get-buffer-props buffer)))

(defun org-buffers-get-buffer-props (buffer)
  (with-current-buffer buffer
    `(("buffer-name" . ,buffer)
      ("major-mode" . ,major-mode)
      ("buffer-file-name" . ,(buffer-file-name))
      ("default-directory" . ,default-directory)
      ("buffer-modified-p" . ,(buffer-modified-p)))))

;;; Follow-link behaviour

(defun org-buffers-follow-link ()
  "Follow link to buffer on this line.
The buffer-switching behaviour of this function is determined by
`org-buffers-follow-link-method'. See also
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
      (if (setq buffer (org-buffers-get-buffer-name))
	  (case method
	    ('org-open-at-point (org-open-at-point))
	    ('current-window (switch-to-buffer buffer))
	    ('other-window (switch-to-buffer-other-window buffer)))))))

(defun org-buffers-get-buffer-name ()
  "Get buffer-name for current entry."
  (or (org-entry-get nil "buffer-name")
      (and (save-excursion
	     (org-back-to-heading)
	     (re-search-forward "\\[\\[buffer:\\([^\]]*\\)" (point-at-eol) t))
	   (org-link-unescape (match-string 1)))))

;;; Setting tags and executing operations

(defun org-buffers-mark-for-deletion ()
  (interactive)
  (org-buffers-set-tags '("delete")))

(defun org-buffers-remove-marks ()
  (interactive)
  (org-buffers-set-tags nil))

(defun org-buffers-set-tags (data)
  "Set tags to DATA at all non top-level headings in region.
DATA should be a list of strings. If DATA is nil, remove all tags
at such headings."
  (let* ((buffer-read-only nil)
	(region-p (org-region-active-p))
	(beg (if region-p (region-beginning) (point)))
	(end (if region-p (region-end) (point)))
	(atom (org-buffers-state-eq :atom 'heading)) beg-line end-line)
    (save-excursion
      (if (eq atom 'heading)
	  (setq beg (progn (org-goto-char beg) (point-at-bol))
		end (progn (org-goto-char end) (org-end-of-subtree) (point)))
	(setq beg-line (progn (goto-char beg) (org-current-line))
	      end-line (progn (goto-char end) (org-current-line)))
	(while (not (org-buffers-state-eq :atom 'heading))
	  (org-buffers-cycle-presentation))
	;; Headings have no contents after cycling, so eol equals e-o-subtree
	(setq beg (progn (org-goto-line beg-line) (point-at-bol))
	      end (progn (org-goto-line end-line) (point-at-eol))))
      (narrow-to-region beg end)
      (goto-char (point-min))
      (org-buffers-map-entries
       (lambda ()
	 (when (or (org-buffers-state-eq :by "none")
		   (> (org-outline-level) 1))
	   (org-set-tags-to
	    (if data (delete-duplicates (append data (org-get-tags)) :test 'string-equal))))))
      (widen)
      (org-content))))

(defun org-buffers-execute-pending-operations ()
  (interactive)
  (unless (org-buffers-state-eq :atom 'heading)
    (error "Cannot operate on non-headings: use \"l\" to toggle view"))
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

;;; Default hook functions
(defun org-buffers-chomp-mode-from-modes ()
  (if (org-buffers-state-eq :by "major-mode")
      (let ((buffer-read-only nil))
	(org-buffers-map-entries
	 (lambda ()
	   (if (and (eq (org-outline-level) 1)
		    (re-search-forward "-mode$" (point-at-eol) t))
	       (replace-match "")))))))


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
