;;; org-buffers.el --- Support for using Org-mode to work with Emacs buffers

;; Copyright (C) 2010  Dan Davison

;; Author: Dan Davison <dandavison0 at gmail.com>
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

;;; Links to buffers
(org-add-link-type "buffer" 'display-buffer)
(add-hook 'org-store-link-functions 'org-buffers-store-link)

(defun org-buffers-store-link ()
  "Store a link to an Emacs buffer."
  (let* ((desc (buffer-name))
	 (target desc) link)
    (org-store-link-props :type "buffer")
    (setq link (org-make-link "buffer:" target))
    (org-add-link-props :link link :description desc)
    link))

;;; Buffer list

(defvar org-buffers-mode-map (make-sparse-keymap)
  "The keymap for `org-buffers-mode'.")

(define-key org-buffers-mode-map [(return)] 'org-buffers-follow-link-at-heading)
(define-key org-buffers-mode-map "b" 'org-buffers-list:by)
(define-key org-buffers-mode-map "d" 'org-buffers-mark-for-deletion)
(define-key org-buffers-mode-map "f" 'org-buffers-list:flat)
(define-key org-buffers-mode-map "g" '(lambda () (interactive) (org-buffers-list 'refresh)))
(define-key org-buffers-mode-map "l" 'org-buffers-list:as-lists)
(define-key org-buffers-mode-map "p" 'org-buffers-list:with-properties)
(define-key org-buffers-mode-map "x" 'org-buffers-execute-pending-operations)

(define-minor-mode org-buffers-mode
  "Org-mode support for buffer management"
  nil " buffer-list" nil)

(defvar org-buffers-buffer-name
  "*Buffers*"
  "Name of buffer in which buffer list is displayed")

(defvar org-buffers-mode-hook
  '(org-buffers-chomp-mode-from-modes)
  "Hook for functions to be called after buffer listing is
  created.")

(defvar org-buffers-params
  '((:by . "major-mode") (:atoms . headings) (:with-props . nil))
  "Alist of parameters controlling org-buffers-list output.")

(defcustom org-buffers-excluded-buffers
  `("*Completions*" ,org-buffers-buffer-name)
  "List of names of buffers (strings) that should not be listed
  by org-buffers-list."
  :group 'org-buffers)

(defun org-buffers-list (&optional refresh property frame)
  "Create an Org-mode listing of Emacs buffers.
Buffers are grouped into one subtree for each major
mode. Optional argument `property' specifies a different property
to group be. Optional argument `frame' specifies the frame whose
buffers should be listed."
  (interactive)
  (pop-to-buffer
   (or
    (and (not refresh) (get-buffer org-buffers-buffer-name))
    (let ((p (if (equal (buffer-name) org-buffers-buffer-name)
		 (point))) ;; TODO how to check for current minor modes?
	  (by (or (cdr (assoc :by org-buffers-params)) "major-mode"))
	  (atoms (cdr (assoc :atoms org-buffers-params))))
      (with-current-buffer (get-buffer-create org-buffers-buffer-name)
	(setq buffer-read-only nil)
	(erase-buffer)
	(org-mode)
	(mapc 'org-buffers-insert-entry
	      (remove-if 'org-buffers-exclude-p (buffer-list frame)))
	(goto-char (point-min))
	(unless (equal by "none") (org-buffers-group-by by atoms))
	(org-sort-entries-or-items nil ?a)
	(org-overview)
	(cond
	 ((equal atoms 'headings) (org-content))
	 ((equal atoms 'items) (show-all)))
	(when p (goto-char p) (beginning-of-line)) ;; TODO try searching for stored entry before goto-char
	(org-buffers-mode)
	(setq buffer-read-only t)
	(current-buffer))))))

(defun org-buffers-list:flat ()
  (interactive)
  (setq org-buffers-params '((:by . "none")))
  (org-buffers-list))

(defun org-buffers-list:by ()
  (interactive)
  (setq org-buffers-params '((:by . "major-mode")))
  (org-buffers-list))

(defun org-buffers-list:as-lists ()
  (interactive)
  (org-buffers-set-params '((:atoms . items)))
  (org-buffers-list 'refresh))

(defun org-buffers-list:with-properties ()
  (interactive)
  (org-buffers-set-params '((:atoms . headings) (:properties . t)))
  (org-buffers-list 'refresh))

(defun org-buffers-group-by (property atoms)
  "Group top level headings according to the value of `property'."
  (save-excursion
    (goto-char (point-min))
    (mapc (lambda (subtree) ;; Create subtree for each value of `property'
	    (org-insert-heading t)
	    (if (> (save-excursion (goto-char (point-at-bol)) (org-outline-level)) 1)
	      (org-promote))
	    (insert (car subtree) "\n")
	    (if (eq atoms 'items) (insert " - ") (org-insert-subheading t))
	    (mapc 'org-buffers-insert-parsed-entry (cdr subtree)))
	  (prog1
	      (mapcar (lambda (val) ;; Form list of parsed entries for each unique value of `property'
			(cons val (org-buffers-parse-selected-entries property val)))
		      (delete-dups (org-map-entries (lambda () (org-entry-get nil property nil)))))
	    (erase-buffer)))))

(defun org-buffers-parse-selected-entries (prop val)
  "Parse all entries with `property' value `val'."
  (delq nil
	(org-map-entries
	 (lambda () (when (equal (org-entry-get nil prop) val)
		      (org-buffers-parse-entry))))))

(defun org-buffers-parse-entry ()
  "Parse a single entry"
  (cons (org-get-heading)
	(org-get-entry)))

(defun org-buffers-insert-parsed-entry (entry)
  "Insert a parsed entry"
  (unless (or (org-on-heading-p) (org-at-item-p))
    (org-insert-heading))
  (insert (car entry) "\n")
  (if (cdr (assoc :properties org-buffers-params))
    (insert (cdr entry))))

(defun org-buffers-insert-entry (buffer)
  "Create an entry for `buffer'.
The heading is a link to `buffer'."
  (let ((buffer-name (buffer-name buffer))
	(major-mode (with-current-buffer buffer major-mode))
	(file (buffer-file-name buffer))
	(dir (with-current-buffer buffer default-directory)))
    (org-insert-heading t)
    (insert
     (org-make-link-string (concat "buffer:" buffer-name) buffer-name) "\n")
    (org-set-property "major-mode" (symbol-name major-mode))
    (org-set-property "buffer-file-name" file)
    (org-set-property "buffer-name" buffer-name)
    (org-set-property "default-directory" dir)))

(defun org-buffers-follow-link-at-heading ()
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (if (re-search-forward "\\[\\[buffer:" (point-at-eol) t)
	(org-open-at-point))))

(defun org-buffers-mark-for-deletion ()
  (interactive)
  (let ((buffer-read-only nil))
    (org-toggle-tag "delete"))
  ;; hack: I'm struggling to make new tag be visible
  (org-show-entry)
  (org-back-to-heading)
  (hide-subtree))

(defun org-buffers-execute-pending-operations ()
  (interactive)
  (org-map-entries
   (lambda () (kill-buffer (org-entry-get nil "buffer-name")))
   "+delete")
  (org-buffers-list))


  (unless (cdr (assoc :properties org-buffers-params))
    (org-buffers-list:with-properties))
(defun org-buffers-chomp-mode-from-modes ()
  (if (equal (cdr (assoc :by org-buffers-params)) "major-mode")
      (org-map-entries
       (lambda () (if (re-search-forward "-mode" (point-at-eol) t)
		      (replace-match ""))))))

(provide 'org-buffers)
;;; org-buffers.el ends here
(defun org-buffers-set-params (params)
  "Add settings to global parameter list.
New settings have precedence over existing ones."
  (mapc
   (lambda (pair) (unless (assoc (car pair) params)
		    (add-to-list 'params pair)))
   org-buffers-params)
  (setq org-buffers-params params))

