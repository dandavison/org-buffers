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
(add-hook 'org-store-link-functions 'org-buffers-link-store-link)

(defun org-buffers-link-store-link ()
  "Store a link to an Emacs buffer."
  (let* ((desc (buffer-name))
	 (target desc) link)
    (org-store-link-props :type "buffer")
    (setq link (org-make-link "buffer:" target))
    (org-add-link-props :link link :description desc)
    link))

(defun org-buffers-link-open (buffer)
  "Open a link to an Emacs buffer."
  (error "Not used")
  (display-buffer buffer))

;;; Buffer listing
(defun org-buffers-list-buffers (&optional frame)
  "Create an Org-mode listing of Emacs buffers.
The buffers are grouped by major mode."
  (interactive)
  (pop-to-buffer
   (with-current-buffer (get-buffer-create "*Buffer Tree*")
     (erase-buffer)
     (org-mode)
     (mapc 'org-buffers-insert-entry (buffer-list frame))
     (goto-char (point-min))
     (org-buffers-group-entries-by-property "major-mode")
     (current-buffer))))

(defun org-buffers-group-entries-by-property (property)
  "Group toplevel headings according to the value of `property'."
  ;; Create subtree for each value of `property'
  (mapc (lambda (subtree)
	  (org-insert-heading t)
	  (if (> (org-outline-level) 1) (org-promote))
	  (insert (replace-regexp-in-string "-mode$" "" (car subtree)) "\n")
	  (org-insert-subheading t)
	  (mapc 'org-buffers-insert-parsed-entry (cdr subtree)))
	(prog1
	    ;; Form list of parsed entries for each value of `property'
	    (mapcar (lambda (val)
		      (cons val (org-buffers-get-info-for-entries property val)))
		    ;; Find unique values of `property'
		    (delete-dups (org-map-entries (lambda () (org-entry-get nil property nil)))))
	  (erase-buffer)))
  (goto-char (point-min))
  (org-sort-entries-or-items nil ?a)
  (org-overview)
  (org-content)
  (current-buffer))

(defun org-buffers-get-info-for-entries (prop val)
  "Parse all entries with `property' value `val'."
  (delq nil
	(org-map-entries
	 (lambda () (when (equal (org-entry-get nil prop) val)
		      (org-buffers-parse-entry))))))

(defun org-buffers-parse-entry ()
  "Parse a single entry"
  (cons (org-get-heading)
	(org-get-entry)))

(defun org-buffers-insert-parsed-entry (entry &optional heading-only)
  "Insert a parsed entry"
  (unless (org-on-heading-p)
    (org-insert-heading t))
  (insert (car entry) "\n")
  (unless heading-only
    (insert (cdr entry))))

(defun org-buffers-insert-entry (buffer &optional hidden-ok)
  "Create an entry for `buffer'.
The heading is a link to `buffer'."
  (let ((buffer-name (buffer-name buffer))
	(major-mode (with-current-buffer buffer major-mode)))
    (when (or hidden-ok
	      (not (string= (substring buffer-name 0 1) " ")))
      (org-insert-heading t)
      (insert
       (org-make-link-string (concat "buffer:" buffer-name) buffer-name) "\n")
      (org-set-property "major-mode" (symbol-name major-mode)))))

(provide 'org-buffers)
;;; org-buffers.el ends here
