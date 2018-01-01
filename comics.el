;;; comics.el --- querying the comics movie database
;; Copyright (C) 2017 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: movies

;; This file is not part of GNU Emacs.

;; comics.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; comics.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(defun comics-gather-data (publisher times)
  (let* ((url "https://www.comics.org/search/advanced/process/?ind_pub_notes=&rating=&pages_uncertain=&letters=&brand_group=&series=&binding=&feature=&issue_notes=&synopsis=&colors=&keywords=&isbn=&tracking_notes=&job_number=&issues=&paper_stock=&issue_reprinted=&dimensions=&title=&is_comics=&series_notes=&indicia_publisher=&pub_name=%s&is_indexed=&reprint_notes=&start_date=&pub_notes=&inks=&issue_title=&end_date=&variant_name=&brand_notes=&price=&barcode=&issue_date=&volume=&brand_emblem=&pages=&characters=&genre=&issue_pages=&order2=series&order3=&color=&order1=date&pencils=&target=series&publishing_format=&story_editing=&notes=&is_surrogate=&issue_count=&issue_pages_uncertain=&method=icontains&script=&issue_editing=&logic=False&is_variant=&series_year_began=&indicia_frequency=&story_reprinted=&page=%d")
	 (data
	  (loop with dom
		for page from 1 upto times
		append
		(with-current-buffer (url-retrieve-synchronously (format url publisher page))
		  (goto-char (point-min))
		  (setq dom (libxml-parse-html-region (point) (point-max)))
		  (loop for line in (cdr (dom-by-tag (dom-by-tag dom 'table) 'tr))
			for tds = (dom-non-text-children line)
			collect (list :publisher (string-trim (dom-texts (nth 1 tds)))
				      :title (string-trim (dom-texts (nth 2 tds)))
				      :url (dom-attr (dom-by-tag (nth 2 tds) 'a) 'href)
				      :year (string-trim (dom-texts (nth 3 tds)))
				      :issues (string-trim (dom-texts (nth 4 tds)))
				      :date (string-trim (dom-texts (nth 6 tds)))))))))
    (with-temp-buffer
      (pp data (current-buffer))
      (write-region (point-min) (point-max)
		    (comics-file publisher)))))

(defun comics-file (publisher)
  (format "~/.emacs.d/%s.data" publisher))

(defun comics (publisher)
  (switch-to-buffer "*comics*")
  (comics-mode)
  (let ((inhibit-read-only t)
	(data (with-temp-buffer
		(insert-file-contents (comics-file publisher))
		(read (current-buffer)))))
    (erase-buffer)
    (dolist (elem data)
      (comics-line elem))
    (goto-char (point-min))
    (setq comics-data data
	  comics-publisher publisher)
    nil))

(defun comics-line (elem)
  (insert (propertize
	   (format "%s %3d %-10s %-15s %s\n"
		   (comics-date elem)
		   (comics-issues elem)
		   (comics-limit (getf elem :missing "*") 10)
		   (comics-limit (getf elem :publisher) 15)
		   (getf elem :title))
	   'data elem
	   'face `(:foreground
		   ,(let ((missing (getf elem :missing "")))
		      (cond
		       ((string-match "=" missing)
			"orange")
		       ((equal missing "+")
			"#006000")
		       ((equal missing "-")
			"#00b000")
		       ((equal missing "")
			"#ff0000")
		       (t
			"#c00000")))))))

(defun comics-limit (string length)
  (unless string
    (setq string ""))
  (if (> (length string) length)
      (substring string 0 length)
    string))

(defvar comics-english-months
  '("january" "february" "march" "april" "may" "june" "july"
    "august" "september" "october" "november" "december"))

(defun comics-date (elem)
  (let* ((year (getf elem :year))
	 (date (getf elem :date))
	 (month (car (split-string date)))
	 (nmonth (position (replace-regexp-in-string "[^a-zA-Z]" "" month)
			   comics-english-months
			   :test 'equalp)))
    (if nmonth
	(format "%04s-%02d" year (1+ nmonth))
      (format "%04s-  " year))))		    

(defun comics-issues (elem)
  (string-to-number (car (split-string (getf elem :issues)))))

(defvar comics-mode-map
  (let ((map (make-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "s" 'comics-sort)
    (define-key map "w" 'comics-save-title)
    (define-key map "m" 'comics-edit-missing)
    (define-key map "\r" 'comics-visit)
    (define-key map "=" 'comics-count)
    map))

(define-derived-mode comics-mode special-mode "Comics"
  "Major mode for creating comics images.

\\{comics-mode-map}"
  (setq buffer-read-only t
	truncate-lines t)
  (setq-local comics-sort t)
  (setq-local comics-data nil)
  (setq-local comics-publisher nil))

(defun comics-sort ()
  (interactive)
  (let ((inhibit-read-only t))
    (goto-char (point-min))
    (sort-subr nil (lambda ()
		     )
	       (lambda ()
		 (forward-line 1))
	       nil nil
	       (lambda (k1 k2)
		 (let ((e1 (get-text-property (car k1) 'data))
		       (e2 (get-text-property (car k2) 'data)))
		   (if comics-sort
		       (string< (comics-canon (getf e1 :title)) (comics-canon (getf e2 :title)))
		     (string< (comics-date e1) (comics-date e2)))))))
  (setq comics-sort (not comics-sort)))

(defun comics-canon (string)
  (replace-regexp-in-string "^\\(A \\|The \\)" "" string))

(defun comics-save-title ()
  (interactive)
  (let ((elem (get-text-property (point) 'data)))
    (with-temp-buffer
      (insert (comics-canon (getf elem :title)))
      (copy-region-as-kill (point-min) (point-max))
      (message "Copied '%s'" (buffer-string)))))

(defun comics-edit-missing ()
  (interactive)
  (let* ((elem (get-text-property (point) 'data))
	 (missing (read-from-minibuffer (format "Missing issues for %s: " (getf elem :title))
					(getf elem :missing "")))
	 (inhibit-read-only t))
    (plist-put elem :missing missing)
    (comics-save)
    (delete-region (line-beginning-position)
		   (line-beginning-position 2))
    (comics-line elem)))

(defun comics-save ()
  (let ((data comics-data)
	(publisher comics-publisher))
    (with-temp-buffer
      (pp data (current-buffer))
      (write-region (point-min) (point-max)
		    (comics-file publisher)))))

(defun comics-visit ()
  (interactive)
  (let ((elem (get-text-property (point) 'data)))
    (browse-url (format "http://comics.org%s" (getf elem :url)))))

(defun comics-count ()
  (interactive)
  (let ((hidden 0)
	(pending 0)
	(missings 0)
	(ok 0))
    (dolist (elem comics-data)
      (let ((missing (getf elem :missing "")))
	(cond
	 ((string-match "=" missing)
	  (incf pending))
	 ((equal missing "+")
	  (incf hidden))
	 ((equal missing "-")
	  (incf ok))
	 ((equal missing "")
	  (incf missings))
	 (t
	  (incf missings)))))
    (message "%d pending, %d hidden, %d missing, %d ok, %d total"
	     pending hidden missings ok
	     (+ pending missings ok))))

(provide 'comics)

;;; comics.el ends here
