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

(defvar comics-data nil)
(defvar comics-sort nil)
(defvar comics-publisher nil)

;; URL for imprint
;; https://www.comics.org/search/advanced/process/?target=series&method=icontains&logic=False&keywords=&title=&feature=&job_number=&pages=&pages_uncertain=&script=&pencils=&inks=&colors=&letters=&story_editing=&first_line=&characters=&synopsis=&reprint_notes=&story_reprinted=&notes=&issues=&volume=&issue_title=&variant_name=&is_variant=&issue_date=&indicia_frequency=&price=&issue_pages=&issue_pages_uncertain=&issue_editing=&isbn=&barcode=&rating=&issue_notes=&issue_reprinted=&is_indexed=&order1=date&order2=date&order3=&start_date=&end_date=&updated_since=&pub_name=marvel&pub_notes=&brand_group=%s&brand_emblem=&brand_notes=&indicia_publisher=&is_surrogate=&ind_pub_notes=&series=&series_year_began=&series_notes=&tracking_notes=&issue_count=&is_comics=&color=&dimensions=&paper_stock=&binding=&publishing_format=&page=%d

(defun comics-gather-data (publisher times)
  (let* ((url "https://www.comics.org/search/advanced/process/?ind_pub_notes=&rating=&pages_uncertain=&letters=&characters=&brand_group=&series=&binding=&indicia_frequency=&issue_notes=&synopsis=&colors=&keywords=&first_line=&tracking_notes=&job_number=&issues=&issue_date=&issue_reprinted=&dimensions=&title=&is_comics=&feature=&indicia_publisher=&pub_name=%s&is_indexed=&reprint_notes=&method=icontains&pub_notes=&inks=&issue_title=&end_date=&variant_name=&series_notes=&price=&barcode=&paper_stock=&volume=&brand_emblem=&pages=&isbn=&issue_pages_uncertain=&issue_pages=&order2=date&order3=&color=&order1=date&pencils=&target=series&publishing_format=&story_editing=&notes=&is_surrogate=&issue_count=&issue_editing=&start_date=&script=&updated_since=&logic=False&is_variant=&series_year_began=&brand_notes=&story_reprinted=&page=%d")
	 (data
	  (cl-loop
	   with dom
	   for page from 1 upto times
	   append
	   (with-current-buffer (url-retrieve-synchronously (format url publisher page))
	     (goto-char (point-min))
	     (setq dom (libxml-parse-html-region (point) (point-max)))
	     (cl-loop for line in (cdr (dom-by-tag (dom-by-tag dom 'table) 'tr))
		      for tds = (dom-non-text-children line)
		      collect
		      (list :publisher (string-trim (dom-texts (nth 0 tds)))
			    :title (string-trim
				    (replace-regexp-in-string
				     "\\[.\\]" ""
				     (dom-texts (nth 1 tds))))
			    :url (dom-attr (dom-by-tag (nth 1 tds) 'a) 'href)
			    :year (string-trim (dom-texts (nth 2 tds)))
			    :issues (string-trim (dom-texts (nth 3 tds)))
			    :date (string-trim (dom-texts (nth 5 tds)))))))))
    (with-temp-buffer
      (pp data (current-buffer))
      (write-region (point-min) (point-max)
		    (comics-file publisher)))))

(defun comics-file (publisher)
  (format "~/.emacs.d/comics/%s.data" publisher))

(defun comics (publisher)
  (switch-to-buffer (format "*%s*" publisher))
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
		   (comics-limit (cl-getf elem :missing "*") 10)
		   (comics-limit (cl-getf elem :publisher) 15)
		   (cl-getf elem :title))
	   'data elem
	   'face `(:foreground
		   ,(let ((missing (cl-getf elem :missing "")))
		      (cond
		       ((string-match "=" missing)
			"orange")
		       ((equal missing "+")
			"#808080")
		       ((equal missing "-")
			"#00b000")
		       ((equal missing "!")
			"#c0c0c0")
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
  (let* ((year (cl-getf elem :year))
	 (date (cl-getf elem :date))
	 (month (car (split-string date)))
	 (nmonth (cl-position (replace-regexp-in-string "[^a-zA-Z]" "" month)
			      comics-english-months
			      :test 'equalp)))
    (if nmonth
	(format "%04s-%02d" year (1+ nmonth))
      (format "%04s   " year))))		    

(defun comics-issues (elem)
  (string-to-number (car (split-string (cl-getf elem :issues)))))

(defvar comics-mode-map
  (let ((map (make-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "s" 'comics-sort)
    (define-key map "Q" 'comics-sort-by-quantity)
    (define-key map "w" 'comics-save-title)
    (define-key map "d" 'comics-edit-date)
    (define-key map "m" 'comics-edit-missing)
    (define-key map "a" 'comics-add)
    (define-key map "n" 'comics-edit-shipping)
    (define-key map "M" 'comics-search-mile-high)
    (define-key map "A" 'comics-search-all)
    (define-key map "S" 'comics-search)
    (define-key map "x" 'comics-edit-got-all)
    (define-key map " " 'comics-toggle-mark)
    (define-key map "\r" 'comics-visit)
    (define-key map "&" 'comics-visit-externally)
    (define-key map "!" 'comics-make-marked-read)
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
  "Toggle sorting by name or date."
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
		       (string< (downcase (comics-canon (cl-getf e1 :title)))
				(downcase (comics-canon (cl-getf e2 :title))))
		     (string< (comics-date e1) (comics-date e2)))))))
  (setq comics-sort (not comics-sort)))

(defun comics-sort-by-quantity (&optional no-have)
  "Sort by quantity.
If NO-HAVE (the prefix), sort the no-haves first."
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
		   (< (string-to-number
			(car (split-string (cl-getf e1 :issues ""))))
		       (string-to-number
			(car (split-string (cl-getf e2 :issues ""))))))))))

(defun comics-canon (string)
  (replace-regexp-in-string "^\\(A \\|The \\)" "" string))

(defun comics-save-title ()
  (interactive)
  (let ((elem (get-text-property (point) 'data)))
    (with-temp-buffer
      (insert (comics-canon (cl-getf elem :title)))
      (copy-region-as-kill (point-min) (point-max))
      (message "Copied '%s'" (buffer-string)))))

(defun comics-edit-missing ()
  (interactive)
  (let* ((elem (get-text-property (point) 'data))
	 (missing (read-from-minibuffer (format "Missing issues for %s: " (cl-getf elem :title))
					(cl-getf elem :missing "")))
	 (inhibit-read-only t))
    (plist-put elem :missing missing)
    (comics-save)
    (delete-region (line-beginning-position)
		   (line-beginning-position 2))
    (comics-line elem)))

(defun comics-add (title year issues url)
  (interactive "sTitle: \nsYear: \nsIssues: \nsURL: ")
  (let ((elem (list :publisher (cl-getf (get-text-property (point) 'data)
				     :publisher)
		    :title title
		    :url url
		    :year year
		    :issues (format "%s issues" issues)
		    :date year
		    :missing "*"))
	(inhibit-read-only t))
    (push elem comics-data)
    (comics-save)
    (beginning-of-line)
    (comics-line elem)))		    

(defun comics-edit-shipping ()
  (interactive)
  (let* ((elem (get-text-property (point) 'data))
	 (total (string-to-number
		 (car (split-string (cl-getf elem :issues "")))))
	 (inhibit-read-only t))
    (plist-put elem :missing
	       (if (= total 1)
		   "1="
		 (format "1-%d=" total)))
    (comics-save)
    (delete-region (line-beginning-position)
		   (line-beginning-position 2))
    (comics-line elem)))

(defun comics-edit-got-all ()
  (interactive)
  (let* ((elem (get-text-property (point) 'data))
	 (total (string-to-number
		 (car (split-string (cl-getf elem :issues "")))))
	 (inhibit-read-only t))
    (plist-put elem :missing "-")
    (comics-save)
    (delete-region (line-beginning-position)
		   (line-beginning-position 2))
    (comics-line elem)))

(defun comics-edit-date ()
  (interactive)
  (let* ((elem (get-text-property (point) 'data))
	 (date (read-from-minibuffer (format "Month for %s: "
						(cl-getf elem :title))
					(cl-getf elem :date "")))
	 (inhibit-read-only t))
    (plist-put elem :date date)
    (plist-put elem :year (car (last (split-string date))))
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
    (browse-url (format "https://comics.org%s" (cl-getf elem :url)))))

(defun comics-visit-externally ()
  (interactive)
  (let ((elem (get-text-property (point) 'data))
	(browse-url-browser-function
	 browse-url-secondary-browser-function))
    (browse-url (format "https://comics.org%s/covers/" (cl-getf elem :url)))))

(defun comics-count ()
  (interactive)
  (let ((hidden 0)
	(pending 0)
	(missings 0)
	(shipping 0)
	(got 0)
	(no-got 0)
	(totes 0)
	(read 0)
	(unread-series 0)
	(ok 0))
    (dolist (elem comics-data)
      (let ((missing (cl-getf elem :missing ""))
	    (total (string-to-number
		    (car (split-string (cl-getf elem :issues ""))))))
	(cl-incf totes total)
	(cond
	 ((string-match "=" missing)
	  (cl-incf pending)
	  (let ((have (comics-parse-shipping missing)))
	    (cl-incf shipping (car have))
	    (cl-incf no-got (cadr have))
	    (cl-incf got (- total (car have) (cadr have)))))
	 ((equal missing "+")
	  (cl-incf read total)
	  (cl-incf hidden))
	 ((equal missing "-")
	  (cl-incf ok)
	  (cl-incf unread-series)
	  (cl-incf got total))
	 ((equal missing "!")
	  (cl-incf read total)
	  (cl-incf ok)
	  (cl-incf got total))
	 ((equal missing "")
	  (cl-incf unread-series)
	  (cl-incf missings)
	  (cl-incf no-got total))
	 (t
	  (let ((no-have (cadr (comics-parse-shipping missing))))
	    (cl-incf got (- total no-have))
	    (cl-incf no-got no-have))
	  (cl-incf unread-series)
	  (cl-incf missings)))))
    (message "%d have, %d shipping, %d no-have, %d read, %d unread, %d total issues,\n%d ok, %d pending, %d hidden, %d missing, %d unread, %d total series"
	     got shipping no-got read (- got read) totes
	     ok pending hidden missings unread-series (+ pending missings ok))))

(defun comics-parse-shipping (missing)
  "Say how many comics are shipping.
The format is \"1, 2=, 4\", \"1-4=\", \"(1, 4, 6-8)=\", where the =
signifies that the number/range/parenthesised collection has been ordered."
  (let ((shipping 0)
	(no-shipping 0))
    (with-temp-buffer
      (insert missing)
      (goto-char (point-min))
      (while (re-search-forward "(\\([^)]+\\))=" nil t)
	(let ((sub (match-string 1)))
	  (replace-match "")
	  (let ((elem (comics-parse-shipping
		       (mapconcat
			(lambda (elem)
			  (concat elem "="))
			(split-string sub "[, ]" t)
			", "))))
	    (cl-incf shipping (car elem))
	    (cl-incf no-shipping (cadr elem)))))
      (dolist (elem (split-string (buffer-string) ", " t))
	(let* ((range (mapcar
		       'string-to-number
		       (split-string (replace-regexp-in-string "=" "" elem)
				     "[- ]")))
	       (count (if (= (length range) 2)
			  (1+ (- (cadr range) (car range)))
			1)))
	  (if (string-match "=$" elem)
	      (cl-incf shipping count)
	    (cl-incf no-shipping count))))
      (list shipping no-shipping))))

(defvar comics-marks nil)

(defun comics-toggle-mark ()
  (interactive)
  (let ((elem (get-text-property (point) 'data))
	(inhibit-read-only t))
    (save-excursion
      (beginning-of-line)
      (forward-char 7)
      (delete-region (point) (1+ (point)))
      (if (memq elem comics-marks)
	  (progn
	    (insert " ")
	    (setq comics-marks (delq elem comics-marks)))
	(insert "*")
	(setq comics-marks (append comics-marks (list elem)))))))

(defun comics-make-marked-read ()
  "Change the missing list to \"!\" on the marked comics."
  (interactive)
  (dolist (elem (or comics-marks
		    (list (get-text-property (point) 'data))))
    (plist-put elem :missing "!")
    (comics-update elem))
  (setq comics-marks nil)
  (forward-line 1)
  (comics-save))

(defun comics-update (elem)
  (let ((current nil))
    (save-excursion
      (goto-char (point-min))
      (while (and (not (eobp))
		  (setq current (get-text-property (point) 'data))
		  (not (and (equal (plist-get current :url)
				   (plist-get elem :url))
			    (equal (plist-get current :title)
				   (plist-get elem :title)))))
	(forward-line 1))
      (when (get-text-property (point) 'data)
	(let ((inhibit-read-only t))
	  (delete-region (line-beginning-position)
			 (line-beginning-position 2))
	  (comics-line elem))))))

(defun comics-search-mile-high ()
  (interactive)
  (let ((elem (get-text-property (point) 'data))
	(browse-url-browser-function
	 browse-url-secondary-browser-function))
    (browse-url
     (format "https://www.milehighcomics.com/mcgi-bin/search.cgi?title=%s"
	     (comics-canon (cl-getf elem :title))))))

(defun comics-search-all ()
  (interactive)
  (let ((elem (get-text-property (point) 'data))
	(browse-url-browser-function
	 browse-url-secondary-browser-function))
    (browse-url
     (format "https://www.milehighcomics.com/mcgi-bin/search.cgi?title=%s"
	     (comics-canon (cl-getf elem :title))))
    (browse-url
     (format "https://www.mycomicshop.com/search?q=%s"
	     (comics-canon (cl-getf elem :title))))
    (browse-url
     (format "https://www.ebay.com/sch/m.html?_ssn=cyberspacecomics&LH_PrefLoc=&_from=R40&_trksid=p2499338.m570.l1313.TR12.TRC2.A0.H0.Xpteranoman.TRS0&_nkw=%s&_sacat=0"
	     (comics-canon (cl-getf elem :title))))))

(defun comics-search (string)
  (interactive "sSearch for: ")
  (let ((browse-url-browser-function
	 browse-url-secondary-browser-function))
    (browse-url
     (format "https://www.milehighcomics.com/mcgi-bin/search.cgi?title=%s"
	     string))
    (browse-url
     (format "https://www.mycomicshop.com/search?q=%s"
	     string))
    (browse-url
     (format "http://www.ebaystores.com/mycomicshop?submit=SEARCH&_nkw=%s&"
	     string))))

(provide 'comics)

;;; comics.el ends here
