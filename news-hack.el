;;; news-hack.el --- create newsrc buffers for foreign groups
;; This file is not part of GNU Emacs
;; This is released under the GNU Public License

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this GNU Emacs; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; This is poorly documented, and is likely to stay that way for the
;; forseeable future.  Any help appreciated.
;; Mail me for help, with comments etc.  I like getting mail (that is
;; actually to me, spam I can't stand.)

;; Instructions i)  Run netscape-news-create-newsrc-buffers
;;              ii) Save the buffers

;; This uses *a lot* of recursive functions.  If you get errors, you may need
;; to increase max-lisp-eval-depth to something higher.
;; Put
;; (setq max-lisp-eval-depth 10000)
;; in your .emacs file.  (Replace 10000 with some larger number if necessary)

;; "Author": gowen@gwowen.freeserve.co.uk
;;  URL    : http://www.geocities.com/drgazowen/lisp/

;;; Bugs
;;
;; If your local server is called `dribble' it won't work, since
;; `.newsrc-dribble' is a special buffer in gnus.
;;

(defvar netscape-news-nuke-buffers t
  "Should netscape-news-create-newsrc-buffers overwrite existing buffers?")

(defun netscape-news-parse (newsrc-tmp)
  "Do all that jazz."
  (if newsrc-tmp
      (progn
	(let (newsrc-list-element)
	  (setq newsrc-list-element (car newsrc-tmp))
	  (let (select-method)
	    (progn
	      (setq
	       select-method (car (cdr (cdr (cdr (cdr newsrc-list-element))))))
	      (if select-method
		  (netscape-news-write-buffer
		   newsrc-list-element select-method))
	      (netscape-news-parse (cdr newsrc-tmp))))))))

(defun netscape-news-write-buffer (list-element method)
  "Write newsrc style entries into a suitably named buffer."
  (let ((name (concat ".newsrc-"
		      (if (listp method) (car (cdr method))
			method))))
    (get-buffer-create name)
    (set-buffer name)
    (insert-string
     (concat (netscape-news-get-group-name (car list-element)) ": "
	     (netscape-news-get-read-string (car (cdr (cdr list-element))))))))

(defun netscape-news-get-group-name (string)
  "Return newsgroup name from a previously found foreign group name."
  (string-match "[^:]*$" string)
  (match-string 0 string))

(defun netscape-news-get-read-string (read-list)
  "Convert the read list into a string for newsrc-files."
  (let (read-string)
    (while read-list
      (if (listp (car read-list))
	  (setq read-string
		(concat read-string
			(int-to-string (car (car read-list))) "-"
			(int-to-string (cdr (car read-list))) ","))
	;; else
	(setq read-string (concat read-string (int-to-string (car read-list)) ",")))
      (setq read-list (cdr read-list)))
    (concat read-string "\n")))

(defun netscape-news-create-newsrc-buffers ()
  "Parse the variable gnus-newsrc-alist and create buffers with .newsrc files."
  (interactive)
  (netscape-news-nuke-newsrc-buffers)
  (netscape-news-parse gnus-newsrc-alist))

(defun netscape-news-nuke-newsrc-buffers ()
  "Delete the present contents of all .newsrc buffers"
  (let ((buffers (buffer-list)))
    (while buffers
      (if (and (string-match ".newsrc-"(buffer-name (car buffers)))
	       (not (string-match ".newsrc-dribble"
				  (buffer-name (car buffers)))))
	  (netscape-news-nuke-this-buffer (car buffers)))
      (setq buffers (cdr buffers)))))

(defun netscape-news-nuke-this-buffer (buf)
  (set-buffer buf) (widen) (delete-region (point-min) (point-max)))

(provide 'news-hack)
