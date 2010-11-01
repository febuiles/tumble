;;; tumble-mode.el --- some modes for tumble.el

;; Copyright (C) 2010 Johan Persson

;; Authors: Johan Persson <johan.z.persson@gmail.com>
;;          Federico Builes <federico.builes@gmail.com>
;; Created: 1 Nov 2010
;; Version: 0.1
;; Keywords: tumblr tumble tumble.el

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301 USA

;;; Commentary:

;; This file is part of Tumble, an Emacs client for Tumblr. You
;; can find the latest version at http://github.com/febuiles/tumble

;;; Code:

;;; Major mode (Shamelessly ripped from ELPA's package.el. Many thanks!)
(defvar tumble-menu-mode-map nil
  "Local keymap for `tumble-menu-mode' buffers.")

(unless tumble-menu-mode-map
  (setq tumble-menu-mode-map (make-keymap))
  (suppress-keymap tumble-menu-mode-map)
  (define-key tumble-menu-mode-map "q" 'quit-window)
  (define-key tumble-menu-mode-map "n" 'next-line)
  (define-key tumble-menu-mode-map "p" 'previous-line))

(put 'tumble-menu-mode 'mode-class 'special)

(defun tumble-menu-mode ()
  "Major mode for listing posts in a tumblelog.
\\<tumble-menu-mode-map>
\\{tumble-menu-mode-map}"
  (kill-all-local-variables)
  (use-local-map tumble-menu-mode-map)
  (setq major-mode 'tumble-menu-mode)
  (setq mode-name "Tumble Menu")
  (setq truncate-lines t)
  (setq buffer-read-only t))

;;; The various minor modes under the major tumble menu mode

(define-minor-mode tumble-text-draft-menu-mode
  "Minor mode for use under the Tumble Menu major mode."
  nil
  " Draft"
  '(([(meta p)] . tumble-text-draft-edit)))

(define-minor-mode tumble-text-draft-edit-mode
  "Buffer local keybindings for editing tumblelog text drafts."
  nil
  " Tumble Edit"
  '(([(meta p)] . tumble-text-draft-save)))

;; Draft edit
(defun tumble-text-draft-edit ()
  "Creates a buffer called `*Edit draft*' for use in editing
tumblelog text drafts."
  (interactive)
  (with-current-buffer (get-buffer-create "*Edit draft*")
    (setq tumble-selected-draft (tumble-menu-get-post))
    (erase-buffer)
    (tumble-set-buffer-format)
    (tumble-text-draft-edit-mode)
    (insert (tumble-body-of-post tumble-selected-draft))
    (beginning-of-buffer)
    (pop-to-buffer (current-buffer))))

(defun tumble-text-draft-save ()
  "Save or publish changes of a text draft to tumblelog. Kills `*Edit draft*'"
  (interactive)
  (if (null tumble-selected-draft)
      (message "No selected post")
    (prog1
        (tumble-http-post
         (list
          (cons 'title
                (tumble-get-title-for-post)
          (cons 'state
                (tumble-state-from-partial-string
                 (read-string "State (published or draft): ")))
          (cons 'body
                (buffer-substring-no-properties (point-min) (point-max)))
          (cons 'post-id
                (tumble-id-of-post tumble-selected-draft))))
        (setq tumble-selected-draft nil)
        (kill-buffer "*Edit draft*")))))

;; Displaying list of posts
(defun tumble-list-print-post (num title)
  "Insert ITEM in the position NUM of the list of editable posts."
  (insert (propertize (number-to-string num) 'font-lock-face 'default))
  (indent-to 3 1)
  (insert (propertize title 'font-lock-face 'default))
  (insert "\n"))

(defun tumble-list-posts-internal ()
  "Sets up the buffer `*Posts*', fills it with items and returns
the buffer."
  (with-current-buffer (get-buffer-create "*Posts*")
    (setq buffer-read-only nil)
    (erase-buffer)
    (let ((c 0))
      (mapc (lambda (tumble-post)
              (setq c (1+ c))
              (tumble-list-print-post c
                                      (tumble-title-of-post tumble-post)))
            tumble-posts-cache))
    (beginning-of-buffer)
    (current-buffer)))

(defun tumble-list-posts ()
  "Display the list of posts."
  (with-current-buffer (tumble-list-posts-internal)
    (tumble-menu-mode)
    (tumble-text-draft-menu-mode)
    ;; Set up the header line.
    (setq header-line-format
	  (mapconcat
	   (lambda (pair)
	     (let ((column (car pair))
		   (name (cdr pair)))
	       (concat
		;; Insert a space that aligns the button properly.
		(propertize " " 'display (list 'space :align-to column)
			    'face 'fixed-pitch)
                name)))
	   '((0 . "#")
	     (3 . "Title"))
	   ""))
    (pop-to-buffer (current-buffer))))

(defun tumble-set-buffer-format ()
"Activates the text mode according to FORMAT. Accepted values
are \"html\" and \"markdown\". Defaults to `html-mode' for
unknown values."
    (if (string= tumble-format "markdown")
        (markdown-mode)
      (html-mode)))

(provide 'tumble-mode)