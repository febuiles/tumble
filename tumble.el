;;; tumble.el --- an Tumblr mode for Emacs

;; Copyright (C) 2008-2010 Federico Builes

;; Authors: Federico Builes <federico.builes@gmail.com>
;;          Johan Persson <johan.z.persson@gmail.com>
;; Contributors:
;; Quildreen Motta <quildreen@gmail.com>
;; Created: 1 Dec 2008
;; Version: 2.0
;; Keywords: tumblr

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

;; Tumble is a mode for interacting with Tumblr inside Emacs. It currently
;; provides the following functions:
;;
;; tumble-text-from-region
;;     Posts the selected region as a "Text".
;; tumble-text-from-buffer
;;     Posts the current buffer as a "Text".

;; tumble-quote-from-region
;;     Posts the current region as a "Quote". Prompts
;;     for an optional "source" parameter.

;; tumble-link
;;     Prompts for a title and a URL for a new "Link".
;; tumble-link-with-description
;;     Prompts for a title and a URL for a new "Link" and
;;     uses the selected region as the link's description.

;; tumble-chat-from-region
;;     Posts the selected region as a "Chat".
;; tumble-chat-from-buffer
;;     Posts the current buffer as a "Chat".

;; tumble-photo-from-url
;;     Prompts for a file URL, a caption and a clickthrough and
;;     posts the result as a "Photo".
;; tumble-photo-from-file
;;     Prompts for a local file, a caption and a clickthrough and
;;     posts the result as a Photo.
;; tumble-audio
;;     Prompts for a local file and an optional caption to
;;     upload a MP3 file.

;; tumble-video-from-url
;;     Prompts for an embed code and an optional caption to post a video
;;     to Tumblr.

;; A word of caution: Audio files can take a while to upload and will
;; probably freeze your Emacs until it finishes uploading.

;; EDITING TEXT DRAFTS

;; As of version 2.0, support has been added for listing, editing, saving
;; and publishing text drafts.

;; The following commands are central to this feature:

;; tumble-list-text-drafts
;;     Creates a buffer called `*Posts*' that list the text drafts on the
;;     blog specified by `tumble-url'. This buffer has the major mode
;;     `tumble-menu-mode', which binds `n' for selecting the next item in
;;     the menu, `p' for the previous item and `q' for closing the window,
;;     as well as the minor mode `tumble-text-draft-menu-mode' which binds
;;     `M-p' to `tumble-text-draft-edit', which opens an edit window for the
;;     selected item.

;; tumble-text-draft-edit
;;     May only be used un conjunction with an active `*Posts*'-buffer.
;;     Creates a buffer called `*Edit draft*' based on where the pointer
;;     is in `*Posts*'. The major mode varies between `html-mode' and
;;     `markdown-mode' depending on the value of `tumble-format'.
;;     The minor mode `tumble-text-draft-edit-mode' binds `M-p' to
;;     `tumble-text-draft-save'.

;; tumble-text-draft-save
;;     Saves or publishes the draft. Will prompt for title (defaults to the
;;     old title if left blank) and state (defaults to "published" if left
;;     blank). If "draft" is selected for state, the draft will be saved,
;;     otherwise it will be published. May only be used if there is an active
;;     `*Edit draft*' buffer present.

;; Note: Make sure that `tumble-url' is formatted as "foo.tumblr.com",
;;       that is without the protocol specifier, lest this feature won't
;;       work.

;; You can always find the latest version of Tumble at: http://github.com/febuiles/tumble

;; Installation:

;;
;; Download Tumble to some directory:
;; $ git clone git://github.com/febuiles/tumble.git
;;
;; Add it to your load list and require it:
;;
;; (add-to-list 'load-path "~/some_directory/tumble")
;; (require 'tumble)
;;
;; Optional:
;;
;; Open tumble.el (this file) and modify the following variables:
;; (setq tumble-email "your_email@something.com")
;; (setq tumble-password "your_password")
;; (setq tumble-url "your_tumblelog.tumblr.com")
;;
;; Tumble uses no group for posting and Markdown as the default
;; format but you can change these:
;; (setq tumble-group "your_group.tumblr.com")
;; (setq tumble-format "html")
;;
;; You can also reset the Tumblr login credentials by calling:
;;
;;     tumble-reset-credentials

;;; Code:
(let* ((tumble-dir (file-name-directory
                    (or (buffer-file-name) load-file-name))))
  (add-to-list 'load-path (concat tumble-dir "/."))
  (add-to-list 'load-path (concat tumble-dir "/vendor")))

(require 'http-post-simple)
(require 'tumble-mode)

;; Personal information
(defvar tumble-email nil "Tumblr user email")
(defvar tumble-password nil "Tumblr user password")
(defvar tumble-url nil "URL of your Tumblelog")

;; Optional information
(defvar tumble-group nil "")
(defvar tumble-format "markdown"
  "Format of your Tumblr posts. You can use \"markdown\" or
\"html\".")

;; Stop editing here.

(defvar tumble-write-api-url "https://www.tumblr.com/api/write"
  "URL for the Tumblr API")

(setq tumble-states (list "published" "draft")) ; supported states
(setq tumble-posts-cache nil)
(setq tumble-selected-draft nil)

;;; Login

(defun tumble-login ()
  "Ask the user for his Tumblr credentials"
  (interactive)
  (setq tumble-email (or tumble-email (read-string "Email: ")))
  (setq tumble-password (or tumble-password (read-passwd "Password: ")))
  (setq tumble-group (or tumble-group (read-string "Group (optional): ")))
  (setq tumble-url (or tumble-url (read-string "URL: "))))

(defun tumble-reset-credentials ()
  "Reset the Tumblr login credentials"
  (interactive)
  (setq tumble-email nil)
  (setq tumble-password nil)
  (setq tumble-url nil)
  (setq tumble-group nil))

;;; User-facing posting functions

;;;###autoload
(defun tumble-text-from-region (min max title state)
  "Post the current region as a text in Tumblr"
  (interactive "r \nsTitle: \nsState (published or draft): ")
  (tumble-post-text title (tumble-region-text)
                    (tumble-state-from-partial-string state)))

;;;###autoload
(defun tumble-text-from-buffer (title state)
  "Post the current buffer as a text in Tumblr"
  (interactive "sTitle: \nsState (published or draft): ")
  (tumble-text-from-region (point-min) (point-max) title
                           (tumble-state-from-partial-string state)))

;;;###autoload
(defun tumble-quote-from-region (min max source state)
  "Post a region as a quote in Tumblr"
  (interactive "r \nsSource (optional): \nsState (published or draft): " )
  (tumble-http-post
   (list (cons 'type "quote")
         (cons 'quote (tumble-region-text))
         (cons 'source source)
         (cons 'state (tumble-state-from-partial-string state)))))

;;;###autoload
(defun tumble-link-with-description (min max name url state)
  "Posts a Tumblr link using the region as the description"
  (interactive "r \nsName (optional): \nsLink: \nsState (published or draft): ")
  (tumble-post-link name url (tumble-region-text)
                    (tumble-state-from-partial-string state)))

;;;###autoload
(defun tumble-link (name url state)
  "Posts a Tumblr link without a description"
  (interactive "sName (optional): \nsLink: \nsState (published or draft): ")
  (tumble-post-link name url ""
                    (tumble-state-from-partial-string state)))

;;;###autoload
(defun tumble-chat-from-region (min max title state)
  "Posts a chat to Tumblr using the current region"
  (interactive "r \nsTitle (optional): \nsState (published or draft): ")
  (tumble-post-chat title (tumble-region-text)
                    (tumble-state-from-partial-string state)))
;;;###autoload
(defun tumble-chat-from-buffer (title state)
  "Posts a chat to Tumblr using the current buffer"
  (interactive "sTitle (optional): \nsState (published or draft): ")
  (tumble-chat-from-region (point-min) (point-max) title
                           (tumble-state-from-partial-string state)))

;;;###autoload
(defun tumble-photo-from-url (source caption url state)
  "Posts a photo to Tumblr using an URL as the source"
  (interactive "sURL: \nsCaption (optional): \nsLink (optional): \nsState (published or draft): ")
  (tumble-post-photo source caption url
                     (tumble-state-from-partial-string state)))

;;;###autoload
(defun tumble-photo-from-file (filename caption url state)
  "Posts a local photo to Tumblr"
  (interactive "fPhoto: \nsCaption (optional): \nsLink (optional): \nsState (published or draft): ")
  (let* ((file-format  (format "image/%s" (file-name-extension filename))) ; hack
         (data (tumble-file-data filename))
         (request (list (cons 'type "photo")
                        (cons 'caption caption)
                        (cons 'click-through-url url)
                        (cons 'state (tumble-state-from-partial-string state)))))
    (tumble-multipart-http-post request
                                filename
                                file-format
                                data)))

;;;###autoload
(defun tumble-audio (filename caption state)
  "Posts an audio file to Tumblr"
  (interactive "fAudio: \nsCaption (optional): \nsState (published or draft): ")
  (let* ((data (tumble-file-data filename))
         (request (list (cons 'type "audio")
                        (cons 'caption caption)
                        (cons 'state (tumble-state-from-partial-string state)))))
    (tumble-multipart-http-post request
                                filename
                                "audio/mpeg"
                                data)))

;;;###autoload
(defun tumble-video-from-url ()
  "Uses EMBED to post a video to Tumblr"
  (interactive)
  ;; interactive dies with unquoted strings such as YouTube embed codes
  ;; so we call (read-string) directly.
  (let* ((embed (read-string "Source (embed): "))
         (caption (read-string "Caption (optional): "))
         (state (read-string "State (published or draft): ")))
    (tumble-post-video embed caption (tumble-state-from-partial-string state))))

;;; Internal post-sending

(defun tumble-post-text (title body state)
  "Posts a new text to a tumblelog"
  (tumble-http-post
   (list (cons 'type "regular")
         (cons 'title title)
         (cons 'body body)
         (cons 'state state))))

(defun tumble-post-chat (title chat state)
  "Posts a new chat to a tumblelog"
  (tumble-http-post
   (list (cons 'type "conversation")
         (cons 'title title)
         (cons 'conversation chat)
         (cons 'state state))))

(defun tumble-post-link (name url description state)
  "Posts a link to a tumblelog"
  (tumble-http-post
   (list (cons 'type "link")
         (cons 'name name)
         (cons 'url url)
         (cons 'description description)
         (cons 'state state))))

(defun tumble-post-photo (source caption url state)
  "Posts a photo to a tumblelog"
  (tumble-http-post
   (list (cons 'type "photo")
         (cons 'source source)
         (cons 'caption caption)
         (cons 'click-through-url url)
         (cons 'state state))))

(defun tumble-post-video (embed caption state)
  "Embeds a video in a tumblelog"
  (tumble-http-post
   (list (cons 'type "video")
         (cons 'embed embed)
         (cons 'caption caption)
         (cons 'state state))))

;;; Post editing

(defun tumble-menu-get-post ()
  "Returns the selected buffer from the list of posts in *Posts*"
  (with-current-buffer (get-buffer-create "*Posts*")
    (nth (1- (line-number-at-pos)) tumble-posts-cache)))

(defun tumble-fetch-posts (state)
  "Fetch the raw XML-data from the tumblelog and returns it."
  (multiple-value-bind (data header status) (tumble-fetch-posts-internal state)
    (cond ((eq status 200)
           data)
          (t
           (progn
             (kill-buffer "*Posts*")
             (message (format "Unknown error fetching posts: %d.
Please file a bug at http://github.com/febuiles/tumble/issues/" status)))))))

(defun tumble-parse-posts (data)
  "Receives an XML data string DATA and returns the list of XML
elements."
  (with-temp-buffer
    (insert data)
    (xml-parse-region (point-min) (point-max))))

(defun tumble-extract-posts (state)
  "Extract the posts with a given STATE from the xml-tree."
  (cddar (cdddar (tumble-parse-posts (tumble-fetch-posts state)))))

(defun tumble-id-of-post (post)
  "Extract the id of a post tuple."
  (car post))

(defun tumble-title-of-post (post)
  "Extract the title of a post tuple."
  (cadr post))

(defun tumble-body-of-post (post)
  "Extract the body of a post tuple."
  (caddr post))

(defun tumble-convert-text-post (post)
  "Convert a raw xml-tree post to a post tuple."
  (list (cdaadr post)              ; ID
        (caddr (caddr post))       ; Title
        (caddr (cadddr post))))    ; Body

(defun tumble-get-posts (converter state type)
  "Fetch, parse, extract and convert the posts of a tumblelog."
  (delete nil (mapcar
               (lambda (x)
                 ;; Note: this filter only exists since a POST to
                 ;; foo.tumblr.com/api/read apparently ignore the type
                 ;; parameter. The choice was either to include http-get.el
                 ;; for this purpose or live with the ugly wart below.
                 (if (string= (cdr (nth 3 (cadr x))) type)
                     (funcall converter x)
                   nil))
               (tumble-extract-posts state))))

;;;###autoload
(defun tumble-list-text-drafts ()
  "Display a list of text drafts.
The list is displayed in a buffer named `*Posts*'."
  (interactive)
  (setq tumble-posts-cache (tumble-get-posts
                            'tumble-convert-text-post "draft" "regular"))
  (tumble-list-posts))

;;; HTTP stuff

(defun tumble-default-headers ()
  "Generic Tumblr headers"
  (if (or (not tumble-email) (not tumble-password) (not tumble-group))
      (tumble-login))
  (list (cons 'email     tumble-email)
        (cons 'password  tumble-password)
        (cons 'format    tumble-format)
        (cons 'generator "tumble.el")
        (cons 'group     tumble-group)))

(defun tumble-http-post (request)
  "Send the POST request to Tumblr. Receives a list of POST
parameters in the form:
   (list (cons 'param1 value1)
         (cons 'param2 value2))

The parameters are the parameters expected by the Tumblr API
such as \"caption\" or \"source\".

`tumble-process-response' is called with the return code of
the request."
  (let* ((resp (http-post-simple tumble-write-api-url
                                 (append (tumble-default-headers) request))))
    (tumble-process-response resp)))

(defun tumble-multipart-http-post (request filename mime data)
  "Multipart POST used to upload files to Tumblr"
  (let* ((resp (http-post-simple-multipart tumble-write-api-url
                                           (append (tumble-default-headers)
                                                   request)
                                           (list (list "data" filename mime data)))))
    (tumble-process-response resp)))


(defun tumble-process-response (response)
  "Returns a message based on the response code. RESPONSE
is an HTTP response list where the third field is the HTTP
response code."
  (let* ((code (third response)))       ;
    (message
     (cond ((eq code 200) "No post created")
           ((eq code 201)
            (tumble-paste-url (car response))
            "Post saved" )
           ((eq code 400) "Bad Request")
           ((eq code 403) "Authentication Failed")
           (t "Unknown Response")))))

(defun tumble-paste-url (id)
  "Adds the response URL to the kill ring"
  (let* ((last-char (substring tumble-url -1)))
    (cond ((string= last-char "/")
           (kill-new (concat tumble-url id))) ; url has a trailing slash
          (t (kill-new (concat tumble-url (concat "/" id)))))))

(defun tumble-fetch-posts-internal (state)
  "Returns a list containing the XML data, the HTTP status message
and the HTTP response code."
  (http-post-simple (tumble-read-api-url)
                    (append (tumble-default-headers)
                            (list
                             (cons 'filter "none")
                             (cons 'state state)))))

;;; Helper functions

(defun tumble-read-api-url ()
  "Returns the URL for the read API of the tumblelog appending
https if needed."
  (concat "https://"
          (replace-regexp-in-string "https?://" "" tumble-url)
          "/api/read"))

(defun tumble-region-text()
  "Returns the text of the region inside an (interactive 'r') function"
  (buffer-substring-no-properties min max))

(defun tumble-file-data (filename)
  "Reads filename and returns the data"
  (with-temp-buffer
    (insert-file-contents-literally filename)
    (buffer-substring-no-properties (point-min)
                                    (point-max))))

(defun tumble-state-from-partial-string (st)
  "Receives a partial string ST (e.g. \"dra\") and returns the
closest match from the `tumble-states' list. If no value matches
ST then the default state (\"published\") is returned."
  (let ((state (car tumble-states)))
    (if (string= st "")
        state
      (progn
        (mapc `(lambda (x)
                 (if (string-match (concat "^" ,st) x)
                     (setq state x)))
              tumble-states)
        state))))

(defun tumble-get-title-for-post ()
  "Asks the user for a title for his post. If no title is given
then the original title is used"
  (let ((current-title (tumble-title-of-post tumble-selected-draft))
        (new-title (read-string (concat "Title [" current-title "]: "))))
    (if (string= new-title "")
        current-title
      new-title)))

(provide 'tumble)
;;; tumble.el ends here