;; WARNING: This is pretty experimental right now so proceed with 
;; caution or you might blow something up!

(setq load-path (cons "./vendor"  load-path))
(load "http-post")

;; Fill these fields with your own information and let Tumble do its magic.
(setq tumble-email "federico.builes@gmail.com")
(setq tumble-password "your_password")
;(setq tumble-group "testingtumble.tumblr.com")
;; You can set this to either "markdown" or "html".
(setq tumble-format "markdown")

(defun tumble-default-headers ()
  ;; these headers will go into every request, no need for modification.
  (list (cons "email" tumble-email) 
        (cons "password" tumble-password)
        (cons "format" tumble-format)
        (cons "generator" "tumble.el")
;        (cons "group" tumble-group)
        ))

(defun tumble-text-from-region (min max title)
  "Post the current region as a text in Tumblr"
  (interactive "r \nsTitle: ")
  (tumble-post-text title (region-text)))

(defun tumble-text-from-buffer (title)
  "Post the current buffer as a text in Tumblr"
  (interactive "sTitle: ")
  (tumble-text-from-region (point-min) (point-max) title))

(defun tumble-quote-from-region (min max source)
  "Post a region as a quote in Tumblr"
  (interactive "r \nsSource (optional): " )
  (tumble-http-post
   (list (cons "type" "quote")
         (cons "quote" (region-text))
         (cons "source" source))))

(defun tumble-link-with-description (min max name url)
  "Posts a Tumblr link using the region as the description"
  (interactive "r \nsName (optional): \nsLink: ")
  (tumble-post-link name url (region-text)))

(defun tumble-link (name url)
  "Posts a Tumblr link without description"
  (interactive "sName (optional): \nsLink: ")
  (tumble-post-link name url ""))

(defun tumble-post-link (name url description)
  "Post a link to a tumblelog"
  (tumble-http-post
   (list (cons "type" "link")
         (cons "name" name)
         (cons "url" url)
         (cons "description" description))))

(defun tumble-post-text (title body)
  "Post a new text to a tumblelog" 
  (tumble-http-post 
   (list (cons "type" "regular")
         (cons "title" title)
         (cons "body" body))))

(defun tumble-http-post (request)
  (http-post "http://www.tumblr.com/api/write" 
             (append (tumble-default-headers) request) 
             'utf-8))

(defun region-text () (buffer-substring-no-properties min max))
