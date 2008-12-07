(setq load-path (cons "./vendor"  load-path))
(load "http-post")

;; Fill these fields with your own information and let Tumble do its magic.
(setq tumble-email "federico.builes@gmail.com")
(setq tumble-password "your_password")
;(setq group "testingtumble.tumblr.com")
;; You can set this to either "markdown" or "html".
(setq tumble-format "markdown")

(defun tumble-default-headers ()
  ;; these headers will go into every request, no need for modification.
  (list (cons "email" tumble-email) 
        (cons "password" tumble-password)
        (cons "format" tumble-format)
        (cons "generator" "tumble.el")
;        (cons "group" group)
        ))

(defun tumble-region (min max title)
  "Post the current region as a text in Tumblr"
  (interactive "r \nsTitle: ")
  (let ((body (buffer-substring-no-properties min max)))
    (tumble-text title body)))

(defun tumble-buffer ()
  "Post the current buffer as a text in Tumblr"
  (interactive)
  (tumble-region (point-min) (point-max)))

(defun tumble-quote-from-region (min max source)
  "Post a region as a quote in Tumblr"
  (interactive "r \nsSource (optional): " )
  (tumble-http-post
   (let ((quote (buffer-substring-no-properties min max)))
     (append
      (list (cons "type" "quote")
            (cons "quote" quote)
            (cons "source" source))
      (tumble-default-headers)))))

(defun tumble-link-with-description (min max name url)
  "Posts a Tumblr link using the region as the description"
  (interactive "r \nsName (optional): \nsLink: ")
   (let ((description (buffer-substring-no-properties min max)))
     (tumble-post-link name url description)))

(defun tumble-link (name url)
  "Posts a Tumblr link without description"
  (interactive "sName (optional): \nsLink: ")
  (tumble-post-link name url ""))

(defun tumble-post-link (name url description)
  "Post a link to a tumblelog"
  (tumble-http-post
   (append
    (list (cons "type" "link")
          (cons "name" name)
          (cons "url" url)
          (cons "description" description))
    (tumble-default-headers))))

(defun tumble-text (title body)
  "Post a new text to a tumblelog" 
  (tumble-http-post 
   (append
    (list (cons "type" "regular")
          (cons "title" title)
          (cons "body" body))
    (tumble-default-headers))))
    
(defun tumble-http-post (request)
  (http-post "http://www.tumblr.com/api/write" request 'utf-8))
  