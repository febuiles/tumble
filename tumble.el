(setq load-path (cons "./vendor"  load-path))
(load "http-post")

;; Fill these fields with your own information and let Tumble do its magic.
(setq tumble-email "federico.builes@gmail.com")
(setq tumble-password "your_password")
;(setq group "testingtumble.tumblr.com")
;; You can set this to either "markdown" or "html".
(setq format "markdown")



(defun tumble-default-headers ()
  (list (cons "email" tumble-email) 
        (cons "password" tumble-password)
        (cons "format" tumble-format)
        (cons "generator" "tumble.el")
;        (cons "group" group)
        ))

(defun tumble-region (min max title)
  "Post the current region as a text in Tumblr"
  (interactive "r \nsTitle: ")
  (let* ( 
         (body (buffer-substring-no-properties min max))
         )
    (tumble-text title body)))

(defun tumble-buffer ()
  "Post the current buffer as a text in Tumblr"
  (interactive)
  (tumble-region (point-min) (point-max)))

(defun tumble-text (title body)
  "Post a new text to a tumblelog" 
  (tumble-http-post 
   (append
    (list (cons "type" "regular")
          (cons "title" title)
          (cons "body" body))
    (tumble-default-headers))))
    

(defun tumble-quote-from-region (min max source)
  "Post a region as a quote in Tumblr"
  (interactive "r \nsSource (optional): " )
  
  )

(defun tumble-http-post (request)
  (http-post "http://www.tumblr.com/api/write" request 'utf-8))
  