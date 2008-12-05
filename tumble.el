(setq load-path (cons "./vendor/"  load-path))
(load "http-post")

;; Fill these fields with your own information and let Tumble do its magic.
(setq email "federico.builes@gmail.com")
(setq password "your_password")

(defun tumble-default-headers ()
  (list (cons "email" email) 
        (cons "password" password)
        (cons "format" "markdown")
        (cons "generator" "tumble.el")))

(defun tumble-region (min max)
  "Post the current region as a new text in Tumblr"
  (interactive "r")
  (let* ( 
         (body (buffer-substring-no-properties min max))
         )
    (tumble-text "title" body)))

(defun tumble-buffer ()
  "Post the current buffer as a new text in Tumblr"
  (interactive)
  (tumble-region (point-min) (point-max)))

(defun tumble-text (title body)
  "Post a new text to a tumblelog" 
  (http-post "http://www.tumblr.com/api/write"
             (append
              (list (cons "type" "regular")
                    (cons "title" title)
                    (cons "body" body))
              (tumble-default-headers))
             'utf-8))













