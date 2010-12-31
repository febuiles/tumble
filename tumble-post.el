;; The post type is just a list with four attributes: type, title, body and
;; extra.
;; The six different types of post are: text, link, video, audio, quote and
;; photo. Each one of these types can be encapsulated in the list as follows:
;;
;; text: type = text,  title = title, body = body, extra = nil
;; text: type = link,  title = name, body = url, extra = nil
;; text: type = quote, title = source, body = body, extra = nil
;; text: type = photo, title = caption, body = filename/source, extra = link
;; text: type = audio, title = caption, body = filenamce/source, extra = link
;; text: type = video, title = caption, body = source, extra = nil

(defun tumble-new-post (type title body extra)
  "Returns a list representing that represents a post. The
resulting list has the elements in the same order that they
were passed. Read post.el to find out what each of these values
mean for each type of post."
  (list type title body extra))

(defun tumble-post-type(post)
  "Returns the type of a post."
  (car post))

(defun tumble-post-title(post)
  "Returns the title of a post."
  (cadr post))

(defun tumble-post-body(post)
  "Returns the body of a post."
  (caddr post))

(defun tumble-post-extra(post)
  "Returns the extra information of a post."
  (cadddr post))

(provide 'tumble-post)