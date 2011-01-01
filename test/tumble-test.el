(add-to-list 'load-path "../")
(add-to-list 'load-path "./")
(require 'el-expectations)
(require 'tumble-post)

(defun post ()
  (tumble-new-post "text" "some title" "some body" "extra" "published"))

(expectations

  ;; the extra field is optional, no error should be raised.
  (expect (list "text" "title" "body" nil nil)
    (tumble-new-post "text" "title" "body"))

  ;; test the basic accesor functions
  (expect "text"
    (tumble-post-type (post)))
  (expect "some title"
    (tumble-post-title (post)))
  (expect "some body"
    (tumble-post-body (post)))
  (expect "extra"
    (tumble-post-extra (post)))
  (expect "published"
    (tumble-post-status (post)))
)
