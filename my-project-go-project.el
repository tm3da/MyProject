;; Local Variables:
;; lexical-binding: t
;; End:

(require 'my-project)


;; Class definition
(defclass my-project/go-project (my-project/directory-project)
  ()
  "Class for Go(golang) project.")


;; Methods
(defmethod initialize-instance ((project my-project/go-project) &rest slots)
  (let ((root (plist-get (car slots) :root))
        (main (or (plist-get (car slots) :main) "main.go")))
    (oset project compilation-function `(("go" (("build" ,(concat "cd " (expand-file-name root) "src; GOPATH=\"" (concat (expand-file-name root) ":" (getenv "GOPATH")) "\" go build"))
                                                ("run"   ,(concat "cd " (expand-file-name root) "src; GOPATH=\"" (concat (expand-file-name root) ":" (getenv "GOPATH")) "\" go run " main))
                                                ;("get"   ,(concat "cd " (expand-file-name root) "/src; GOPATH=\"" (concat (expand-file-name root) ":" (getenv "GOPATH"))   "\" go get"))
                                                )))))
  (call-next-method))


(defmethod my-project/project-selected ((project my-project/go-project))
  (start-process "*GoCodeKiller*" nil "sh" "-c" "gocode close")
  (start-process "*GoCode*" nil "sh" "-c" (concat "GOPATH=\"$GOPATH:" (expand-file-name (oref project root)) "\" gocode"))
  (call-next-method))

(provide 'my-project-go-project)
