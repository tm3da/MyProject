;; Local Variables:
;; lexical-binding: t
;; End:

(require 'my-project)
(require 'my-project-java-autocomplete)


;; Class Definition
(defclass my-project/simple-java-project (my-project/java-autocomplete my-project/files-project)
  ())

(provide 'my-project-simple-java-project)

