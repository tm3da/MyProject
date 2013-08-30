;; Local Variables:
;; lexical-binding: t
;; End:

(require 'my-project-ant-plugin)
(require 'my-project-java-autocomplete)

(defclass my-project/ant-java-ac-project (my-project/ant-project my-project/java-autocomplete)
  ()
  "Ant project with Java Tags.")

(provide 'my-project-ant-java-ac-project)

