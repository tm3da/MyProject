;; Local Variables:
;; lexical-binding: t
;; End:

;; Customs
(defcustom my-project/java-autocomplete/classpaths
  '("~/.emacs.d/site-lisp/auto-java-complete")
  "List of classpaths (path to Tags.class must be included)."
  :type '(repeat string))


;; Class definition
(defclass my-project/java-autocomplete ()
  ((classpaths :initarg :classpaths :initform nil))
  "Java Auto-Complete.")


;; Methods
(defmethod  my-project/compile-method :after ((project my-project/java-autocomplete))
  (start-process "JavaTags" nil "sh" "-c" "pkill -f \"java Tags\"")
  (sleep-for 1)
  (start-process "JavaTags" "*JavaTags*" "sh" "-c"
		 (concat "CLASSPATH='" (reduce (lambda (acc el)
						 (concat acc ":" el))
					       (mapcar (lambda (fn) (expand-file-name fn))
						       (append (oref project classpaths)
							       my-project/java-autocomplete/classpaths
							       (list (concat (oref project root) "/build/classes/")))))
			 "' java Tags" ))
  (set-process-sentinel (get-process "JavaTags") (lambda (proc ev)
						   (when (get-buffer" *java-base.tag*")
						     (kill-buffer " *java-base.tag*"))
						   (ajc-reload)))
  (when (next-method-p)
    (call-next-method)))

(provide 'my-project-java-autocomplete)

