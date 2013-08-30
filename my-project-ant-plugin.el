;; Local Variables:
;; lexical-binding: t
;; End:

(require 'my-project)


;; Class definition
(defclass my-project/ant-project (my-project/directory-project)
  ()
  "Class for ant-based project.")


;; Internal functions
(defun my-project-ant-plugin/--get-compilation-function (path filename)
  (labels ((get-imports (root)
			   (let ((result nil))
			     (dolist (prj root result)
			       (when (consp prj)
				 (dolist (inprj prj)
				   (when (and (consp inprj)
					      (eq (car inprj) 'import))
				     (setq result (append result (list (cdr (assoc 'file (cadr inprj))))))))))))
	      (parse-project (project)
			     (let ((result nil))
			       (dolist (inprj (cdr project) result)
				 (when (and (consp inprj)
					    (eq (car inprj) 'target)
					    (not (string-equal (substring (cdr (assoc 'name (cadr inprj))) 0 1) "-")))
				   (setq result (append result (list (list (cdr (assoc 'name (cadr inprj))) (concat "cd " path "; ant " (cdr (assoc 'name (cadr inprj)))))))))))))
    (let ((root (xml-parse-file filename))
	  (result nil))
      (dolist (project root)
	(when (consp project)
	  (let ((prj (parse-project project)))
	    (when prj
	      (setq result (append result (list (list (cdr (assoc 'name (cadr project)))  prj))))))))
      (mapc (lambda (fn) (setq result (append result (my-project-ant-plugin/--get-compilation-function path (expand-file-name (concat path fn)))))) (get-imports root))
      result)))


;; Methods  
(defmethod my-project/generate-cache ((project my-project/ant-project))
  (oset project compilation-function (append (my-project-ant-plugin/--get-compilation-function
					      (expand-file-name (oref project root))
					      (expand-file-name (concat (oref project root) "/build.xml")))
					     (oref project compilation-function)))
  (call-next-method))

(provide 'my-project-ant-plugin)

