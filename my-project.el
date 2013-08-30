;; Local Variables:
;; lexical-binding: t
;; End:

;; Usage:
;; (add-to-list 'load-path "path/to/my/project")
;; (require 'my-project)
;;
;;
;; You can create new project like this:
;; (add-project  (make-instance my-project/project :name "Project name"
;; 			     :compilation-function `(("make" (("all" "make all")
;; 							      ("doc" "make doc")
;; 							      ("plugins" (("plugin 1" "make plugin1") ; There is no limit on how deep you can nest
;; 									  ("plugin 2" "make plugin2")))
;; 							      ("You can use comma quoting" ,(concat "make " "clear")))) 
;; 						     ("show message" (message "You can call elisp function from compile menu."))
;; 						     "ls" ; You can run simple commands without alias
;; 						     )))

(eval-when-compile
  (require 'cl)
  (require 'eieio ))


;; Customization
(defgroup my-project nil
  "Support for simple project management."
  :group 'local
  :prefix "my-project/")

(defcustom my-project/paths-to-project-files
  '("~/.emacs.d/site-lisp/my-project-config.el")
  "Paths to project files."
  :type '(repeat string)
  :group 'my-project)


;; Internal variables
(defmacro defparameter (var value)
  `(defvar ,var ,value)
  `(setq ,var ,value))

(defparameter *my-project/projects* nil)        
(defparameter *my-project/current-project* nil)
(defparameter *my-project/current-master-project* nil)


;; Project structure
(defclass my-project/project ()
  ((compilation-function :initarg :compilation-function :initform nil)
   (name :initarg :name)
   (root :initarg :root)
   (subprojects :initarg :subprojects :initform nil))
  "Base class for all projects.")

(defclass my-project/files-project (my-project/project)
  ((files :initarg :files) 
   (find-file-cache :initform nil)
   (find-file-hash :initform (make-hash-table)))
  "Base class for project with file list in configuration file.")

(defclass my-project/directory-project (my-project/files-project)
  ((ignored-files :initarg :ignored-files :initform nil))
  "Base class for directory based projects")

(defclass my-project/makefile-project (my-project/directory-project)
  ()
  "Class for Makefile-based project.")

(defclass my-project/ftp-files-project (my-project/files-project)
  ((ignored-files :initarg :ignored-files :initform nil))
  "Class for ftp files project.")


;; Generic functions
(defgeneric my-project/compile-method (project)
  "Compile method.")

(defgeneric my-project/find-file-method (project)
  "Find file method.")

(defgeneric my-project/generate-cache (project)
  "Generates cache.")

(defgeneric my-project/project-selected (project)
  "Gets called when project is selected.")

(defgeneric my-project/project-file-saved (project)
  "Gets called when file from project is saved.")


;; Methods
(defmethod my-project/compile-method (project)
  (if (oref project compilation-function)
      (my-project/--compile (oref project compilation-function) (oref project root))
    (call-interactively 'compile)))

(defmethod my-project/find-file-method (project)
  (if *my-project/current-project*
    (error "No files in this project.")
    (my-project/select-project)))

(defmethod my-project/generate-cache (project))

(defmethod my-project/project-selected (project))

(defmethod my-project/project-file-saved ((project my-project/project)))
(defmethod my-project/project-selected ((project my-project/project)))

(defmethod my-project/project-selected ((project my-project/files-project))
  (my-project/generate-cache project)
  (call-next-method)) 

(defmethod my-project/find-file-method ((project my-project/files-project))
  (find-file (gethash (ido-completing-read "Find file: " (oref project find-file-cache))
		      (oref project find-file-hash))))

(defmethod my-project/generate-cache ((project my-project/files-project))
  (unless (oref project find-file-cache)
    (with-slots (files find-file-cache find-file-hash name root) project
      (mapc (lambda (file)
	      (let ((path (expand-file-name file)))
		(setq key (replace-regexp-in-string "\\(.*?\\)\\([^/]+?\\)$" "\\2 (\\1)" path))
		(setq key (replace-regexp-in-string (concat (expand-file-name root) "/?") (concat name "::/") key))
		(puthash key path find-file-hash)
		(push key find-file-cache)))
	    files)
      (oset project files nil))))

(defmethod my-project/generate-cache ((project my-project/directory-project))
  (with-slots (ignored-files find-file-cache root) project
    (unless find-file-cache
      (let ((my-project-root (expand-file-name  root)))
	(oset project files (remove-if
			     (lambda (a) (string-match "^\s*$" a))
			     (split-string 
			      (shell-command-to-string
			       (concat "grep -I -R -l \'\' \'"
				       my-project-root
				       "\' 2> /dev/null | egrep -v \'("
				       (if  ignored-files
					   (reduce (lambda (a b) (concat a "|" b)) ignored-files)
					 "*.out|*~|*.log|*.backup|\\.bzr/.*")  ")$\'")) "\n"))))
      (call-next-method))))

(defmethod my-project/generate-cache ((project my-project/makefile-project))
  (oset project compilation-function (cons (let ((path (expand-file-name (oref project root))))
					     (cons "Make" (list (mapcar
								 (lambda (opt) (list opt (concat (when path (concat "cd " path "; ")) "make " opt))) 
								 (nbutlast (split-string 
									    (shell-command-to-string
									     (concat  "cat " path "Makefile |"
										      " sed -n -e 's/^\\([^\\.\\#[:space:]\\$][^:[:space:]\\$\\=]*\\):.*/\\1/p' "
										      "| grep -v '/'| sort -u")) "\n"))))))
					   (oref project compilation-function)))
  (call-next-method))

(defmethod my-project/generate-cache ((project my-project/ftp-files-project))
  (with-slots (root ignored-files) project
    (cl-labels ((get-files-from-dir (dir)
				    (let* ((files (mapcar (lambda (a) (concat dir a)) 
							  (remove-if (lambda (a) 
								       (string-match 
									(concat "\\(" (reduce (lambda (a b) (concat a "\\|" b)) (cons "\\.\\.?/" ignored-files)) "\\)$") a))
								     (tramp-completion-handle-file-name-all-completions "" dir))))
					   (dirs  (remove-if-not (lambda (a) (string-match "/$" a)) files))
					   (files-only (remove-if (lambda (a) 
								    (string-match "/$" a)) files)))
				      (append files-only (reduce #'append  (mapcar #'get-files-from-dir dirs))))))
      (oset project files (get-files-from-dir root))))
  (call-next-method))


;; Functions
(defun my-project/--compile-command (command path)
  (unless (string-equal (substring command 0 3) "cd ")
    (setq command (concat "cd " (expand-file-name path) "; " command)))
  (compile (read-string "Compile command: " command)))

(defun my-project/--get-subproject-assoc (subprj)
  (cons (list "." nil) (mapcar
			(lambda (prj) (list (oref prj name) prj))
			(oref subprj subprojects))))

(defun my-project/--compile (compilation-opts root)
  (let* ((option (ido-completing-read "Compile: " (mapcar
						   (lambda (c) (if (consp c) (car c) c))
						   compilation-opts)))
	 (cmd (cadr (assoc option compilation-opts))))
    (cond ((and (stringp option) (null cmd)) (my-project/--compile-command option root))
	  ((stringp cmd) (my-project/--compile-command cmd root))
	  ((and (consp cmd) (symbolp (car cmd))) (eval cmd))
	  ((consp cmd) (my-project/--compile cmd root)))))

(defun my-project/--select-project (project)
  (setq *my-project/current-project* project
	*my-project/current-master-project* project)
  (my-project/project-selected project)
  (easy-menu-define my-project-mode-menu
    my-project-mode-map
    "Menu"
    (append (list "MyProject" 
		  (cons "Projects" (mapcar (lambda (el) `[,(car el) (my-project/--select-project ,(cdr el))]) *my-project/projects*)))
	    (if (and *my-project/current-master-project* (oref *my-project/current-master-project* subprojects))
		(list (cons "Subprojects" (cdr (my-project/--generate-subprojects-menu  *my-project/current-master-project*)))))
	    (list
	     "---"
	     ["Find file" my-project/find-file]
	     ["Compile" my-project/compile]
	     ["Load project files" my-project/load-projects]))))

(defun my-project/--select-subproject (project)
  (setq *my-project/current-project* project)
  (my-project/project-selected project))

(defun my-project/--generate-subprojects-menu (project)
  (if (oref project subprojects)
      (cons (oref project name) (mapcar #'my-project/--generate-subprojects-menu (oref project subprojects)))
    `[,(oref project name) (my-project/--select-subproject ,project)]))


;; Interactive functions
(defun my-project/select-project ()
  "Select project."
  (interactive)
  (my-project/--select-project (cdr (assoc (ido-completing-read "Select project: " (mapcar #'car *my-project/projects*)) *my-project/projects*))))

(defun my-project/select-subproject ()
  "Select subproject."
  (interactive)
  (labels ((select-subproject-ido (prj)
				     (let* ((subprj-assoc (my-project/--get-subproject-assoc prj))
					    (selection (if (oref prj subprojects)
							   (cadr (assoc (ido-completing-read "Select subproject: " (mapcar #'car subprj-assoc)) subprj-assoc)))))
				       (cond ((null selection) prj)
					     ((oref selection subprojects) (select-subproject-ido selection))
					     (t selection)))))
    (setq *my-project/current-project* (select-subproject-ido *my-project/current-master-project*))
    (my-project/project-selected *my-project/current-project*)))

(defun my-project/compile ()
  "Shows compile menu."
  (interactive)
  (if *my-project/current-project*
      (if (oref *my-project/current-project* compilation-function)
	  (my-project/compile-method *my-project/current-project*)
	(my-project/compile-method *my-project/current-master-project*))
    (call-interactively 'compile)))

(defun my-project/find-file ()
  "Find file in current subproject"
  (interactive)
  (my-project/find-file-method *my-project/current-project*))

(defun my-project/load-projects ()
  "Loads config files to create project list."
  (interactive)
  (setq *my-project/projects*        nil
	*my-project/current-project* nil
	*my-project/current-master-project* nil)
  (mapc (lambda (file) (load (expand-file-name file))) my-project/paths-to-project-files)
  (easy-menu-define my-project-mode-menu
    my-project-mode-map
    "Menu"
    (list "MyProject" 
	  (cons "Projects" (mapcar (lambda (el) `[,(car el) (my-project/--select-project ,(cdr el))]) *my-project/projects*))
	  "---"
	  ["Find file" my-project/find-file]
	  ["Compile" my-project/compile]
	  ["Load project files" my-project/load-projects])))

(defun my-project/after-save-function ()
  "Calls project-file-saved method."
  (interactive)
  (when *my-project/current-master-project*
    (my-project/project-file-saved *my-project/current-master-project*)
    (unless (eq *my-project/current-project*
		*my-project/current-master-project*)
      (my-project/project-file-saved *my-project/current-project*))))


;; Macros
(defmacro new-project (project-type &rest args)
  "Creates new project."
  `(make-instance (intern (concat "my-project/" (symbol-name (quote ,project-type)))) ,@args))

(defmacro add-project (project-type &rest args)
  "Adds project"
  `(let ((project (new-project ,project-type ,@args)))
     (push (cons (oref project name) project) *my-project/projects*)))


;; Mode definition
(defconst my-project-mode-map
  (let ((map (make-sparse-keymap)))
    ;;(define-key map [mode-line down-mouse-1] my-project-mode-menu)
    (define-key map (kbd "s-SPC l") #'my-project/load-projects)
    (define-key map (kbd "s-SPC p") #'my-project/select-project)
    (define-key map (kbd "s-SPC SPC") #'my-project/find-file)
    (define-key map (kbd "s-SPC s-SPC") #'my-project/select-subproject)
    (define-key map (kbd "<f5>") #'my-project/compile)
    map))

(define-minor-mode my-project-mode
  "Simplified project management"
  nil
  :lighter (:eval (concat " MyProject{" (cond ((and *my-project/current-master-project* *my-project/current-project*
						    (not (eql *my-project/current-master-project* *my-project/current-project*)))
					       (concat (oref *my-project/current-master-project* name) "(" (oref *my-project/current-project* name) ")"))
					      (*my-project/current-master-project* (oref *my-project/current-master-project* name))
					      (t "∅")) "}"))
  :keymap my-project-mode-map
  (add-hook 'after-save-hook #'my-project/after-save-function)
  )


;; Globalized mode
(define-globalized-minor-mode global-my-project-mode
  my-project-mode 
  (lambda () (my-project-mode 1)))

(provide 'my-project)

