;;
;;  system  -  minimalist system loader
;;
;;  Copyright 2016-2018 Thomas de Grivel <thoxdg@gmail.com>
;;

(in-package :common-lisp-user)

(when (find-package :asdf/interface)
  (delete-package :asdf/user)
  (delete-package :asdf))

(defpackage :package-system
  (:use :common-lisp)
  (:nicknames :asdf)
  (:export
   #:defsystem
   #:*dir*
   #:*system-definition-search-functions*
   #:child-component
   #:component
   #:component-components
   #:component-depends-on
   #:component-dir
   #:component-extension
   #:component-name
   #:component-parent
   #:component-pathname
   #:component-relative-dir
   #:containe
   #:find-system
   #:load-asd
   #:load-system
   #:parse-component
   #:system
   #:*systems*
   ))

(defpackage :package-system-user
  (:use :common-lisp :package-system))

(in-package :package-system)

;;  string functions

(defun str (&rest parts)
  (with-output-to-string (out)
    (labels ((part (x)
               (typecase x
                 (string (write-string x out))
                 (null nil)
                 (cons (mapc #'part x))
                 (pathname (part (namestring x)))
                 (t (prin1 x out)))))
      (part parts))))

(defun string-ends-with (x string)
  (declare (type string x string))
  (let* ((lx (length x))
	 (ls (length string))
	 (dl (- ls lx)))
    (when (and (>= ls lx)
	       (string= x string :start2 dl))
      dl)))

;;  Path functions

(defun path-dirname (x)
  (declare (type string x))
  (let ((slash (position #\/ x :from-end t
			 :end (or (string-ends-with "/" x)
                                  (length x)))))
    (cond ((null slash) "")
	  ((= 0 slash) "/")
	  (t (subseq x 0 slash)))))

(defun path-basename (x)
  (declare (type string x))
  (let ((slash (position #\/ x :from-end t
			 :end (or (string-ends-with "/" x)
                                  (length x)))))
    (if slash
	(subseq x (1+ slash))
	x)))

;;  component classes

(defclass component ()
  ((depends-on :initarg :depends-on
	       :initform nil
	       :reader component-depends-on
	       :type list)
   (name :initarg :name
	 :reader component-name
	 :type string)
   (pathname :initarg :pathname
	     :type string)
   (dir :initarg :dir
        :type string)))

(defclass container (component)
  ((components :initarg :components
	       :initform nil
	       :reader component-components
	       :type list)))

(defclass system (container)
  ((author :initarg :author
	   :reader component-author
	   :type string)
   (description :initarg :description
		:reader component-description
		:type string)
   (license :initarg :license
	    :reader component-license
	    :type string)
   (version :initarg :version
	    :reader component-version
	    :type string)))

(defclass child-component (component)
  ((parent :initarg :parent
           :initform nil
           :reader component-parent
           :type container)))

(defclass module (container child-component) ())

(defclass cl-source-file (child-component) ())

(defclass static-file (child-component) ())

(defgeneric component-dir (component))
(defgeneric component-extension (component))
(defgeneric component-pathname (component))
(defgeneric component-relative-dir (component))

(defmethod component-relative-dir ((obj component))
  nil)

(defmethod component-relative-dir ((obj module))
  (str (component-name obj) "/"))

(defmethod component-dir ((obj component))
  (if (slot-boundp obj 'dir)
      (slot-value obj 'dir)
      (setf (slot-value obj 'dir)
            (cond ((slot-boundp obj 'pathname)
                   (path-dirname (component-pathname obj)))
                  (t (call-next-method))))))

(defmethod component-dir ((obj child-component))
  (str (component-dir (component-parent obj))
       (component-relative-dir obj)))

(defmethod component-extension ((obj system)) ".asd")
(defmethod component-extension ((obj module)) "")
(defmethod component-extension ((obj cl-source-file)) ".lisp")

(defmethod component-pathname ((obj component))
  (str (component-dir obj) "/"
       (component-name obj)
       (component-extension obj)))

(defmethod component-pathname :around ((obj component))
  (if (slot-boundp obj 'pathname)
      (slot-value obj 'pathname)
      (setf (slot-value obj 'pathname) (call-next-method))))

(defmethod print-object ((obj system) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (when (and (slot-boundp obj 'name)
               (slot-boundp obj 'components))
      (let ((n (length (the list (component-components obj)))))
	(format stream "~S ~D component~P"
		(component-name obj) n n)))))

(defun parse-component (parent desc)
  (destructuring-bind (kind name &rest initargs) desc
    (let ((class (ecase kind
		   (:file 'cl-source-file)
		   (:module 'module)
		   (:static-file 'static-file))))
      (apply 'make-instance class :name name
             :parent parent initargs))))

(defun parse-components (parent components)
  (mapcar (lambda (comp)
            (parse-component parent comp))
          components))

(defun component-is-dependent-on (component other)
  (find other (the list (component-depends-on component)) :test #'eq))

(defun sort-components (components)
  (declare (type list components))
  (sort components 'component-is-dependent-on))

(defmethod initialize-instance :after ((obj container) &rest initargs
                                       &key &allow-other-keys)
  (declare (ignore initargs))
  (with-slots (components) obj
    (setf components
          (sort-components (parse-components obj components)))))

(defun plist-merge (to add &rest more-lists)
  (cond
    ((endp add) (if (endp more-lists)
		    to
		    (plist-merge to (first more-lists) (rest more-lists))))
    ((endp (rest add)) (error "Incomplete property list"))
    (t (setf (getf to (first add)) (first (rest add)))
       (plist-merge to (rest (rest add))))))

(defparameter *systems*
  (make-hash-table :test 'equal))

(defvar *pathname*)
(defvar *dir*)

(defmacro defsystem (name &body options)
  (let* ((name (string-downcase name))
	 (system (apply 'make-instance 'system
			(plist-merge options
                                     `(:name ,name
                                       :dir ,*dir*
                                       :pathname ,*pathname*)))))
    `(setf (gethash ,name *systems*) ,system)))

(defun load-asd (pathname)
  (let* ((*package* (find-package :package-system-user))
         (*pathname* (str pathname))
         (*dir* (path-dirname *pathname*)))
    (load pathname)))

;;  find system

(defun find-system (name)
  (gethash (string-downcase name) *systems*))

;;  loading

(defclass op () ())

(defgeneric op (op component))

(defclass load-op (op)
  ((components :initform nil
	       :accessor op-components
	       :type list)))

(defmethod op ((op (eql 'load-op)) component)
  (op (make-instance op) component))

(defmethod op ((op load-op) (source cl-source-file))
  (load (component-pathname source)))

;;  searching systems

(defvar *system-definition-search-functions*
  '())

(defun sysdef (x)
  (let ((x (string-downcase x)))
    (labels ((sysdef-search (functions)
	       (unless (endp functions)
		 (or (funcall (first functions) x)
		     (sysdef-search (rest functions))))))
      (sysdef-search *system-definition-search-functions*))))

(defun system (s)
  (or (find-system s)
      (let ((sysdef (sysdef s)))
	(when sysdef
	  (load-asd sysdef)
	  (find-system (pathname-name sysdef))))
      (error "system not found ~S" s)))

(defun load-system (s)
  (op 'load-op (system s)))

(defmethod op ((op load-op) (system system))
  (unless (find system (op-components op))
    (push system (op-components op))
    (dolist (sys (component-depends-on system))
      (op op (system sys)))
    (dolist (comp (component-components system))
      (op op comp))))
