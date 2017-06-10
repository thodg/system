;;
;;  system  -  minimalist system loader
;;
;;  Copyright 2016-2017 Thomas de Grivel <thomas@lowh.net>
;;

(in-package :common-lisp-user)

(when (find-package :asdf/interface)
  (delete-package :asdf/user)
  (delete-package :asdf/interface))

(defpackage :system
  (:use :common-lisp)
  (:nicknames :asdf)
  (:export #:system
	   #:defsystem
	   #:load-asd
	   #:load-system
	   #:*system-definition-search-functions*))

(defpackage :system-user
  (:use :common-lisp :system))

(in-package :system)

;;  string functions

(defun string-ends-with (x string)
  (let* ((lx (length x))
	 (ls (length string))
	 (dl (- ls lx)))
    (when (and (>= ls lx)
	       (string= x string :start2 dl))
      dl)))

(defun dirname (x)
  (let ((slash (position #\/ x :from-end t
			 :end (or (string-ends-with "/" x) (length x)))))
    (cond ((null slash) "")
	  ((= 0 slash) "/")
	  (t (subseq x 0 slash)))))

(defun basename (x)
  (let ((slash (position #\/ x :from-end t
			 :end (or (string-ends-with "/" x) (length x)))))
    (if slash
	(subseq x (1+ slash))
	x)))

(defun str (&rest parts)
  (labels ((to-str (x)
	     (typecase x
	       (string x)
	       (null "")
	       (cons (apply 'str x))
	       (pathname (namestring x))
	       (t (prin1-to-string x)))))
    (apply 'concatenate 'string (mapcar #'to-str parts))))

;;  component classes

(defclass component ()
  ((depends-on :initarg :depends-on
	       :initform nil
	       :reader component-depends-on
	       :type list)
   (name :initarg :name
	 :reader component-name
	 :type string)
   (pathname :initarg pathname
	     :reader component-pathname
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

(defclass module (container) ())

(defclass cl-source-file (component) ())

(defclass static-file (component) ())

(defvar *dir*)

(defgeneric component-extension (obj))
(defmethod component-extension ((obj system)) ".asd")
(defmethod component-extension ((obj module)) "")
(defmethod component-extension ((obj cl-source-file)) ".lisp")

(defmethod initialize-instance :after ((obj system) &rest initargs
				       &key serial &allow-other-keys)
  (declare (ignore initargs serial))
  (with-slots (pathname) obj
    (setf pathname (str *dir* "/" (component-name obj)
			(component-extension obj)))))

(defmethod initialize-instance :after ((obj module) &rest initargs
				       &key serial &allow-other-keys)
  (declare (ignore initargs serial))
  (with-slots (pathname) obj
    (setf pathname (str *dir* "/" (component-name obj)
			(component-extension obj)))))

(defmethod initialize-instance :after ((obj cl-source-file) &rest initargs
				       &key serial &allow-other-keys)
  (declare (ignore initargs serial))
  (with-slots (pathname) obj
    (setf pathname (str *dir* "/" (component-name obj)
			(component-extension obj)))))

(defmethod print-object ((obj system) stream)
  (let ((n (length (component-components obj))))
    (print-unreadable-object (obj stream :type t :identity t)
      (when (and (slot-boundp obj 'name)
		 (slot-boundp obj 'components))
	(format stream "~S ~D component~P"
		(component-name obj) n n)))))

(defun component (desc)
  (destructuring-bind (kind name &rest initargs) desc
    (let ((class (ecase kind
		   (:file 'cl-source-file)
		   (:module 'module)
		   (:static-file 'static-file))))
      (apply 'make-instance class :name name initargs))))

(defun parse-components (desc)
  (mapcar 'component desc))

(defun is-dependent-on (a b)
  (find (component-name a) (component-depends-on b) :test #'string=))

(defun sort-components (components)
  (sort components 'is-dependent-on))

(defmethod initialize-instance :after ((obj container) &rest initargs)
  (declare (ignore initargs))
  (with-slots (components) obj
    (setf components (sort-components (parse-components components)))))

(defparameter *systems*
  (make-hash-table :test 'equal))

(defun plist-merge (to add &rest more-lists)
  (cond
    ((endp add) (if (endp more-lists)
		    to
		    (plist-merge to (first more-lists) (rest more-lists))))
    ((endp (rest add)) (error "Incomplete property list"))
    (t (setf (getf to (first add)) (first (rest add)))
       (plist-merge to (rest (rest add))))))

(defmacro defsystem (name &body options)
  (let* ((name (string-downcase name))
	 (system (apply 'make-instance 'system
			(plist-merge options `(:name ,name)))))
    `(setf (gethash ,name *systems*) ,system)))

(defun load-asd (pathname)
  (let ((*package* (find-package :system-user))
	(*dir* (dirname (str pathname))))
    (load pathname)))

(load-asd "/home/de-gri_t/common-lisp/thodg/repo/repo.asd")

;;  find system

(defun find-system (name)
  (gethash (string-downcase name) *systems*))

(find-system :repo)

;;  loading

(defclass op () ())

(defgeneric op (op component))

(defclass load-op (op)
  ((components :initform nil
	       :accessor op-components
	       :type list)))

(defmethod op ((op (eql 'load-op)) component)
  (op (make-instance op) component))

(defmethod op ((op load-op) (system system))
  (unless (find system (op-components op))
    (push system (op-components op))
    (dolist (sys (component-depends-on system))
      (op op sys))
    (dolist (comp (component-components system))
      (op op comp))))

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
	  (find-system (pathname-name sysdef))))))

(defun load-system (s)
  (op 'load-op (system s)))
