(in-package :cl-user)

;;------------------------------Preferences------------------------------

(declaim (inline println))
(defun println (obj &optional (stream *standard-output*))
  (let ((*read-default-float-format* 'double-float))
    (prog1 obj
      (princ obj stream)
      (terpri stream))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+swank (declaim (optimize (speed 3) (safety 2)))
  #-swank (declaim (optimize (speed 3) (safety 0) (debug 0)))
  #+swank (load "~/Dropbox/Code/atcoder/ac-tools/act.lisp")
  #+swank (ql:quickload :prove)
  #-swank (declaim (sb-ext:muffle-conditions sb-ext:compiler-note))
  #-swank (sb-ext:disable-debugger)
  (pushnew :inline-generic-funcion *features*))

;;---------------------------------Body---------------------------------

(defmacro nlet (name binds &body body)
  `(labels ((,name (,@(mapcar #'first binds))
              ,@body))
     (,name ,@(mapcar #'second binds))))

(declaim (inline read-fixnum))
(defun read-fixnum (&optional (in *standard-input*))
  ;; Ref: https://competitive12.blogspot.com/2020/03/common-lisp.html
  ;;        partially modified
  (declare (inline read-byte))
  (flet ((%read-byte ()
           (the fixnum #+swank (char-code (read-char in nil #\Nul))
                       #-swank (read-byte in nil #.(char-code #\Nul))))
         (%byte->num (b)
           (the fixnum (- b #.(char-code #\0))))
         (%digit-p (byte)
           (declare (fixnum byte))
           (<= #.(char-code #\0) byte #.(char-code #\9))))
    (declare (inline %read-byte %byte->num %digit-p))
    (let ((minus nil)
          (res 0))
      (declare (boolean minus) (fixnum res))
      (loop for byte of-type fixnum = (%read-byte)
            do (cond
                 ((%digit-p byte)
                  (setf res (%byte->num byte))
                  (return))
                 ((= byte #.(char-code #\Nul))
                  (error "EOF"))
                 ((= byte #.(char-code #\-))
                  (setf minus t))))
      (loop for byte of-type fixnum = (%read-byte)
            do (cond
                 ((%digit-p byte)
                  (setf res (the fixnum (+ (* res 10) (%byte->num byte)))))
                 (t (return))))
      (the fixnum (if minus (- res) res)))))

(defun main ()
  (let* ((n (read))
         (k (read))
         (mat (make-array (list n n)))
         (ps nil))
    (dotimes (i n)
      (dotimes (j n)
        (push (list (read-fixnum)
                    i
                    j)
              ps)))
    (setf ps (sort ps #'< :key #'first))
    (loop for )))

#-swank (main)
