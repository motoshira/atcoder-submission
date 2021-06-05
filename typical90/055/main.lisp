(in-package :cl-user)

;;------------------------------Preferences------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+swank (declaim (optimize (speed 3) (safety 2)))
  #-swank (declaim (optimize (speed 3) (safety 0) (debug 0)))
  #+swank (load "~/Dropbox/Code/atcoder/ac-tools/act.lisp")
  #+swank (ql:quickload :prove)
  #-swank (declaim (sb-ext:muffle-conditions sb-ext:compiler-note))
  #-swank (sb-ext:disable-debugger)
  (pushnew :inline-generic-funcion *features*))

;;---------------------------------Body---------------------------------

(declaim (inline println))
(defun println (obj &optional (stream *standard-output*))
  (let ((*read-default-float-format* 'double-float))
    (prog1 obj
      (princ obj stream)
      (terpri stream))))

(deftype uint () '(unsigned-byte 60))

(defstruct pair
  (sum 0 :type uint)
  (cnt 0 :type uint))

(defun main ()
  (let* ((n (read))
         (p (read))
         (q (read))
         (as (loop repeat n collect (read))))
    (declare (uint n p q)
             (list as))
    (let ((stack nil)
          (res 0))
      (declare (list stack)
               (uint res))
      (push (make-pair :sum 1
                       :cnt 0)
            stack)
      (dolist (a as)
        (declare (fixnum a))
        (let ((tmp nil))
          (declare (list tmp))
          (dolist (pair stack)
            (declare (pair pair))
            (with-slots ((val pri)
                         (cnt pos))
                pair
              (declare (uint val cnt))
              (let ((next (rem (the uint (* val a))
                               p)))
                (declare (uint next))
                (cond
                  ((< cnt 4)
                   (push (make-pair :pri next
                                    :pos (1+ cnt))
                         tmp))
                  (:else
                   (when (= next q)
                     (incf res)))))))
          (dolist (pair tmp)
            (push pair stack))))
      (println res))))

#-swank (main)
