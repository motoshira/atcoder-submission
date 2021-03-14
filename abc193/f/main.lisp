(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *opt*
    #+swank '(optimize (speed 3) (safety 2))
    #-swank '(optimize (speed 3) (safety 0) (debug 0)))
  #+swank (load "~/Dropbox/Code/atcoder/ac-tools/act.lisp")
  #-swank (declaim (sb-ext:muffle-conditions sb-ext:compiler-note))
  #-swank (sb-ext:disable-debugger)
  (pushnew :inline-generic-funcion *features*))

(defmacro dbg (&rest forms)
  #-swank (declare (ignore forms))
  #+swank `(format *error-output* "~a => ~a~&" ',forms `(,,@forms)))

(defmacro do-rep (count &body body) `(loop repeat ,count do ,@body))

(defmacro nlet (name binds &body body)
  `(labels ((,name (,@(mapcar #'first binds))
              ,@body))
     (,name ,@(mapcar #'second binds))))

(defmacro dotimes! ((var count &optional (index-origin 0) (unroll 60)) &body body)
  (macrolet ((with-gensyms ((&rest args) &body body)
               `(let (,@(mapcar (lambda (arg) `(,arg (gensym))) args))
                  ,@body)))
    (with-gensyms (cnt q r)
      `(multiple-value-bind (,q ,r) (truncate ,count ,unroll)
         (declare (fixnum ,q ,r))
         (do ((,cnt 0 (the fixnum (1+ ,cnt))) (,var ,index-origin))
             ((>= ,cnt ,q) (loop repeat ,r do (progn ,@body (setf (the fixnum ,var) (the fixnum (1+ ,var))))))
           (declare (fixnum ,cnt ,var))
           ,@(loop repeat unroll append `(,@body (setf (the fixnum ,var) (the fixnum (1+ ,var))))))))))

(declaim (inline println))
(defun println (obj &optional (stream *standard-output*))
  (let ((*read-default-float-format* 'double-float))
    (prog1 obj
      (princ obj stream)
      (terpri))))

(defun read-nums (count &optional (element-type '(simple-array fixnum (*))))
  (declare (fixnum count))
  (coerce (loop repeat count collect (read)) element-type))

(define-modify-macro maxf (var) max)
(define-modify-macro minf (var) min)

(defconstant +inf+ #.(expt 10 14))

;;;
;;; Body
;;;

(defmacro m-labels (((fn-name args &body fn-body)) &body body)
  (let ((val (gensym))
        (memo (gensym)))
    `(let ((,memo (make-hash-table :test #'equal)))
       (labels ((,fn-name ,args
                  (let ((,val (gethash (list  ,@args) ,memo)))
                    (when ,val (return-from ,fn-name ,val))
                    (setf (gethash (list ,@args) ,memo)
                          (progn
                            ,@fn-body)))))
         ,@body))))

(defmacro dolists ((&rest forms) &body body)
  (reduce (lambda (res f)
            ())
          forms
          :initial-value body))

(defun main ()
  (declare #.*opt*)
  (let ((n (read)))
    (let ((mat (make-array (list n n) :element-type 'fixnum :initial-element 0)))
      (dotimes (i n)
        (let ((tmp (coerce (read-line) 'simple-base-string)))
          (dotimes (j n)
            (setf (aref mat i j)
                  (ecase (schar tmp j)
                    (#\B 0)
                    (#\W 1)
                    (#\? 2))))))
      (m-labels ((rec (y x c cy cx)
                   (cond
                     ((= y x 0)
                      0)
                     ((or (minusp y)
                          (minusp x))
                      0)
                     ((\= 2 (aref mat y x))
                      (let ((c (aref mat y x)))
                        (loop for cy below 2
                              maximize (loop for cx below 2
                                             maximize (loop for (dy dx) in '((0 -1)
                                                                             (-1 0))
                                                            for ny = (+ y dy)
                                                            for nx = (+ x dx)
                                                            maximize (+ (rec ny nx c cy cx)
                                                                        (if (/= c cy)
                                                                            1
                                                                            0)
                                                                        (if (/= c cx)
                                                                            1
                                                                            0)))))))
                     (t (loop for c below 2
                              maximize (loop for cy below 2
                                             maximize (loop for cx below 2
                                                            maximize (loop for (dy dx) in '((0 -1)
                                                                                            (-1 0))
                                                                           for ny = (+ y dy)
                                                                           for nx = (+ x dx)
                                                                           maximize (+ (rec ny nx c cy cx)
                                                                                       (if (/= c cy)
                                                                                           1
                                                                                           0)
                                                                                       (if (/= c cx)
                                                                                           1
                                                                                           0))))))))))
        (println (loop for c below 2 maximize
                                     (loop for cy below 2 maximize
                                                          (loop for cx below 2 maximize
                                                                               (loop for dy below 2 maximize
                                                                                                    (loop for dx below 2 maximize
                                                                                                                         (rec (1- n) (1- n) c cy cx)))))))))))

#-swank
(progn
  (setf (sb-alien:extern-alien "thread_control_stack_size" sb-kernel::os-vm-size-t)
        (* 256 1024 1024))
  (sb-thread:join-thread (sb-thread:make-thread #'main)))
