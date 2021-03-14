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

(defmacro defconstant* (name value &optional doc)
  "Ensure VALUE is evaluated only once."
  ;; Ref: https://g000001.cddddr.org/1260010568
  `(defconstant ,name (if (boundp ',name)
                          (symbol-value ',name)
                          ,value)
     ,@(when doc (list doc))))

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

(defun solve (n k as)
  (declare ((unsigned-byte 8) n k)
           ((simple-array fixnum (*)) as))
  (loop for mask of-type fixnum below (ash 1 n)
        when (>= (logcount mask) k)
          minimize (nlet rec ((idx 0)
                              (current-max 0)
                              (acc 0))
                     (declare (fixnum idx current-max acc))
                     (cond
                       ((= idx n) acc)
                       ((logbitp idx mask)
                        (let ((new-max (max (1+ current-max)
                                            (aref as idx))))
                          (rec (1+ idx)
                               new-max
                               (+ acc
                                  (max (1+ (- current-max
                                              (aref as idx)))
                                       0)))))
                       (t (let ((new-max (max current-max
                                              (aref as idx))))
                            (rec (1+ idx)
                                 new-max
                                 acc)))))))

(defun main ()
  (declare #.*opt*)
  (let ((n (read))
        (k (read)))
    (declare (fixnum n k))
    (let ((as (make-array n :element-type 'fixnum)))
      (dotimes (i n)
        (setf (aref as i)
              (read)))
      (dbg as)
      (println (solve n k as)))))

#-swank
(progn
  (setf (sb-alien:extern-alien "thread_control_stack_size" sb-kernel::os-vm-size-t)
        (* 256 1024 1024))
  (sb-thread:join-thread (sb-thread:make-thread #'main)))
