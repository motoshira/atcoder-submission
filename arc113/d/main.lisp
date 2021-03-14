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
;;; Beginning of inserted contents
;;;

;; modint functions

;; (defconstant +mod+ 1000000007)
(defconstant +mod+ 998244353)

(declaim (inline modint)
         (ftype (function (integer &optional fixnum) fixnum) modint))
(defun modint (integer &optional (m +mod+))
  (declare (integer integer))
  (loop while (minusp integer)
        do (incf integer m))
  (the fixnum
       (if (< integer m)
           integer
           (mod integer m))))


(defmacro define-modulo-operation (fn-name op-long op-short)
  `(progn
     (declaim (ftype (function (&rest fixnum) fixnum) ,fn-name)
              (inline ,fn-name))
     (defun ,fn-name (&rest args)
       (reduce (lambda (x y)
                 ,op-long)
               (rest args)
               :initial-value (modint (first args))))

     (define-compiler-macro ,fn-name (&whole form &rest args)
       (if (< (length args) 10)
           (reduce (lambda (x y)
                     ,op-short)
                   (rest args)
                   :initial-value `(modint ,(first args)))
           form))))



(define-modulo-operation mod+ (modint (+ x (modint y))) `(modint (+ ,x (modint ,y))))
(define-modulo-operation mod- (modint (- x (modint y))) `(modint (- ,x (modint ,y))))
(define-modulo-operation mod* (modint (* x (modint y))) `(modint (* ,x (modint ,y))))
(define-modulo-operation mod/ (modint (* x (mod-inv y))) `(modint (* ,x (mod-inv ,y))))

(declaim (ftype (function (fixnum &optional fixnum) fixnum) mod-inv))
(defun mod-inv (a &optional (m +mod+))
       "Reference:https://qiita.com/drken/items/3b4fdf0a78e7a138cd9a"
       (declare (fixnum a))
       (let ((b m)
             (u 1)
             (v 0))
         (declare (fixnum b u v))
         (loop until (zerop b) do
           (let ((w (truncate a b)))
             (decf a (* w b))
             (rotatef a b)
             (decf u (* w v))
             (rotatef u v))
               finally
                  (return (modint u)))))

(declaim (ftype (function (fixnum (integer 0)) fixnum) mod-power)
         (inline mod-power))

(define-modify-macro incmodf (&optional (val 1)) (lambda (place val) (mod+ place val)))
(define-modify-macro decmodf (&optional (val 1)) (lambda (place val) (mod- place val)))
(define-modify-macro mulmodf (&optional (val 1)) (lambda (place val) (mod* place val)))
(define-modify-macro divmodf (&optional (val 1)) (lambda (place val) (mod/ place val)))

(declaim (ftype (function (fixnum) (simple-array fixnum 1)) make-mod-table)
         (inline make-mod-table))
(defun make-mod-table (size)
  (let ((table (make-array (1+ size)
                           :element-type 'fixnum
                           :adjustable nil)))
    (setf (aref table 0) 1)
    (dotimes (i size)
      (setf (aref table (1+ i))
            (mod* (aref table i)
                  (1+ i))))
    table))

(declaim (ftype (function (fixnum fixnum (simple-array fixnum 1)) fixnum) mod-combi)
         (inline mod-combi))
(defun mod-combi (n k table)
  (declare (fixnum n k)
           ((simple-array fixnum 1) table))
  (if (or (< n k)
          (< n 0)
          (< k 0))
      0
      (the fixnum
           (mod* (aref table n)
                 (mod-inv (aref table k))
                 (mod-inv (aref table (- n k)))))))



;;;
;;; End of inserted contents
;;;


;;;
;;; Body
;;;

(defun main ()
  (declare #.*opt*)
  (let ((n (read))
        (m (read))
        (k (read)))
    (declare (fixnum n m k))
    (println (cond
               ((= m 1)
                (mod-power k n))
               ((= n 1)
                (mod-power k m))
               (t (loop for x of-type fixnum from 1 to k
                        with res of-type fixnum = 0
                        do (incmodf res
                               (mod* (mod- (mod-power x n)
                                           (mod-power (1- x) n))
                                     (mod-power (mod+ k (- x) 1)
                                                m)))
                        finally (return res)))))))

#-swank
(progn
  (setf (sb-alien:extern-alien "thread_control_stack_size" sb-kernel::os-vm-size-t)
        (* 256 1024 1024))
  (sb-thread:join-thread (sb-thread:make-thread #'main)))
