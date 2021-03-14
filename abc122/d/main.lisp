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

;;;
;;; Beginning of inserted contents
;;;

;; modint functions

(defconstant +mod+ 1000000007)
;; (defconstant +mod+ 998244353)

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
(defun mod-power (base power)
  ;; Reference:https://qiita.com/drken/items/3b4fdf0a78e7a138cd9a
  (declare (fixnum base)
           ((integer 0) power))
  (loop while (plusp power)
        with res of-type fixnum = 1
        do (when (logbitp 0 power)
             (setf res (mod* res base)))
           (setf base (mod* base base))
           (setf power (ash power -1))
        finally
           (return res)))

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


(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (inline calc))
  (defun calc (ppp pp p now)
    (declare (fixnum ppp pp p now))
    (the fixnum 
         (+ (* ppp 1000)
            (* pp 100)
            (* p 10)
            now)))
  (defconstant +a+ 0)
  (defconstant +c+ 1)
  (defconstant +g+ 2)
  (defconstant +t+ 3)
  (sb-int:defconstant-eqx +ng-list+
      (remove-duplicates
       `(,@(loop for n below 4 collect (calc +a+ +g+ +c+ n))
           ,@(loop for n below 4 collect (calc +g+ +a+ +c+ n))
           ,@(loop for n below 4 collect (calc +a+ +c+ +g+ n))
           ,@(loop for n below 4 collect (calc n +a+ +g+ +c+))
           ,@(loop for n below 4 collect (calc n +g+ +a+ +c+))
           ,@(loop for n below 4 collect (calc n +a+ +c+ +g+))
           ,@(loop for n below 4 collect (calc +a+ +g+ n +c+))
           ,@(loop for n below 4 collect (calc +a+ n +g+ +c+))))
    #'equal))

(defun main ()
  (declare #.*opt*)
  (let* ((n (read))
         (dp (make-array (list (1+ n) 4 4 4 4) :element-type 'fixnum
                                               :initial-element 0)))
    (declare (fixnum n)
             ((simple-array fixnum (* 4 4 4 4)) dp))
    (macrolet ((ng (ppp pp p now)
                 `(let ((x (calc ppp pp p now)))
                    (declare (fixnum x))
                    
                    (or ,@(loop for ng in +ng-list+
                                collect `(= x ,ng))))))
      (dotimes (pp 4)
        (dotimes (p 4)
          (dotimes (now 4)
            (let ((x (+ (* pp 100)
                        (* p 10)
                        now)))
              (declare (fixnum x))
              (unless (member x '(21 12 201))
                (setf (aref dp 3 +t+ pp p now) 1))))))
      (loop for i of-type fixnum from 3 below n do
        (loop for ppp of-type fixnum below 4 do
          (loop for pp of-type fixnum below 4 do
            (loop for p of-type fixnum below 4 do
              (loop for now of-type fixnum below 4 do
                (unless (ng ppp pp p now)
                  (loop for pppp of-type fixnum below 4
                        with tmp of-type fixnum = 0 do
                          (unless (ng pppp ppp pp p)
                            (incmodf tmp
                                (aref dp i pppp ppp pp p)))
                        finally (incmodf (aref dp (1+ i) ppp pp p now) tmp))))))))
      (dbg dp)
      (let ((res 0))
        (declare (fixnum res))
        (dotimes (ppp 4)
          (dotimes (pp 4)
            (dotimes (p 4)
              (dotimes (now 4)
                (unless (ng ppp pp p now)
                  (incmodf res
                      (aref dp n ppp pp p now)))))))
        (println res)))))

#-swank
(progn
  (setf (sb-alien:extern-alien "thread_control_stack_size" sb-kernel::os-vm-size-t)
        (* 256 1024 1024))
  (sb-thread:join-thread (sb-thread:make-thread #'main)))
