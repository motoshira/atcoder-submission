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

(defstruct (pair (:constructor make-pair (&optional sup pos)))
  (sup 0 :type fixnum)
  (pos 0 :type fixnum))


(defun main ()
  (declare #.*opt*)
  (let* ((n (read))
         (ps (loop repeat (* n n) collect (read)))
         (mat (make-array (list n n) :element-type 'fixnum :adjustable nil :initial-element 1))
         (costs (make-array (list n n) :element-type 'fixnum :adjustable nil :initial-element 0))
         (res 0)
         (dy-dx (loop for (dy dx) in '((1 0)
                                       (0 1)
                                       (-1 0)
                                       (0 -1))
                      collect (make-pair dy dx))))
    (declare (fixnum n res)
             (list ps dy-dx)
             ((simple-array fixnum (* *)) mat costs))
    (dotimes (i n)
      (dotimes (j n)
        (setf (aref costs i j)
              (min i
                   j
                   (- n i 1)
                   (- n j 1)))))
    (dbg costs)
    (flet ((update! (y x)
             (declare (fixnum y x))
             (assert (= 1 (aref mat y x)))
             (setf (aref mat y x) 0)
             (nlet rec ((y y)
                        (x x)
                        (cost (1- (aref costs y x))))
               (declare (fixnum y x cost))
               (dolist (d dy-dx)
                 (with-slots ((dy sup) (dx pos)) d
                   (declare (fixnum dy dx))
                   (let ((ny (+ y dy))
                         (nx (+ x dx)))
                     (declare (fixnum ny nx))
                     (when (and (<= 0 ny (1- n))
                                (<= 0 nx (1- n))
                                (< #1=(the fixnum (+ (aref mat ny nx)
                                                     cost))
                                   (aref costs ny nx)))
                       (minf (aref costs ny nx)
                             #1#)
                       (rec ny nx #1#))))))))
      (declare (inline update!))
      (dolist (p ps)
        (declare (fixnum p))
        (multiple-value-bind (y x) (truncate (the fixnum (1- p)) n)
          (declare (fixnum y x))
          (dbg (aref costs y x))
          (incf res (aref costs y x))
          (update! y x))))
    (println res)))

#-swank
(progn
  (setf (sb-alien:extern-alien "thread_control_stack_size" sb-kernel::os-vm-size-t)
        (* 256 1024 1024))
  (sb-thread:join-thread (sb-thread:make-thread #'main)))
