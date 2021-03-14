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

(defmacro with-gensyms ((&rest args) &body body)
  `(let ,(mapcar (lambda (arg) `(,arg (gensym))) args)
     ,@body))

(defmacro do-bfs ((&rest args) &body body)
  (with-gensyms (front tail empty-p pop!)
    (let ((vars (mapcar #'first args))
          (inits (mapcar #'second args))
          (types (mapcar #'third args)))
      `(let ((,front nil)
             (,tail nil))
         (declare (list ,front ,tail))
         (labels ((call (&rest args)
                    (push args ,tail))
                  (,empty-p ()
                    (and (null ,front)
                         (null ,tail)))
                  (,pop! ()
                    (when (null ,front)
                      (setf ,front (nreverse ,tail)
                            ,tail nil))
                    (pop ,front)))
           (declare (inline call ,empty-p ,pop!))
           (call ,@inits)
           (loop until (,empty-p)
                 for ,vars
                   of-type ,(or types (loop repeat (length args) collect t)) = (,pop!)
                 do ,@body))))))


(defun main ()
  (declare #.*opt*)
  (let ((h (read))
        (w (read)))
    (let ((mat (make-array (list h w) :element-type 'bit)))
      (dotimes (i h)
        (dotimes (j w (read-char))
          (setf (aref mat i j)
                (if (char= #\# (read-char))
                    1
                    0))))
      (let ((visited (make-array (list h w) :element-type 'bit :initial-element 0))
            (res -1))
        (do-bfs ((y 0)
                 (x 0)
                 (cnt 1))
          (when (zerop (aref visited y x))
            (cond
              ((and (= y (1- h))
                    (= x (1- w)))
               (setf res (- (loop for y below h
                                  sum (loop for x below w
                                            if (zerop (aref mat y x))
                                              count x))
                            cnt))
               (return))
              (t (setf (aref visited y x) 1)
                 (dolist (d '((1 0)
                              (0 1)
                              (-1 0)
                              (0 -1)))
                   (destructuring-bind (dy dx) d
                     (let ((ny (+ y dy))
                           (nx (+ x dx)))
                       (when (and (<= 0 ny (1- h))
                                  (<= 0 nx (1- w))
                                  (zerop (aref mat ny nx))
                                  (zerop (aref visited ny nx)))
                         (call ny nx (1+ cnt))))))))))
        (println res)))))

#-swank
(progn
  (setf (sb-alien:extern-alien "thread_control_stack_size" sb-kernel::os-vm-size-t)
        (* 256 1024 1024))
  (sb-thread:join-thread (sb-thread:make-thread #'main)))
