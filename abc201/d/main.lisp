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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *dy-dx* '((0 1) (1 0))))

(define-modify-macro maxf (var) max)

(defparameter *inf* (expt 10 12))

(declaim (inline encode))
(defun encode (x y)
  (declare (fixnum x y))
  (the fixnum
       (+ (the fixnum (* x 10000))
          y)))

(defun main ()
  (let* ((h (read))
         (w (read))
         (mat (make-array (list h w))))
    (dotimes (i h)
      (let ((tmp (read-line)))
        (dotimes (j w)
          (setf (aref mat i j)
                (if (char= #\+ (char tmp j))
                    1
                    -1)))))
    (let ((memo (make-hash-table :test #'eq)))
      (labels ((goal-p (y x)
                 (and (= y (1- h))
                      (= x (1- w))))
               (valid-p (y x)
                 (and (<= 0 y (1- h))
                      (<= 0 x (1- w))))
               (rec (y x)
                 (or #1=(gethash (encode y x) memo)
                     (setf #1#
                           (cond
                             ((goal-p y x) 0)
                             ((not (valid-p y x)) (- *inf*))
                             (:else
                              (loop for (dy dx) in *dy-dx*
                                    for ny = (+ y dy)
                                    for nx = (+ x dx)
                                    when (valid-p ny nx)
                                    maximize (cond
                                               ((goal-p ny nx) 0)
                                               (:else
                                                (loop for (ddy ddx) in *dy-dx*
                                                      for nny = (+ ny ddy)
                                                      for nnx = (+ nx ddx)
                                                      when (valid-p nny nnx)
                                                      minimize (+ (aref mat ny nx)
                                                                  (- (aref mat nny nnx))
                                                                  (rec nny nnx))))))))))))
        (let ((point (rec 0 0)))
          (println
           (cond
             ((plusp point) "Takahashi")
             ((zerop point) "Draw")
             (:else "Aoki"))))))))

#-swank (main)
