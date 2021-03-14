(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *opt*
    #+swank '(optimize (speed 3) (safety 2))
    #-swank '(optimize (speed 3) (safety 0) (debug 0)))
  #+swank (load "~/Dropbox/Code/atcoder/ac-tools/act.lisp")
  #+swank (ql:quickload :prove)
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
;;; BOF
;;;

;; Skew Heap

(defstruct (heap (:constructor make-heap (val &key l r)))
  (val 0 :type list)
  (l nil :type (or null heap))
  (r nil :type (or null heap)))

(defun heap-empty-p (heap)
  (null heap))

(defun heap-meld (l r)
  (declare ((or null heap) l r))
  (the (or null heap)
       (cond
         ((null l) r)
         ((null r) l)
         (:else (when (> (the fixnum (first (heap-val l)))
                         (the fixnum (first (heap-val r))))
                  (rotatef l r))
                (setf (heap-r l) (heap-meld (heap-r l) r))
                (rotatef (heap-l l) (heap-r l))
                l))))

(defmacro heap-push! (heap item)
  (multiple-value-bind (args argvs val setter getter)
      (get-setf-expansion heap)
    `(locally (declare ((or null heap) heap)
                       (list item))
       (let ,(mapcar #'list args argvs)
         (let ((,@val (heap-meld (make-heap ,item) ,getter)))
           ,setter)))))

(defmacro heap-pop! (heap)
  (let ((item (gensym)))
    (multiple-value-bind (args argvs val setter getter)
        (get-setf-expansion heap)
      `(locally (declare ((or null heap) heap)
                         (list item))
         (let ,(mapcar #'list args argvs)
           (let ((,item (heap-val ,getter))
                 (,@val (heap-meld (heap-l ,getter)
                                   (heap-r ,getter))))
             ,setter
             ,item))))))

;;;
;;; EOF
;;;



;;;
;;; Body
;;;

(sb-int:defconstant-eqx +dy-dx+
    '((1 0)
      (0 1)
      (-1 0)
      (0 -1))
  #'equal)


(defun main ()
  (declare #.*opt*)
  (let ((h (read))
        (w (read)))
    (declare (fixnum h w))
    (let ((mat (make-array (list h w)
                           :element-type 'base-char))
          (tele (make-array 26 :element-type 'list :initial-element nil))
          sy sx gy gx)
      (declare ((simple-array base-char (* *)) mat)
               ((simple-array list (*)) tele)
               ((or null fixnum) sy sx gy gx))
      (dotimes (i h)
        (let ((tmp (coerce (read-line) 'simple-base-string)))
          (dotimes (j w)
            (setf (aref mat i j)
                  (schar tmp j))
            (case (schar tmp j)
              (#\S (setf sy i
                         sx j))
              (#\G (setf gy i
                         gx j)))
            (when (lower-case-p (schar tmp j))
              (push (list i j)
                    (aref tele (- (char-code (char tmp j))
                                  #.(char-code #\a))))))))
      (let ((heap nil)
            (costs (make-array (list h w)
                               :element-type 'fixnum
                               :initial-element +inf+)))
        (declare ((or null heap) heap)
                 ((simple-array fixnum (* *)) costs))
        (heap-push! heap (list 0 sy sx))
        (labels ((valid-p (y x)
                   (declare (fixnum y x))
                   (and (<= 0 y (1- h))
                        (<= 0 x (1- w))
                        (char/= #\#
                                (aref mat y x)))))
          (declare (inline valid-p))
          (loop while (not (heap-empty-p heap))
                for (cost y x) of-type (fixnum fixnum fixnum) = (heap-pop! heap)
                do (when (and (= y gy)
                                (= x gx))
                       (println cost)
                       (return-from main))
                     (when (<= cost (aref costs y x))
                       (setf (aref costs y x)
                             cost)
                       (dolist (d +dy-dx+)
                         (let ((dy (first d))
                               (dx (second d)))
                           (declare (fixnum dy dx))
                           (let ((ny (+ y dy))
                                 (nx (+ x dx)))
                             (declare (fixnum ny nx))
                             (when (valid-p ny nx)
                               (let ((ncost (1+ cost)))
                                 (when (< ncost (aref costs ny nx))
                                   (setf (aref costs ny nx)
                                         ncost)
                                   (heap-push! heap (list ncost
                                                          ny
                                                          nx))))))))
                       (when (lower-case-p (aref mat y x))
                         (let ((c-id (- (char-code (aref mat y x))
                                        #.(char-code #\a)))
                               (ncost (1+ cost)))
                           (declare ((integer 0 26) c-id)
                                    (fixnum ncost))
                           (dolist (nxt (aref tele c-id))
                             (destructuring-bind (ny nx) nxt
                               (declare (fixnum ny nx))
                               (when (< ncost (aref costs ny nx))
                                 (setf (aref costs ny nx)
                                       ncost)
                                 (heap-push! heap (list ncost
                                                       ny
                                                       nx)))))
                           (setf (aref tele c-id) nil)))))
          (println -1))))))

#-swank
(progn
  (setf (sb-alien:extern-alien "thread_control_stack_size" sb-kernel::os-vm-size-t)
        (* 256 1024 1024))
  (sb-thread:join-thread (sb-thread:make-thread #'main)))
