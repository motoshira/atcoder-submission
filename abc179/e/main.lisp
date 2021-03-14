(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *opt*
    #+swank '(optimize (speed 3) (safety 2))
    #-swank '(optimize (speed 3) (safety 0) (debug 0)))
  #+swank (load "~/Dropbox/Code/atcoder/ac-tools/act.lisp")
  )

(defmacro dbg (&rest forms)
  #-swank (declare (ignore forms))
  #+swank `(format *error-output* "~a => ~a~&" ',forms `(,,@forms)))

(defmacro do-iota ((var count &optional (begin 0) (step 1)) &body body)
  (let ((cnt (gensym)))
    `(loop for ,cnt of-type fixnum below ,count
           with ,var of-type fixnum = ,begin
           do ,@body
              (incf (the fixnum ,var) (the fixnum ,step)))))

(defmacro do-rep (count &body body) `(loop repeat ,count do ,@body))

(defmacro nlet (name binds &body body)
  `(labels ((,name (,@(mapcar #'first binds))
              ,@body))
     (,name ,@(mapcar #'second binds))))

(declaim (inline println))
(defun println (obj &optional (stream *standard-output*))
  (let ((*read-default-float-format* 'double-float))
    (princ obj stream)
    (terpri)))

;;;
;;; BOF
;;;


(defmacro with-doubling ((getter count dist first-lambda get-lambda &key (init 0) (element-type 'fixnum)) &body body)
  ;; (getter cnt start)
  (let ((log-count (gensym "LOG-COUNT"))
        (dists (gensym "DISTS"))
        (dub (gensym "DUB"))
        (amount (gensym))
        (pos (gensym))
        (n (gensym))
        (nn (gensym))
        (k (gensym))
        (i (gensym)))
    `(let* ((,amount (length ,dist))
            (,log-count (loop while (<= (ash 1 ,log-count) ,count)
                              with ,log-count of-type fixnum = 1
                              do (incf (the fixnum ,log-count))
                              finally (return ,log-count)))
            (,dists (make-array (list ,log-count ,amount) :element-type 'fixnum))
            (,dub (make-array (list ,log-count ,amount) :element-type ',element-type :initial-element ,init)))
       (declare (fixnum ,log-count)
                ((simple-array fixnum (* *)) ,dists)
                ((simple-array ,element-type (* *)) ,dub))
       (loop for ,i of-type fixnum below ,amount
             for ,n of-type fixnum = (aref ,dist ,i)
             do (setf (aref ,dists 0 ,i) ,n
                      (aref ,dub 0 ,i) (the ,element-type (,first-lambda ,i))))
       (loop for ,k of-type fixnum below (1- ,log-count)
             do (loop for ,pos of-type fixnum below ,amount
                      for ,n of-type fixnum = (aref ,dists ,k ,pos)
                      for ,nn of-type fixnum = (aref ,dists ,k ,n)
                      do (setf (aref ,dists (1+ ,k) ,pos) ,nn)
                      do (setf (aref ,dub (1+ ,k) ,pos)
                               (the ,element-type (,get-lambda (aref ,dub ,k ,pos)
                                                               (aref ,dub ,k ,n))))))
       (flet ((,getter (cnt start)
                (declare (fixnum cnt start))
                (loop for ,k of-type fixnum
                        below ,log-count
                      with res of-type ,element-type = ,init
                      with pos of-type fixnum = start
                      when (zerop cnt)
                        return (values res pos)
                      when (logbitp 0 cnt)
                        do (setf res (,get-lambda res
                                                  (aref ,dub ,k pos))
                                 pos (aref ,dists ,k pos))
                           
                      do (setf cnt (ash cnt -1))
                      finally (return (values res pos)))))
         ,@body))))

;;;
;;; EOF
;;;


(defun main ()
  (declare #.*opt*)
  (let* ((n (read))
         (x (read))
         (m (read))
         (dist (make-array m :element-type 'fixnum)))
    (declare (fixnum n x m)
             ((simple-array fixnum (*)) dist))
    (flet ((f (x)
             (declare (fixnum x))
             (the fixnum (mod (the fixnum (* x x)) m))))
      (declare (inline f))
      (dotimes (i m)
        (setf (aref dist i)
              (f i)))
      (with-doubling (getter
                      n
                      dist
                      identity
                      (lambda (x y)
                        (declare (fixnum x y))
                        (the fixnum (+ x y))))
        (println (getter n x))))))

#-swank
(progn
  (setf (sb-alien:extern-alien "thread_control_stack_size" sb-kernel::os-vm-size-t)
        (* 256 1024 1024))
  (sb-thread:join-thread (sb-thread:make-thread #'main)))
