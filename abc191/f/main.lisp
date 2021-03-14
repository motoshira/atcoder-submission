#+swank (declaim (optimize (speed 3) (safety 2)))
#-swank (declaim (optimize (speed 3) (safety 0) (debug 0)))

(defmacro do-iota ((var count &optional (begin 0) (step 1)) &body body)
  (let ((cnt (gensym)))
    `(loop for ,cnt of-type fixnum below ,count
           with ,var of-type fixnum = ,begin
           do ,@body
              (incf (the fixnum ,var) (the fixnum ,step)))))

(defmacro do-rep (count &body body) `(loop repeat ,count do ,@body))

(declaim (inline println))
(defun println (obj &optional (stream *standard-output*))
  (let ((*read-default-float-format* 'double-float))
    (princ obj stream)
    (terpri)))

(declaim (inline enumerate-divisor))
(defun enumerate-divisor (k)
  (declare (inline sort sb-impl::stable-sort-list)
           (fixnum k))
  (let ((acc nil)
        (m (isqrt k)))
    (loop for x from 1 to m
          when (zerop (rem k x))
            do (if (= (* x x) k)
                   (push x acc)
                   (progn
                     (push x acc)
                     (push (floor k x) acc))))
    (sort acc #'<)))

(defun main ()
  (let* ((n (read))
         (as (loop repeat n collect (read)))
         (cands (let ((acc nil)
                      (table (make-hash-table :test #'eq)))
                  (dolist (a as acc)
                    (loop for x in (enumerate-divisor a)
                          when (not (gethash x table))
                            do (push x acc)
                            and do (setf (gethash x table) t))))))
    (println cands)
    (let ((memo (make-hash-table :test #'eq)))
      (labels ((rec (x)
                 (or (gethash x memo)
                     (setf (gethash x memo)
                           (cond
                             ((<= x a-min)
                              (some (lambda (y)
                                      (= (gcd y)
                                         x)))))))))
        (println (count-if #'rec cands))))))

#+swank
(load "~/Dropbox/Code/atcoder/ac-tools/act.lisp")
#-swank
(progn
  (setf (sb-alien:extern-alien "thread_control_stack_size" sb-kernel::os-vm-size-t)
        (* 256 1024 1024))
  (sb-thread:join-thread (sb-thread:make-thread #'main)))
