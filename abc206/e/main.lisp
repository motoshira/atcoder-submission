(in-package :cl-user)

;;------------------------------Preferences------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+swank (declaim (optimize (speed 3) (safety 2)))
  #-swank (declaim (optimize (speed 3) (safety 0) (debug 0)))
  #+swank (load "~/ghq/github.com/motoshira/atcoder-submission/ac-tools/act.lisp")
  #+swank (ql:quickload :prove)
  #-swank (declaim (sb-ext:muffle-conditions sb-ext:compiler-note))
  #-swank (sb-ext:disable-debugger)
  (pushnew :inline-generic-funcion *features*))

;;---------------------------------Body---------------------------------

;; Functions for prime

(declaim (inline prime-factorize-to-list))
(defun prime-factorize-to-list (integer)
  ;; 素因数分解分解してリストで返す(昇順)
  (declare ((integer 0) integer))
  (the list
       (if (<= integer 1)
           nil
           (loop
              while (<= (* f f) integer)
              with acc list = nil
              with f integer = 2
              do
                (if (zerop (rem integer f))
                    (progn
                      (push f acc)
                      (setq integer (floor integer f)))
                    (incf f))
              finally
                (when (/= integer 1)
                  (push integer acc))
                (return (reverse acc))))))

(declaim (inline prime-p))
(defun prime-p (integer)
  (declare ((integer 1) integer))
  (if (= integer 1)
      nil
      (loop
         with f = 2
         while (<= (* f f) integer)
         do
           (when (zerop (rem integer f))
             (return nil))
           (incf f)
         finally
           (return t))))

(defun enumerate-divisor (k)
  (if (= k 1)
      (list 1)
      (labels ((sub (k d acc)
                 (cond
                   ((> (* d d)
                       k)
                    (sort acc #'<))
                   ((zerop (rem k d))
                    (sub k
                         (1+ d)
                         (if (= (* d d)
                                k)
                             (cons d acc)
                             (cons d
                                   (cons (floor k d)
                                         acc)))))
                   (t
                    (sub k
                         (1+ d)
                         acc)))))
        (sub k 1 nil))))


(declaim (inline println))
(defun println (obj &optional (stream *standard-output*))
  (let ((*read-default-float-format* 'double-float))
    (prog1 obj
      (princ obj stream)
      (terpri stream))))

(defmacro dbg (&rest forms)
  #-swank (declare (ignore forms))
  #+swank `(format *error-output* "~a => ~a~&" ',forms `(,,@forms)))

#+swank
(defun take (fn amount &optional (index-origin 1))
  (loop for i below amount
        for idx = (+ i index-origin)
        collect (funcall fn idx)))

#+swank
(defun take-2d (fn height width)
  (let ((res (make-array (list height width))))
    (dotimes (i height)
      (dotimes (j width)
        (setf (aref res i j)
              (funcall fn i j))))
    res))

(defmacro defun-memoize (name lambda-list &body body)
  (sb-int:with-unique-names (memo)
    `(let ((,memo (make-hash-table :test #'equal)))
       (defun ,name ,lambda-list
         (or (gethash (list ,@lambda-list) ,memo)
             (setf (gethash (list ,@lambda-list) ,memo)
                   (progn ,@body)))))))

(defun-memoize rec-all (g l r)
  (loop for p from 1 to (floor r g)
        sum (if (<= l (* g p) r)
                1
                0)))

(defun-memoize rec (g l r)
  (let ((cnt (- (rec-all g l r)
                (if (<= l g r)
                    1
                    0))))
    (- (* cnt (1- cnt))
       (loop for p from 2 to r
             for cnt = (rec-all (* p g) l r)
             when (<= l (* g p) r)
               sum (* cnt (1- cnt))))))

(defun main ()
  (let* ((l (read))
         (r (read)))
    (println (loop for g from 2 to r
                   when (prime-p g)
                     sum (rec g l r)))))

#-swank (main)


#+swank
(let ((prove:*enable-colors* nil))
  (prove:ok (= 1 1)))
