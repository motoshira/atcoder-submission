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

(defparameter *fact-table*
  (let ((arr (make-array 61)))
    (setf (aref arr 0) 1)
    (dotimes (i 60 arr)
      (setf (aref arr (1+ i))
            (* (aref arr i)
               (1+ i))))))

(defun combi (n r)
  (floor (floor (aref *fact-table* n)
                (aref *fact-table* (- n r)))
         (aref *fact-table* r)))

(defun solve (a b k)
  (let ((n (+ a b)))
    (let ((res nil)
          (a-cnt a)
          (b-cnt b)
          (cnt 0))
      (loop repeat n
            do (block continue
                 (when (and (plusp a-cnt)
                            (plusp b-cnt))
                   (let ((m (combi (+ a-cnt b-cnt -1)
                                   b-cnt)))
                     (when (<= (+ m cnt)
                               k)
                       (psetf b-cnt (1- b-cnt)
                              cnt (+ cnt m))
                       (push #\b res)
                       (return-from continue))))
                 (cond
                   ((plusp a-cnt)
                    (psetf a-cnt (1- a-cnt))
                    (push #\a res))
                   (:else
                    (setf b-cnt (1- b-cnt))
                    (push #\b res)))))
      (concatenate 'string (nreverse res)))))

(declaim (inline println))
(defun println (obj &optional (stream *standard-output*))
  (let ((*read-default-float-format* 'double-float))
    (prog1 obj
      (princ obj stream)
      (terpri stream))))

(defun main ()
  (let ((a (read))
        (b (read))
        (k (1- (read))))
    (println (solve a b k))))

#-swank (main)
