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

(defparameter ints (loop for x from 0 to 9 collect x))

(defun char->int (c)
  (- (char-code c)
     (char-code #\0)))


(defun main ()
  (let* ((s (read-line))
         (os nil)
         (xs nil)
         (es nil))
    (loop for c across s
          for x from 0
          do (ecase c
               (#\o (push x os))
               (#\x (push x xs))
               (#\? (push x es))))
    (flet ((judge (ss)
             (and (every (lambda (n)
                           (some (lambda (c)
                                   (= (char->int c) n))
                                 ss))
                         os)
                  (every (lambda (n)
                           (not (some (lambda (c)
                                        (= (char->int c) n))
                                      ss)))
                         xs))))
      (println
       (loop for x1 in ints
             sum (loop for x2 in ints
                       sum (loop for x3 in ints
                                 sum (loop for x4 in ints
                                           for ss = (with-output-to-string (m)
                                                      (dolist (x (list x1 x2 x3 x4))
                                                        (princ x m)))
                                           when (judge ss)
                                             count ss))))))))

#-swank (main)
