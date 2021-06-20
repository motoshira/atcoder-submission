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

(defun get-code (base power)
  (cond
    ((zerop base) :zero)
    ((plusp base) :plus)
    ((evenp power) :plus)
    (:else :minus)))

(defun main ()
  (let* ((a (read))
         (b (read))
         (c (read))
         (x-code (get-code a c))
         (y-code (get-code b c)))
    (let ((res (ecase x-code
                 (:zero ;; x = 0
                  (ecase y-code
                    (:zero "=")
                    (:plus "<")
                    (:minus ">")))
                 (:plus ;; x > 0
                  (ecase y-code
                    (:zero ">")
                    (:plus (cond
                             ((< (abs a) (abs b))
                              "<")
                             ((= (abs a) (abs b))
                              "=")
                             (:else ">")))
                    (:minus ">")))
                 (:minus ;; x < 0
                  (ecase y-code
                    (:zero "<")
                    (:plus "<")
                    (:minus (cond
                              ((< (abs a) (abs b))
                               ">")
                              ((= (abs a) (abs b))
                               "=")
                              (:else "<"))))))))
      (println res))))

#-swank (main)
