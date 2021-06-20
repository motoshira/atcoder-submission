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

(defun solve (n xs)
  (let ((ys (loop for (l r) in (sort xs #'< :key #'first)
                  with res = nil
                  with cnt = 0
                  with tmp = nil
                  do (cond
                       ((or (null tmp)
                            (<= (first tmp) r)
                            (<= l (second tmp)))
                        (let ((ll (if (null tmp)
                                      l
                                      (min l (first tmp))))
                              (rr (if (null tmp)
                                      r
                                      (max r (second tmp)))))
                          (setf tmp (list ll rr))
                          (incf cnt)))
                       (:else
                        (push (list cnt tmp) res)
                        (setf cnt 1
                              tmp nil)))
                  finally (progn
                            (push (list cnt tmp) res)
                            (return res)))))
    (let ((res nil)
          (now 0))
      (loop for (cnt _) in ys
            do (let ((tmp nil))
                 (loop for val in res
                       do (cond
                            ((>= cnt 2)
                             (push (logxor val cnt)
                                   res)
                             (Push (logxor val cnt)))))))
      (if (zerop res)
          "Bob"
          "Alice"))))

(defun main ()
  (loop repeat (read)
        for n = (read)
        for xs = (loop repeat n collect (list (read)
                                              (read)))
        do (println (solve n xs))))

#-swank (main)
