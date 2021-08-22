(in-package #:cl-user)

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

(in-package #:cl-user)

(declaim (inline read-fixnum))
(defun read-fixnum (&optional (in *standard-input*))
  ;; Ref: https://competitive12.blogspot.com/2020/03/common-lisp.html
  ;;        partially modified
  (declare (inline read-byte))
  (flet ((%read-byte ()
           (the fixnum #+swank (char-code (read-char in nil #\Nul))
                       #-swank (read-byte in nil #.(char-code #\Nul))))
         (%byte->num (b)
           (the fixnum (- b #.(char-code #\0))))
         (%digit-p (byte)
           (declare (fixnum byte))
           (<= #.(char-code #\0) byte #.(char-code #\9))))
    (declare (inline %read-byte %byte->num %digit-p))
    (let ((minus nil)
          (res 0))
      (declare (boolean minus) (fixnum res))
      (loop for byte of-type fixnum = (%read-byte)
            do (cond
                 ((%digit-p byte)
                  (setf res (%byte->num byte))
                  (return))
                 ((= byte #.(char-code #\Nul))
                  (error "EOF"))
                 ((= byte #.(char-code #\-))
                  (setf minus t))))
      (loop for byte of-type fixnum = (%read-byte)
            do (cond
                 ((%digit-p byte)
                  (setf res (the fixnum (+ (* res 10) (%byte->num byte)))))
                 (t (return))))
      (the fixnum (if minus (- res) res)))))

(declaim (inline println))
(defun println (obj &optional (stream *standard-output*))
  (let ((*read-default-float-format* 'double-float))
    (prog1 obj
      (princ obj stream)
      (terpri stream))))

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

(defun main ()
  (let* ((n (read))
         (m (read))
         (as (make-array n))
         (ok (make-array (1+ m) :initial-element t))
         (primes nil))
    (dotimes (i n)
      (setf (aref as i) (read-fixnum)))
    (sb-int:dovector (a as)
      (let ((xs (prime-factorize-to-list a)))
        (dolist (x xs)
          (push x primes))))
    (setf primes (remove-duplicates primes))
    (dolist (p primes)
      (let ((y p))
        (loop while (<= y m)
              do (setf (aref ok y) nil)
                 (incf y p))))
    (println (count t (subseq ok 1)))
    (loop for x from 1 to m
          when (aref ok x)
            do (println x))))

#-swank (main)
