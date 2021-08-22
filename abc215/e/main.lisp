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

;;;
;;; BOF
;;;

(defpackage :rolling-hash-table
  (:nicknames :rhs)
  (:use :cl)
  (:export :make-rolling-hash-table
           :get-val
           :count-substrings
           :get-lowest-common-prefix))


(in-package :rolling-hash-table)

(defconstant +modulo+ (1- (ash 1 61)))
(defparameter *base* 1007)

(defstruct (rolling-hash-table (:conc-name rhs-)
                               (:constructor %make-rhs))
  (hash nil :type (simple-array fixnum 1))
  (pow nil :type (simple-array fixnum 1))
  (length nil :type fixnum))

(declaim (ftype (function (string) rolling-hash-table) make-rolling-hash-table))
(defun make-rolling-hash-table (string)
  (let ((n (length string)))
    (declare (fixnum n))
    (let ((rhs (%make-rhs :hash (make-array (1+ n)
                                            :element-type 'fixnum
                                            :adjustable nil
                                            :initial-element 0)
                          :pow (make-array (1+ n)
                                           :element-type 'fixnum
                                           :adjustable nil
                                           :initial-element 1)
                          :length n)))
      (loop for i of-type fixnum below n
            do (setf (aref (rhs-hash rhs) (1+ i)) (rem (+ (* (aref (rhs-hash rhs) i)
                                                              *base*)
                                                           (char-code (char string i)))
                                                       +modulo+)
                     (aref (rhs-pow rhs) (1+ i)) (rem (* (aref (rhs-pow rhs) i)
                                                         *base*)
                                                      +modulo+)))
      rhs)))

()
(defmethod get-val ((rhs rolling-hash-table)
                    (l fixnum)
                    (r fixnum))
  (let ((res (- (aref (rhs-hash rhs) r)
                 (rem (* (aref (rhs-hash rhs) l)
                         (aref (rhs-pow rhs) (- r l)))
                      +modulo+))))
    (if (minusp res)
        (+ res +modulo+)
        res)))

(defmethod count-substrings ((mainstr string)
                             (substr string))
  (let* ((rhs-main (make-rolling-hash-table mainstr))
         (rhs-sub (make-rolling-hash-table substr))
         (n (rhs-length rhs-main))
         (m (rhs-length rhs-sub)))
    (loop for i of-type fixnum
            from 0 to (- n m)
          with h1 of-type fixnum = (get-val rhs-sub 0 m)

          for h2 of-type fixnum = (get-val rhs-main i (+ i m))
          when (= h1 h2)
            count i)))

(defmethod get-lowest-common-prefix ((idx1 fixnum)
                                     (idx2 fixnum)
                                     (rhs rolling-hash-table))
  (labels ((%get-lcp (ok ng)
             (if (<= (abs (- ok ng)) 1)
                 ok
                 (let ((mid (ash (+ ok ng) -1)))
                   (if (= (get-val rhs idx1 (+ idx1 mid))
                          (get-val rhs idx2 (+ idx2 mid)))
                       (%get-lcp mid ng)
                       (%get-lcp ok mid))))))
    (%get-lcp 0 (- (rhs-length rhs) (max idx1 idx2)))))

;;;
;;; EOF
;;;


(in-package #:cl-user)

;;;
;;; Beginning of inserted contents
;;;

;; modint functions

;; (define-symbol-macro *mod* 1000000007)
(define-symbol-macro *mod* 998244353)

(deftype mint () `(integer 0 #.(1- *mod*)))

(declaim (inline modint)
         (ftype (function (integer) mint) modint))
(defun modint (integer)
  ;; (integer) -> (mint)
  (declare (integer integer))
  (loop while (minusp integer)
        do (incf integer *mod*))
  (the mint
       (if (< integer *mod*)
           integer
           (mod integer *mod*))))

(defmacro define-modulo-operation (fn-name op-long op-short)
  `(progn
     ;; (&REST mint) -> (mint)
     (declaim (ftype (function (&rest mint) mint) ,fn-name)
              (inline ,fn-name))
     (defun ,fn-name (&rest args)
       (reduce (lambda (x y)
                 ,op-long)
               (rest args)
               :initial-value (first args)))

     (define-compiler-macro ,fn-name (&whole form &rest args)
       (if (< (length args) 10)
           (reduce (lambda (x y)
                     ,op-short)
                   (rest args)
                   :initial-value (first args))
           form))))



(define-modulo-operation mod+ (modint (+ x y)) `(modint (+ ,x ,y)))
(define-modulo-operation mod- (modint (- x y)) `(modint (- ,x ,y)))
(define-modulo-operation mod* (modint (* x y)) `(modint (* ,x ,y)))
(define-modulo-operation mod/ (modint (* x (mod-inv y))) `(modint (* ,x (mod-inv ,y))))

(declaim (ftype (function (fixnum) mint) mod-inv))
(defun mod-inv (a)
  "Reference:https://qiita.com/drken/items/3b4fdf0a78e7a138cd9a"
  (declare (fixnum a)
           (optimize (speed 3) (safety 2)))
  (let ((b *mod*)
        (u 1)
        (v 0))
    (declare (fixnum b u v))
    (loop until (zerop b) do
      (let ((w (truncate a b)))
        (declare (fixnum w))
        (decf a (the fixnum (* w b)))
        (rotatef a b)
        (decf u (the fixnum (* w v)))
        (rotatef u v)))
    (modint u)))

(declaim (ftype (function (mint (integer 0)) mint) mod-power)
         (inline mod-power))
(defun mod-power (base power)
  ;; Reference:https://qiita.com/drken/items/3b4fdf0a78e7a138cd9a
  (declare (mint base)
           ((integer 0) power))
  (loop while (plusp power)
        with res of-type mint = 1
        do (psetq base (the mint (mod* base base))
                  power (the (integer 0) (ash power -1))
                  res (the mint (if (logbitp 0 power)
                                    (mod* res base)
                                    res)))
        finally (return res)))

(define-modify-macro incmodf (&optional (val 1)) (lambda (place val) (mod+ place val)))
(define-modify-macro decmodf (&optional (val 1)) (lambda (place val) (mod- place val)))
(define-modify-macro mulmodf (&optional (val 1)) (lambda (place val) (mod* place val)))
(define-modify-macro divmodf (&optional (val 1)) (lambda (place val) (mod/ place val)))

(declaim (ftype (function (mint) (simple-array mint (*))) make-mod-table)
         (inline make-mod-fact-table))
(defun make-mod-fact-table (size)
  (declare (mint size))
  (let ((table (make-array (1+ size)
                           :element-type 'mint)))
    (declare ((simple-array mint (*)) table))
    (setf (aref table 0) 1)
    (loop for i of-type fixnum below size
          do (setf (aref table (1+ i))
                   (mod* (aref table i)
                         (the mint (1+ i)))))
    table))

(declaim (ftype (function (mint mint (simple-array mint (*))) mint) mod-combi-with-table)
         (inline mod-combi-with-table))
(defun mod-combi-with-table (n k table)
  (declare (mint n k)
           ((simple-array mint (*)) table))
  (the mint
       (if (or (< n k)
               (< n 0)
               (< k 0))
           0
           (mod* (aref table n)
                 (mod-inv (aref table k))
                 (mod-inv (aref table (the mint (- n k))))))))


;;;
;;; End of inserted contents
;;;

(declaim (inline println))
(defun println (obj &optional (stream *standard-output*))
  (let ((*read-default-float-format* 'double-float))
    (prog1 obj
      (princ obj stream)
      (terpri stream))))

(declaim (inline encode))
(defun encode (x y)
  (declare (fixnum x y))
  (the fixnum
       (+ (the fixnum (* 1010 x))
          y)))

(defun main ()
  (let* ((n (read))
         (s (read-line))
         (rhs (rhs:make-rolling-hash-table s))
         (checked (make-hash-table :test #'eq))
         (res 0))
    (declare (fixnum res))
    (loop for d from 10 downto 1
          do (dotimes (begin n)
               (when (and (<= (+ begin d)
                              n)
                          (not (gethash (encode begin (+ begin d)) checked)))
                 (let ((cnt 0)
                       (pos begin)
                       (otehon (rhs:get-val rhs begin (+ begin d))))
                   (loop
                     (setf (gethash (encode pos (+ pos d)) checked)
                           t)
                     (cond
                       ((and (<= (+ pos d)
                                 n)
                             (= (rhs:get-val rhs pos (+ pos d))
                                otehon))
                        (incf cnt)
                        (incf pos d))
                       (:else
                        (incmodf res (mod/ (mod* cnt (mod+ cnt 1))
                                           2))
                        (return))))))))
    (println res)))

#-swank (main)
