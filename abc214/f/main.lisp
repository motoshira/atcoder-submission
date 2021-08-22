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

;;;
;;; Beginning of inserted contents
;;;

;; modint functions

(define-symbol-macro *mod* 1000000007)
;; (define-symbol-macro *mod* 998244353)

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


;;---------------------------------Body---------------------------------

(in-package #:cl-user)

(declaim (inline println))
(defun println (obj &optional (stream *standard-output*))
  (let ((*read-default-float-format* 'double-float))
    (prog1 obj
      (princ obj stream)
      (terpri stream))))

(defconstant +magic+ 26)

(declaim (inline char->int))
(defun char->int (c)
  (- (char-code c)
     #.(char-code #\a)))

(defun next-indexes-accessor (string)
  (let* ((n (length string))
         (ss (map 'vector #'char->int string))
         (res (make-array (list (1+ n)
                                +magic+)
                          :element-type '(or null fixnum)
                          :initial-element nil)))
    (declare ((simple-array (or null fixnum) (* *)) res))
    (loop for i of-type fixnum from (1- n) downto 0
          do (dotimes (j 26)
               (declare (fixnum j))
               (setf (aref res i j)
                     (aref res (1+ i) j)))
             (setf (aref res i (aref ss i))
                   i))
    (lambda (idx j)
      (declare (fixnum idx j))
      (the (or null fixnum)
           (aref (the (simple-array (or null fixnum) (* *))
                      res)
                 idx
                 j)))))

(defun main ()
  (let* ((s (read-line))
         (n (length s))
         (getter (next-indexes-accessor s)))
    (declare (string s)
             (fixnum n)
             ((function (fixnum fixnum) (or null fixnum)) getter))
    (let ((dp (make-array (list (+ n 2)
                                +magic+)
                          :element-type 'fixnum
                          :initial-element 0)))
      (declare ((simple-array fixnum (* *)) dp))
      (dotimes (j +magic+)
        (let ((next (funcall getter 0 j)))
          (when next
            (setf (aref dp next j)
                  1))))
      (dotimes (i n)
        (declare (fixnum ))
        (dotimes (j +magic+)
          (declare (fixnum j))
          (dotimes (k +magic+)
            (declare (fixnum k))
            (let* ((next? (funcall getter (1+ i) k))
                   (next (if (and next?
                                  (= next? (1+ i)))
                             (funcall getter (1+ next?) k)
                             next?)))
              (when next
                (incmodf (aref dp next k)
                    (aref dp i j)))))))
      #+swank (println dp)
      (let ((res 0))
        (declare (fixnum res))
        (dotimes (i n)
          (dotimes (j +magic+)
            (incmodf (the fixnum res)
                (the fixnum (aref dp i j)))))
        (println res)))))

#-swank (main)
