(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *opt*
    #+swank '(optimize (speed 3) (safety 2))
    #-swank '(optimize (speed 3) (safety 0) (debug 0)))
  #+swank (load "~/Dropbox/Code/atcoder/ac-tools/act.lisp")
  #+swank (ql:quickload :prove)
  #-swank (declaim (sb-ext:muffle-conditions sb-ext:compiler-note))
  #-swank (sb-ext:disable-debugger)
  (pushnew :inline-generic-funcion *features*))

(defmacro dbg (&rest forms)
  #-swank (declare (ignore forms))
  #+swank `(format *error-output* "~a => ~a~&" ',forms `(,,@forms)))

(defmacro do-rep (count &body body) `(loop repeat ,count do ,@body))

(defmacro nlet (name binds &body body)
  `(labels ((,name (,@(mapcar #'first binds))
              ,@body))
     (,name ,@(mapcar #'second binds))))

(defmacro dotimes! ((var count &optional (index-origin 0) (unroll 60)) &body body)
  (macrolet ((with-gensyms ((&rest args) &body body)
               `(let (,@(mapcar (lambda (arg) `(,arg (gensym))) args))
                  ,@body)))
    (with-gensyms (cnt q r)
      `(multiple-value-bind (,q ,r) (truncate ,count ,unroll)
         (declare (fixnum ,q ,r))
         (do ((,cnt 0 (the fixnum (1+ ,cnt))) (,var ,index-origin))
             ((>= ,cnt ,q) (loop repeat ,r do (progn ,@body (setf (the fixnum ,var) (the fixnum (1+ ,var))))))
           (declare (fixnum ,cnt ,var))
           ,@(loop repeat unroll append `(,@body (setf (the fixnum ,var) (the fixnum (1+ ,var))))))))))

(declaim (inline println))
(defun println (obj &optional (stream *standard-output*))
  (let ((*read-default-float-format* 'double-float))
    (prog1 obj
      (princ obj stream)
      (terpri))))

(defun read-nums (count &optional (element-type '(simple-array fixnum (*))))
  (declare (fixnum count))
  (coerce (loop repeat count collect (read)) element-type))

(define-modify-macro maxf (var) max)
(define-modify-macro minf (var) min)

(defconstant +inf+ #.(expt 10 14))

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

(declaim (ftype (function (fixnum fixnum (simple-array mint (*))) mint) mod-combi-with-table)
         (inline mod-combi-with-table))
(defun mod-combi-with-table (n k table)
  (declare (fixnum n k)
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

;;
;; BOF
;;

(declaim (ftype (function (sequence) simple-base-string) unwrap))
(defun unwrap (sequence)
  ;; e.g. (unwrap (list 1 2 3 4 5)) => "1 2 3 4 5"
  (let ((*standard-output* (make-string-output-stream :element-type 'base-char)))
    (let ((init nil))
      (declare (boolean init))
      (map nil
           (lambda (x)
             (when init
               (princ #\space))
             (setq init t)
             (princ x))
           sequence))
    (coerce (get-output-stream-string *standard-output*) 'simple-base-string)))

(defmacro with-buffered-stdout (&body body)
  ;; Quoted from: https://competitive12.blogspot.com/2020/03/common-lisp.html
  (let ((out (gensym)))
    `(let ((,out (make-string-output-stream :element-type 'base-char)))
       (let ((*standard-output* ,out))
         ,@body)
       (write-string (get-output-stream-string ,out)))))

(declaim (inline read-fixnum read-nums println))
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

(defun set! (arr count)
  (dotimes (i count)
    (setf (aref arr i)
          (read-fixnum))))

(defun read-base-char (&optional (in *standard-input*) (eof #\Newline))
  (declare (inline read-byte)
           #-swank (sb-kernel:ansi-stream in)
           (base-char eof))
  #+swank (coerce (read-char in nil eof) 'base-char)
  #-swank
  (the base-char (code-char (the (integer 0 127) (read-byte in nil (char-code eof))))))

(defmacro read-line! (simple-base-string &optional (in *standard-input*) (term #\Newline))
  "Read characters and DESTRUCTIVELY fill SIMPLE-BASE-STRING with them."
  (let ((n (gensym))
        (c (gensym))
        (i (gensym)))
    `(locally (declare (inline read-base-char))
       (let ((,n (length ,simple-base-string)))
         (declare (fixnum ,n))
         (loop for ,c of-type base-char = (read-base-char ,in #\Newline)
               with ,i of-type fixnum = 0
               until (char= ,c ,term)
               do (unless (< ,i ,n)
                    (error "Reached the end of ~a." ',simple-base-string))
                  (setf (schar ,simple-base-string ,i)
                        ,c)
                  (incf ,i))))))

(defun split (string &optional (separator #\space))
  (declare (base-string string)
           (base-char separator))
  (let ((pos (position separator string)))
    (if pos
        (cons (subseq string 0 pos)
              (split (subseq string (1+ pos))
                     separator))
        (list string))))

;;
;; EOF
;;


;;;
;;; Body
;;;

(defun main ()
  (declare #.*opt*)
  (let* ((n (read))
         (k (read))
         (max! 0)
         (min! 0)
         (as (make-array n :element-type 'fixnum))
         (table (make-mod-fact-table 300010)))
    (declare (fixnum n k max! min!)
             ((simple-array fixnum (*)) as))
    (dotimes! (i n 0 60)
      (setf (aref as i)
            (read-fixnum)))
    (setf as (sort as #'<))
    (dotimes! (i n 0 60)
      (let ((a (modint (aref as i))))
        (declare (mint a))
        (incmodf max! (mod* a (mod-combi-with-table i
                                                    (1- k)
                                                    table)))
        (incmodf min! (mod* a (mod-combi-with-table (- n i 1)
                                                    (1- k)
                                                    table))))
      (dbg max! min!))
    (println (mod- max! min!))))

#-swank
(progn
  (setf (sb-alien:extern-alien "thread_control_stack_size" sb-kernel::os-vm-size-t)
        (* 256 1024 1024))
  (sb-thread:join-thread (sb-thread:make-thread #'main)))
