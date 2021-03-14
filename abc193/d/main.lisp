(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *opt*
    #+swank '(optimize (speed 3) (safety 2))
    #-swank '(optimize (speed 3) (safety 0) (debug 0)))
  #+swank (load "~/Dropbox/Code/atcoder/ac-tools/act.lisp")
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

(defmacro read-line! (&optional (buffer-size 20) (in *standard-input*) (term #\Newline))

  (let ((res (gensym))
        (c (gensym))
        (i (gensym)))
    `(let ((,res (load-time-value (make-string ,buffer-size :element-type 'base-char))))
       (declare (simple-base-string ,res)
                (inline read-base-char))
       (loop for ,c of-type base-char = (read-base-char ,in)
             for ,i of-type fixnum below ,buffer-size
             until (char= ,c ,term) do (setf (schar ,res ,i)
                                             ,c))
       ,res)))

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

(defun calc-score (&rest args)
  (let ((counter (make-array 11 :element-type 'fixnum :initial-element 0)))
    (dolist (x args)
      (incf (aref counter x)))
    (loop for x of-type fixnum from 0 to 10
          sum (* x (expt 10 (aref counter x))))))

(defun main ()
  (declare #.*opt*)
  (let ((k (read))
        (tak (map 'list #'digit-char-p (subseq (read-line) 0 4)))
        (aoki (map 'list #'digit-char-p (subseq (read-line) 0 4))))
    (let ((tak-cnt (make-array 11 :element-type 'fixnum :initial-element 0))
          (aoki-cnt (make-array 11 :element-type 'fixnum :initial-element 0))
          (total-rest (- (* 9 k) 8)))
      (declare ((simple-array fixnum (11)) tak-cnt aoki-cnt)
               (fixnum total-rest))
      (dolist (x tak)
        (incf (aref tak-cnt x)))
      (dolist (x aoki)
        (incf (aref aoki-cnt x)))
      (dbg tak-cnt aoki-cnt)
      (println
       (loop for i of-type fixnum from 1 to 9
             sum (loop for j of-type fixnum from 1 to 9
                       for tak-score of-type fixnum = (apply #'calc-score i tak)
                       for aoki-score of-type fixnum = (apply #'calc-score j aoki)
                       for i-rest of-type fixnum = (- k
                                                      (aref tak-cnt i)
                                                      (aref aoki-cnt i))
                       for j-rest of-type fixnum = (- k
                                                      (aref tak-cnt j)
                                                      (aref aoki-cnt j))
                       when (> tak-score aoki-score)
                         sum (the double-float
                                  (float (if (/= i j)
                                             (/ (* i-rest j-rest)
                                                (* total-rest (1- total-rest)))
                                             (/ (ash (* i-rest (1- i-rest)) -1)
                                                (ash (* total-rest (1- total-rest))
                                                     -1)))
                                         0d0))))))))

#-swank
(progn
  (setf (sb-alien:extern-alien "thread_control_stack_size" sb-kernel::os-vm-size-t)
        (* 256 1024 1024))
  (sb-thread:join-thread (sb-thread:make-thread #'main)))
