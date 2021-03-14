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
;;; BOF
;;;

;; Segment-tree (1-indexed)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun symb (&rest args)
    (intern
     (with-output-to-string (s)
       (dolist (x args)
         (princ (string-upcase x) s))))))

(defmacro define-segment-tree (struct-name &key element-type result-type fn e)
  `(progn

     (defstruct (,struct-name (:conc-name ,(symb (symbol-name struct-name) "-"))
                              (:constructor ,(symb "%make-" (symbol-name struct-name))))
       (m nil :type fixnum)
       (data nil :type (simple-array ,element-type (*))))

     (declaim (inline ,(symb "make-" (symbol-name struct-name))
                      ,(symb (symbol-name struct-name) "-fold")
                      ,(symb (symbol-name struct-name) "-update")))
     (defun ,(symb "make-" (symbol-name struct-name)) (size)
       (declare (fixnum size))
       (let ((m (sb-int:named-let rec ((m 1))
                  (if (>= m size)
                      m
                      (rec (ash m 1))))))
         (declare (fixnum m))
         (,(symb "%make-" (symbol-name struct-name)) :m m
                                                     :data (make-array (the fixnum (ash m 1))
                                                                       :element-type ',element-type
                                                                       :adjustable nil
                                                                       :initial-element ,e))))

     (defun ,(symb (symbol-name struct-name) "-fold") (seg l r)
       (declare (,struct-name seg)
                (fixnum l r))
       (with-slots (m data) seg
         (let ((l (+ l (,(symb (symbol-name struct-name) "-m") seg)))
               (r (+ r (,(symb (symbol-name struct-name) "-m") seg))))
           (declare (fixnum l r))
           (loop while (< l r)
                 with res of-type ,result-type = ,e
                 when (logbitp 0 l)
                   do (setf res (,fn res (aref data l)))
                   and do (incf l)
                 when (logbitp 0 r)
                   do (setf res (,fn res (aref data (1- r))))
                   and do (decf r)
                 do (setq l (ash l -1))
                    (setq r (ash r -1))
                 finally
                    (return res)))))


     (defun ,(symb (symbol-name struct-name) "-update") (seg i val)
       (declare (,struct-name seg)
                (fixnum i)
                (,element-type val))
       (with-slots (m data) seg
         (let ((i (the fixnum (+ i m))))
           (declare (fixnum i))
           (setf (aref data i) val)
           (let ((i (ash i -1)))
             (declare (fixnum i))
             (loop while (plusp i)
                   do (setf (aref data i)
                            (the ,result-type
                                 (,fn (aref data (the fixnum (logior 0 (ash i 1))))
                                      (aref data (the fixnum (logior 1 (ash i 1)))))))
                      (setf i (the fixnum (ash i -1))))))))))

;; e.g. Range-Minimum-Query(RMQ)
(define-segment-tree seg
  :element-type fixnum
  :result-type fixnum
  :fn (lambda (x y) (declare (fixnum x y)) (+ x y))
  :e 0)

;;;
;;; EOF
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

(defconstant +max+ #.(* 15 (expt 10 5)))


(defconstant +t+ 1)
(defconstant +f+ 0)

(defun main ()
  (declare #.*opt*)
  (let* ((n (read))
         (m (read))
         (as (read-nums n))
         (seg (make-seg #.(+ +max+ 10)))
         (counter (make-hash-table :test #'eq))
         (res +inf+))
    (declare (fixnum n m res)
             ((simple-array fixnum (*)) as)
             (hash-table counter)
             (seg seg))
    (loop for x from 0 to #.(+ +max+ 10)
          do (seg-update seg x +t+))
    (dotimes (i n)
      (declare (fixnum i))
      (let ((a (aref as i)))
        (declare (fixnum a))
        (seg-update seg a +f+)
        (incf (gethash a counter 0))
        (when (>= i m)
          (let ((b (aref as (- i m))))
            (declare (fixnum b))
            (decf (gethash b counter))
            (when (zerop (gethash b counter))
              (seg-update seg b +t+))))
        (dbg (loop for x from 0 to 10
                   collect (seg-fold seg x (1+ x))))
        (when (>= i (1- m))
          (minf res
                (nlet rec ((ok 0)
                           (ng #.(+ +max+ 10)))
                  (declare (fixnum ok ng))
                  (if (<= (abs (- ok ng)) 1)
                      ok
                      (let ((mid (ash (+ ok ng) -1)))
                        (declare (fixnum mid))
                        (if (zerop (seg-fold seg 0 mid))
                            (rec mid ng)
                            (rec ok mid)))))))))
    (println res)))

#-swank
(progn
  (setf (sb-alien:extern-alien "thread_control_stack_size" sb-kernel::os-vm-size-t)
        (* 256 1024 1024))
  (sb-thread:join-thread (sb-thread:make-thread #'main)))
