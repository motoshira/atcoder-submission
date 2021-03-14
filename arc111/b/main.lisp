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
  (let ((cnt (gensym))
        (q (gensym))
        (r (gensym)))
    `(multiple-value-bind (,q ,r) (truncate ,count ,unroll)
       (declare (fixnum ,q ,r))
       (do ((,cnt 0 (the fixnum (1+ ,cnt))) (,var ,index-origin))
           ((>= ,cnt ,q) (loop repeat ,r do (progn ,@body (setf (the fixnum ,var) (the fixnum (1+ ,var))))))
         (declare (fixnum ,cnt ,var))
         ,@(loop repeat unroll append `(,@body (setf (the fixnum ,var) (the fixnum (1+ ,var)))))))))
 
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

;;;
;;; BOF
;;;

;; Union-Find Tree (0-indexed)

(defpackage :union-find
  (:nicknames :uf)
  (:use :cl)
  (:shadow :find)
  (:export :make-uf-tree :find :unite :friends-p :show-parents :get-tree-size :count-trees))

(in-package :union-find)

(defstruct (union-find-tree (:conc-name uf-)
                            (:constructor make-uf-tree (size)))
  (dat (make-array size :element-type 'fixnum
                        :adjustable nil
                        :initial-element -1)
   :type simple-array)
  (size size :type fixnum)
  (count size :type fixnum))


(defun find (uf x)
  (declare (union-find-tree uf)
           (fixnum x))
  (with-slots (dat size count) uf
    (declare (ignorable dat size count))
    (if (minusp (aref dat x))
        x
        (setf (aref dat x)
              (find uf (aref dat x))))))


(defun show-parents (uf)
  ;; Return the parent of each member in list.
  #-swank (declare (ignore uf))
  #+swank
  (locally (declare (union-find-tree uf))
    (with-slots (dat size count) uf
      (declare (ignorable count))
      (loop for i
              below size
            collect (if (minusp (aref dat i))
                        i
                        (find uf (aref dat i)))))))
  

(defmethod unite (uf x y)
  (declare (union-find-tree uf)
           (fixnum x y))
  (with-slots (dat size count) uf
    (declare (ignore size))
    (let ((x-parent (find uf x))
          (y-parent (find uf y)))
      (declare (fixnum x-parent y-parent))
      (when (> x-parent y-parent)
        (rotatef x-parent y-parent))
      (unless (= x-parent y-parent)
        (incf (aref dat x-parent)
              (aref dat y-parent))
        (setf (aref dat y-parent)
              x-parent)
        (decf count)))))

(defun get-tree-size (uf x)
  (declare (union-find-tree uf)
           (fixnum x))
  (with-slots (dat size count) uf
    (declare (ignorable size count))
    (the fixnum (- (aref dat (find uf x))))))

(defun friends-p (uf x y)
  (declare (union-find-tree uf)
           (fixnum x y))
  (= (find uf x)
     (find uf y)))

(defun count-trees (uf)
  (declare (union-find-tree uf))
  (uf-count uf))

(in-package :cl-user)

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


(defconstant +magic+ 400000)

(defun main ()
  (declare #.*opt*)
  (let* ((n (read))
         (uf (uf:make-uf-tree (1+ +magic+)))
         (appeared (make-hash-table :test #'eq))
         (cycled (make-hash-table :test #'eq)))
    (declare (fixnum n)
             (hash-table appeared cycled)
             (uf::union-find-tree uf))
    (do-rep n
      (let ((a (read-fixnum))
            (b (read-fixnum)))
        (declare (fixnum a b))
        (setf (gethash a appeared) t
              (gethash b appeared) t)
        (when (or (= a b)
                  (uf:friends-p uf a b))
          (setf (gethash (uf:find uf a) cycled) t
                (gethash (uf:find uf b) cycled) t))
        (uf:unite uf a b)))
    (let ((res 0))
      (maphash (lambda (node _)
                 (declare (ignore _))
                 (dbg (gethash node cycled))
                 (when (= node (uf:find uf node))
                   (let ((cnt (uf:get-tree-size uf node)))
                     (dbg cnt)
                     (incf res
                           (if (gethash node cycled)
                               cnt
                               (1- cnt))))))
               appeared)
      (println res))))

#-swank
(progn
  (setf (sb-alien:extern-alien "thread_control_stack_size" sb-kernel::os-vm-size-t)
        (* 256 1024 1024))
  (sb-thread:join-thread (sb-thread:make-thread #'main)))
