#-swank
(unless (member :child-sbcl *features*)
  (quit
   :recklessly-p t
   :unix-status
   (process-exit-code
    (run-program *runtime-pathname*
                 `("--control-stack-size" "256MB"
                                          "--noinform" "--disable-ldb" "--lose-on-corruption" "--end-runtime-options"
                                          "--eval" "(push :child-sbcl *features*)"
                                          "--script" ,(namestring *load-pathname*))
                 :output t :error t :input t))))

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

(defmacro with-buffered-stdout (&body body)
  ;; Quoted from: https://competitive12.blogspot.com/2020/03/common-lisp.html
  (let ((out (gensym)))
    `(let ((,out (make-string-output-stream :element-type 'base-char)))
       (let ((*standard-output* ,out))
         ,@body)
       (write-string (get-output-stream-string ,out)))))

(declaim (inline println))
(defun println (obj &optional (stream *standard-output*))
  (let ((*read-default-float-format* 'double-float))
    (prog1 obj
      (princ obj stream)
      (terpri stream))))

(defun main ()
  (let* ((n (read))
         (edges (make-array n :element-type 'list
                              :initial-element nil)))
    (declare (fixnum n)
             ((simple-array list (*)) edges))
    (dotimes (_ (1- n))
      (let ((a (1- (read-fixnum)))
            (b (1- (read-fixnum))))
        (declare (fixnum a b))
        (push b (aref edges a))
        (push a (aref edges b))))
    (dotimes (i n)
      (setf (aref edges i)
            (sort (aref edges i) #'<)))
    (let ((res nil)
          (visited (make-array n :element-type 'fixnum
                                 :initial-element -1))
          (node 0))
      (declare (list res)
               ((simple-array fixnum (*)) visited)
               (fixnum node))
      (setf (aref visited 0) (expt 10 12))
      (push 0 res)
      #+swank (println edges)
      (loop
        (block continue
          (dolist (next (aref edges node))
            (declare (fixnum next))
            (when (= -1 (aref visited next))
              (push next res)
              (setf (the fixnum (aref visited next)) (the fixnum node)
                    (the fixnum node) (the fixnum next))
              (setf (the list (aref edges node))
                    (the list (remove next (aref edges node) :count 1)))
              (return-from continue)))
          (when (zerop node)
            (return))
          (let ((next (aref visited node)))
            (declare (fixnum next))
            (push next res)
            (setf node next))))
      (with-buffered-stdout
        (mapc #'println (mapcar #'1+ (reverse res)))))))

#-swank (main)
