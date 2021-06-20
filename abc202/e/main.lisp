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

(defmacro nlet (name binds &body body)
  `(labels ((,name (,@(mapcar #'first binds))
              ,@body))
     (,name ,@(mapcar #'second binds))))

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


(declaim (inline encode))
(defun encode (x y)
  (declare (fixnum x y))
  (the fixnum
       (+ (the fixnum (* x 200020))
          y)))

(defun main ()
  (let* ((n (read))
         (cs (make-array n :initial-element nil :element-type 'list))
         (ds (make-array (1+ n) :initial-element 0 :element-type 'fixnum))
         (qs nil))
    (declare (fixnum n)
             ((simple-array list (*)) cs)
             ((simple-array fixnum (*)) ds)
             (list qs))
    (loop for i from 1 below n do
      (let ((parent (1- (read-fixnum))))
        (push i (aref cs parent))))
    (nlet rec ((node 0)
               (depth 0))
      (declare (fixnum node depth))
      (setf (aref ds depth)
            (logior (aref ds depth)
                    (ash 1 node)))
      (dolist (next (aref cs node))
        (rec next (1+ depth))))
    (loop repeat (read)
          do (push (list (1- (read-fixnum))
                         (read-fixnum))
                   qs))
    (setf qs (nreverse qs))
    (let ((childs (make-array n :element-type 'list :initial-element nil)))
      (labels ((rec (node)
                 (declare (fixnum node))
                 (or (aref childs node)
                     (setf (aref childs node)
                           (the fixnum
                                (loop for next in (aref cs node)
                                      with res = (ash 1 node)
                                      do (setf res
                                               (logior res
                                                       (rec next)))
                                      finally (return res)))))))
        (loop for (u d) in qs
              do (println (logcount (logand (aref ds d)
                                            (rec u)))))))))

#-swank (main)
