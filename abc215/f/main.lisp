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
(define-segment-tree seg-min
  :element-type fixnum
  :result-type fixnum
  :fn min
  :e (ash 1 55))

(define-segment-tree seg-max
  :element-type fixnum
  :result-type fixnum
  :fn max
  :e 0)

;;;
;;; EOF
;;;


;;---------------------------------Body---------------------------------

(in-package #:cl-user)

(declaim (inline println))
(defun println (obj &optional (stream *standard-output*))
  (let ((*read-default-float-format* 'double-float))
    (prog1 obj
      (princ obj stream)
      (terpri stream))))

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

(defun lower-bound (fn ok ng)
  (if (<= (abs (- ok ng))
          1)
      ok
      (let ((mid (ash (+ ok ng) -1)))
        (if (funcall fn mid)
            (lower-bound fn mid ng)
            (lower-bound fn ok mid)))))

(declaim (inline judge))
(defun judge (d n ps seg-min seg-max)
  "答えはd以上か？"
  ;; しゃくとりをする
  (flet ((exist-p (r yl)
           ;; r以降に条件を満たすものがあるかどうか
           (or (>= (abs (- yl (seg-min-fold seg-min r n)))
                   d)
               (>= (abs (- yl (seg-max-fold seg-max r n)))
                   d))))
    (declare (inline exist-p))
    (let ((r 0))
      (dotimes (l n)
        (destructuring-bind (xl yl) (aref ps l)
          (loop while (and (< r n)
                           (destructuring-bind (xr _yr) (aref ps r)
                             (declare (ignore _yr))
                             (< (abs (- xr xl))
                                d)))
                do (incf r))
          (when (< r n)
            (let ((xr (first (aref ps r))))
              (when (and (>= (abs (- xr xl))
                             d)
                         (exist-p r yl))
                (return-from judge t))))
          (when (= l r)
            (incf r)))))))

(defun main ()
  (declare (inline sort))
  (let* ((n (read))
         (ps (make-array n :element-type 'list
                           :initial-element nil))
         (seg-min (make-seg-min n))
         (seg-max (make-seg-max n)))
    (dotimes (i n)
      (let ((x (read-fixnum))
            (y (read-fixnum)))
        (setf (aref ps i) (list x y))))
    (setf ps (sort ps #'< :key #'first))
    (dotimes (i n)
      (let ((y (second (aref ps i))))
        (seg-min-update seg-min i y)
        (seg-max-update seg-max i y)))
    #+swank (println ps)
    (println (lower-bound (lambda (d)
                            (judge d n ps seg-min seg-max))
                          0
                          (expt 10 12)))))
#-swank (main)
