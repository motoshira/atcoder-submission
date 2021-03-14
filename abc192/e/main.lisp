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

(defmacro defconstant* (name value &optional doc)
  "Ensure VALUE is evaluated only once."
  ;; Ref: https://g000001.cddddr.org/1260010568
  `(defconstant ,name (if (boundp ',name)
                          (symbol-value ',name)
                          ,value)
     ,@(when doc (list doc))))

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

(defconstant +inf+ #.(expt 10 16))

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
;;; BOF
;;;

;; Binary heap (1-based)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun symb (&rest args)
    (intern
     (with-output-to-string (s)
       (dolist (x args)
         (princ (string-upcase x) s))))))

;; TODO :
;; - heap-full-p
;; - invoke error when attempting to insert item to full heap


(defmacro define-binary-heap (struct-name &key element-type predicate key-fn key-type init)
  (let* ((name-str (symbol-name struct-name))
         (constructor (symb "make-" name-str))
         (empty-p (symb name-str "-empty-p"))
         (peek (symb name-str "-peek"))
         (push (symb name-str "-push"))
         (heapify-up (symb name-str "-heapify-up"))
         (pop (symb name-str "-pop"))
         (heapify-down (symb name-str "-heapify-down")))
    
    `(progn
       
       (defstruct (,struct-name (:constructor ,constructor (size))
                                (:copier nil))
         ;; 
         ;; Example: TODO
         ;;
         (data (make-array (the fixnum (1+ size)) :element-type ',element-type :adjustable nil :initial-element ,init) :type (simple-array ,element-type (*)))
         (count 0 :type fixnum))

       (declaim (inline ,empty-p ,push ,heapify-up ,pop ,heapify-down ,peek))
       (defun ,empty-p (heap)
         (declare (,struct-name heap))
         (with-slots (data count) heap
           (declare (ignorable data))
           (zerop count)))

       (defun ,heapify-up (heap node-index)
         (declare (,struct-name heap)
                  (fixnum node-index))
         (with-slots (data count) heap
           (declare (ignorable count))
           (loop do
             (let ((parent-index (floor node-index 2)))
               (declare (fixnum parent-index))
               (labels ((ordered-p (parent child)
                          (declare (fixnum parent child))
                          (the boolean
                               (,predicate (the ,key-type (,key-fn (aref data parent)))
                                           (the ,key-type (,key-fn (aref data child)))))))
                 (when (or (<= parent-index 0)
                           (ordered-p parent-index
                                      node-index))
                   (return))
                 (rotatef (the ,element-type (aref data node-index))
                          (the ,element-type (aref data parent-index)))
                 (setf node-index parent-index))))))

       (defun ,heapify-down (heap root-index)
         (declare (,struct-name heap)
                  (fixnum root-index))
         (with-slots (data count) heap
           (declare (ignorable count))
           (loop do
             (let ((root root-index)
                   (l (* root-index 2))
                   (r (1+ (* root-index 2))))
               (declare (fixnum root l r))
               (labels ((ordered-p (p c)
                          (declare (fixnum p c))
                          (the boolean
                               (,predicate (the ,key-type (,key-fn (aref data p)))
                                           (the ,key-type (,key-fn (aref data c))))))
                        (swap! (x y)
                          (rotatef (the ,element-type (aref data x))
                                   (the ,element-type (aref data y)))))
                 (cond ((and (<= l count)
                             (or (> r count)
                                 (ordered-p l r))
                             (not (ordered-p root l)))
                        (swap! root l)
                        (setf root-index l))
                       ((and (<= r count)
                             (not (ordered-p root r)))
                        (swap! root r)
                        (setf root-index r))
                       (t (return))))))))
       
       (defun ,push (heap item)
         (declare (,struct-name heap)
                  (,element-type item))
         (with-slots (data count) heap
           (incf (the fixnum count))
           (setf (aref data count) item)
           (,heapify-up heap count)))

       (defun ,peek (heap)
         (declare (,struct-name heap))
         (with-slots (data count) heap
           (declare (ignorable count))
           (aref data 1)))

       (defun ,pop (heap)
         (declare (,struct-name heap))
         (with-slots (data count) heap
           (when (,empty-p heap)
             (error "Heap is empty."))
           (let ((res (aref data 1)))
             (declare (,element-type res))
             (prog1 res
               (setf (aref data 1)
                     (the ,element-type (aref data count)))
               (decf (the fixnum count))
               (,heapify-down heap 1))))))))

;; e.g. : for Dijkstra algorithm
;; (cost node)

(define-binary-heap heap
  :element-type list
  :predicate <
  :key-fn first
  :key-type fixnum
  :init nil)


;;;
;;; EOF
;;;


;;;
;;; Body
;;;

(defun main ()
  (declare #.*opt*)
  (let ((n (read-fixnum))
        (m (read-fixnum))
        (start (1- (read-fixnum)))
        (goal (1- (read-fixnum))))
    (declare (fixnum n m start goal))
    (let ((edges (make-array n :element-type 'list :initial-element nil)))
      (declare ((simple-array list (*)) edges))
      (do-rep m
        (let ((a (1- (read-fixnum)))
              (b (1- (read-fixnum)))
              (time (read-fixnum))
              (period (read-fixnum)))
          (declare (fixnum a b time period))
          (push (list b time period) (aref edges a))
          (push (list a time period) (aref edges b))))
      (let ((h (make-heap 300000))
            (costs (make-array n :element-type 'fixnum :initial-element +inf+)))
        (declare (heap h))
        (heap-push h (list 0 start)) ;; time, pos
        (setf (aref costs start) 0)
        (loop while (not (heap-empty-p h))
              for (time pos) of-type (fixnum fixnum) = (heap-pop h)
              when (>= (aref costs pos) time)
                do (setf (aref costs pos) time)
                   (dolist (next (aref edges pos))
                     (declare (list next))
                     (destructuring-bind (next-pos dt period) next
                       (declare (fixnum next-pos dt period))
                       (let* ((nt (if (zerop (rem time period))
                                      (+ time dt)
                                      (+ (* (ceiling time period)
                                            period)
                                         dt))))
                         (declare (fixnum nt))
                         (when (< nt (aref costs next-pos))
                           (setf (aref costs next-pos)
                                 nt)
                           (heap-push h (list nt next-pos)))))))
        (dbg edges)
        (dbg costs)
        (let ((res (aref costs goal)))
          (println (if (< res +inf+)
                       res
                       -1)))))))

#-swank
(progn
  (setf (sb-alien:extern-alien "thread_control_stack_size" sb-kernel::os-vm-size-t)
        (* 256 1024 1024))
  (sb-thread:join-thread (sb-thread:make-thread #'main)))
