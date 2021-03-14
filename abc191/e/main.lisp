#+swank (declaim (optimize (speed 3) (safety 2)))
#-swank (declaim (optimize (speed 3) (safety 0) (debug 0)))

(defmacro do-iota ((var count &optional (begin 0) (step 1)) &body body)
  (let ((cnt (gensym)))
    `(loop for ,cnt of-type fixnum below ,count
           with ,var of-type fixnum = ,begin
           do ,@body
              (incf (the fixnum ,var) (the fixnum ,step)))))

(defmacro do-rep (count &body body) `(loop repeat ,count do ,@body))

(declaim (inline println))
(defun println (obj &optional (stream *standard-output*))
  (let ((*read-default-float-format* 'double-float))
    (princ obj stream)
    (terpri)))

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

(declaim (ftype (function * (values fixnum &optional)) read-fixnum))
(defun read-fixnum (&optional (in *standard-input*))
  ;; Quoted from: https://competitive12.blogspot.com/2020/03/common-lisp.html
  (declare (inline read-byte)
           #-swank (sb-kernel:ansi-stream in))
  (macrolet ((%read-byte ()
               `(the (unsigned-byte 8)
                     #+swank (char-code (read-char in nil #\Nul))
                     #-swank (read-byte in nil 0))))
    (let* ((minus nil)
           (result (loop (let ((byte (%read-byte)))
                           (cond ((<= 48 byte 57)
                                  (return (- byte 48)))
                                 ((zerop byte) ; #\Nul
                                  (error "Read EOF or #\Nul."))
                                 ((= byte #.(char-code #\-))
                                  (setq minus t)))))))
      (declare ((integer 0 #.most-positive-fixnum) result))
      (loop
        (let* ((byte (%read-byte)))
          (if (<= 48 byte 57)
              (setq result (+ (- byte 48) (the (integer 0 #.(floor most-positive-fixnum 10)) (* result 10))))
              (return (if minus (- result) result))))))))

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

(defconstant +inf+ (expt 10 12))
(define-modify-macro minf (var) (lambda (place var) (min place var)))

(declaim (inline dijkstra))
(defun dijkstra (start n edges)
  (declare (fixnum start n)
           ((simple-array list (*)) edges))
  (let ((costs (make-array n :element-type 'fixnum :adjustable nil :initial-element +inf+))
        (h (make-heap 200000)))
    (declare ((simple-array fixnum (*)) costs)
             (heap h))
    (heap-push h (list 0 start))
    (setf (aref costs start) 0)
    (loop while (not (heap-empty-p h))
          for (cost pos) of-type (fixnum fixnum) = (heap-pop h)
          do (when (<= cost (aref costs pos))
               (dolist (edge (aref edges pos))
                 (destructuring-bind (next dc) edge
                   (declare (fixnum next dc))
                   (let ((ncost (+ cost dc)))
                     (declare (fixnum ncost))
                     (when (< ncost (aref costs next))
                       (setf (aref costs next) ncost)
                       (heap-push h (list ncost next))))))))
    costs))

(defun main ()
  (let* ((n (read))
         (m (read))
         (edges (make-array n :element-type 'list :adjustable nil :initial-element nil))
         (edges-rev (make-array n :element-type 'list :adjustable nil :initial-element nil)))
    (declare (fixnum n m)
             ((simple-array list (*)) edges edges-rev))
    (do-rep m
      (let ((a (1- (read-fixnum)))
            (b (1- (read-fixnum)))
            (c (read-fixnum)))
        (declare (fixnum a b c))
        (push (list b c) (aref edges a))
        (push (list a c) (aref edges-rev b))))
    (with-buffered-stdout
      (dotimes (i n)
        (declare (fixnum i))
        (let ((costs-go (dijkstra i n edges))
              (costs-back (dijkstra i n edges-rev)))
          (declare ((simple-array fixnum (*)) costs-go costs-back))
          (let ((res +inf+))
            (declare (Fixnum res))
            (dotimes (j n)
              (declare (fixnum j))
              (minf res
                    (the fixnum
                         (cond
                           ((= i j)
                            (loop for (to cost) of-type (fixnum fixnum) in (aref edges i)
                                  with acc of-type fixnum = +inf+
                                  when (= to i)
                                    do (minf acc cost)
                                  finally
                                     (return acc)))
                           (t
                            (+ (aref costs-go j)
                               (aref costs-back j)))))))
            (println (if (< res +inf+)
                         res
                         -1))))))))

#+swank
(load "~/Dropbox/Code/atcoder/ac-tools/act.lisp")

#-swank
(progn
  (setf (sb-alien:extern-alien "thread_control_stack_size" sb-kernel::os-vm-size-t)
        (* 256 1024 1024))
  (sb-thread:join-thread (sb-thread:make-thread #'main)))
