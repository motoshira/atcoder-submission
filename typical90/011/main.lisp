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

(defstruct (job (:constructor make-job (&key
                                          (deadline 0)
                                          (time-needed 0)
                                          (value 0))))
  (start-limit (1+ (- deadline time-needed)) :type fixnum)
  (value 0 :type fixnum))

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
  :element-type (or null job)
  :predicate >
  :key-fn job-value
  :key-type fixnum
  :init nil)


;;;
;;; EOF
;;;

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

(defun main ()
  (let* ((n (read))
         (jobs (loop repeat n
                     collect (make-job :deadline (read-fixnum)
                                       :time-needed (read-fixnum)
                                       :value (read-fixnum)))))
    (setf jobs (sort jobs #'> :key #'job-start-limit))
    #+swank (println jobs)
    (let ((heap (make-heap 2010))
          (res 0))
      (declare (heap heap)
               (fixnum res))
      (loop for day of-type fixnum from 5000 downto 0
            do (loop while (and jobs
                                (= (job-start-limit (first jobs))
                                   day))
                     do (heap-push heap
                                   (pop jobs)))
               (unless (heap-empty-p heap)
                 (let ((value (job-value (heap-pop heap))))
                   (declare (fixnum value))
                   (incf res value))))
      (println res))))

#-swank (main)
