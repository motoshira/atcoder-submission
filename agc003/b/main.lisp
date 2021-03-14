#|
------------------------------------
                Utils                
------------------------------------
|#

;;; Most of these utils are quoted from https://competitive12.blogspot.com/2020/03/common-lisp.html. Thank you!


;;;----------------init----------------

(in-package :cl-user)

#-swank
(unless (member :child-sbcl *features*)
  (quit
   :unix-status
   (process-exit-code
    (run-program *runtime-pathname*
                 `("--control-stack-size" "128MB"
                   "--noinform" "--disable-ldb" "--lose-on-corruption" "--end-runtime-options"
                   "--eval" "(push :child-sbcl *features*)"
                   "--script" ,(namestring *load-pathname*))
                 :output t :error t :input t))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  #+swank (ql:quickload '(:sb-concurrency :fiveam :cl-debug-print :sb-sprof) :slient t)
  #-swank (require :sb-concurrency)
  #-swank (set-dispatch-macro-character
           #\# #\> (lambda (s c p) (declare (ignore c p)) `(values ,(read s nil nil t)))))


(defpackage :procon-user
  #+swank (:use :cl :sb-int :sb-concurrency :cl-debug-print :sb-sprof)
  #-swank (:use :cl :sb-int :sb-concurrency)
  #+swank (:import-from :fiveam)
  (:shadow :collect :run :test)
  (:export :main))

(in-package :procon-user)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter OPT
    #+swank '(optimize (speed 3) (safety 2))
    #-swank '(optimize (speed 3) (safety 0) (debug 0))))
#+swank (cl-syntax:use-syntax cl-debug-print:debug-print-syntax)



(defconstant +mod+ 1000000007)
;(defconstant +mod+ 998244353)

;;;----------------aliases----------------


(defun set-aliases (forms)
  (mapc (lambda (form)
          (setf (rest form)
                (first form)))
        forms))

(defun set-aliases-for-function (forms)
  (mapc (lambda (form)
          (setf (symbol-function (rest form))
                (symbol-function (first form))))
        forms))



(set-aliases-for-function
 '((digit-char-p . char->int)
   (remove-if-not . filter)
   (aref . ar)))

(defmacro ^ (args &body body)
  `(lambda ,args
     (progn
       ,@body)))

(defmacro with-gensyms ((&rest args) &body body)
  `(let (,@(mapcar (lambda (arg)
                     (list arg `(gensym)))
                   args))
     ,@body))


;;;----------------int-types----------------

(defmacro define-int-types (&rest bits)
  `(progn
     ,@(mapcar (lambda (b) `(deftype ,(intern (format nil "INT~a" b)) () '(signed-byte ,b))) bits)
     ,@(mapcar (lambda (b) `(deftype ,(intern (format nil "UINT~a" b)) () '(unsigned-byte ,b))) bits)))

(define-int-types 2 4 8 16 32 64)

;;;----------------stream----------------

(defmacro buffered-read-line (&optional (buffer-size 30) (in '*standard-input*) (term-char #\Space))
  (let ((buffer (gensym))
        (character (gensym))
        (idx (gensym)))
    `(let* ((,buffer (load-time-value (make-string ,buffer-size :element-type 'base-char))))
       (declare (simple-base-string ,buffer)
                (inline read-byte))
       (loop for ,character of-type base-char =
                ,(if (member :swank *features*)
                     `(read-char ,in nil #\Newline) ; on SLIME
                     `(code-char (read-byte ,in nil #.(char-code #\Newline))))
             for ,idx from 0
             until (char= ,character #\Newline)
             do (setf (schar ,buffer ,idx) ,character)
             finally (when (< ,idx ,buffer-size)
                       (setf (schar ,buffer ,idx) ,term-char))
                     (return (values ,buffer ,idx))))))

(declaim (ftype (function * (values fixnum &optional)) read-fixnum))
(defun read-fixnum (&optional (in *standard-input*))
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


(defun read-bignum (&optional (in *standard-input*))
  (declare (inline read-byte)
           #-swank (sb-kernel:ansi-stream in))
  (macrolet ((%read-byte ()
               `(the (unsigned-byte 8)
                     #+swank (char-code (read-char in nil #\Nul))
                     #-swank (read-byte in nil 0))))
    (let* ((minusp nil)
           (result (loop (let ((byte (%read-byte)))
                           (cond ((<= 48 byte 57)
                                  (return (- byte 48)))
                                 ((zerop byte) ; #\Nul
                                  (error "Read EOF or #\Nul."))
                                 ((= byte #.(char-code #\-))
                                  (setq minusp t))))))
           (mid-result 0)
           (index-mod18 0))
      (declare (fixnum mid-result)
               ((integer 0 19) index-mod18)
               (integer result))
      (loop
        (when (= index-mod18 18)
          (setq result (+ mid-result (* result #.(expt 10 18))))
          (setq mid-result 0)
          (setq index-mod18 0))
        (let ((byte (%read-byte)))
          (unless (<= 48 byte 57) (return))
          (setq mid-result (+ (- byte 48) (* 10 (the (mod #.(expt 10 17)) mid-result))))
          (incf index-mod18)))
      (setq result (+ mid-result (* result (expt 10 index-mod18))))
      (if minusp (- result) result))))


(defun exp-double->fixed-double-str (double-float-num)
  (format nil "~,10f" double-float-num))

(defun println (object &optional (stream *standard-output*))
  (when (typep object 'double-float) (setf object (exp-double->fixed-double-str object)))
  (write object :stream stream :escape nil :readably nil)
  (terpri))

(defun read-numbers-to-list (size)
  (loop repeat size collect (read-fixnum)))

(defun read-numbers-to-array (size)
  (let ((arr (make-array size
                         :element-type 'fixnum
                         :adjustable nil)))
    (declare ((array fixnum 1) arr))
    (loop for i of-type fixnum below size do
         (setf (aref arr i) (read-fixnum))
       finally
         (return arr))))


(defparameter *mark->int-alist*
  '((#\. . 0)
    (#\# . 1)
    (#\S . 2)
    (#\G . 3)))

(declaim (inline mark->int int->mark))
(defun mark->int (mark)
  (rest (assoc mark *mark->int-alist*
               :test #'char-equal)))

(defun int->mark (int)
  (first (rassoc int *mark->int-alist*
                 :test #'=)))

(defun read-characters-to-board (row-size column-size)
  (let ((board (make-array `(,row-size ,column-size)
                           :element-type '(unsigned-byte 4)
                           :adjustable nil)))
    (dotimes (r row-size board)
      (let ((tmp (read-line)))
        (dotimes (c column-size)
          (let ((val (mark->int (char tmp c))))
            (declare ((or (unsigned-byte 4) null) val))
            (if val
                (setf (aref board r c) val)
                (error "Character ~a not found in the alist." (char tmp c)))))))))


(defmethod princ-for-each-line ((sequence list))
  (format t "~{~a~&~}" sequence))

(defmethod princ-for-each-line ((sequence vector))
  (loop for i below (length sequence) do
       (princ (aref sequence i))
       (fresh-line)))

(declaim (inline unwrap))
(defun unwrap (list)
  (the string
       (format nil "~{~a~^ ~}" list)))

(defmacro with-buffered-stdout (&body body)
  (let ((out (gensym)))
    `(let ((,out (make-string-output-stream :element-type 'base-char)))
       (let ((*standard-output* ,out))
         ,@body)
       (write-string (get-output-stream-string ,out)))))

;;;----------------others----------------

(defmacro defnt (function-spec (&rest arg-specs) &body body)
  "Quoted from: https://masatoi.github.io/2017/11/21/typed-defun."
  `(progn
     (declaim (ftype (function ,(mapcar #'cadr arg-specs) ,(cadr function-spec)) ,(car function-spec)))
     (defun ,(car function-spec) ,(mapcar #'car arg-specs)
       (declare (optimize (speed 3) (safety 0) (debug 0))
                ,@(mapcar (lambda (arg arg-type)
                            (list 'type arg-type arg))
                          (mapcar #'car arg-specs)
                          (mapcar #'cadr arg-specs)))
       ,@body)))


(defmacro tlet (bindings &body body)
  "Quoted from: https://masatoi.github.io/2017/11/21/typed-defun."
  `(let (,@(mapcar (lambda (binding)
                     (subseq binding 0 2))
                   bindings))
     (declare ,@(mapcar (lambda (binding)
                          (list 'type (caddr binding) (car binding)))
                        bindings))
     ,@body))

(defmacro tlet* (bindings &body body)
  "Inspired by: https://masatoi.github.io/2017/11/21/typed-defun."
  `(let* (,@(mapcar (lambda (binding)
                      (subseq binding 0 2))
                    bindings))
     (declare ,@(mapcar (lambda (binding)
                          (list 'type (caddr binding) (car binding)))
                        bindings))
     ,@body))

(defmacro ntlet (function-spec args-spec &body body)
  "\"ntlet\" is abbrev. of named-typed-let. Inspired by: https://masatoi.github.io/2017/11/21/typed-defun."
  `(declare (ftype (function (,@ (mapcar #'third args-spec)) ,(second function-spec)) ,(first function-spec)))
  `(labels ((,(first function-spec) (,@(mapcar #'first args-spec))
              (declare ,@(mapcar (lambda (arg-spec)
                                   (list (third arg-spec)
                                         (first arg-spec)))
                                 args-spec))
              ,@body))
     (,(first function-spec) ,@(mapcar #'second args-spec))))

(defmacro tlambda ((&rest args-spec) &body body)
  `(lambda (,@(mapcar #'first args-spec))
     (declare ,@(mapcar (lambda (arg-spec)
                          (list (second arg-spec)
                                (first arg-spec)))
                        args-spec))
     ,@body))

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it
         ,then-form
         ,else-form)))


(defmacro aunless (test &body body)
  `(let ((it ,test))
     (unless it
       ,@body)))


(defmacro safe-sort (list &key (test '<) (key #'identity))
  `(progn
    (declaim (inline sort sb-impl::stable-sort-list))
    (sort (copy-seq ,list) ,test :key ,key)))

(define-modify-macro maxf (var) max)
(define-modify-macro minf (var) min)
(define-modify-macro modf () (lambda (place)
                               (mod place +mod+)))
(declaim (inline << >>))
(defun << (int count)
  (ash int count))

(defun >> (int count)
  (ash int (- count)))

(declaim (inline iota))
(defun iota (count &optional (start 0) (step 1))
  (loop for i from 0 below count collect (+ start (* i step))))

(declaim (inline int->lst))
(defun int->lst (integer)
  (declare ((integer 0) integer))
  (labels ((sub (int &optional (acc nil))
             (declare ((integer 0) int)
                      (list acc))
             (if (zerop int)
                 acc
                 (sub (floor int 10) (cons (rem int 10) acc)))))
    (sub integer)))

(declaim (inline lst->int))
(defun lst->int (list)
  (declare (list list))
  (labels ((sub (xs &optional (acc 0))
             (declare (ftype (function (list &optional (integer 0)) (integer 0)) sub))
             (declare (list xs)
                      ((integer 0) acc))
             (if (null xs)
                 acc
                 (sub (rest xs) (+ (* acc 10)
                                   (rem (first xs) 10))))))
    (the fixnum
         (sub list))))

(defun int->str (integer)
  (format nil "~a" integer))


(defun str->int (str)
  (parse-integer str))

(declaim (inline next-char prev-char))

(defun next-char (character)
  (if (char-equal character #\z)
      #\a
      (code-char (1+ (char-code character)))))

(defun prev-char (character)
  (if (char-equal character #\a)
      #\z
      (code-char (1- (char-code character)))))

(declaim (inline prime-factorize-to-list))
(defun prime-factorize-to-list (integer)
  (declare ((integer 0) integer))
  (the list
       (if (<= integer 1)
           nil
           (loop
              while (<= (* f f) integer)
              with acc list = nil
              with f integer = 2
              do
                (if (zerop (rem integer f))
                    (progn
                      (push f acc)
                      (setq integer (floor integer f)))
                    (incf f))
              finally
                (when (/= integer 1)
                  (push integer acc))
                (return (reverse acc))))))

(declaim (inline prime-p))
(defun prime-p (integer)
  (declare ((integer 1) integer))
  (if (= integer 1)
      nil
      (loop
         with f = 2
         while (<= (* f f) integer)
         do
           (when (zerop (rem integer f))
             (return nil))
           (incf f)
         finally
           (return t))))

(declaim (inline count-subsequence))
(defun count-subsequence (mainstr substr)
  (let ((main-len (length mainstr))
        (sub-len (length substr)))
    (count-if (lambda (i)
                (every (lambda (j)
                         (char-equal (char mainstr (+ i j))
                                     (char substr j)))
                       (iota sub-len)))
              (iota (1+ (- main-len sub-len))))))

(defun memoize (fn)
  (let ((memo (make-hash-table)))
    (lambda (&rest args)
      (multiple-value-bind (val win) (gethash args memo)
        (if win
            val
            (setf (gethash args memo)
                  (apply fn args)))))))


(defmacro -> (obj &rest forms)
  (labels ((sub (obj forms)
             (if (null forms)
                 obj
                 (let ((form (first forms)))
                   (cond
                     ((atom form)
                      (sub `(,form ,obj)
                           (rest forms)))
                     ((null (cdr form))
                      (sub `(,(first form) ,obj)
                           (rest forms)))
                     ((find :@ form)
                      (sub  (substitute obj :@ form)
                            (rest forms)))
                     (t (sub`(,(first form) ,obj ,@(rest form))
                            (rest forms))))))))
    (sub obj forms)))


(defmacro while (test &body body)
  `(loop
      while ,test
      do
        (progn
          ,@body)))

(defmacro until (test &body body)
  `(loop
      until ,test
      do
        (progn
          ,@body)))

(defmacro repeat (times &body body)
  `(loop
      repeat ,times
      do
        (progn
          ,@body)))

(defmacro href (hash-table key &optional (default nil))
  `(gethash ,key ,hash-table ,default))

(defun zip (&rest lists)
  (apply 'mapcar 'list lists))

(defun compose (fn &rest functions)
  (reduce (lambda (f g)
            (lambda (&rest args)
              (funcall f (apply g args))))
          functions
          :initial-value fn))

(defun conjoin (&rest functions)
  (lambda (&rest args)
    (every (lambda (fn)
             (apply fn args))
           functions)))


(defun disjoin (&rest functions)
  (lambda (&rest args)
    (or (lambda (fn)
             (apply fn args))
           functions)))

(defun split-string-into-list (string &optional (separator #\space))
  (remove separator
          (concatenate 'list string)
          :test #'char-equal))

(defmethod positions (item (sequence list) &key (test 'eql))
  (labels ((sub (item xs pos acc)
             (cond
               ((null xs) (reverse acc))
               ((funcall test
                         (first xs)
                        item)
                (sub item
                     (rest xs)
                     (1+ pos)
                     (cons pos
                           acc)))
               (t
                (sub item
                     (rest xs)
                     (1+ pos)
                     acc)))))
    (sub item sequence 0 nil)))



(defun enumerate-divisor (k)
  (if (= k 1)
      (list 1)
      (labels ((sub (k d acc)
                 (cond
                   ((> (* d d)
                       k)
                    (sort acc #'<))
                   ((zerop (rem k d))
                    (sub k
                         (1+ d)
                         (if (= (* d d)
                                k)
                             (cons d acc)
                             (cons d
                                   (cons (floor k d)
                                         acc)))))
                   (t
                    (sub k
                         (1+ d)
                         acc)))))
        (sub k 1 nil))))

(defun group-by (fn list &key key)
  (let ((memo (make-hash-table :test #'equal)))
    (mapc (lambda (x)
            (let ((x (funcall (or key 'identity) x)))
              (push x (gethash (funcall fn x) memo))))
          list)
    (let ((res nil))
      (maphash (lambda (_ val)
                 (push (sort val #'<) res))
               memo)
      res)))

(defun chunk-every (count list &optional (step count))
  (let ((acc nil))
    (labels ((sub (xs)
               (cond
                 ((null (nthcdr (1- count) xs))
                  (when xs
                    (push xs acc)))
                 (t (push (butlast xs (- (length xs)
                                         count))
                          acc)
                    (sub (nthcdr step xs))))))
      (sub list)
      (reverse acc))))


;;;----------------debug----------------



#+swank
(defun get-clipboard ()
  (with-output-to-string (out)
    (sb-ext:run-program "/usr/bin/bash" '("-c" "parcellite" "-p") :output out :search t)))

#+swank
(defun remove-first-line (string)
  (let ((first-n (position #\Newline string)))
    (when (null first-n)
      (error "No Newline in the string."))
    (subseq string (1+ first-n))))


#+swank
(defun run (&key (take-time nil) (fn 'main) (out *standard-output*))
  (let ((*standard-output* out))
    (with-input-from-string (*standard-input* (delete #\Return (remove-first-line (get-clipboard))))
      (if take-time
          (time
           (progn
             (sb-sprof:start-profiling)
             (funcall fn)
             (sb-sprof:stop-profiling)
             (sb-sprof:report)))
          (funcall fn)))))

#|
------------------------------------
                Body                
------------------------------------
|#


(in-package :procon-user)



(defun main ()
  (declare #.OPT)
  (let ((n (read)))
    (let ((a (read-numbers-to-list n)))
      (println (labels ((rec (xs res)
                          (if (null xs)
                              res
                              (if (and (second xs)
                                       (not (zerop (second xs))))
                                  (rec (cons (- (second xs)
                                                (rem (first xs) 2))
                                             (cddr xs))
                                       (+ res
                                          (ceiling (first xs)
                                                   2)))
                                  (rec (rest xs)
                                       (+ res
                                          (floor (first xs)
                                                 2)))))))
                 (rec a 0))))))



#-swank (procon-user:main)
