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
  (when (typep object 'double-float) (setq object (exp-double->fixed-double-str object)))
  (write object :stream stream :escape nil :readably nil)
  (terpri))

(defun read-numbers-to-list (size)
  (loop repeat size collect (read-fixnum)))

(defun read-numbers-to-array (size)
  (declare (fixnum size))
  (let ((arr (make-array size
                         :element-type 'fixnum
                         :adjustable nil)))
    (declare ((array fixnum 1) arr))
    (loop for i of-type fixnum below size do
         (setf (aref arr i) (read-fixnum))
       finally
         (return arr))))




(defmethod princ-for-each-line ((sequence list))
  (format t "~{~a~&~}" sequence))

(defmethod princ-for-each-line ((sequence vector))
  (loop for i below (length sequence) do
       (princ (aref sequence i))
       (fresh-line)))

(declaim (inline unwrap))
(defun unwrap (sequence)
  ;; Ex. (unwrap (list 1 2 3 4 5)) => "1 2 3 4 5"
  (let ((*standard-output* (make-string-output-stream)))
    (let ((init nil))
      (map nil
           (lambda (x)
             (when init
               (princ #\space))
             (setq init t)
             (princ x))
           sequence))
    (get-output-stream-string *standard-output*)))

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

(declaim (inline iota))
(defun iota (count &optional (start 0) (step 1))
  (loop for i from 0 below count collect (+ start (* i step))))


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


(declaim (inline count-subsequence))
(defun count-subsequence (mainstr substr)
  ;; mainstrに含まれる substr(部分列ではない)のindexを返す
  ;; O(n^2)
  (let ((main-len (length mainstr))
        (sub-len (length substr)))
    (remove-if-not (lambda (i)
                     (every (lambda (j)
                              (char-equal (char mainstr (+ i j))
                                          (char substr j)))
                            (iota sub-len)))
                   (iota (1+ (- main-len sub-len))))))

(defun memoize (fn)
  ;; メモ化関数をクロージャとして返す
  ;; setf symbol-functionして使う
  (let ((memo (make-hash-table)))
    (lambda (&rest args)
      (multiple-value-bind (val win) (gethash args memo)
        (if win
            val
            (setf (gethash args memo)
                  (apply fn args)))))))

(defmacro -> (initial-val &rest forms)
  ;; Port of threading macro from Clojure.
  ;; form内に :@ キーワードがあればその位置に, なければ第一引数の位置に
  ;; 式を挿入する
  ;; e.g. (-> (list 1 2 3 4 5) cdr) => (2 3 4 5)
  ;;      (-> (list 1 2 3 4 5) (position 1 :@) => 0
  (loop
    for form in forms
    with res = initial-val
    do
       (cond
         ((atom form)    (setf res `(,form ,res)))
         ((find :@ form) (setf res (substitute res :@ form)))
         (t              (setf res (append (first form)
                                           res
                                           (rest form)))))
    finally
       (return res)))


(defmacro ->> (initial-val &rest forms)
  ;; Port of threading macro from Clojure.
  ;; form内に :@ キーワードがあればその位置に, なければ最後の引数の位置に
  ;; 式を挿入する
  ;; e.g. (-> (list 1 2 3 4 5) cdr) => (2 3 4 5)
  ;;      (-> (list 1 2 3 4 5) (position 1 :@) => 0
  (loop
    for form in forms
    with res = initial-val
    do
       (cond
         ((atom form)    (setf res `(,form ,res)))
         ((find :@ form) (setf res (substitute res :@ form)))
         (t              (setf res (append form (list res)))))
    finally
       (return res)))

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
  ;; 
  ;; Example.
  ;; 
  ;; (split-string-into-list "abc def ehi jk")
  ;;  ;=> ("abc" "def" "ehi" "jk")
  ;;
  (let ((pos (position #\space string)))
    (if pos
        (cons (subseq string 0 pos)
              (split-string-into-list (subseq string (1+ pos))
                                      separator))
        (list string))))

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

(defun map-every (fn list &optional (count 2) (step count))
  (mapcar (lambda (xs) (apply fn xs)) (chunk-every count list step)))


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

(defconstant +mod+ 998244353)

#|
------------------------------------
                Body                
------------------------------------
|#


(in-package :procon-user)


;;; start inserted contents




(declaim (ftype (function (finxum &optional fixnum) fixnum) modint))
(defun modint (x &optional (m +mod+))
  (declare (integer x))
  (cond
    ((and (>= x 0) (< x m)) x)
    ((minusp x) (modint (+ x m)))
    (t (mod x m))))

(declaim (ftype (function (&rest fixnum) fixnum) mod+))
(defun mod+ (&rest args)
  (declare (inline modint))
  (if (null args)
      0
      (modint (reduce #'+ args))))


(declaim (ftype (function (&rest fixnum) fixnum) mod-))
(defun mod- (&rest args)
  (declare (inline modint))
  (if (null args)
      0
      (modint (reduce #'- args))))

(declaim (ftype (function (&rest fixnum) fixnum) mod*))
(defun mod* (&rest args)
  (declare (inline modint))
  (if (null args)
      0
      (modint (reduce #'* args))))

(declaim (ftype (function (fixnum &optional fixnum) fixnum) mod-inv))
(defun mod-inv (a &optional (m +mod+))
  (declare (integer a m))
  (let ((b m)
        (u 1)
        (v 0))
    (loop until (zerop b) do
         (let ((w (truncate a b)))
           (decf a (* w b))
           (rotatef a b)
           (decf u (* w v))
           (rotatef u v))
       finally
         (loop while (minusp u) do
              (incf u m))
         (return (mod u m)))))

(declaim (ftype (function (&rest fixnum) fixnum) mod/))
(defun mod/ (&rest args)
  (declare (inline modint))
  (if (null args)
      0
      (reduce (lambda (x y)
                (modint (* x (mod-inv y))))
              args
              :initial-value 1)))

(declaim (ftype (function (finxum fixnum &optional fixnum) fixnum) mod-power))
(defun mod-power (a n &optional (m +mod+))
  (declare (integer a)
           ((integer 0) n))
  (labels ((sub (a n &optional (res 1))
             (if (zerop n)
                 res
                 (sub (mod (* a a) m)
                      (truncate n 2)
                      (if (oddp n)
                          (mod (* res a) m)
                          res)))))
    (sub a n)))


(declaim (ftype (function (fixnum fixnum &optional fixnum) fixnum) mod-binomial))
(defun mod-binomial (n k &optional (m +mod+))
  (declare ((integer 0) m))
  (if (or (< n k) (< n 0) (< k 0))
      0
      (let ((k (if (< k (- n k)) k (- n k)))
            (num 1)
            (denom 1))
        (declare ((integer 0) k num denom))
        (loop for x from n above (- n k) do
             (setq num (mod (* num x) m)))
        (loop for x from 1 to k do
             (setq denom (mod (* denom x) m)))
        (mod (* num (mod-inv denom m)) m))))


;;; end inserted contents

(defun sum* (n)
  (mod* n (mod+ n 1)))

(defun main ()
  (declare #.OPT)
  (let ((a (read))
        (b (read))
        (c (read)))
    (println (mod* (mod* (sum* a)
                         (sum* b)
                         (sum* c))
                   (mod-inv 8)))))
        


#-swank (procon-user:main)
