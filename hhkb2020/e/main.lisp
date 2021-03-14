#|
------------------------------------
                Utils                
------------------------------------
|#

;;; Most of these utils are quoted from https://competitive12.blogspot.com/2020/03/common-lisp.html. Thank you!

(in-package :cl-user)

(defconstant +mod+ 1000000007)
;(defconstant +mod+ 998244353)

;;;----------------init----------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter OPT
    #+swank '(optimize (speed 3) (safety 2))
    #-swank '(optimize (speed 3) (safety 0) (debug 0)))
  #+swank (progn (ql:quickload '(:cl-debug-print :fiveam))
                 (shadow :run)
                 (use-package :fiveam)))
#+swank (cl-syntax:use-syntax cl-debug-print:debug-print-syntax)

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


(declaim (inline println))
(defun println (obj &optional (stream *standard-output*))
  (let ((*read-default-float-format* 'double-float))
    (prog1
        (write obj :stream stream)
      (fresh-line stream))))


(declaim (inline read-numbers-to-list))
(defun read-numbers-to-list (size)
  (loop repeat size collect (read-fixnum)))

(declaim (inline read-numbers-to-array))
(defun read-numbers-to-array (size)
  (let ((arr (make-array size
                         :element-type 'fixnum
                         :adjustable nil)))
    (declare ((array fixnum 1) arr))
    (loop for i of-type fixnum below size do
         (setf (aref arr i) (read-fixnum))
       finally
         (return arr))))

(defmacro read-characters-to-board (row-size column-size)
  (let ((board (gensym))
        (r (gensym))
        (c (gensym))
        (tmp (gensym)))
    `(let ((,board (make-array (,row-size ,column-size) :element-type 'character :adjustable nil)))
       (dotimes (,r ,row-size ,board)
         (let ((,tmp (buffered-read-line)))
           (dotimes (,c ,column-size)
             (setf (aref ,board ,r ,c) (char ,tmp ,c))))))))

(declaim (inline princ-for-each-line))
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

(defmacro safe-sort (list test &key (key #'identity))
  `(progn
    (declaim (inline sort sb-impl::stable-sort-list))
    (sort (copy-seq ,list) ,test :key ,key)))

(defmacro maxf (place cand)
  `(setf ,place (max ,place ,cand)))

(defmacro minf (place cand)
  `(setf ,place (min ,place ,cand)))

(defmacro modf (place &optional (m +mod+))
  `(setf ,place (mod ,place ,m)))

(defmacro alambda (parms &body body)
  `(labels ((self ,parms ,@body))
     #'self))

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

(defmacro def-memoized-function (name lambda-list &body body)
  (let ((cache (gensym))
        (val (gensym))
        (win (gensym)))
    `(let ((,cache (make-hash-table :test #'equal)))
       (defun ,name ,lambda-list
         (multiple-value-bind (,val ,win) (gethash (list ,@lambda-list) ,cache)
           (if ,win
               ,val
               (setf (gethash (list ,@lambda-list) ,cache)
                    (progn
                       ,@body))))))))

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


(defmacro href (hash-table key &optional default)
  `(gethash ,key ,hash-table &optional ,default))

(defun zip (&rest lists)
  (apply 'mapcar 'list lists))

(defun compose (fn &rest functions)
  (reduce (lambda (f g)
            (lambda (args)
              (funcall f (apply g args))))
          functions
          :initial-value fn))

(defun conjoin (&rest functions)
  (lambda (x)
    (every (lambda (fn)
             (funcall fn x))
           functions)))


(defun disjoin (&rest functions)
  (lambda (x)
    (or (lambda (fn)
             (funcall fn x))
           functions)))


(defun split (string &optional (separator #\space))
  (labels ((sub (xs sep &optional (acc nil))
             (let ((pos (position sep xs)))
               (if pos
                   (sub (nthcdr (1+ pos) xs)
                        sep
                        (cons (concatenate 'string (subseq xs 0 pos))
                              acc))
                   (if (null xs)
                       (reverse acc)
                       (reverse (cons (concatenate 'string xs)
                                      acc)))))))
    (sub (concatenate 'list string)
         separator)))

(defun rempick (n list)
  (if (zerop n)
      nil
      (cons (car list)
            (rempick (1- n)
                     (cdr list)))))

(defun set-p (list &key (test #'equal))
  (let ((memo (make-hash-table :test test)))
    (labels ((%set-p (list)
               (cond
                 ((null list) t)
                 ((gethash (first list) memo) nil)
                 (t
                  (setf (gethash (first list)
                                 memo)
                        t)
                  (%set-p (rest list))))))
      (%set-p list))))


#|
------------------------------------
                Body                
------------------------------------
|#

(in-package :cl-user)


;;; start inserted contents




(declaim (ftype (function (fixnum &optional fixnum) fixnum) modint))
(defun modint (x &optional (m +mod+))
  (declare (integer x))
  (cond
    ((and (>= x 0) (< x m)) x)
    ((minusp x) (mod (+ x m) m))
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

(declaim (ftype (function (fixnum fixnum &optional fixnum fixnum) fixnum) mod-power))

(let ((memo (make-hash-table :test #'equal)))
  (defun mod-power (a n &optional (res 1) (m +mod+))
    (declare (fixnum a n res m))
    (multiple-value-bind (val win) (gethash (list a n res) memo)
        (cond
          (win val)
          ((zerop n) res)
          (t (setf (gethash (list a n res) memo)
                   (mod-power (mod (* a a) m)
                              (truncate n 2)
                              (if (oddp n)
                                  (mod (* res a) m)
                                  res)
                              m)))))))


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


(in-package :cl-user) ;; end inserted contents

(defun main ()
  (declare #.OPT)
  (let ((h (read))
        (w (read)))
    (declare (uint16 h w))
    (let ((board (make-array (list h w)
                             :element-type 'bit
                             :adjustable nil
                             :initial-element 0))
          (k 0))
      (declare ((array bit 2) board)
               (fixnum k))
      (dotimes (i h)
        (let ((tmp (read-line)))
          (declare (string tmp))
         (dotimes (j w)
            (if (char-equal (char tmp j)
                            #\#)
                (setf (aref board i j) 1)
                (progn
                  (setf (aref board i j) 0)
                  (incf k))))))
      (labels ((procedure-w (fn)
                 "水平方向に照らすマスの数を配列として返す"
                 (let ((arr (make-array (list h w)
                                        :element-type 'uint16
                                        :adjustable nil)))
                   (declare ((array uint16 2) arr))
                   (dotimes (i h arr)
                     (setf (aref arr i (funcall fn 0))
                           (if (zerop (aref board i (funcall fn 0)))
                               1
                               0))
                     (dotimes (j (1- w))
                       (if (zerop (aref board
                                        i
                                        (funcall fn (1+ j))))
                           (setf (aref arr
                                       i
                                       (funcall fn (1+ j)))
                                 (1+ (aref arr
                                           i
                                           (funcall fn j))))
                           (setf (aref arr
                                       i
                                       (funcall fn (1+ j)))
                                 0))))))
               (procedure-h (fn)
                 "垂直方向に照らすマスの数を配列として返す"
                 (let ((arr (make-array (list h w)
                                        :element-type 'uint16
                                        :adjustable nil)))
                   (declare ((array uint16 2) arr))
                   (dotimes (j w arr)
                     (setf (aref arr (funcall fn 0) j)
                           (if (zerop (aref board (funcall fn 0) j))
                               1
                               0))
                     (dotimes (i (1- h))
                       (if (zerop (aref board
                                        (funcall fn (1+ i))
                                        j))
                           (setf (aref arr
                                       (funcall fn (1+ i))
                                       j)
                                 (1+ (aref arr
                                           (funcall fn i)
                                           j)))
                           (setf (aref arr
                                       (funcall fn (1+ i))
                                       j)
                                 0)))))))
        (let ((left  (procedure-w (lambda (x) x)))
              (right (procedure-w (lambda (x) (- w x 1))))
              (up    (procedure-h (lambda (x) x)))
              (down  (procedure-h (lambda (x) (- h x 1)))))
          (declare ((array uint16 2) left right up down))
          (let ((res (mod* k (mod-power 2 k))))
            (declare ((integer 0 1000000007) res))
            (dotimes (i h)
              (dotimes (j w)
                (when (zerop (aref board i j))
                  (let ((space-count (- (+ (aref left i j)
                                           (aref right i j)
                                           (aref up i j)
                                           (aref down i j))
                                        3)))
                    (declare (uint32 space-count))
                    (setf res (mod- res
                                    (mod-power 2 (- k space-count))))))))
            (princ res)
            (terpri)))))))



#-swank (main)
