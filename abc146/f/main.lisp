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

(defconstant +inf+ #.(expt 10 14))

;; e.g. Range-Minimum-Query(RMQ)
(define-segment-tree seg
  :element-type fixnum
  :result-type fixnum
  :fn min
  :e +inf+)

;;;
;;; EOF
;;;


(defun main ()
  (declare #.*opt*)
  (let* ((n (read))
         (m (read))
         (s (read-line))
         (dp (make-seg (1+ n))))
    (declare (fixnum n m)
             (string s)
             (seg dp))
    (seg-update dp 0 0)
    (dotimes! (i n 1)
      (when (char= #\0 (char s i))
        (seg-update dp
                    i
                    (min +inf+
                         (1+ (seg-fold dp
                                       (max 0 (- i m))
                                       i))))))
    (let ((res (seg-fold dp n (1+ n))))
      (when (= res +inf+)
        (println -1)
        (return-from main))
      (labels ((build (node cnt acc)
                 (cond
                   ((zerop node)
                    (assert (zerop cnt))
                    (let ((init nil))
                      (dolist (x acc)
                        (when init
                          (princ #\Space))
                        (setf init t)
                        (princ x))
                      (terpri)
                      (return-from main)))
                   (t (when (plusp cnt)
                        (loop for i from (- node m) to (- node 1) do
                          (when (and (>= i 0)
                                     (= (1- cnt)
                                        (seg-fold dp i (1+ i))))
                            (build i
                                   (1- cnt)
                                   (cons (- node i) acc)))))))))
        (build n res nil)))))

#-swank
(progn
  (setf (sb-alien:extern-alien "thread_control_stack_size" sb-kernel::os-vm-size-t)
        (* 256 1024 1024))
  (sb-thread:join-thread (sb-thread:make-thread #'main)))
