(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *opt*
    #+swank '(optimize (speed 3) (safety 2))
    #-swank '(optimize (speed 3) (safety 0) (debug 0)))
  #+swank (load "~/Dropbox/Code/atcoder/ac-tools/act.lisp")
  #-swank (declaim (sb-ext:muffle-conditions sb-ext:compiler-note)))

(defmacro dbg (&rest forms)
  #-swank (declare (ignore forms))
  #+swank `(format *error-output* "~a => ~a~&" ',forms `(,,@forms)))

(defmacro do-iota ((var count &optional (begin 0) (step 1)) &body body)
  (let ((cnt (gensym)))
    `(loop for ,cnt of-type fixnum below ,count
           with ,var of-type fixnum = ,begin
           do ,@body
              (incf (the fixnum ,var) (the fixnum ,step)))))

(defmacro do-rep (count &body body) `(loop repeat ,count do ,@body))

(defmacro nlet (name binds &body body)
  `(labels ((,name (,@(mapcar #'first binds))
              ,@body))
     (,name ,@(mapcar #'second binds))))

(declaim (inline println))
(defun println (obj &optional (stream *standard-output*))
  (let ((*read-default-float-format* 'double-float))
    (princ obj stream)
    (terpri)))

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

;; Modify here depending to circumustances
;; e.g. Range-Minimum-Query(RMQ)
(define-segment-tree seg
  :element-type fixnum
  :result-type fixnum
  :fn +
  :e 0)

;;;
;;; EOF
;;;


(defun main ()
  (declare #.*opt*
           (inline sort))
  (let ((n (read))
        (d (read))
        (a (read)))
    (declare (fixnum n d a))
    (let ((points (make-array n :element-type 'list :adjustable nil :initial-element nil)))
      (declare ((simple-array list (*)) points))
      (dotimes (i n)
        (setf (aref points i) (list (read) (ceiling (read) a))))
      (setf points (sort points #'< :key #'first))
      (let ((r 0)
            (res 0)
            (seg (make-seg n)))
        (declare (fixnum r res)
                 (seg seg))
        (dotimes (l n)
          (declare (fixnum l))
          (destructuring-bind (xl dl) (aref points l)
            (declare (fixnum xl dl))
            (loop while (and (< r n)
                             (destructuring-bind (xr dr) (aref points r)
                               (declare (fixnum xr)
                                        (ignorable dr))
                               (<= (- xr xl)
                                   (ash d 1))))
                  do (incf r))
            (let ((val (max 0 (- dl
                                 (seg-fold seg l r)))))
              (declare (fixnum val))
              (incf res val)
              (seg-update seg (1- r)
                          (the fixnum (+ (seg-fold seg (1- r) r)
                                         val))))))
        (println res)))))

#-swank
(progn
  (setf (sb-alien:extern-alien "thread_control_stack_size" sb-kernel::os-vm-size-t)
        (* 256 1024 1024))
  (sb-thread:join-thread (sb-thread:make-thread #'main)))
