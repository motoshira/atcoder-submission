(in-package :cl-user)

;;------------------------------Preferences------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+swank (declaim (optimize (speed 3) (safety 2)))
  #-swank (declaim (optimize (speed 3) (safety 0) (debug 0)))
  #+swank (load "~/ghq/github.com/motoshira/atcoder-submission/ac-tools/act.lisp")
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

;;;
;;; BOF
;;;

;; Union-Find Tree (0-indexed)

(defpackage :union-find
  (:nicknames :uf)
  (:use :cl)
  (:shadow :find)
  (:export :make-uf-tree :find :unite :friends-p :show-parents :get-tree-size :count-trees))

(in-package :union-find)

(defstruct (union-find-tree (:conc-name uf-)
                            (:constructor make-uf-tree (size)))
  (dat (make-array size :element-type 'fixnum
                        :adjustable nil
                        :initial-element -1)
   :type simple-array)
  (size size :type fixnum)
  (count size :type fixnum))


(defun find (uf x)
  (declare (union-find-tree uf)
           (fixnum x))
  (with-slots (dat size count) uf
    (declare (ignorable dat size count))
    (if (minusp (aref dat x))
        x
        (setf (aref dat x)
              (find uf (aref dat x))))))


(defun show-parents (uf)
  ;; Return the parent of each member in list.
  #-swank (declare (ignore uf))
  #+swank
  (locally (declare (union-find-tree uf))
    (with-slots (dat size count) uf
      (declare (ignorable count))
      (loop for i
              below size
            collect (if (minusp (aref dat i))
                        i
                        (find uf (aref dat i)))))))


(defmethod unite (uf x y)
  (declare (union-find-tree uf)
           (fixnum x y))
  (with-slots (dat size count) uf
    (declare (ignore size))
    (let ((x-parent (find uf x))
          (y-parent (find uf y)))
      (declare (fixnum x-parent y-parent))
      (when (> x-parent y-parent)
        (rotatef x-parent y-parent))
      (unless (= x-parent y-parent)
        (incf (aref dat x-parent)
              (aref dat y-parent))
        (setf (aref dat y-parent)
              x-parent)
        (decf count)))))

(defun get-tree-size (uf x)
  (declare (union-find-tree uf)
           (fixnum x))
  (with-slots (dat size count) uf
    (declare (ignorable size count))
    (the fixnum (- (aref dat (find uf x))))))

(defun friends-p (uf x y)
  (declare (union-find-tree uf)
           (fixnum x y))
  (= (find uf x)
     (find uf y)))

(defun count-trees (uf)
  (declare (union-find-tree uf))
  (uf-count uf))

(in-package :cl-user)

;;;
;;; EOF
;;;

(defparameter *max* (* 2 (expt 10 5)))

(defun main ()
  (let* ((n (read))
         (as (make-array n)))
    (dotimes (i n)
      (setf (aref as i)
            (read)))
    (let ((res 0)
          (uf (uf:make-uf-tree (1+ *max*))))
      (dotimes (i (ash n -1))
        (let ((l (aref as i))
              (r (aref as (- n i 1))))
          (when (and (/= l r)
                     (not (uf:friends-p uf l r)))
            (uf:unite uf l r)
            (incf res))))
      (println res))))

#-swank (main)
