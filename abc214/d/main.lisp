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


;;---------------------------------Body---------------------------------

(in-package #:cl-user)

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

(declaim (inline println))
(defun println (obj &optional (stream *standard-output*))
  (let ((*read-default-float-format* 'double-float))
    (prog1 obj
      (princ obj stream)
      (terpri stream))))

(defun main ()
  (let* ((n (read))
         (edges (loop repeat (1- n) collect
                                    (let ((u (1- (read-fixnum)))
                                          (v (1- (read-fixnum)))
                                          (w (read-fixnum)))
                                      (list w u v)))))
    (setf edges (sort edges #'< :key #'first))
    (let ((res 0)
          (uf (uf:make-uf-tree n)))
      (loop for (w u v) in edges
            do (incf res
                     (* w
                        (uf:get-tree-size uf u)
                        (uf:get-tree-size uf v)))
               (uf:unite uf u v))
      (println res))))

#-swank (main)
