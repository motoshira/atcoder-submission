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

;;---------------------------------Body---------------------------------

(in-package #:cl-user)

(declaim (inline println))
(defun println (obj &optional (stream *standard-output*))
  (let ((*read-default-float-format* 'double-float))
    (prog1 obj
      (princ obj stream)
      (terpri stream))))

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

;; BOF

(declaim (ftype (function (sequence) (values hash-table hash-table)) gen-decompressor))
(defun gen-compressor (sequence &optional (index-origin 0))
  (declare (inline sort sb-impl::stable-sort))
  (flet ((%remove-duplicates-into-list (sequence)
           (let ((used (make-hash-table :test #'eql))
                 (acc nil))
             (declare (hash-table used)
                      (list acc))
             (map nil
                  (lambda (x)
                    (unless (gethash x used)
                      (setf (gethash x used) t)
                      (push x acc)))
                  sequence)
             acc)))
    (let ((set (sort (%remove-duplicates-into-list sequence) #'<))
          (comp (make-hash-table :test #'eql))
          (decomp (make-hash-table :test #'eql)))
      (let ((n (the fixnum (length set))))
        (loop for i of-type fixnum from index-origin below (+ n index-origin)
              for x of-type integer in set
              do (setf (gethash x comp) i)
                 (setf (gethash i decomp) x))
        (values comp decomp)))))

;; EOF


(defun main ()
  (declare (inline sort))
  (let* ((h (read))
         (w (read))
         (n (read))
         (ps (loop repeat n collect (list (read) (read))))
         (y-comp (gen-compressor (mapcar #'first ps) 1))
         (x-comp (gen-compressor (mapcar #'second ps) 1)))
    (declare (ignorable h w))
    (loop for (y x) in ps
          do (println (unwrap
                       (list (gethash y y-comp)
                             (gethash x x-comp)))))))

#-swank (main)
