(defpackage clw-sample-game-algorithm/utils
  (:use :cl)
  (:export :use-this-package-as-sample
           :make-js-main-file)
  (:import-from :ps-experiment
                :defvar.ps+
                :defun.ps+))
(in-package :clw-sample-game-algorithm/utils)

(defvar *sample-package-table (make-hash-table))

(defmacro use-this-package-as-sample (&key
                                        (init-func (intern "INIT-FUNC" *package*))
                                        (update-func (intern "UPDATE-FUNC" *package*)))
  `(progn
     (defun ,(intern "OUTPUT-JS-CODE" *package*) (stream)
       (princ
        (pse:with-use-ps-pack (:this :clw-sample-game-algorithm/utils)
          (let ((width *screen-width*)
                (height *screen-height*))
            (cl-web-2d-game:start-2d-game
             :screen-width width
             :screen-height height
             :resize-to-screen-p t
             :camera (cl-web-2d-game:init-camera 0 0 width height)
             :rendered-dom (document.query-selector "#renderer")
             :stats-dom (document.query-selector "#stats-output")
             :monitoring-log-dom (document.query-selector "#monitor")
             :event-log-dom (document.query-selector "#eventlog")
             :init-function ,init-func
             :update-function ,update-func)))
        stream))))

(defun make-js-main-file (name path)
  (with-open-file (out path
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (funcall (intern "OUTPUT-JS-CODE"
                     (find-package (format nil "CLW-SAMPLE-GAME-ALGORITHM/SAMPLE/~A"
                                           (string-upcase name))))
             out)))

(eval-when (:execute :compile-toplevel :load-toplevel)
  (defvar.ps+ *screen-width* 800)
  (defvar.ps+ *screen-height* 600)

  (defun.ps+ calc-absolute-length (relative-length base-length)
    (* relative-length base-length 0.001))

  "#Ex1. '#lx500' represents a half length of the field width."
  "#Ex2. '#ly500' represents a half length of the field height."
  (set-dispatch-macro-character
   #\# #\l
   #'(lambda (stream &rest rest)
       (declare (ignore rest))
       (case (peek-char nil stream)
         (#\x (read-char stream)
              `(calc-absolute-length ,(read stream) *screen-width*))
         (#\y (read-char stream)
              `(calc-absolute-length ,(read stream) *screen-height*))
         (t (error "Not recognized character after #l"))))))
