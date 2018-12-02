(defpackage clw-sample-game-algorithm/sample/SAT/collision
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :init-sat-object
           :collide-sat-object-p)
  (:import-from :clw-sample-game-algorithm/sample/SAT/axis
                :get-intersected-projection-length
                :init-axis-sat
                :project-polygon-to-axis)
  (:import-from :ps-experiment/common-macros
                :with-slots-pair))
(in-package :clw-sample-game-algorithm/sample/SAT/collision)

;; point-list is a list of point-2d
;; axis-list is a list of axis-sat
(defstruct.ps+ sat-object point-list axis-list)

(defun.ps+ calc-normal (point1 point2)
  (with-slots-pair (((x1 x) (y1 y)) point1
                    ((x2 x) (y2 y)) point2)
    (make-vector-2d :x (* -1 (- y2 y1))
                    :y (- x2 x1))))

(defun.ps+ init-sat-object (point-list)
  (let ((result (make-sat-object :point-list point-list))
        (axis-list (list))
        (len (length point-list)))
    (dotimes (i len)
      (let ((point1 (nth i point-list))
            (point2 (nth (mod (1+ i) len) point-list)))
        (push (init-axis-sat (make-point-2d)
                             (calc-normal point1 point2))
              axis-list)))
    (setf (sat-object-axis-list result)
          (nreverse axis-list))
    result))

(defun.ps+ intersects-on-all-axis (point-list1 point-list2 axis-list)
  (every (lambda (axis)
           (>= (get-intersected-projection-length
                (project-polygon-to-axis point-list1 axis)
                (project-polygon-to-axis point-list2 axis))
               0))
         axis-list))

(defun.ps+ collide-sat-object-p (obj1 obj2)
  (check-type obj1 sat-object)
  (check-type obj2 sat-object)
  (with-slots-pair (((point-list1 point-list) (axis-list1 axis-list)) obj1
                    ((point-list2 point-list) (axis-list2 axis-list)) obj2)
    (and (intersects-on-all-axis point-list1 point-list2 axis-list1)
         (intersects-on-all-axis point-list1 point-list2 axis-list2))))
