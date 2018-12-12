(defpackage clw-sample-game-algorithm/sample/SAT/axis
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game/core/basic-components
        :cl-web-2d-game/utils/calc)
  (:export :project-polygon-to-axis
           :get-intersected-projection-length
           :init-axis-sat
           :axis-sat-base-point
           :axis-sat-unit-vector

           :projection-sat
           :projection-sat-axis
           :projection-sat-max
           :projection-sat-min)
  (:import-from :ps-experiment/common-macros
                :with-slots-pair))
(in-package :clw-sample-game-algorithm/sample/SAT/axis)

(defstruct.ps+ axis-sat base-point unit-vector)
(defstruct.ps+ projection-sat axis min max)

(defun.ps+ init-axis-sat (base-point direction-vector)
  (let ((result (make-axis-sat)))
    (with-slots ((base base-point) (unit unit-vector)) result
      (setf base (clone-vector-2d base-point)
            unit (clone-vector-2d direction-vector))
      (setf-vector-2d-abs unit 1.0))
    result))

(defun.ps+ project-point-to-length-on-axis (point axis)
  (let ((rel-vector (clone-vector-2d point)))
    (decf-vector-2d rel-vector
                    (axis-sat-base-point axis))
    (let ((angle (diff-angle (vector-2d-angle rel-vector)
                             (vector-2d-angle (axis-sat-unit-vector axis)))))
      (* (vector-2d-abs rel-vector) (cos angle)))))

(defun.ps+ project-polygon-to-axis (point-list axis)
  (let ((result (make-projection-sat :axis axis)))
    (with-slots (min max) result
      (dolist (point point-list)
        (let ((len (project-point-to-length-on-axis point axis)))
          (when (or (null min)
                    (< len min))
            (setf min len))
          (when (or (null max)
                    (> len max))
            (setf max len)))))
    result))

(defun.ps+ get-intersected-projection-length (proj1 proj2)
  "The return value >= 0, they are intersected. Otherwise, they are not."
  (assert (eq (projection-sat-axis proj1)
              (projection-sat-axis proj2)))
  (with-slots-pair (((min1 min) (max1 max)) proj1
                    ((min2 min) (max2 max)) proj2)
    (- (min max1 max2)
       (max min1 min2))))
