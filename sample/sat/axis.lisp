(defpackage clw-sample-game-algorithm/sample/SAT/axis
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game/core/basic-components
        :cl-web-2d-game/utils/calc)
  (:export :project-polygon-to-axis
           :get-intersected-projection
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

(defun.ps+ get-intersected-projection (proj1 proj2)
  "If the two projectios are intersected, returns a new intersected projection.
Otherwise returns nil."
  (assert (eq (projection-sat-axis proj1)
              (projection-sat-axis proj2)))
  (with-slots-pair (((min1 min) (max1 max)) proj1
                    ((min2 min) (max2 max)) proj2)
    (when (>= (- (min max1 max2)
                 (max min1 min2))
              0)
      (make-projection-sat :axis (projection-sat-axis proj1)
                           :min (max min1 min2)
                           :max (min max1 max2)))))

