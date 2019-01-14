(defpackage :clw-sample-game-algorithm/sample/vehicle/steering
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :steering
           :init-steering
           :calc-steering
           :set-seek-point)
  (:import-from :clw-sample-game-algorithm/sample/vehicle/component
                :vehicle-component
                :vehicle-component-velocity
                :vehicle-component-max-force))
(in-package :clw-sample-game-algorithm/sample/vehicle/steering)

(defstruct.ps+ (steering (:include ecs-component))
    target-point
  (seek-on-p t))

(defun.ps+ init-steering ()
  (make-steering))

(defun.ps+ calc-steering (vehicle)
  (with-ecs-components (point-2d vehicle-component steering) vehicle
    (with-slots (target-point seek-on-p)
        steering
      (let ((result-force (make-vector-2d)))
        (when (and seek-on-p target-point)
          (incf-vector-2d result-force
                          (seek vehicle-component point-2d target-point)))
        (truncate-vector-2d result-force
                            (vehicle-component-max-force vehicle-component))))))

(defun.ps+ set-seek-point (steering target-point)
  (check-type target-point vector-2d)
  (setf (steering-target-point steering)
        (clone-vector-2d target-point)))

(defun.ps+ seek (vehicle-cmp vehicle-point target-point)
  (let ((desired-velocity (decf-vector-2d (clone-point-2d target-point)
                                          vehicle-point)))
    (decf-vector-2d desired-velocity
                    (vehicle-component-velocity vehicle-cmp))))
