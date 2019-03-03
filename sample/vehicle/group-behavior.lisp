(defpackage :clw-sample-game-algorithm/sample/vehicle/group-behavior
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :set-group-alignment)
  (:import-from :clw-sample-game-algorithm/sample/vehicle/steering
                :register-force-calculator))
(in-package :clw-sample-game-algorithm/sample/vehicle/group-behavior)

;; --- setters for each behavior --- ;;

(defun.ps+ set-group-alignment (steering
                                &key neighbors (weight 1))
  (register-force-calculator :group-alignment steering
                             (init-alignment
                              neighbors :weight weight)))

;; --- behavior --- ;;

(defun.ps+ init-alignment (neighbors &key (weight 1))
  (lambda (vehicle-cmp vehicle-point)
    (declare (ignore vehicle-cmp))
    (let ((num-neighbors (length neighbors)))
      (if (> num-neighbors 0)
          (let ((sum-angle 0))
            (dolist (neighbor-vehicle neighbors)
              (incf sum-angle
                    (point-2d-angle (calc-global-point neighbor-vehicle))))
            (let ((target-angle (/ sum-angle num-neighbors)))
              (*-vec-scalar
               (make-vector-2d :x (cos target-angle)
                               :y (sin target-angle))
               weight)))
          (make-vector-2d)))))
