(defpackage :clw-sample-game-algorithm/sample/vehicle/group-behavior
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :set-group-alignment
           :set-group-cohesion)
  (:import-from :clw-sample-game-algorithm/sample/vehicle/steering
                :register-force-calculator
                :seek))
(in-package :clw-sample-game-algorithm/sample/vehicle/group-behavior)

;; --- setters for each behavior --- ;;

(defun.ps+ set-group-alignment (steering
                                &key neighbors (weight 1))
  (register-force-calculator :group-alignment steering
                             (init-alignment
                              neighbors :weight weight)))

(defun.ps+ set-group-cohesion (steering
                               &key neighbors (weight 1))
  (register-force-calculator :group-cohesion steering
                             (init-cohesion
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

(defun.ps+ init-cohesion (neighbors &key (weight 1))
  (lambda (vehicle-cmp vehicle-point)
    (let ((num-neighbors (length neighbors)))
      (if (> num-neighbors 0)
          (let ((sum-point (make-vector-2d)))
            (dolist (neighbor-vehicle neighbors)
              (incf-vector-2d sum-point (calc-global-point neighbor-vehicle)))
            (let ((seek-force (seek vehicle-cmp vehicle-point
                                    (/-vec-scalar sum-point num-neighbors))))
              (setf-vector-2d-abs seek-force weight)
              seek-force))
          (make-vector-2d)))))
