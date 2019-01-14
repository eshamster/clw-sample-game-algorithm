(defpackage :clw-sample-game-algorithm/sample/vehicle/system
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :init-vehicle-system)
  (:import-from :clw-sample-game-algorithm/sample/vehicle/component
                :vehicle-component)
  (:import-from :clw-sample-game-algorithm/sample/vehicle/steering
                :steering
                :calc-steering)
  (:import-from :ps-experiment/common-macros
                :setf-with))
(in-package :clw-sample-game-algorithm/sample/vehicle/system)

(defun.ps+ init-vehicle-system ()
  (register-ecs-system "vehicle" (make-vehicle-system)))

(defstruct.ps+
    (vehicle-system
     (:include ecs-system
               (target-component-types '(point-2d vehicle-component steering))
               (process #'update-vehicle))))

(defun.ps+ update-vehicle (vehicle)
  (with-ecs-components (vehicle-component point-2d) vehicle
    (with-slots (velocity mass max-speed heading side) vehicle-component
      (let* ((steering-force (calc-steering vehicle))
             (accele (/-vec-scalar steering-force mass))
             (time-elapsed 1))
        (incf-vector-2d velocity (*-vec-scalar accele time-elapsed))
        (truncatef-vector-2d velocity max-speed)
        (incf-vector-2d point-2d (*-vec-scalar velocity time-elapsed))
        (when (> (vector-2d-abs velocity) 0.000001)
          (setf heading (setf-vector-2d-abs (clone-vector-2d velocity) 1.0))
          (setf-with side
            x (vector-2d-y heading)
            y (* -1 (vector-2d-x heading)))
          (setf (point-2d-angle point-2d)
                (vector-2d-angle heading)))))))
