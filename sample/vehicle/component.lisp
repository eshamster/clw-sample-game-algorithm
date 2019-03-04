(defpackage :clw-sample-game-algorithm/sample/vehicle/component
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :vehicle-component
           :init-vehicle-component
           :vehicle-component-parent
           :vehicle-component-children
           :vehicle-component-registerp
           :vehicle-component-velocity
           :vehicle-component-mass
           :vehicle-component-max-speed
           :vehicle-component-max-force
           :vehicle-component-max-turn-rate
           ;; utils
           :vehicle-component-speed))
(in-package :clw-sample-game-algorithm/sample/vehicle/component)

(defstruct.ps+ (vehicle-component (:include ecs-component))
    velocity mass max-speed max-force max-turn-rate)

(defun.ps+ init-vehicle-component (&key (mass 1)
                                        (max-speed #lx5)
                                        (max-force #lx0.5)
                                        (max-turn-rate (/ PI 30)))
  (make-vehicle-component
   :velocity (make-vector-2d)
   :mass mass
   :max-speed max-speed
   :max-force max-force
   :max-turn-rate max-turn-rate))

;; --- utils --- ;;

(defun.ps+ vehicle-component-speed (vehicle-component)
  (vector-2d-abs (vehicle-component-velocity vehicle-component)))
