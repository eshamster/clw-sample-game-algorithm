(defpackage :clw-sample-game-algorithm/sample/vehicle/test-state/wander
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:import-from :clw-sample-game-algorithm/sample/vehicle/steering
                :steering
                :set-wander-behavior)
  (:import-from :clw-sample-game-algorithm/sample/vehicle/test-state/utils
                :def-test-state
                :make-wander-vehicle
                :update-wander-param)
  (:import-from :clw-sample-game-algorithm/sample/vehicle/test-state/utils-panel
                :add-changing-max-speed-panel
                :add-changing-max-force-panel))
(in-package :clw-sample-game-algorithm/sample/vehicle/test-state/wander)

;; --- control panel --- ;;

(defun.ps+ add-changing-wander-control-panel (vehicle)
  (flet ((add-panel (title key init-value min max step)
           (add-panel-number
            title init-value
            :min min :max max :step step
            :on-change (lambda (value)
                         (apply #'update-wander-param vehicle key value)))))
    (add-panel "Wander Radius" :wander-radius #lx20 #lx1 #lx80 #lx1)
    (add-panel "Wander Distance" :wander-dist #lx60 #lx10 #lx180 #lx1)
    (add-panel "Wander Jitter" :wander-jitter #lx3 #lx0.1 #lx12 #lx0.1)))

(defun.ps+ init-control-panel (vehicle)
  (add-changing-max-speed-panel vehicle)
  (add-changing-max-force-panel vehicle)
  (add-changing-wander-control-panel vehicle))

;; --- state --- ;;

(def-test-state wander-state ()
  :start-process
  (state-lambda ()
    ;; init vehicle
    (let ((vehicle (make-wander-vehicle)))
      (add-ecs-entity vehicle)
      (init-control-panel vehicle)))

  :register-name-initializer-pairs
  ((:wander (make-wander-state))))
