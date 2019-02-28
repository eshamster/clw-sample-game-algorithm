(defpackage :clw-sample-game-algorithm/sample/vehicle/test-state/avoid-obstacle
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:import-from :clw-sample-game-algorithm/sample/vehicle/component
                :vehicle-component
                :vehicle-component-velocity
                :vehicle-component-max-speed)
  (:import-from :clw-sample-game-algorithm/sample/vehicle/steering
                :steering
                :set-avoid-obstacle)
  (:import-from :clw-sample-game-algorithm/sample/vehicle/test-state/utils
                :def-test-state
                :make-wander-vehicle
                :make-wander-avoiding-vehicle
                :init-obstacles
                :update-avoiding-obstacle-param)
  (:import-from :clw-sample-game-algorithm/sample/vehicle/test-state/utils-panel
                :add-changing-max-speed-panel
                :add-changing-max-force-panel)
  (:import-from :ps-experiment/common-macros
                :setf-with))
(in-package :clw-sample-game-algorithm/sample/vehicle/test-state/avoid-obstacle)

(defun.ps+ add-changing-avoiding-control-panel (vehicle)
  (flet ((add-panel (title key init-value min max step)
           (add-panel-number
            title init-value
            :min min :max max :step step
            :on-change (lambda (value)
                         (apply #'update-avoiding-obstacle-param
                                vehicle key value)))))
    (add-panel "Vehicle Width" :vehicle-width #lx20 #lx1 #lx80 #lx1)
    (add-panel "Max Search Dist" :max-search-dist #lx100 #lx50 #lx200 #lx0.1)))

(defun.ps+ init-control-panel (vehicle)
  (add-changing-max-speed-panel vehicle :init-value #lx2)
  (add-changing-max-force-panel vehicle :init-value #lx0.1)
  (add-changing-avoiding-control-panel vehicle))

(def-test-state avoid-obstacle-state ()
  :start-process
  (state-lambda ()
    (init-obstacles)
    (let ((vehicle (make-wander-avoiding-vehicle
                    :display-search-dist t)))
      (add-ecs-entity vehicle)
      (init-control-panel vehicle)))

  :register-name-initializer-pairs
  ((:avoid-obstacle (make-avoid-obstacle-state))))
