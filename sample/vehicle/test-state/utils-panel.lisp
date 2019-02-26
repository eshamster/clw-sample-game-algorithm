(defpackage :clw-sample-game-algorithm/sample/vehicle/test-state/utils-panel
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :add-changing-max-speed-panel
           :add-changing-max-force-panel)
  (:import-from :clw-sample-game-algorithm/sample/vehicle/component
                :vehicle-component)
  (:import-from :ps-experiment/common-macros
                :setf-with))
(in-package :clw-sample-game-algorithm/sample/vehicle/test-state/utils-panel)

(defun.ps+ add-changing-max-speed-panel (vehicle)
  (add-panel-number
   "Max Speed" #lx5
   :min #lx1 :max #lx15 :step #lx0.5
   :on-change (lambda (value)
                (setf-with (get-component vehicle)
                  max-speed value))))

(defun.ps+ add-changing-max-force-panel (vehicle)
  (add-panel-number
   "Max Force" #lx0.5
   :min #lx0.1 :max #lx1.5 :step #lx0.05
   :on-change (lambda (value)
                (setf-with (get-component vehicle)
                  max-force value))))

(defun.ps+ get-component (vehicle)
  (get-ecs-component 'vehicle-component vehicle))
