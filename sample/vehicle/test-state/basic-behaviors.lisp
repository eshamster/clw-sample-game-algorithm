(defpackage :clw-sample-game-algorithm/sample/vehicle/test-state/basic-behaviors
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:import-from :clw-sample-game-algorithm/sample/vehicle/steering
                :steering
                :set-seek-point
                :set-flee-point
                :set-arrive-point)
  (:import-from :clw-sample-game-algorithm/sample/vehicle/test-state/utils
                :def-test-state
                :make-test-vehicle
                :make-target-entity
                :warp-when-over-edge)
  (:import-from :clw-sample-game-algorithm/sample/vehicle/test-state/utils-panel
                :add-changing-max-speed-panel
                :add-changing-max-force-panel)
  (:import-from :ps-experiment/common-macros
                :setf-with))
(in-package :clw-sample-game-algorithm/sample/vehicle/test-state/basic-behaviors)

(defun.ps+ init-control-panel (vehicle)
  (add-changing-max-speed-panel vehicle)
  (add-changing-max-force-panel vehicle))

(def-test-state basic-behavior-state (mode) ; :seek, :flee or :arrive
  :start-process
  (state-lambda (mode)
    (let ((vehicle (make-test-vehicle))
          (target (make-target-entity)))
      (add-ecs-entity target)
      (add-ecs-component-list
       vehicle
       (make-script-2d
        :func (lambda (entity)
                (let ((steering (get-ecs-component 'steering vehicle))
                      (target-point (get-ecs-component 'point-2d target)))
                  (ecase mode
                    (:seek (set-seek-point steering target-point))
                    (:flee (set-flee-point steering target-point))
                    (:arrive (set-arrive-point steering target-point))))
                (warp-when-over-edge entity))))
      (add-ecs-entity vehicle)
      (init-control-panel vehicle)))

  :register-name-initializer-pairs
  ((:seek (make-basic-behavior-state :mode :seek))
   (:flee (make-basic-behavior-state :mode :flee))
   (:arrive (make-basic-behavior-state :mode :arrive))))
