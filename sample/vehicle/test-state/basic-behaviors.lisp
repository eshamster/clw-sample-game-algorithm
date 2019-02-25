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
                :warp-when-over-edge))
(in-package :clw-sample-game-algorithm/sample/vehicle/test-state/basic-behaviors)

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
      (add-ecs-entity vehicle)))

  :register-name-initializer-pairs
  ((:seek (make-basic-behavior-state :mode :seek))
   (:flee (make-basic-behavior-state :mode :flee))
   (:arrive (make-basic-behavior-state :mode :arrive))))
