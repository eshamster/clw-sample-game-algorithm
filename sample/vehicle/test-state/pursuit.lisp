(defpackage :clw-sample-game-algorithm/sample/vehicle/test-state/pursuit
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:import-from :clw-sample-game-algorithm/sample/vehicle/component
                :vehicle-component)
  (:import-from :clw-sample-game-algorithm/sample/vehicle/steering
                :steering
                :set-pursuit-target)
  (:import-from :clw-sample-game-algorithm/sample/vehicle/test-state/utils
                :def-test-state
                :make-test-vehicle
                :make-wander-vehicle))
(in-package :clw-sample-game-algorithm/sample/vehicle/test-state/pursuit)

(defun.ps+ make-pursuit-vehicle (target)
  (let ((vehicle (make-test-vehicle :first-x #lx100
                                    :first-y #ly100)))
    (set-pursuit-target
     (get-ecs-component 'steering vehicle)
     target)
    vehicle))

(defun.ps+ make-pursuit-target-visualizer (vehicle evader)
  (let ((visualizer (make-ecs-entity)))
    (add-ecs-component-list
     visualizer
     (make-point-2d :x #lx-1000 :y #ly1000)
     (make-model-2d :model (make-wired-circle :r #lx5 :color #xff0000)
                    :depth 100)
     (make-script-2d
      :func (lambda (entity)
              (with-ecs-components (point-2d) entity
                (copy-vector-2d-to
                 point-2d
                 (clw-sample-game-algorithm/sample/vehicle/steering::calc-pursuit-point
                  (get-ecs-component 'vehicle-component vehicle)
                  (calc-global-point vehicle)
                  (get-ecs-component 'vehicle-component evader)
                  (calc-global-point evader)))))))
    visualizer))

(def-test-state pursuit-state ()
  :start-process
  (state-lambda ()
    (let* ((evader (make-wander-vehicle))
           (pursuit-vehicle (make-pursuit-vehicle evader)))
      (add-ecs-entity evader)
      (add-ecs-entity pursuit-vehicle)
      (add-ecs-entity (make-pursuit-target-visualizer
                       pursuit-vehicle evader))))

  :register-name-initializer-pairs
  ((:pursuit (make-pursuit-state))))
