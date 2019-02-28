(defpackage :clw-sample-game-algorithm/sample/vehicle/test-state/interpose
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:import-from :clw-sample-game-algorithm/sample/vehicle/steering
                :steering
                :set-interpose)
  (:import-from :clw-sample-game-algorithm/sample/vehicle/test-state/utils
                :def-test-state
                :make-test-vehicle
                :make-wander-vehicle))
(in-package :clw-sample-game-algorithm/sample/vehicle/test-state/interpose)

(def-test-state interpose-state ()
  :start-process
  (state-lambda ()
    (let ((target1 (make-wander-vehicle :display-wander-circle-p nil))
          (target2 (make-wander-vehicle :display-wander-circle-p nil))
          (vehicle (make-test-vehicle :color #xff0000)))
      (add-ecs-entity target1)
      (add-ecs-entity target2)
      (set-interpose (get-ecs-component 'steering vehicle)
                     :target-vehicle1 target1
                     :target-vehicle2 target2)
      (add-ecs-component-list
       vehicle)
      (add-ecs-entity vehicle)))

  :register-name-initializer-pairs
  ((:interpose (make-interpose-state))))
