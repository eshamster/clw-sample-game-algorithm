(defpackage :clw-sample-game-algorithm/sample/vehicle/test-state/wander
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:import-from :clw-sample-game-algorithm/sample/vehicle/test-state/utils
                :def-test-state
                :make-wander-vehicle))
(in-package :clw-sample-game-algorithm/sample/vehicle/test-state/wander)

(def-test-state wander-state ()
  :start-process
  (state-lambda ()
    (add-ecs-entity (make-wander-vehicle)))

  :register-name-initializer-pairs
  ((:wander (make-wander-state))))
