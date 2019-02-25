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
                :init-obstacles)
  (:import-from :ps-experiment/common-macros
                :setf-with))
(in-package :clw-sample-game-algorithm/sample/vehicle/test-state/avoid-obstacle)

(def-test-state avoid-obstacle-state ()
  :start-process
  (state-lambda ()
    (init-obstacles)
    (add-ecs-entity (make-wander-avoiding-vehicle
                     :display-search-dist t)))

  :register-name-initializer-pairs
  ((:avoid-obstacle (make-avoid-obstacle-state))))
