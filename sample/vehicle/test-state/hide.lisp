(defpackage :clw-sample-game-algorithm/sample/vehicle/test-state/hide
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:import-from :clw-sample-game-algorithm/sample/vehicle/component
                :vehicle-component)
  (:import-from :clw-sample-game-algorithm/sample/vehicle/steering
                :steering
                :set-avoid-obstacle
                :set-hide)
  (:import-from :clw-sample-game-algorithm/sample/vehicle/test-state/utils
                :def-test-state
                :make-test-vehicle
                :make-wander-vehicle
                :make-wander-avoiding-vehicle
                :init-obstacles)
  (:import-from :ps-experiment/common-macros
                :setf-with))
(in-package :clw-sample-game-algorithm/sample/vehicle/test-state/hide)

(defun.ps+ make-hiding-vehicle (&key enemy-vehicle)
  (let* ((vehicle (make-test-vehicle :color #x00ffff))
         (steering (get-ecs-component 'steering vehicle))
         (vehicle-width #lx20)
         (min-search-dist #lx30)
         (max-search-dist #lx50))
    (setf-with (get-ecs-component 'vehicle-component vehicle)
      max-speed #lx2
      max-force #lx0.08)
    (set-avoid-obstacle steering
                        :vehicle-width vehicle-width
                        :min-search-dist min-search-dist
                        :max-search-dist max-search-dist)
    (set-hide steering :enemy-vehicle enemy-vehicle)
    vehicle))

(def-test-state hide-state ()
  :start-process
  (state-lambda ()
    (init-obstacles :num 7
                    :min-dist #lx60
                    :margin #lx80)
    (let* ((enemy (make-wander-avoiding-vehicle))
           (hider (make-hiding-vehicle :enemy-vehicle enemy)))
      (add-ecs-entity enemy)
      (add-ecs-entity hider)))

  :register-name-initializer-pairs
  ((:hide (make-hide-state))))
