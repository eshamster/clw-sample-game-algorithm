(defpackage :clw-sample-game-algorithm/sample/vehicle/test-state/offset-pursuit
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:import-from :clw-sample-game-algorithm/sample/vehicle/component
                :vehicle-component)
  (:import-from :clw-sample-game-algorithm/sample/vehicle/steering
                :steering
                :set-arrive-point
                :set-offset-pursuit)
  (:import-from :clw-sample-game-algorithm/sample/vehicle/test-state/utils
                :def-test-state
                :make-test-vehicle
                :make-target-entity
                :get-steering))
(in-package :clw-sample-game-algorithm/sample/vehicle/test-state/offset-pursuit)

(def-test-state offset-pursuit-state ()
  :start-process
  (state-lambda (parent)
    (let ((target (make-target-entity))
          (leader (make-test-vehicle :scale 1.2)))
      (add-ecs-entity target)
      ;; leader
      (add-ecs-component-list
       leader
       (make-script-2d :func (lambda (entity)
                               (set-arrive-point
                                (get-steering entity)
                                (calc-global-point target)))))
      (add-ecs-entity leader)
      ;; formation
      (dolist (offset (list (make-vector-2d :x #lx-40 :y #lx50)
                            (make-vector-2d :x #lx-40 :y #lx-50)
                            (make-vector-2d :x #lx-70 :y 0)))
        (let ((member (make-test-vehicle :scale 0.8)))
          (set-offset-pursuit (get-steering member)
                              :leader leader :offset offset)
          (add-ecs-entity member)))))

  :register-name-initializer-pairs
  ((:offset-pursuit (make-offset-pursuit-state))))
