(defpackage clw-sample-game-algorithm/sample/SAT/sat-tester
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :init-sat-tester)
  (:import-from :clw-sample-game-algorithm/sample/SAT/collision
                :init-sat-object
                :collide-sat-object-p)
  (:import-from :clw-sample-game-algorithm/sample/SAT/sat-system
                :make-sat-component)
  (:import-from :ps-experiment/common-macros
                :with-slots-pair))
(in-package :clw-sample-game-algorithm/sample/SAT/sat-tester)

(defun.ps+ init-sat-tester ()
  (add-ecs-entity (make-sat-entity
                   :point-list
                   (list (make-point-2d :x 0 :y 0)
                         (make-point-2d :x 200 :y 100)
                         (make-point-2d :x 50 :y 200))
                   :point (make-point-2d :x 100 :y 100)))
  (let ((entity (make-sat-entity
                 :point-list
                 (list (make-point-2d :x 0 :y 0)
                       (make-point-2d :x 200 :y 100)
                       (make-point-2d :x 50 :y 200))
                 :point (make-point-2d :angle (/ PI 4)))))
    (add-ecs-component-list
     entity
     (make-script-2d :func (lambda (target)
                             (with-ecs-components (point-2d) target
                               (with-slots (x y) point-2d
                                 (setf x (get-mouse-x)
                                       y (get-mouse-y)))))))
    (add-ecs-entity entity)))

(defun.ps+ make-sat-entity (&key point-list point)
  (check-type point point-2d)
  (let ((result (make-ecs-entity)))
    (add-ecs-component-list
     result
     point
     (make-model-2d :model (make-wired-polygon
                            :pnt-list (mapcar (lambda (point)
                                                (with-slots (x y) point
                                                  (list x y)))
                                              point-list)
                            :color #xff0000)
                    :depth 0)
     (make-sat-component :point-list point-list)
     (make-physic-polygon :pnt-list (mapcar #'clone-point-2d point-list)))
    result))
