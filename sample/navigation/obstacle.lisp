(defpackage clw-sample-game-algorithm/sample/navigation/obstacle
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :init-obstacle))
(in-package :clw-sample-game-algorithm/sample/navigation/obstacle)

(defun.ps+ init-obstacle ()
  (let* ((circle (make-ecs-entity))
         (r 60))
    (add-entity-tag circle :obstacle)
    (add-ecs-component-list
     circle
     (make-point-2d :x -1000 :y -1000)
     (make-model-2d :model (make-wired-regular-polygon :n 60 :color #xff0000 :r r)
                    :depth 0)
     (make-physic-circle :r r)
     (make-script-2d :func (lambda (entity)
                             (with-ecs-components (point-2d) entity
                               (with-slots (x y) point-2d
                                 (setf x (get-mouse-x)
                                       y (get-mouse-y))))
                             (when (eq (get-left-mouse-state) :down-now)
                               (register-next-frame-func
                                (lambda ()
                                  (delete-ecs-component (get-ecs-component 'script-2d entity)
                                                        entity)
                                  (init-obstacle)))))))
    (add-ecs-entity circle)))
