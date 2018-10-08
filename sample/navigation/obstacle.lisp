(defpackage clw-sample-game-algorithm/sample/navigation/obstacle
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :init-obstacle))
(in-package :clw-sample-game-algorithm/sample/navigation/obstacle)

(defun.ps+ delete-obstacle (entity)
  (when (and (> (get-mouse-down-count :left) 30)
             (collide-entities-p (find-a-entity-by-tag :mouse)
                                 entity))
    (register-next-frame-func
     (lambda ()
       (delete-ecs-entity entity)))))

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
                             ;; move
                             (with-ecs-components (point-2d) entity
                               (with-slots (x y) point-2d
                                 (setf x (get-mouse-x)
                                       y (get-mouse-y))))
                             ;; place
                             (when (eq (get-left-mouse-state) :down-now)
                               (register-next-frame-func
                                (lambda ()
                                  (delete-ecs-component (get-ecs-component 'script-2d entity)
                                                        entity)
                                  (add-ecs-component
                                   (make-script-2d :func #'delete-obstacle)
                                   entity)
                                  (init-obstacle)))))))
    (add-ecs-entity circle)))
