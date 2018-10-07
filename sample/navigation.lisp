(defpackage :clw-sample-game-algorithm/sample/navigation
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:import-from :clw-sample-game-algorithm/sample/navigation/nav-mesh
                :init-nav-mesh
                :update-nav-mesh))
(in-package :clw-sample-game-algorithm/sample/navigation)

(clw-sample-game-algorithm/utils:use-this-package-as-sample)

(defvar.ps+ *nav-mesh* nil)

(defun.ps+ init-func (scene)
  (setf-collider-model-enable nil)
  (let* ((circle (make-ecs-entity))
         (r 40))
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
                                       y (get-mouse-y)))))))
    (add-ecs-entity circle))
  (setf *nav-mesh*
        (init-nav-mesh :rect (make-rect-2d :x 0 :y 0
                                           :width 800 :height 600)
                       :num-x 20
                       :num-y 15))
  (init-default-systems :scene scene)
  (init-input))

(defun.ps+ update-func ()
  (update-nav-mesh *nav-mesh*))
