(defpackage :clw-sample-game-algorithm/sample/navigation
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:import-from :clw-sample-game-algorithm/sample/navigation/a-star-tester
                :init-test-a-star
                :test-a-star)
  (:import-from :clw-sample-game-algorithm/sample/navigation/nav-mesh
                :init-nav-mesh
                :destroy-nav-mesh
                :setf-nav-mesh-display-p
                :update-nav-mesh)
  (:import-from :clw-sample-game-algorithm/sample/navigation/obstacle
                :init-obstacle))
(in-package :clw-sample-game-algorithm/sample/navigation)

(clw-sample-game-algorithm/utils:use-this-package-as-sample)

(defvar.ps+ *nav-mesh* nil)
(defvar.ps+ *grid-num-x* 20)
(defvar.ps+ *grid-num-y* 15)

(defun.ps+ init-mouse-entity ()
  (let ((mouse (make-ecs-entity)))
    (add-entity-tag mouse :mouse)
    (add-ecs-component-list
     mouse
     (make-point-2d :x -1000 :y -1000)
     (make-physic-circle :r 0)
     (make-script-2d :func (lambda (entity)
                             (with-ecs-components (point-2d) entity
                               (with-slots (x y) point-2d
                                 (setf x (get-mouse-x)
                                       y (get-mouse-y)))))))
    (add-ecs-entity mouse))
  (init-test-a-star))

(defun.ps+ reset-nav-mesh ()
  (register-next-frame-func
   (lambda ()
     (when *nav-mesh*
       (destroy-nav-mesh *nav-mesh*))
     (setf *nav-mesh*
           (init-nav-mesh :rect (make-rect-2d :x 0 :y 0
                                              :width 800 :height 600)
                          :num-x *grid-num-x*
                          :num-y *grid-num-y*)))))

(defun.ps+ init-my-gui ()
  (init-gui)
  (add-panel-bool "Dispaly Mesh" t
                  :on-change (lambda (value)
                               (setf-nav-mesh-display-p *nav-mesh* value)))
  (let ((folder (add-panel-folder "Grid Density")))
    (add-panel-number "X" *grid-num-x*
                      :min 11 :max 30 :step 1
                      :on-change (lambda (value)
                                   (setf *grid-num-x* value)
                                   (reset-nav-mesh))
                      :folder folder)
    (add-panel-number "Y" *grid-num-y*
                      :min 11 :max 30 :step 1
                      :on-change (lambda (value)
                                   (setf *grid-num-y* value)
                                   (reset-nav-mesh))
                      :folder folder)))

(defun.ps+ init-func (scene)
  (setf-collider-model-enable nil)
  (init-obstacle)
  (init-my-gui)
  (reset-nav-mesh)
  (init-default-systems :scene scene)
  (init-input)
  (init-mouse-entity))

(defun.ps+ update-func ()
  (update-nav-mesh *nav-mesh*)
  (test-a-star *nav-mesh*))
