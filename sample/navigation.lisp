(defpackage :clw-sample-game-algorithm/sample/navigation
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:import-from :clw-sample-game-algorithm/sample/navigation/nav-mesh
                :init-nav-mesh
                :setf-nav-mesh-display-p
                :update-nav-mesh)
  (:import-from :clw-sample-game-algorithm/sample/navigation/obstacle
                :init-obstacle))
(in-package :clw-sample-game-algorithm/sample/navigation)

(clw-sample-game-algorithm/utils:use-this-package-as-sample)

(defvar.ps+ *nav-mesh* nil)

(defun.ps+ init-my-gui ()
  (init-gui)
  (add-panel-bool "Dispaly Mesh" t
                  :on-change (lambda (value)
                               (setf-nav-mesh-display-p *nav-mesh* value))))

(defun.ps+ init-func (scene)
  (setf-collider-model-enable nil)
  (init-obstacle)
  (init-my-gui)
  (setf *nav-mesh*
        (init-nav-mesh :rect (make-rect-2d :x 0 :y 0
                                           :width 800 :height 600)
                       :num-x 20
                       :num-y 15))
  (init-default-systems :scene scene)
  (init-input))

(defun.ps+ update-func ()
  (update-nav-mesh *nav-mesh*))
