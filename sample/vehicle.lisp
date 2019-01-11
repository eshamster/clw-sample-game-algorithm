(defpackage :clw-sample-game-algorithm/sample/vehicle
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game))
(in-package :clw-sample-game-algorithm/sample/vehicle)

(clw-sample-game-algorithm/utils:use-this-package-as-sample)

(defun.ps+ init-func (scene)
  (init-default-systems :scene scene)
  (init-input)
  )

(defun.ps+ update-func ()
  )
