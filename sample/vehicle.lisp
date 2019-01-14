(defpackage :clw-sample-game-algorithm/sample/vehicle
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:import-from :clw-sample-game-algorithm/sample/vehicle/system
                :init-vehicle-system)
  (:import-from :clw-sample-game-algorithm/sample/vehicle/tester
                :init-vehicle-tester))
(in-package :clw-sample-game-algorithm/sample/vehicle)

(clw-sample-game-algorithm/utils:use-this-package-as-sample)

(defun.ps+ init-func (scene)
  (init-default-systems :scene scene)
  (init-vehicle-system)
  (init-input)
  (init-vehicle-tester))

(defun.ps+ update-func ()
  )
