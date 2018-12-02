(defpackage :clw-sample-game-algorithm/sample/SAT
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:import-from :clw-sample-game-algorithm/sample/SAT/sat-system
                :init-sat-system)
  (:import-from :clw-sample-game-algorithm/sample/SAT/sat-tester
                :init-sat-tester))
(in-package :clw-sample-game-algorithm/sample/SAT)

(clw-sample-game-algorithm/utils:use-this-package-as-sample)

(defun.ps+ init-func (scene)
  (init-default-systems :scene scene)
  (init-input)
  (init-sat-system)
  (init-sat-tester))

(defun.ps+ update-func ()
  )
