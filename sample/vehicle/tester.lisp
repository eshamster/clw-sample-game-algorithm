(defpackage :clw-sample-game-algorithm/sample/vehicle/tester
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :init-vehicle-tester)
  (:import-from :clw-sample-game-algorithm/sample/vehicle/test-state/package
                :init-vehicle-test-state))
(in-package :clw-sample-game-algorithm/sample/vehicle/tester)

(defun.ps+ init-vehicle-tester ()
  (init-vehicle-test-state))
