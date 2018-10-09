(defpackage clw-sample-game-algorithm/sample/navigation/a-star-tester
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:import-from :clw-sample-game-algorithm/sample/navigation/a-star
                :search-path)
  (:export :test-a-star))
(in-package :clw-sample-game-algorithm/sample/navigation/a-star-tester)

(defun.ps+ test-a-star (nav-mesh)
  (let ((path (search-path :nav-mesh nav-mesh
                            :start-x 1 :start-y 2
                            :goal-x 10 :goal-y 10
                            :enable-slant-p t))
        (result ""))
    (dolist (node path)
      (setf result (+ result "(" (car node) "," (cadr node) ") ")))
    (add-to-monitoring-log result)))
