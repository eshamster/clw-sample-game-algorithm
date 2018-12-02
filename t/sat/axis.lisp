(defpackage clw-sample-game-algorithm/t/sat/axis
  (:use :cl
        :rove
        :ps-experiment
        :ps-experiment/t/test-utils
        :cl-web-2d-game/core/basic-components
        :clw-sample-game-algorithm/sample/sat/axis))
(in-package :clw-sample-game-algorithm/t/sat/axis)

(defun.ps+ within (result expected &optional (tolerance 1e-4))
  (<= (- expected tolerance) result (+ expected tolerance)))

(defun.ps+ make-x-axis ()
  (init-axis-sat (make-point-2d)
                 (make-vector-2d :x 1 :y 0)))

(defun.ps+ make-y-axis ()
  (init-axis-sat (make-point-2d)
                 (make-vector-2d :x 0 :y 1)))

(deftest.ps+ test-project-polygon-to-axis
  (let ((point-list (list (make-point-2d :x 5 :y 0)
                          (make-point-2d :x 10 :y 8)
                          (make-point-2d :x -3 :y -5))))
    (testing "x-axis"
      (let* ((axis (make-x-axis))
             (project (project-polygon-to-axis point-list axis)))
        (ok (within (projection-sat-min project) -3))
        (ok (within (projection-sat-max project) 10))))
    (testing "y-axis"
      (let* ((axis (make-y-axis))
             (project (project-polygon-to-axis point-list axis)))
        (ok (within (projection-sat-min project) -5))
        (ok (within (projection-sat-max project) 8))))))

(deftest.ps+ test-get-intersected-projection-length
  (let ((x-axis (make-x-axis)))
    (labels ((make-test-projection (min max)
               (let ((result (project-polygon-to-axis
                              (list (make-point-2d :x min :y 0)
                                    (make-point-2d :x max :y 0))
                              x-axis)))
                 (testing "Prepare: Verify projection"
                   (ok (within (projection-sat-min result) min))
                   (ok (within (projection-sat-max result) max)))
                 result))
             (test-intersect (min1 max1 min2 max2 expected)
               (let ((result (get-intersected-projection-length
                              (make-test-projection min1 max1)
                              (make-test-projection min2 max2))))
                 (if expected
                     (ok (within result expected))
                     (ok (< result 0))))))
      (test-intersect 0     5
                        2 3
                      1)
      (test-intersect   2   5
                      0   3
                      1)
      (test-intersect 0 2
                          3 5
                      nil))))
