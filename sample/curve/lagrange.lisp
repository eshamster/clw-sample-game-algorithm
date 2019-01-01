(defpackage :clw-sample-game-algorithm/sample/curve/lagrange
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :calc-lagrange-point))
(in-package :clw-sample-game-algorithm/sample/curve/lagrange)

;; Laglange interpolation

(defun.ps+ calc-lagrange-point (control-points alpha)
  "If alpha = 0, the result is (first control-points).
If alpha = 1, the result is (car (last control-points))"
  ;; TODO: Check length of control-points
  (let ((result-point (make-point-2d))
        (num-control-points (length control-points)))
    (dotimes (i num-control-points)
      (let ((ratio (calc-blending-ratio num-control-points i (* (1- num-control-points) alpha)))
            (target-point (nth i control-points)))
        (incf (point-2d-x result-point)
              (* (point-2d-x target-point) ratio))
        (incf (point-2d-y result-point)
              (* (point-2d-y target-point) ratio))))
    result-point))

(defun.ps+ calc-blending-ratio (num-control-points index alpha)
  "So-called blending function"
  (/ (reduce (lambda (a b) (* a b))
             (loop :for k :from 0 :below num-control-points
                :when (not (= k index))
                :collect (- alpha k)))
     (reduce (lambda (a b) (* a b))
             (loop :for k :from 0 :below num-control-points
                :for value = (- index k)
                :when (not (= value 0))
                :collect value))))
