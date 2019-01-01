(defpackage :clw-sample-game-algorithm/sample/curve
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:import-from :clw-sample-game-algorithm/sample/curve/lagrange
                :calc-lagrange-point))
(in-package :clw-sample-game-algorithm/sample/curve)

(clw-sample-game-algorithm/utils:use-this-package-as-sample)

(defun.ps+ add-spline (curve-entity fn-calc-point &key color)
  (check-entity-tags curve-entity :curve)
  (let ((curve-points (list)) ; ((x1 y1) (x2 y2) ...)
        (num-curve-point 30))
    (dotimes (i num-curve-point)
      (let ((point-on-curve (funcall fn-calc-point
                                     (get-entity-param curve-entity :control-points)
                                     (* 1.0 (/ i (1- num-curve-point))))))
        (push (list (point-2d-x point-on-curve)
                    (point-2d-y point-on-curve))
              curve-points)))
    (add-ecs-component
     (make-model-2d :model (make-lines :pnt-list curve-points :color color)
                    :depth 100)
     curve-entity)))

(defun.ps+ init-curve-entity (control-points)
  (let ((entity (make-ecs-entity)))
    (add-entity-tag entity :curve)
    (add-ecs-component-list
     entity
     (make-point-2d))
    (dolist (point control-points)
      (let ((r 3))
        (add-ecs-component-list
         entity
         (make-model-2d :offset (clone-point-2d point)
                        :model (make-solid-circle :r r :color #xffffff))
         (init-entity-params :control-points control-points))))
    (add-spline entity #'calc-lagrange-point :color #xffff00)
    (add-ecs-entity entity)))

(defun.ps+ init-func (scene)
  (init-default-systems :scene scene)
  (init-input)
  (init-curve-entity (list (make-point-2d :x 100 :y 100)
                           (make-point-2d :x 120 :y 400)
                           (make-point-2d :x 400 :y 500)
                           (make-point-2d :x 700 :y 300))))

(defun.ps+ update-func ()
  )
