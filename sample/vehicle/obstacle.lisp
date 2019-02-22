(defpackage :clw-sample-game-algorithm/sample/vehicle/obstacle
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :init-vehicle-obstacle
           :do-vehicle-obstacle
           :calc-intersect-distance
           :get-obstacle-r))
(in-package :clw-sample-game-algorithm/sample/vehicle/obstacle)

(defun.ps+ init-vehicle-obstacle (&key point r)
  (let ((obstacle (make-ecs-entity)))
    (add-entity-tag obstacle :vehicle-obstacle)
    (add-ecs-component-list
     obstacle
     (clone-point-2d point)
     (make-model-2d :model (make-wired-circle :r r :color #x888888)
                    :depth -100)
     (init-entity-params :r r))
    obstacle))

(defun.ps+ get-obstacle-r (obstacle)
  (get-entity-param obstacle :r))

(defmacro.ps+ do-vehicle-obstacle ((var) &body body)
  `(do-tagged-ecs-entities (,var :vehicle-obstacle)
     ,@body))

(defun.ps+ calc-intersect-distance (obstacle vehicle-point vehicle-width search-dist)
  "Calculate intersected point as local x value in vehicle coordinate.
If it doesn't intersect, returns nil."
  (check-entity-tags obstacle :vehicle-obstacle)
  (let ((local-obstacle-point (transformf-point-inverse
                               (calc-global-point obstacle)
                               vehicle-point))
        (obstacle-r (+ (get-obstacle-r obstacle)
                       (/ vehicle-width 2))))
    (with-slots ((local-x x) (local-y y)) local-obstacle-point
      (when (and (>= local-x 0)
                 (<= (abs local-y) obstacle-r))
        (let* ((half-intersected-len (sqrt (- (expt obstacle-r 2)
                                              (expt local-y 2))))
               (candidate-dist (if (> (- local-x half-intersected-len) 0)
                                   (- local-x half-intersected-len)
                                   (+ local-x half-intersected-len))))
          (when (< candidate-dist search-dist)
            candidate-dist))))))
