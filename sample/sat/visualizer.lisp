(defpackage clw-sample-game-algorithm/sample/SAT/visualizer
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :init-visualize-system
           :make-visualizer)
  (:import-from :clw-sample-game-algorithm/sample/SAT/axis
                :init-axis-sat
                :axis-sat-base-point
                :axis-sat-unit-vector
                :projection-sat
                :projection-sat-axis
                :projection-sat-min
                :projection-sat-max
                :project-polygon-to-axis)
  (:import-from :clw-sample-game-algorithm/sample/SAT/sat-system
                :sat-component
                :sat-component-point-list))
(in-package :clw-sample-game-algorithm/sample/SAT/visualizer)

(defstruct.ps+ (visualizer (:include ecs-component))
  fn-get-projection)

(defstruct.ps+
    (visualize-system
     (:include ecs-system
               (target-component-types '(visualizer point-2d params))
               (process #'update-visualizer))))

(defun.ps+ update-visualizer (entity)
  (with-ecs-components (visualizer point-2d)
      entity
    (let ((proj (funcall (visualizer-fn-get-projection visualizer)))
          (old-proj-model (get-entity-param entity :proj-line-model)))
      (when old-proj-model
        (delete-ecs-component old-proj-model entity))
      (let ((new-proj-model (make-projection-line-model proj)))
        (add-ecs-component new-proj-model entity)
        (set-entity-param entity :proj-line-model new-proj-model))
      (copy-vector-2d-to
       point-2d
       (axis-sat-base-point (projection-sat-axis proj))))))

(defun.ps+ init-visualize-system ()
  (register-ecs-system "visualizer" (make-visualize-system))
  (add-sample-visualizer))

;; DEBUG
(defun.ps+ add-sample-visualizer ()
  (let ((vis (make-ecs-entity)))
    (add-ecs-component-list
     vis
     (make-point-2d)
     (make-visualizer
      :fn-get-projection (lambda ()
                           (let* ((entity (find-a-entity-by-tag :sat-object))
                                  (global-point (calc-global-point entity))
                                  (global-point-list (list)))
                             (dolist (point (extract-point-list entity))
                               (push (transformf-point (clone-point-2d point) global-point)
                                     global-point-list))
                             (project-polygon-to-axis
                              global-point-list
                              (init-axis-sat (make-point-2d :x 50 :y 50)
                                             (make-point-2d :x 1 :y 0))))))
     (init-entity-params))
    (add-ecs-entity vis)))

;; DEBUG: copied from sat-system.lisp
(defun.ps+ extract-point-list (entity)
  (sat-component-point-list (get-ecs-component 'sat-component entity)))

(defun.ps+ make-projection-line-model (proj)
  (check-type proj projection-sat)
  (with-slots (axis min max) proj
    (flet ((make-a-point (len)
             (with-slots (base-point unit-vector) axis
               (list (+ (vector-2d-x base-point) (* (vector-2d-x unit-vector) len))
                     (+ (vector-2d-y base-point) (* (vector-2d-y unit-vector) len))))))
      (make-model-2d 
       :depth 100
       :model (make-line :pos-a (make-a-point min)
                         :pos-b (make-a-point max)
                         :color #xff0000)))))
