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
    fn-get-axis-list
  fn-get-target-point-lists)

(defstruct.ps+
    (visualize-system
     (:include ecs-system
               (target-component-types '(visualizer point-2d))
               (process #'update-visualizer))))

(defvar.ps+ *color-list* (list #xff0000 #x00ffff))

(defun.ps+ update-visualizer (entity)
  (with-ecs-components (visualizer)
      entity
    (let ((axis-list (funcall (visualizer-fn-get-axis-list visualizer)))
          (target-point-lists
           (funcall (visualizer-fn-get-target-point-lists visualizer))))
      (delete-ecs-component-type 'model-2d entity)
      (dolist (axis axis-list)
        (let ((count 0))
          (dolist (point-list target-point-lists)
            (let* ((proj (project-polygon-to-axis point-list axis))
                   (new-proj-model (make-projection-line-model
                                    proj
                                    (nth (mod count (length *color-list*))
                                         *color-list*))))
              (add-ecs-component new-proj-model entity)
              (incf count))))))))

(defun.ps+ init-visualize-system ()
  (register-ecs-system "visualizer" (make-visualize-system))
  (add-sample-visualizer))

;; DEBUG
(defun.ps+ add-sample-visualizer ()
  (let ((vis (make-ecs-entity)))
    (add-ecs-component-list
     vis
     (make-point-2d :x 50 :y 50)
     (make-visualizer
      :fn-get-axis-list #'get-axis-list
      :fn-get-target-point-lists #'get-target-point-lists)
     (init-entity-params))
    (add-ecs-entity vis)))

;; DEBUG
(defun.ps+ get-axis-list ()
  (list
   (init-axis-sat (make-point-2d :x 50 :y 50)
                  (make-point-2d :x 1 :y 0))
   (init-axis-sat (make-point-2d :x 50 :y 50)
                  (make-point-2d :x 0 :y 1))))

(defun.ps+ get-target-point-lists ()
  (let ((result (list)))
    (do-tagged-ecs-entities (entity :sat-object)
      (push (calc-global-point-list entity) result))
    (nreverse result)))

(defun.ps+ calc-global-point-list (entity)
  (let* ((global-point (calc-global-point entity))
         (global-point-list (list)))
    (dolist (point (extract-point-list entity))
      (push (transformf-point (clone-point-2d point) global-point)
            global-point-list))
    global-point-list))

;; DEBUG: copied from sat-system.lisp
(defun.ps+ extract-point-list (entity)
  (sat-component-point-list (get-ecs-component 'sat-component entity)))

(defun.ps+ make-projection-line-model (proj color)
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
                         :color color)))))
