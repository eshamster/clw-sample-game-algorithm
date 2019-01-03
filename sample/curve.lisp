(defpackage :clw-sample-game-algorithm/sample/curve
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:import-from :clw-sample-game-algorithm/sample/curve/b-spline
                :calc-b-spline-point
                :make-knots-uniform-generator
                :make-knots-open-uniform-generator
                :make-knots-bezier-generator)
  (:import-from :clw-sample-game-algorithm/sample/curve/lagrange
                :calc-lagrange-point))
(in-package :clw-sample-game-algorithm/sample/curve)

(clw-sample-game-algorithm/utils:use-this-package-as-sample)

(defvar.ps+ *control-points*
    (list (make-point-2d :x 100 :y 100)
          (make-point-2d :x 120 :y 400)
          (make-point-2d :x 400 :y 200)
          (make-point-2d :x 700 :y 300)))
(defvar.ps+ *dim-b-spline-uniform* 2)
(defvar.ps+ *dim-b-spline-open-uniform* 2)

(defun.ps+ add-or-replace-spline (curve-entity fn-calc-point &key color name)
  (check-entity-tags curve-entity :curve)
  (let ((curve-points (list)) ; ((x1 y1) (x2 y2) ...)
        (num-curve-point 100))
    (dotimes (i num-curve-point)
      (let ((point-on-curve (funcall fn-calc-point
                                     (get-entity-param curve-entity :control-points)
                                     (* 1.0 (/ i (1- num-curve-point))))))
        (push (list (point-2d-x point-on-curve)
                    (point-2d-y point-on-curve))
              curve-points)))
    (let ((spline-model (make-model-2d :model (make-lines :pnt-list curve-points :color color)
                                       :depth 100))
          (prev-model-table (get-entity-param curve-entity :spline-models)))
      (when name
        (let ((prev-model (gethash name prev-model-table)))
          (when prev-model
            (delete-ecs-component prev-model curve-entity))))
      (add-ecs-component spline-model curve-entity)
      (when name
        (setf (gethash name prev-model-table) spline-model)))))

(defun.ps+ add-or-replace-b-spline (curve-entity knots-generator &key color name)
  (add-or-replace-spline curve-entity
                         (lambda (control-points alpha)
                           (calc-b-spline-point control-points
                                                knots-generator
                                                (* 0.9999 alpha)))
                         :color color
                         :name name))

(defun.ps+ add-or-replace-b-spline-uniform (curve-entity
                                            &optional (dim *dim-b-spline-uniform*))
  (add-or-replace-b-spline curve-entity
                           (make-knots-uniform-generator dim)
                           :color #x00ff00
                           :name :b-spline-uniform))

(defun.ps+ add-or-replace-b-spline-open-uniform (curve-entity
                                                 &optional (dim *dim-b-spline-open-uniform*))
  (add-or-replace-b-spline curve-entity
                           (make-knots-open-uniform-generator dim)
                           :color #x6666ff
                           :name :b-spline-open-uniform))

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
         (init-entity-params :control-points control-points
                             :spline-models (make-hash-table)))))
    (add-or-replace-spline entity #'calc-lagrange-point :color #xffff00)
    (add-or-replace-b-spline entity (make-knots-bezier-generator)
                             :color #xff0000)
    (add-or-replace-b-spline-uniform entity *dim-b-spline-uniform*)
    (add-or-replace-b-spline-open-uniform entity *dim-b-spline-open-uniform*)
    (add-ecs-entity entity)))

(defun.ps+ init-gui-panel ()
  (add-panel-folder "Lagrange interpolation (yellow)")
  (add-panel-folder "Bezier (red)")
  (with-gui-default-folder
      ((add-panel-folder "B-spline [uniform] (green)"))
    (add-panel-number "dim" *dim-b-spline-uniform*
                      :min 1 :max 10 :step 1
                      :on-change (lambda (value)
                                   (setf *dim-b-spline-uniform* value)
                                   (add-or-replace-b-spline-uniform
                                    (find-a-entity-by-tag :curve)))))
  (with-gui-default-folder
      ((add-panel-folder "B-spline [open uniform] (blue)"))
    (add-panel-number "dim" *dim-b-spline-open-uniform*
                      :min 1 :max 10 :step 1
                      :on-change (lambda (value)
                                   (setf *dim-b-spline-open-uniform* value)
                                   (add-or-replace-b-spline-open-uniform
                                    (find-a-entity-by-tag :curve))))))

(defun.ps+ init-func (scene)
  (init-default-systems :scene scene)
  (init-input)
  (init-gui)
  (init-gui-panel)
  (init-curve-entity *control-points*))

(defun.ps+ update-func ()
  )
