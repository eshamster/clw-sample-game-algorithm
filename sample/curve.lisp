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

;; --- curve info --- ;;

(defstruct.ps+ curve-info
    model (enable-p t))

(defun.ps+ get-curve-info (curve-entity name)
  (let* ((table (get-entity-param curve-entity :curve-info-table))
         (info (gethash name table)))
    (unless info
      (setf info (make-curve-info)
            (gethash name table) info))
    info))

(defun.ps+ get-prev-model (curve-entity name)
  (curve-info-model (get-curve-info curve-entity name)))

(defun.ps+ set-prev-model (curve-entity name model)
  (when model
    (check-type model model-2d))
  (setf (curve-info-model (get-curve-info curve-entity name))
        model))

(defun.ps+ get-curve-enable-p (curve-entity name)
  (curve-info-enable-p (get-curve-info curve-entity name)))

(defun.ps+ set-curve-enable-p (curve-entity name value)
  (setf (curve-info-enable-p (get-curve-info curve-entity name))
        value)
  (let ((model (get-prev-model curve-entity name)))
    (if value
        (enable-model-2d curve-entity :target-model-2d model)
        (disable-model-2d curve-entity :target-model-2d model))))

;; --- data --- ;;

(defvar.ps+ *control-points*
    (list (make-point-2d :x 100 :y 100)
          (make-point-2d :x 120 :y 400)
          (make-point-2d :x 400 :y 200)
          (make-point-2d :x 700 :y 300)))
(defvar.ps+ *dim-b-spline-uniform* 2)
(defvar.ps+ *dim-b-spline-open-uniform* 2)

;; --- --- ;;

(defun.ps+ add-or-replace-spline (curve-entity fn-calc-point &key color name)
  (check-entity-tags curve-entity :curve)
  (assert name)
  (let ((curve-points (list)) ; ((x1 y1) (x2 y2) ...)
        (num-curve-point 100)
        (control-points (get-entity-param curve-entity :control-points)))
    (let ((prev-model (get-prev-model curve-entity name)))
      (when prev-model
        (delete-ecs-component prev-model curve-entity)
        (set-prev-model curve-entity name nil)))
    (unless (> (length control-points) 1)
      (return-from add-or-replace-spline))
    (dotimes (i num-curve-point)
      (let ((point-on-curve (funcall fn-calc-point
                                     control-points
                                     (* 1.0 (/ i (1- num-curve-point))))))
        (push (list (point-2d-x point-on-curve)
                    (point-2d-y point-on-curve))
              curve-points)))
    (let ((spline-model (make-model-2d :model (make-lines :pnt-list curve-points :color color)
                                       :depth 100)))
      (add-ecs-component spline-model curve-entity)
      (set-prev-model curve-entity name spline-model)
      (unless (get-curve-enable-p curve-entity name)
        (disable-model-2d curve-entity :target-model-2d spline-model)))))

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

(defun.ps+ add-or-replace-all-splines (curve-entity control-points)
  (set-entity-param curve-entity :control-points control-points)
  (add-or-replace-spline curve-entity #'calc-lagrange-point
                         :color #xffff00 :name :lagrange)
  (add-or-replace-b-spline curve-entity (make-knots-bezier-generator)
                           :color #xff0000 :name :bezier)
  (add-or-replace-b-spline-uniform curve-entity)
  (add-or-replace-b-spline-open-uniform curve-entity))

(defun.ps+ add-or-replace-control-points (control-points)
  (register-next-frame-func
   (lambda ()
     (do-tagged-ecs-entities (point-entity :control-point)
       (delete-ecs-entity point-entity))
     (dolist (point control-points)
       (let ((point-entity (make-ecs-entity)))
         (add-entity-tag point-entity :control-point)
         (let ((r 3))
           (add-ecs-component-list
            point-entity
            (clone-point-2d point)
            (make-model-2d :model (make-solid-circle :r r :color #xffffff))))
         (add-ecs-entity point-entity))))))

(defun.ps+ init-curve-entity (control-points
                              &optional (curve-info-table (make-hash-table)))
  (let ((entity (make-ecs-entity)))
    (add-entity-tag entity :curve)
    (add-ecs-component-list
     entity
     (make-point-2d)
     (init-entity-params :curve-info-table curve-info-table))
    (add-or-replace-all-splines entity control-points)
    (add-ecs-entity entity))
  (add-or-replace-control-points control-points))

(defun.ps+ update-control-points (control-points)
  (add-or-replace-control-points control-points)
  (add-or-replace-all-splines (find-a-entity-by-tag :curve)
                              control-points))

;; --- GUI panel --- ;;

(defmacro.ps+ with-curve-folder ((name text) &body body)
  (let ((value (gensym "value")))
    `(with-gui-default-folder ((add-panel-folder ,text))
       (add-panel-bool
        "Enable" t
        :on-change (lambda (,value)
                     (set-curve-enable-p
                      (find-a-entity-by-tag :curve) ,name ,value)))
       ,@body)))

(defun.ps+ init-gui-panel ()
  (with-curve-folder (:lagrange "Lagrange interpolation (yellow)"))
  (with-curve-folder (:bezier "Bezier (red)"))
  (with-curve-folder
      (:b-spline-uniform "B-spline [uniform] (green)")
    (add-panel-number "dim" *dim-b-spline-uniform*
                      :min 1 :max 10 :step 1
                      :on-change (lambda (value)
                                   (setf *dim-b-spline-uniform* value)
                                   (add-or-replace-b-spline-uniform
                                    (find-a-entity-by-tag :curve)))))
  (with-curve-folder
      (:b-spline-open-uniform "B-spline [open uniform] (blue)")
    (add-panel-number "dim" *dim-b-spline-open-uniform*
                      :min 1 :max 10 :step 1
                      :on-change (lambda (value)
                                   (setf *dim-b-spline-open-uniform* value)
                                   (add-or-replace-b-spline-open-uniform
                                    (find-a-entity-by-tag :curve))))))

;; --- keyboard controls --- ;;

(defvar.ps+ *add-control-point-key* :a)
(defvar.ps+ *reset-key* :b)

(defun.ps+ process-keyboard ()
  (add-to-monitoring-log (get-left-mouse-state))
  (when (and (key-down-p *add-control-point-key*)
             (= (get-left-mouse-state) :down-now))
    (setf *control-points*
          (append *control-points*
                  (list (make-point-2d :x (get-mouse-x)
                                       :y (get-mouse-y)))))
    (update-control-points *control-points*))
  (when (key-down-now-p *reset-key*)
    (setf *control-points* (list))
    (update-control-points *control-points*)))

(defun.ps+ add-keyboard-help ()
  (add-to-event-log (+ "Reset control points by \""
                       (get-physical-key-name *reset-key*)
                       "\" key"))
  (add-to-event-log (+ "Add control point by click with \""
                       (get-physical-key-name *add-control-point-key*)
                       "\" key")))

;; --- basic init and update functions --- ;;

(defun.ps+ init-func (scene)
  (init-default-systems :scene scene)
  (init-input)
  (init-gui)
  (init-gui-panel)
  (init-curve-entity *control-points*)
  (add-keyboard-help))

(defun.ps+ update-func ()
  (process-keyboard))
