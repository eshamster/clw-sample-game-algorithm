(defpackage :clw-sample-game-algorithm/sample/vehicle/test-state/follow-path
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:import-from :clw-sample-game-algorithm/sample/vehicle/component
                :vehicle-component)
  (:import-from :clw-sample-game-algorithm/sample/vehicle/steering
                :steering
                :set-follow-path)
  (:import-from :clw-sample-game-algorithm/sample/vehicle/test-state/utils
                :def-test-state
                :make-test-vehicle
                :random1)
  (:import-from :clw-sample-game-algorithm/sample/vehicle/test-state/utils-panel
                :add-changing-max-speed-panel
                :add-changing-max-force-panel))
(in-package :clw-sample-game-algorithm/sample/vehicle/test-state/follow-path)

(defvar.ps+ *default-waypoint-seek-dist* #lx30)

(defun.ps+ add-changing-seek-dist-panel (vehicle)
  (add-panel-number
   "Seek Dist" *default-waypoint-seek-dist*
   :min #lx5 :max #lx90 :step #lx10
   :on-change (lambda (seek-dist)
                ;; TODO: Don't reset path
                (set-follow-path
                 (get-ecs-component 'steering vehicle)
                 :path (get-entity-param vehicle :path)
                 :loop-p (get-entity-param vehicle :loop-p)
                 :waypoint-seek-dist seek-dist)
                (update-seek-dist-circle vehicle seek-dist))))

(defun.ps+ init-control-panel (vehicle)
  (add-changing-max-speed-panel vehicle)
  (add-changing-max-force-panel vehicle)
  (add-changing-seek-dist-panel vehicle))

(defun.ps+ init-path (&key num-waypoint (loop-p t))
  (assert (and num-waypoint (> num-waypoint 0)))
  (let ((center-x #lx500)
        (center-y #ly500)
        (xr #lx300)
        (yr #ly300)
        (x-max-swing #lx100)
        (y-max-swing #ly100)
        (waypoints (list)))
    (flet ((calc-swing (max)
             (lerp-scalar (* -1 max) max (random1))))
      (dotimes (i num-waypoint)
        (let* ((theta (* 2 PI (/ i num-waypoint)))
               (base-x (+ center-x (* xr (cos theta))
                          (calc-swing x-max-swing)))
               (base-y (+ center-y (* yr (sin theta))
                          (calc-swing y-max-swing))))
          (push (list base-x base-y)
                waypoints))))
    (let* ((path-entity (make-ecs-entity))
           (color #xffffff)
           (model (if loop-p
                      (make-wired-polygon :pnt-list waypoints :color color)
                      (make-lines :pnt-list waypoints :color color))))
      (add-entity-tag path-entity :path)
      (add-ecs-component-list
       path-entity
       (make-point-2d)
       (make-model-2d :model model)
       (init-entity-params :path (mapcar (lambda (pair)
                                           (make-vector-2d :x (car pair)
                                                           :y (cadr pair)))
                                         waypoints)))
      (add-ecs-entity path-entity))))

(defun.ps+ update-seek-dist-circle (vehicle seek-dist &optional (init-p nil))
  (let ((old-model (get-entity-param vehicle :seek-dist-circle-model))
        (new-model (make-model-2d
                    :model (make-wired-circle :r seek-dist :color #x888888))))
    (set-entity-param vehicle :seek-dist-circle-model nil)
    (register-next-frame-func
     (lambda ()
       (when old-model
         (delete-ecs-component old-model vehicle))
       (when (or init-p old-model)
         (add-ecs-component new-model vehicle)
         (set-entity-param vehicle :seek-dist-circle-model new-model))))))

(defun.ps+ search-path ()
  (let ((entity (find-a-entity-by-tag :path)))
    (assert entity)
    (get-entity-param entity :path)))

(def-test-state follow-path-state ()
  :start-process
  (state-lambda ()
    (let ((loop-p t))
      (init-path :num-waypoint 5
                 :loop-p loop-p)
      (let ((vehicle (make-test-vehicle))
            (path (search-path)))
        (add-ecs-entity vehicle)
        (add-ecs-component-list
         vehicle
         (init-entity-params :path path
                             :loop-p loop-p))
        (set-follow-path (get-ecs-component 'steering vehicle)
                         :path path
                         :loop-p loop-p
                         :waypoint-seek-dist *default-waypoint-seek-dist*)
        (update-seek-dist-circle vehicle *default-waypoint-seek-dist* t)
        (init-control-panel vehicle))))

  :register-name-initializer-pairs
  ((:follow-path (make-follow-path-state))))
