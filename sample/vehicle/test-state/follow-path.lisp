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
                :random1))
(in-package :clw-sample-game-algorithm/sample/vehicle/test-state/follow-path)

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
      (let ((vehicle (make-test-vehicle)))
        (add-ecs-entity vehicle)
        (set-follow-path (get-ecs-component 'steering vehicle)
                         :path (search-path)
                         :loop-p loop-p))))

  :register-name-initializer-pairs
  ((:follow-path (make-follow-path-state))))
