(defpackage :clw-sample-game-algorithm/sample/vehicle/tester
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :init-vehicle-tester)
  (:import-from :clw-sample-game-algorithm/sample/vehicle/component
                :vehicle-component
                :init-vehicle-component)
  (:import-from :clw-sample-game-algorithm/sample/vehicle/steering
                :steering
                :init-steering
                :set-seek-point)
  (:import-from :ps-experiment/common-macros
                :setf-with))
(in-package :clw-sample-game-algorithm/sample/vehicle/tester)

(defun.ps+ init-vehicle-tester ()
  (add-ecs-entity (make-state-manager-entity)))

(defun.ps+ make-state-manager-entity ()
  (let ((entity (make-ecs-entity))
        (manager (init-game-state-manager (make-seek-state))))
    (add-ecs-component-list
     entity
     (make-script-2d :func (lambda (entity)
                             (declare (ignore entity))
                             (process-game-state manager))))
    entity))

(defun.ps+ make-test-vehicle ()
  (let* ((vehicle (make-ecs-entity))
         (width #lx30)
         (height (/ width 2)))
    (add-entity-tag vehicle :vehicle)
    (add-ecs-component-list
     vehicle
     (make-point-2d :x #lx500 :y #ly500)
     (init-vehicle-component)
     (init-steering)
     (make-model-2d :model (make-solid-polygon
                            :pnt-list (list (list (* -1/2 width) (*  1/2 height))
                                            (list (* -1/2 width) (* -1/2 height))
                                            (list (* 1/2 width) 0))
                            :color #xffffff)))
    vehicle))

(defstruct.ps+
    (vehicle-tester-state
     (:include game-state)))

;; --- seek --- ;;

(defstruct.ps+
    (seek-state
     (:include vehicle-tester-state
               (start-process
                (state-lambda ()
                  (let ((vehicle (make-test-vehicle))
                        (target (make-target-entity)))
                    (add-ecs-entity target)
                    (add-ecs-component-list
                     vehicle
                     (make-script-2d
                      :func (lambda (entity)
                              (declare (ignore entity))
                              (set-seek-point
                               (get-ecs-component 'steering vehicle)
                               (get-ecs-component 'point-2d target)))))
                    (add-ecs-entity vehicle)))))))

(defun.ps+ make-target-entity ()
  (let ((target (make-ecs-entity))
        (r #lx8)
        (color #xffffff))
    (add-entity-tag target :target)
    (add-ecs-component-list
     target
     (make-point-2d :x #lx800 :y #ly800)
     (make-model-2d :model (make-wired-circle :r r :color color))
     (make-script-2d :func (lambda (entity)
                             (when (> (get-mouse-down-count :left) 0)
                               (with-ecs-components (point-2d) entity
                                 (setf-with point-2d
                                   x (get-mouse-x)
                                   y (get-mouse-y)))))))
    target))
