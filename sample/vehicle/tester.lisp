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
                :set-seek-point
                :set-flee-point
                :set-arrive-point)
  (:import-from :ps-experiment/common-macros
                :setf-with))
(in-package :clw-sample-game-algorithm/sample/vehicle/tester)

(defun.ps+ init-vehicle-tester ()
  (add-ecs-entity (make-state-manager-entity)))

(defun.ps+ make-state-manager-entity ()
  (let ((entity (make-ecs-entity))
        (manager (init-game-state-manager (make-seek-or-flee-state :mode :arrive))))
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

;; --- seek, flee or arrive --- ;;

(defstruct.ps+
    (seek-or-flee-state
     (:include vehicle-tester-state
               (start-process
                (state-lambda (mode)
                  (let ((vehicle (make-test-vehicle))
                        (target (make-target-entity)))
                    (add-ecs-entity target)
                    (add-ecs-component-list
                     vehicle
                     (make-script-2d
                      :func (lambda (entity)
                              (declare (ignore entity))
                              (let ((steering (get-ecs-component 'steering vehicle))
                                    (target-point (get-ecs-component 'point-2d target)))
                                (ecase mode
                                  (:seek (set-seek-point steering target-point))
                                  (:flee (set-flee-point steering target-point))
                                  (:arrive (set-arrive-point steering target-point)))))))
                    (add-ecs-entity vehicle))))))
    mode ; :seek, :flee or :arrive
  )

;; --- utils --- ;;

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
