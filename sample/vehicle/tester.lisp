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
                :set-arrive-point
                :set-wander-behavior)
  (:import-from :ps-experiment/common-macros
                :setf-with))
(in-package :clw-sample-game-algorithm/sample/vehicle/tester)

(defun.ps+ init-vehicle-tester ()
  (add-ecs-entity (make-state-manager-entity)))

(defun.ps+ make-state-manager-entity ()
  (let ((entity (make-ecs-entity))
        (manager (init-game-state-manager (make-wander-state))))
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

;; --- wander --- ;;

(defun.ps+ make-wander-vehicle ()
  (let ((vehicle (make-test-vehicle))
        (wander-radius #lx20)
        (wander-dist #lx40))
    (with-ecs-components (steering) vehicle
      (set-wander-behavior steering
                           :wander-radius wander-radius
                           :wander-dist wander-dist)
      (add-ecs-component-list
       vehicle
       (make-script-2d :func (lambda (entity)
                               (with-ecs-components (point-2d) entity
                                 (setf-with point-2d
                                   x (mod x #lx1000)
                                   y (mod y #ly1000)))))
       (make-model-2d :model (make-wired-circle :r wander-radius
                                                :color #x888888)
                      :offset (make-point-2d :x wander-dist)
                      :depth 100)))
    vehicle))

(defstruct.ps+
    (wander-state
     (:include vehicle-tester-state
               (start-process
                (state-lambda ()
                  (add-ecs-entity (make-wander-vehicle)))))))

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
