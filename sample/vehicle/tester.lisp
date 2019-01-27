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
                :set-wander-behavior
                :set-pursuit-target)
  (:import-from :ps-experiment/common-macros
                :setf-with))
(in-package :clw-sample-game-algorithm/sample/vehicle/tester)

(defun.ps+ init-vehicle-tester ()
  (add-ecs-entity (make-state-manager-entity)))

(defun.ps+ make-state-manager-entity ()
  (let ((entity (make-ecs-entity))
        (manager (init-game-state-manager (make-tester-state :pursuit))))
    (add-ecs-component-list
     entity
     (make-script-2d :func (lambda (entity)
                             (declare (ignore entity))
                             (process-game-state manager))))
    entity))

(defun.ps+ make-tester-state (mode)
  (ecase mode
    ((:seek :flee :arrive) (make-seek-or-flee-state :mode mode))
    (:pursuit (make-pursuit-state))
    (:wander (make-wander-state))))

(defun.ps+ make-test-vehicle (&key (first-x #lx500)
                                   (first-y #ly500))
  (let* ((vehicle (make-ecs-entity))
         (width #lx30)
         (height (/ width 2)))
    (add-entity-tag vehicle :vehicle)
    (add-ecs-component-list
     vehicle
     (make-point-2d :x first-x :y first-y)
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

;; --- pursuit --- ;;

(defun.ps+ make-pursuit-vehicle (target)
  (let ((vehicle (make-test-vehicle :first-x #lx100
                                    :first-y #ly100)))
    (set-pursuit-target
     (get-ecs-component 'steering vehicle)
     target)
    vehicle))

(defun.ps+ make-pursuit-target-visualizer (vehicle evader)
  (let ((visualizer (make-ecs-entity)))
    (add-ecs-component-list
     visualizer
     (make-point-2d :x #lx-1000 :y #ly1000)
     (make-model-2d :model (make-wired-circle :r #lx5 :color #xff0000)
                    :depth 100)
     (make-script-2d
      :func (lambda (entity)
              (with-ecs-components (point-2d) entity
                (copy-vector-2d-to
                 point-2d
                 (clw-sample-game-algorithm/sample/vehicle/steering::calc-pursuit-point
                  (get-ecs-component 'vehicle-component vehicle)
                  (calc-global-point vehicle)
                  (get-ecs-component 'vehicle-component evader)
                  (calc-global-point evader)))))))
    visualizer))

(defstruct.ps+
    (pursuit-state
     (:include vehicle-tester-state
               (start-process
                (state-lambda ()
                  (let* ((evader (make-wander-vehicle))
                         (pursuit-vehicle (make-pursuit-vehicle evader)))
                    (add-ecs-entity evader)
                    (add-ecs-entity pursuit-vehicle)
                    (add-ecs-entity (make-pursuit-target-visualizer
                                     pursuit-vehicle evader))))))))

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
