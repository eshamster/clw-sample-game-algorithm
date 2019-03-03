(defpackage :clw-sample-game-algorithm/sample/vehicle/test-state/flocking
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:import-from :clw-sample-game-algorithm/sample/vehicle/group-behavior
                :set-group-alignment
                :set-group-cohesion
                :set-group-separation)
  (:import-from :clw-sample-game-algorithm/sample/vehicle/steering
                :steering)
  (:import-from :clw-sample-game-algorithm/sample/vehicle/test-state/utils
                :def-test-state
                :make-test-vehicle
                :make-wander-vehicle
                :random1)
  (:import-from :ps-experiment/common-macros
                :setf-with))
(in-package :clw-sample-game-algorithm/sample/vehicle/test-state/flocking)

;; --- params --- ;;

(defvar.ps+ *num-vehicle* 40)
(defvar.ps+ *vehicle-scale* 0.6)
(defvar.ps+ *vehicle-search-dist* #lx50)

(defvar.ps+ *default-alignment-weight* 75)
(defvar.ps+ *default-cohesion-weight* 20)
(defvar.ps+ *default-separation-weight* 10)

(defvar.ps+ *wander-radius* #lx15)
(defvar.ps+ *wander-dist* #lx45)

;; --- controler --- ;;

(defun.ps+ add-changing-weight-panels (updater)
  (flet ((add-panel (title key)
           (add-panel-number
            title (get-entity-param updater key)
            :min 0 :max 100 :step 1
            :on-change (lambda (value)
                         (set-entity-param updater key value)))))
    (add-panel "Alignment" :alignment-weight)
    (add-panel "Cohesion" :cohesion-weight)
    (add-panel "Separation" :separation-weight)))

(defun.ps+ init-control-panel (updater)
  (check-entity-tags updater :group-behavior-updater)
  (add-changing-weight-panels updater))

;; --- state and utils --- ;;

(defstruct.ps+ vehicle-point-pair vehicle point)

(defun.ps+ find-neighbor-points (target-vehicle-point-pair
                                 vehicle-point-pairs
                                 vehicle-search-dist)
  (with-slots ((target-vehicle vehicle)w (target-point point))
      target-vehicle-point-pair
    (let ((result (list)))
      (dolist (other-pair vehicle-point-pairs)
        (with-slots ((other-vehicle vehicle) (other-point point))
            other-pair
          (when (and (not (eq other-vehicle target-vehicle))
                     (< (calc-dist-p2 target-point other-point)
                        (* vehicle-search-dist vehicle-search-dist)))
            (push other-point result))))
      result)))

(defun.ps+ make-group-behavior-updater (vehicles)
  (let ((updater (make-ecs-entity)))
    (add-entity-tag updater :group-behavior-updater)
    (add-ecs-component-list
     updater
     (init-entity-params
      :alignment-weight *default-alignment-weight*
      :cohesion-weight *default-cohesion-weight*
      :separation-weight *default-separation-weight*)
     (make-script-2d
      :func (lambda (entity)
              (let ((pairs (mapcar
                            (lambda (vehicle)
                              (make-vehicle-point-pair
                               :vehicle vehicle
                               :point (calc-global-point vehicle)))
                            vehicles)))
                (dolist (pair pairs)
                  (with-slots (vehicle) pair
                    (let ((steering (get-ecs-component 'steering vehicle))
                          (neighbor-points
                           (find-neighbor-points pair pairs
                                                 *vehicle-search-dist*)))
                      (set-group-alignment
                       steering
                       :neighbor-points neighbor-points
                       :weight (get-entity-param entity :alignment-weight))
                      (set-group-cohesion
                       steering
                       :neighbor-points neighbor-points
                       :weight (get-entity-param entity :cohesion-weight))
                      (set-group-separation
                       steering
                       :neighbor-points neighbor-points
                       :weight (get-entity-param entity :separation-weight)))))))))
    updater))

(def-test-state flocking-state ()
  :start-process
  (state-lambda ()
    (let ((vehicles
           (loop :for i :from 0 :below *num-vehicle*
              :collect (make-wander-vehicle
                        :display-wander-circle-p nil
                        :wander-radius *wander-radius*
                        :wander-dist *wander-dist*
                        :scale *vehicle-scale*))))
      (dolist (vehicle vehicles)
        (setf-with (get-ecs-component 'point-2d vehicle)
          x (lerp-scalar 0 #lx1000 (random1))
          y (lerp-scalar 0 #ly1000 (random1))
          angle (lerp-scalar (* -1 PI) PI (random1)))
        (add-ecs-entity vehicle))
      (let ((updater (make-group-behavior-updater vehicles)))
        (init-control-panel updater)
        (add-ecs-entity updater))))

  :register-name-initializer-pairs
  ((:flocking (make-flocking-state))))
