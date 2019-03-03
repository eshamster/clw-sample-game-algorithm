(defpackage :clw-sample-game-algorithm/sample/vehicle/test-state/flocking
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:import-from :clw-sample-game-algorithm/sample/vehicle/group-behavior
                :set-group-alignment)
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

(defvar.ps+ *num-vehicle* 40)
(defvar.ps+ *vehicle-scale* 0.6)
(defvar.ps+ *vehicle-search-dist* #lx50)

(defvar.ps+ *default-alignment-weight* 75)

(defvar.ps+ *wander-radius* #lx15)
(defvar.ps+ *wander-dist* #lx45)

(defstruct.ps+ vehicle-point-pair vehicle point)

(defun.ps+ find-neighbors (target-vehicle-point-pair
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
            (push other-vehicle result))))
      result)))

(defun.ps+ make-group-behavior-updater (vehicles)
  (let ((updater (make-ecs-entity)))
    (add-ecs-component-list
     updater
     (make-script-2d
      :func (lambda (entity)
              (declare (ignore entity))
              (let ((pairs (mapcar
                            (lambda (vehicle)
                              (make-vehicle-point-pair
                               :vehicle vehicle
                               :point (calc-global-point vehicle)))
                            vehicles)))
                (dolist (pair pairs)
                  (with-slots (vehicle) pair
                    (set-group-alignment
                     (get-ecs-component 'steering vehicle)
                     :neighbors
                     (find-neighbors pair pairs
                                     *vehicle-search-dist*)
                     :weight *default-alignment-weight*)))))))
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
      (add-ecs-entity
       (make-group-behavior-updater vehicles))))

  :register-name-initializer-pairs
  ((:flocking (make-flocking-state))))
