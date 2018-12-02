(defpackage clw-sample-game-algorithm/sample/SAT/sat-system
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :make-sat-component
           :sat-component
           :sat-component-point-list
           :init-sat-system)
  (:import-from :clw-sample-game-algorithm/sample/SAT/collision
                :init-sat-object
                :collide-sat-object-p)
  (:import-from :ps-experiment/common-macros 
                :with-slots-pair))
(in-package :clw-sample-game-algorithm/sample/SAT/sat-system)

(defstruct.ps+ (sat-component (:include ecs-component))
    point-list)

(defstruct.ps+
    (sat-system
     (:include ecs-system
               (target-component-types '(point-2d sat-component))
               (process-all #'process-sat-system))))

(defun.ps+ init-sat-system ()
  (register-ecs-system "sat-collision" (make-sat-system)))

(defun.ps+ process-sat-system (system)
  (with-slots ((entities target-entities)) system
    (let ((len (length entities)))
      (loop :for outer-idx :from 0 :below len :do
         (loop :for inner-idx :from (1+ outer-idx) :below len :do
            (add-to-monitoring-log "do sat")
            (process-entity-collision
             (nth outer-idx entities)
             (nth inner-idx entities)))))))

(defun.ps+ process-entity-collision (entity1 entity2)
  (let ((sat1 (extract-sat-object entity1))
        (sat2 (extract-sat-object entity2)))
    (when (collide-sat-object-p sat1 sat2)
      (add-to-monitoring-log "Collide!!"))))

(defun.ps+ extract-sat-object (entity)
  (let ((global-point (calc-global-point entity))
        (global-point-list (list)))
    (dolist (point (extract-point-list entity))
      (push (transformf-point (clone-point-2d point) global-point)
            global-point-list))
    (init-sat-object global-point-list)))

(defun.ps+ extract-point-list (entity)
  (sat-component-point-list (get-ecs-component 'sat-component entity)))
