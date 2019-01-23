(defpackage :clw-sample-game-algorithm/sample/vehicle/steering
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :steering
           :init-steering
           :calc-steering
           :set-seek-point
           :set-flee-point
           :set-arrive-point
           :set-wander-behavior)
  (:import-from :clw-sample-game-algorithm/sample/vehicle/component
                :vehicle-component
                :vehicle-component-velocity
                :vehicle-component-max-force
                :vehicle-component-max-speed))
(in-package :clw-sample-game-algorithm/sample/vehicle/steering)

(defstruct.ps+ (steering (:include ecs-component))
    (force-calculators (make-hash-table)))

(defun.ps+ init-steering ()
  (make-steering))

(defun.ps+ calc-steering (vehicle)
  (with-ecs-components (point-2d vehicle-component steering) vehicle
    (with-slots (force-calculators)
        steering
      (let ((result-force (make-vector-2d)))
        (maphash (lambda (kind calculator)
                   (declare (ignore kind))
                   (incf-vector-2d result-force
                                   (funcall calculator vehicle-component point-2d)))
                 force-calculators)
        (truncate-vector-2d result-force
                            (vehicle-component-max-force vehicle-component))))))

;; --- setters for each behavior --- ;;

(defun.ps+ register-force-calculator (mode steering calculator)
  (setf (gethash mode (steering-force-calculators steering))
        calculator))

(defun.ps+ set-seek-point (steering target-point)
  (check-type target-point vector-2d)
  (register-force-calculator :seek steering
                             (lambda (vehicle-cmp vehicle-point)
                               (seek vehicle-cmp vehicle-point target-point))))

(defun.ps+ set-flee-point (steering target-point &key (panic-distance #lx200))
  (check-type target-point vector-2d)
  (register-force-calculator :flee steering
                             (lambda (vehicle-cmp vehicle-point)
                               (flee vehicle-cmp vehicle-point target-point
                                     :panic-distance panic-distance))))

(defun.ps+ set-arrive-point (steering target-point &key (diceleration 0.05))
  (check-type target-point vector-2d)
  (register-force-calculator :arrive steering
                             (lambda (vehicle-cmp vehicle-point)
                               (arrive vehicle-cmp vehicle-point target-point
                                       :diceleration diceleration))))

(defun.ps+ set-wander-behavior (steering &key
                                         (wander-radius #lx20)
                                         (wander-dist #lx30)
                                         (wander-jitter #lx3))
  (register-force-calculator :wander steering
                             (init-wander :wander-radius wander-radius
                                          :wander-dist wander-dist
                                          :wander-jitter wander-jitter)))

;; TODO: functions to disable each behavior

;; --- behavior --- ;;

(defun.ps+ seek (vehicle-cmp vehicle-point target-point)
  (let ((desired-velocity (decf-vector-2d (clone-point-2d target-point)
                                          vehicle-point)))
    (decf-vector-2d desired-velocity
                    (vehicle-component-velocity vehicle-cmp))))

(defun.ps+ flee (vehicle-cmp vehicle-point target-point &key panic-distance)
  (let ((force (*-vec-scalar (seek vehicle-cmp vehicle-point target-point)
                             -1)))
    (if (< (vector-2d-abs force) panic-distance)
        force
        (make-vector-2d))))

(defun.ps+ arrive (vehicle-cmp vehicle-point target-point &key diceleration)
  (let ((dist (calc-dist target-point vehicle-point)))
    (if (> dist 0)
        (let* ((speed (min (vehicle-component-max-speed vehicle-cmp)
                           (* dist diceleration)))
               (to-target (decf-vector-2d (clone-vector-2d target-point)
                                          vehicle-point))
               (desired-velocity (*-vec-scalar to-target (/ speed dist))))
          (decf-vector-2d desired-velocity
                          (vehicle-component-velocity vehicle-cmp)))
        (make-vector-2d))))

(defun.ps+ init-wander (&key wander-radius wander-dist wander-jitter)
  (let ((wander-target (make-vector-2d :x wander-radius :y 0)))
    (lambda (vehicle-cmp vehicle-point)
      (declare (ignore vehicle-cmp))
      (incf-vector-2d wander-target
                      (make-vector-2d :x (* (random-clamped) wander-jitter)
                                      :y (* (random-clamped) wander-jitter)))
      (setf-vector-2d-abs wander-target wander-radius)
      (let* ((target-local (incf-vector-2d (clone-vector-2d wander-target)
                                           (make-vector-2d :x wander-dist)))
             (target-world (transformf-point
                            (make-point-2d :x (vector-2d-x target-local)
                                           :y (vector-2d-y target-local))
                            vehicle-point)))
        (decf-vector-2d (clone-vector-2d target-world)
                        vehicle-point)))))

;; --- aux --- ;;

;; Note: (ps (random))     -> Math.random()
;;       (ps (random 1.0)) -> Math.floor(1.0 * Math.random());
(defun.ps random-clamped ()
  (1- (* 2 (random))))

(defun random-clamped ()
  (1- (random 2.0)))
