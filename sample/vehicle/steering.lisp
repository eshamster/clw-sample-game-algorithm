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
           :set-wander-behavior
           :set-pursuit-target)
  (:import-from :clw-sample-game-algorithm/sample/vehicle/component
                :vehicle-component
                :vehicle-component-heading
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

(defun.ps+ set-pursuit-target (steering evader)
  (check-type evader ecs-entity)
  (register-force-calculator :pursuit steering
                             (lambda (vehicle-cmp vehicle-point)
                               (with-ecs-components (vehicle-component) evader
                                 (pursuit vehicle-cmp vehicle-point
                                          vehicle-component
                                          (calc-global-point evader))))))

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
               (to-target (sub-vector-2d target-point vehicle-point))
               (desired-velocity (*-vec-scalar to-target (/ speed dist))))
          (decf-vector-2d desired-velocity
                          (vehicle-component-velocity vehicle-cmp)))
        (make-vector-2d))))

(defun.ps+ calc-pursuit-point (vehicle-cmp vehicle-point evader-vehicle-cmp evader-point)
  (let* ((to-evader (sub-vector-2d evader-point vehicle-point))
         (vehicle-heading (vehicle-component-heading vehicle-cmp))
         (relative-heading (calc-inner-product
                            vehicle-heading
                            (vehicle-component-heading evader-vehicle-cmp))))
    ;; If face each other, only do seek
    (when (and (> (calc-inner-product to-evader vehicle-heading))
               ;; acos(0.95) = 18 degrees
               (< relative-heading -0.95))
      (return-from calc-pursuit-point evader-point))
    (let* ((evader-velocity (vehicle-component-velocity evader-vehicle-cmp))
           (look-ahead-time
            (/ (vector-2d-abs to-evader)
               (+ (vehicle-component-max-speed vehicle-cmp)
                  (vector-2d-abs evader-velocity)))))
      (add-vector-2d evader-point
                     (*-vec-scalar evader-velocity look-ahead-time)))))

(defun.ps+ pursuit (vehicle-cmp vehicle-point evader-vehicle-cmp evader-point)
  (seek vehicle-cmp vehicle-point
        (calc-pursuit-point vehicle-cmp vehicle-point evader-vehicle-cmp evader-point)))

(defun.ps+ init-wander (&key wander-radius wander-dist wander-jitter)
  (let ((wander-target (make-vector-2d :x wander-radius :y 0)))
    (lambda (vehicle-cmp vehicle-point)
      (declare (ignore vehicle-cmp))
      (incf-vector-2d wander-target
                      (make-vector-2d :x (* (random-clamped) wander-jitter)
                                      :y (* (random-clamped) wander-jitter)))
      (setf-vector-2d-abs wander-target wander-radius)
      (let* ((target-local (add-vector-2d wander-target
                                          (make-vector-2d :x wander-dist)))
             (target-world (transformf-point
                            (make-point-2d :x (vector-2d-x target-local)
                                           :y (vector-2d-y target-local))
                            vehicle-point)))
        (sub-vector-2d target-world vehicle-point)))))

;; --- aux --- ;;

;; Note: (ps (random))     -> Math.random()
;;       (ps (random 1.0)) -> Math.floor(1.0 * Math.random());
(defun.ps random-clamped ()
  (1- (* 2 (random))))

(defun random-clamped ()
  (1- (random 2.0)))
