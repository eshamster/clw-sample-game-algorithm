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
           :set-pursuit-target
           :set-avoid-obstacle
           :set-interpose)
  (:import-from :clw-sample-game-algorithm/sample/vehicle/component
                :vehicle-component
                :vehicle-component-heading
                :vehicle-component-velocity
                :vehicle-component-max-force
                :vehicle-component-max-speed)
  (:import-from :clw-sample-game-algorithm/sample/vehicle/obstacle
                :do-vehicle-obstacle
                :calc-intersect-distance
                :get-obstacle-r))
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

(defun.ps+ set-avoid-obstacle (steering &key
                                        (vehicle-width #lx10)
                                        (min-search-dist #lx20)
                                        (max-search-dist #lx30))
  (register-force-calculator :avoid-obstacle steering
                             (init-avoid-obstacle
                              :vehicle-width vehicle-width
                              :min-search-dist min-search-dist
                              :max-search-dist max-search-dist)))

(defun.ps+ set-interpose (steering &key target-vehicle1 target-vehicle2)
  (register-force-calculator :interpose steering
                             (init-interpose :target1 target-vehicle1
                                             :target2 target-vehicle2)))

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
  (assert (and diceleration (> diceleration 0)))
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

;; avoid obstacle

(defun.ps+ init-avoid-obstacle (&key vehicle-width min-search-dist max-search-dist
                                     (breaking-weight 0.4))
  (assert (>= max-search-dist min-search-dist))
  (lambda (vehicle-cmp vehicle-point)
    (let ((search-dist (lerp-scalar min-search-dist
                                    max-search-dist
                                    (/ (vector-2d-abs (vehicle-component-velocity vehicle-cmp))
                                       (vehicle-component-max-speed vehicle-cmp))))
          (closest-obstacle nil)
          (closest-dist nil))
      (do-vehicle-obstacle (obstacle)
        (let ((dist (calc-intersect-distance
                     obstacle vehicle-point vehicle-width search-dist)))
          (when (and dist
                     (or (null closest-dist)
                         (< dist closest-dist)))
            (setf closest-obstacle obstacle
                  closest-dist dist))))
      (if closest-obstacle
          (let* ((obstacle-r (get-obstacle-r closest-obstacle))
                 (obstacle-local-y (vector-2d-y
                                    (transformf-point-inverse
                                     (calc-global-point closest-obstacle)
                                     vehicle-point)))
                 (multiplier (* 10 (+ 1.0 (/ (- search-dist closest-dist)
                                             search-dist))))
                 (local-force-y (* (- (+ obstacle-r vehicle-width)
                                      (abs obstacle-local-y))
                                   (if (> obstacle-local-y 0) -1 1)
                                   multiplier))
                 (local-force-x (* (- obstacle-r closest-dist) breaking-weight)))
            (transformf-point (make-point-2d :x local-force-x :y local-force-y)
                              (make-point-2d :angle (point-2d-angle vehicle-point))))
          (make-vector-2d)))))

;; interpose

(defun.ps+ init-interpose (&key target1 target2)
  (flet ((calc-mid-point (vec1 vec2)
           (/-vec-scalar (add-vector-2d vec1 vec2) 2.0)))
    (lambda (vehicle-cmp vehicle-point)
      (let* ((pnt1 (calc-global-point target1))
             (pnt2 (calc-global-point target2))
             (mid-pnt (calc-mid-point pnt1 pnt2))
             (time-to-reach-mid-point
              (/ (calc-dist vehicle-point mid-pnt)
                 (vehicle-component-max-speed vehicle-cmp))))
        (flet (;; Assume target will go to straight with same speed
               (estimate-future-point (cur-pnt cmp)
                 (add-vector-2d cur-pnt
                                (*-vec-scalar (vehicle-component-velocity cmp)
                                              time-to-reach-mid-point))))
          (with-ecs-components ((cmp1 vehicle-component)) target1
            (with-ecs-components ((cmp2 vehicle-component)) target2
              (arrive vehicle-cmp vehicle-point
                      (calc-mid-point (estimate-future-point pnt1 cmp1)
                                      (estimate-future-point pnt2 cmp2))
                      :diceleration 0.1))))))))

;; --- aux --- ;;

;; Note: (ps (random))     -> Math.random()
;;       (ps (random 1.0)) -> Math.floor(1.0 * Math.random());
(defun.ps random-clamped ()
  (1- (* 2 (random))))

(defun random-clamped ()
  (1- (random 2.0)))
