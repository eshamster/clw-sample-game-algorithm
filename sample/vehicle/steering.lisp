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
           :set-interpose
           :set-hide
           :set-follow-path
           :set-offset-pursuit
           ;; for group behavior
           :register-force-calculator
           :seek)
  (:import-from :clw-sample-game-algorithm/sample/vehicle/component
                :vehicle-component
                :vehicle-component-velocity
                :vehicle-component-max-force
                :vehicle-component-max-speed
                :vehicle-component-speed)
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

(defun.ps+ set-hide (steering &key enemy-vehicle)
  (register-force-calculator :hide steering
                             (init-hide enemy-vehicle)))

(defun.ps+ set-follow-path (steering &key
                                     path
                                     (loop-p t)
                                     (waypoint-seek-dist #lx30))
  (register-force-calculator :follow-path steering
                             (init-follow-path path
                                               :loop-p loop-p
                                               :waypoint-seek-dist waypoint-seek-dist)))

(defun.ps+ set-offset-pursuit (steering &key leader offset)
  (check-type leader ecs-entity)
  (check-type offset vector-2d)
  (register-force-calculator :offset-pursuit steering
                             (init-offset-pursuit leader offset)))

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

(defun.ps+ arrive (vehicle-cmp vehicle-point target-point &key (diceleration 0.1))
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

(defun.ps+ extract-heading-from-point-2d (point)
  (let ((angle (point-2d-angle point)))
    (make-vector-2d :x (cos angle)
                    :y (sin angle))))

(defun.ps+ calc-pursuit-point (vehicle-cmp vehicle-point evader-vehicle-cmp evader-point)
  (let* ((to-evader (sub-vector-2d evader-point vehicle-point))
         (vehicle-heading (extract-heading-from-point-2d vehicle-point))
         (relative-heading (calc-inner-product
                            vehicle-heading
                            (extract-heading-from-point-2d evader-point))))
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
                                    (/ (vehicle-component-speed vehicle-cmp)
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

;; hide

(defun.ps+ get-hiding-position (enemy-point obstacle-r obstacle-point
                                              &key (dist-from-obstacle #lx30))
  (let ((dist-away (+ obstacle-r dist-from-obstacle))
        (to-obstacle (sub-vector-2d obstacle-point enemy-point)))
    (setf-vector-2d-abs to-obstacle dist-away)
    (add-vector-2d obstacle-point to-obstacle)))

(defun.ps+ init-hide (enemy-vehicle)
  (lambda (vehicle-cmp vehicle-point)
    (let (dist-to-closest
          best-hiding-spot
          (enemy-point (calc-global-point enemy-vehicle)))
      (do-vehicle-obstacle (ob)
        (let* ((hiding-spot (get-hiding-position
                             enemy-point
                             (get-obstacle-r ob)
                             (calc-global-point ob)))
               (dist (calc-dist-p2 hiding-spot vehicle-point)))
          (when (or (not dist-to-closest)
                    (< dist dist-to-closest))
            (setf dist-to-closest dist
                  best-hiding-spot hiding-spot))))
      (if dist-to-closest
          (arrive vehicle-cmp vehicle-point best-hiding-spot
                  :diceleration 0.1)
          (make-point-2d)))))

;; follow path

(defun.ps+ init-follow-path (path &key
                                  (loop-p t)
                                  waypoint-seek-dist)
  (assert (and path (nth 0 path)))
  (assert (and waypoint-seek-dist (> waypoint-seek-dist 0)))
  (let ((current-index 0)
        (waypoint-seek-dist-p2 (* waypoint-seek-dist waypoint-seek-dist)))
    (lambda (vehicle-cmp vehicle-point)
      (labels ((get-current-waypoint ()
                 (nth current-index path))
               (last-waypoint-p ()
                 (and (not loop-p)
                      (= current-index (1- (length path)))))
               (goto-next-wayponit-p ()
                 (and (not (last-waypoint-p))
                      (< (calc-dist-p2 vehicle-point (get-current-waypoint))
                         waypoint-seek-dist-p2)))
               (set-next-waypoint ()
                 (let ((path-count (length path)))
                   (setf current-index
                         (if loop-p
                             (mod (1+ current-index) path-count)
                             (1+ current-index)))
                   (assert (< current-index path-count)))))
        (when (goto-next-wayponit-p)
          (set-next-waypoint))
        (if (not (last-waypoint-p))
            (seek vehicle-cmp vehicle-point (get-current-waypoint))
            (arrive vehicle-cmp vehicle-point (get-current-waypoint)
                    :diceleration 0.1))))))

;; offset pursuit

(defun.ps+ init-offset-pursuit (leader offset)
  (lambda (vehicle-cmp vehicle-point)
    (let* ((global-offset (transformf-point
                           (make-point-2d :x (vector-2d-x offset)
                                          :y (vector-2d-y offset))
                           (calc-global-point leader)))
           (to-offset (sub-vector-2d vehicle-point global-offset))
           (leader-velocity (vehicle-component-velocity
                             (get-ecs-component 'vehicle-component leader)))
           (look-ahead-time
            (/ (vector-2d-abs to-offset)
               (+ (vehicle-component-max-speed vehicle-cmp)
                  (vector-2d-abs leader-velocity)))))
      (arrive vehicle-cmp vehicle-point
              (add-vector-2d global-offset
                             (*-vec-scalar leader-velocity
                                           look-ahead-time))))))

;; --- aux --- ;;

;; Note: (ps (random))     -> Math.random()
;;       (ps (random 1.0)) -> Math.floor(1.0 * Math.random());
(defun.ps random-clamped ()
  (1- (* 2 (random))))

(defun random-clamped ()
  (1- (random 2.0)))
