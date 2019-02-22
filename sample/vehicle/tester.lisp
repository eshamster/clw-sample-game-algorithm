(defpackage :clw-sample-game-algorithm/sample/vehicle/tester
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :init-vehicle-tester)
  (:import-from :clw-sample-game-algorithm/sample/vehicle/component
                :vehicle-component
                :vehicle-component-max-speed
                :vehicle-component-velocity
                :init-vehicle-component)
  (:import-from :clw-sample-game-algorithm/sample/vehicle/obstacle
                :init-vehicle-obstacle)
  (:import-from :clw-sample-game-algorithm/sample/vehicle/steering
                :steering
                :init-steering
                :set-seek-point
                :set-flee-point
                :set-arrive-point
                :set-wander-behavior
                :set-pursuit-target
                :set-avoid-obstacle
                :set-interpose
                :set-hide
                :set-follow-path
                :set-offset-pursuit)
  (:import-from :clw-sample-game-algorithm/sample/vehicle/test-state-manager
                :init-test-state-manager
                :register-test-state
                :switch-test-state)
  (:import-from :ps-experiment/common-macros
                :setf-with))
(in-package :clw-sample-game-algorithm/sample/vehicle/tester)

(defun.ps+ init-vehicle-tester ()
  (let ((test-state-manager (init-test-state-manager)))
    (register-all-test-states test-state-manager)
    (switch-test-state test-state-manager :seek)))

(defun.ps+ register-all-test-states (test-state-manager)
  (dolist (mode (list :seek :flee :arrive
                      :pursuit :wander :avoid-obstacle :interpose
                      :hide :follow-path :offset-pursuit))
    (register-test-state test-state-manager mode
                         (lambda () (make-tester-state mode)))))

(defun.ps+ make-tester-state (mode)
  (ecase mode
    ((:seek :flee :arrive) (make-seek-or-flee-state :mode mode))
    (:pursuit (make-pursuit-state))
    (:wander (make-wander-state))
    (:avoid-obstacle (make-avoid-obstacle-state))
    (:interpose (make-interpose-state))
    (:hide (make-hide-state))
    (:follow-path (make-follow-path-state))
    (:offset-pursuit (make-offset-pursuit-state))))

(defun.ps+ make-test-vehicle (&key (first-x #lx500)
                                   (first-y #ly500)
                                   (color #xffffff)
                                   (scale 1))
  (let* ((vehicle (make-ecs-entity))
         (width (* #lx30 scale))
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
                            :color color)))
    vehicle))

(defstruct.ps+
    (vehicle-tester-state
     (:include game-state
               (end-process
                (state-lambda (parent)
                  (when (find-the-entity parent)
                    (register-next-frame-func
                     (lambda () (delete-ecs-entity parent))))))))
    (parent (make-ecs-entity)))

;; --- seek, flee or arrive --- ;;

;; TODO: Rename state

(defstruct.ps+
    (seek-or-flee-state
     (:include vehicle-tester-state
               (start-process
                (state-lambda (mode parent)
                  (add-ecs-entity parent)
                  (with-ecs-entity-parent (parent)
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
                      (add-ecs-entity vehicle)))))))
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
                (state-lambda (parent)
                  (add-ecs-entity parent)
                  (with-ecs-entity-parent (parent)
                    (let* ((evader (make-wander-vehicle))
                           (pursuit-vehicle (make-pursuit-vehicle evader)))
                      (add-ecs-entity evader)
                      (add-ecs-entity pursuit-vehicle)
                      (add-ecs-entity (make-pursuit-target-visualizer
                                       pursuit-vehicle evader)))))))))

;; --- wander --- ;;

(defun.ps+ make-wander-vehicle (&key (display-wander-circle-p t))
  (let ((vehicle (make-test-vehicle))
        (wander-radius #lx20)
        (wander-dist #lx60))
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
                                   y (mod y #ly1000))))))
      (when display-wander-circle-p
        (add-ecs-component-list
         vehicle
         (make-model-2d :model (make-wired-circle :r wander-radius
                                                  :color #x888888)
                        :offset (make-point-2d :x wander-dist)
                        :depth 100))))
    vehicle))

(defstruct.ps+
    (wander-state
     (:include vehicle-tester-state
               (start-process
                (state-lambda (parent)
                  (add-ecs-entity parent)
                  (with-ecs-entity-parent (parent)
                    (add-ecs-entity (make-wander-vehicle))))))))

;; --- avoid obstacle --- ;;

(defun.ps+ init-obstacles (&key (num 15) (min-r #lx40) (max-r #lx80)
                                (min-dist #lx20) (margin #lx30))
  (let ((obstacles (list))
        (max-trial 30))
    (labels ((overlap-p (r x y)
               (dolist (old obstacles)
                 (let ((old-x (getf old :x))
                       (old-y (getf old :y))
                       (old-r (getf old :r)))
                   (when (< (calc-dist (make-point-2d :x x :y y)
                                       (make-point-2d :x old-x :y old-y))
                            (+ r old-r min-dist))
                     (return-from overlap-p t))))
               nil)
             (make-one (rest-trial)
               (let* ((r (lerp-scalar min-r max-r (random1)))
                      (x (lerp-scalar (+ r margin) (- #lx1000 r margin) (random1)))
                      (y (lerp-scalar (+ r margin) (- #ly1000 r margin) (random1))))
                 (if (or (<= rest-trial 0)
                         (not (overlap-p r x y)))
                     (progn
                       (push (list :x x :y y :r r) obstacles)
                       (init-vehicle-obstacle :point (make-point-2d :x x :y y)
                                              :r r))
                     (make-one (1- rest-trial))))))
      (dotimes (i num)
        (add-ecs-entity (make-one max-trial))))))

(defun.ps+ update-search-dist-model (vehicle vehicle-width min-search-dist max-search-dist)
  (with-ecs-components (vehicle-component) vehicle
    (let* ((search-dist (lerp-scalar
                         min-search-dist max-search-dist
                         (/ (vector-2d-abs (vehicle-component-velocity vehicle-component))
                            (vehicle-component-max-speed vehicle-component))))
           (model (make-model-2d :model (make-wired-rect :width search-dist
                                                         :height vehicle-width
                                                         :color #x888888)
                                 :offset (make-point-2d :y (* vehicle-width -1/2))))
           (pre-model (get-entity-param vehicle :search-dist-model)))
      (when pre-model
        (delete-ecs-component pre-model vehicle))
      (add-ecs-component model vehicle)
      (set-entity-param vehicle :search-dist-model model))))

(defun.ps+ make-wander-avoiding-vehicle (&key (display-search-dist nil))
  (let* ((vehicle (make-wander-vehicle :display-wander-circle-p nil))
         (steering (get-ecs-component 'steering vehicle))
         (vehicle-width #lx20)
         (min-search-dist #lx50)
         (max-search-dist #lx100))
    (when display-search-dist
      (add-ecs-component-list
       vehicle
       (make-script-2d
        :func (lambda (entity)
                (update-search-dist-model
                 entity
                 vehicle-width min-search-dist max-search-dist)))
       (init-entity-params :search-dist-model nil)))
    (setf-with (get-ecs-component 'vehicle-component vehicle)
      max-speed #lx2
      max-force #lx0.08)
    (set-avoid-obstacle steering
                        :vehicle-width vehicle-width
                        :min-search-dist min-search-dist
                        :max-search-dist max-search-dist)
    vehicle))

(defstruct.ps+
    (avoid-obstacle-state
     (:include vehicle-tester-state
               (start-process
                (state-lambda (parent)
                  (add-ecs-entity parent)
                  (with-ecs-entity-parent (parent)
                    (init-obstacles)
                    (add-ecs-entity (make-wander-avoiding-vehicle
                                     :display-search-dist t)))
                  t)))))

;; interpose

(defstruct.ps+
    (interpose-state
     (:include vehicle-tester-state
               (start-process
                (state-lambda (parent)
                  (add-ecs-entity parent)
                  (with-ecs-entity-parent (parent)
                    (let ((target1 (make-wander-vehicle :display-wander-circle-p nil))
                          (target2 (make-wander-vehicle :display-wander-circle-p nil))
                          (vehicle (make-test-vehicle :color #xff0000)))
                      (add-ecs-entity target1)
                      (add-ecs-entity target2)
                      (set-interpose (get-ecs-component 'steering vehicle)
                                     :target-vehicle1 target1
                                     :target-vehicle2 target2)
                      (add-ecs-component-list
                       vehicle)
                      (add-ecs-entity vehicle)))
                  t)))))

;; hide

(defun.ps+ make-hiding-vehicle (&key enemy-vehicle)
  (let* ((vehicle (make-test-vehicle :color #x00ffff))
         (steering (get-ecs-component 'steering vehicle))
         (vehicle-width #lx20)
         (min-search-dist #lx30)
         (max-search-dist #lx50))
    (setf-with (get-ecs-component 'vehicle-component vehicle)
      max-speed #lx2
      max-force #lx0.08)
    (set-avoid-obstacle steering
                        :vehicle-width vehicle-width
                        :min-search-dist min-search-dist
                        :max-search-dist max-search-dist)
    (set-hide steering :enemy-vehicle enemy-vehicle)
    vehicle))

(defstruct.ps+
    (hide-state
     (:include vehicle-tester-state
               (start-process
                (state-lambda (parent)
                  (add-ecs-entity parent)
                  (with-ecs-entity-parent (parent)
                    (init-obstacles :num 7
                                    :min-dist #lx60
                                    :margin #lx80)
                    (let* ((enemy (make-wander-avoiding-vehicle))
                           (hider (make-hiding-vehicle :enemy-vehicle enemy)))
                      (add-ecs-entity enemy)
                      (add-ecs-entity hider)))
                  t)))))

;; follow path

(defun.ps+ init-path (&key num-waypoint (loop-p t))
  (assert (and num-waypoint (> num-waypoint 0)))
  (let ((center-x #lx500)
        (center-y #ly500)
        (xr #lx300)
        (yr #ly300)
        (x-max-swing #lx100)
        (y-max-swing #ly100)
        (waypoints (list)))
    (flet ((calc-swing (max)
             (lerp-scalar (* -1 max) max (random1))))
      (dotimes (i num-waypoint)
        (let* ((theta (* 2 PI (/ i num-waypoint)))
               (base-x (+ center-x (* xr (cos theta))
                          (calc-swing x-max-swing)))
               (base-y (+ center-y (* yr (sin theta))
                          (calc-swing y-max-swing))))
          (push (list base-x base-y)
                waypoints))))
    (let* ((path-entity (make-ecs-entity))
           (color #xffffff)
           (model (if loop-p
                      (make-wired-polygon :pnt-list waypoints :color color)
                      (make-lines :pnt-list waypoints :color color))))
      (add-entity-tag path-entity :path)
      (add-ecs-component-list
       path-entity
       (make-point-2d)
       (make-model-2d :model model)
       (init-entity-params :path (mapcar (lambda (pair)
                                           (make-vector-2d :x (car pair)
                                                           :y (cadr pair)))
                                         waypoints)))
      (add-ecs-entity path-entity))))

(defun.ps+ search-path ()
  (let ((entity (find-a-entity-by-tag :path)))
    (assert entity)
    (get-entity-param entity :path)))

(defstruct.ps+
    (follow-path-state
     (:include vehicle-tester-state
               (start-process
                (state-lambda (parent)
                  (add-ecs-entity parent)
                  (with-ecs-entity-parent (parent)
                    (let ((loop-p t))
                      (init-path :num-waypoint 5
                                 :loop-p loop-p)
                      (let ((vehicle (make-test-vehicle)))
                        (add-ecs-entity vehicle)
                        (set-follow-path (get-ecs-component 'steering vehicle)
                                         :path (search-path)
                                         :loop-p loop-p))))
                  t)))))

;; offset pursuit

(defstruct.ps+
    (offset-pursuit-state
     (:include vehicle-tester-state
               (start-process
                (state-lambda (parent)
                  (add-ecs-entity parent)
                  (with-ecs-entity-parent (parent)
                    (let ((target (make-target-entity))
                          (leader (make-test-vehicle :scale 1.2)))
                      (add-ecs-entity target)
                      ;; leader
                      (add-ecs-component-list
                       leader
                       (make-script-2d :func (lambda (entity)
                                               (set-arrive-point
                                                (get-steering entity)
                                                (calc-global-point target)))))
                      (add-ecs-entity leader)
                      ;; formation
                      (dolist (offset (list (make-vector-2d :x #lx-40 :y #lx50)
                                            (make-vector-2d :x #lx-40 :y #lx-50)
                                            (make-vector-2d :x #lx-70 :y 0)))
                        (let ((member (make-test-vehicle :scale 0.8)))
                          (set-offset-pursuit (get-steering member)
                                              :leader leader :offset offset)
                          (add-ecs-entity member)))))
                  t)))))

;; --- utils --- ;;

;; Note: (ps (random))     -> Math.random()
;;       (ps (random 1.0)) -> Math.floor(1.0 * Math.random());
(defun.ps random1 ()
  (random))
(defun random1 ()
  (random 1.0))

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

(defun.ps+ get-steering (entity)
  (get-ecs-component 'steering entity))
