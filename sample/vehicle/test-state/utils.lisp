(defpackage :clw-sample-game-algorithm/sample/vehicle/test-state/utils
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export
   ;; state
   :vehicle-tester-state
   :def-test-state
   ;; entities
   :make-test-vehicle
   :make-wander-vehicle
   :make-target-entity
   :make-wander-avoiding-vehicle
   :init-obstacles
   ;; utils
   :warp-when-over-edge
   :get-steering
   :get-test-state-manager
   :random1
   :update-wander-param
   :update-avoiding-obstacle-param)
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
                :set-wander-behavior
                :set-avoid-obstacle)
  (:import-from :clw-sample-game-algorithm/sample/vehicle/test-state-manager
                :init-test-state-manager
                :register-test-state
                :switch-test-state)
  (:import-from :ps-experiment/common-macros
                :setf-with)
  (:import-from :alexandria
                :with-gensyms))
(in-package :clw-sample-game-algorithm/sample/vehicle/test-state/utils)

;; --- initialize --- ;;

(defvar.ps *test-state-manager* (init-test-state-manager))

(defun.ps+ get-test-state-manager ()
  *test-state-manager*)

(defstruct.ps+
    (vehicle-tester-state
     (:include game-state))
    (parent (make-ecs-entity)))

(defmacro def-test-state (structure-name (&rest slots)
                          &key start-process process end-process
                            ;; ((:seek (make-basic-behavior-state :mode :seek))
                            ;;  (:flee (make-basic-behavior-state :mode :flee))
                            ;;  ...)
                            register-name-initializer-pairs)
  (with-gensyms (state parent)
    `(progn (defstruct.ps+
                (,structure-name
                 (:include vehicle-tester-state
                           (start-process
                            (lambda (,state)
                              (let ((,parent (vehicle-tester-state-parent ,state)))
                                (add-ecs-entity ,parent)
                                (with-ecs-entity-parent (,parent)
                                  ,(when start-process
                                     `(funcall ,start-process ,state))))
                              t))
                           (process
                            (lambda (,state)
                              (let ((,parent (vehicle-tester-state-parent ,state)))
                                (with-ecs-entity-parent (,parent)
                                  ,(when process
                                     `(funcall ,process ,state))))))
                           (end-process
                            (lambda (,state)
                              (let ((,parent (vehicle-tester-state-parent ,state)))
                                (with-ecs-entity-parent (,parent)
                                  ,(when end-process
                                     `(funcall ,end-process ,state)))
                                (when (find-the-entity ,parent)
                                  (register-next-frame-func
                                   (lambda () (delete-ecs-entity ,parent))))
                                (clear-gui-panel))
                              t))))
                ,@slots)
            (defun.ps+ ,(intern "REGISTER-THIS-TEST-STATE" *package*) ()
              ,@(mapcar (lambda (pair)
                          `(register-test-state
                            (get-test-state-manager)
                            ,(car pair)
                            (lambda () ,(cadr pair))))
                        register-name-initializer-pairs)))))

;; --- some typical entities --- ;;

;; vehicles

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

(defun.ps+ make-wander-vehicle (&key (display-wander-circle-p t)
                                     (wander-radius #lx20)
                                     (wander-dist #lx60)
                                     (wander-jitter #lx3))
  (let ((vehicle (make-test-vehicle)))
    (add-entity-tag vehicle :wander-vehicle)
    (with-ecs-components (steering) vehicle
      (set-wander-behavior steering
                           :wander-radius wander-radius
                           :wander-dist wander-dist
                           :wander-jitter wander-jitter)
      (add-ecs-component-list
       vehicle
       (make-script-2d :func #'warp-when-over-edge)
       (init-entity-params
        :display-wander-circle-p display-wander-circle-p
        :wander-radius wander-radius
        :wander-dist wander-dist
        :wander-jitter wander-jitter))
      (when display-wander-circle-p
        (let ((wander-circle-model
               (make-wander-circle-model :wander-radius wander-radius
                                         :wander-dist wander-dist)))
          (add-ecs-component-list vehicle wander-circle-model)
          (set-entity-param vehicle
                            :wander-circle-model wander-circle-model))))
    vehicle))

(defun.ps+ make-wander-circle-model (&key wander-radius wander-dist)
  (make-model-2d :model (make-wired-circle :r wander-radius
                                           :color #x888888)
                 :offset (make-point-2d :x wander-dist)
                 :depth 100))

(defun.ps+ update-wander-param (wander-vehicle
                                &key wander-radius wander-dist wander-jitter)
  (check-entity-tags wander-vehicle :wander-vehicle)
  ;; params
  (flet ((update-a-param (key value)
           (when value
             (set-entity-param wander-vehicle key value))))
    (update-a-param :wander-radius wander-radius)
    (update-a-param :wander-dist wander-dist)
    (update-a-param :wander-jitter wander-jitter))
  (let ((radius (get-entity-param wander-vehicle :wander-radius))
        (dist (get-entity-param wander-vehicle :wander-dist))
        (jitter (get-entity-param wander-vehicle :wander-jitter)))
    (let ((steering (get-ecs-component 'steering wander-vehicle)))
      (set-wander-behavior steering
                           :wander-radius radius
                           :wander-dist dist
                           :wander-jitter jitter))
    ;; model
    (when (get-entity-param wander-vehicle :display-wander-circle-p)
      (let ((old-model (get-entity-param wander-vehicle :wander-circle-model))
            (new-model
             (make-wander-circle-model :wander-radius radius
                                       :wander-dist dist)))
        (assert old-model)
        (register-next-frame-func
         (lambda ()
           (delete-ecs-component old-model wander-vehicle)
           (add-ecs-component-list wander-vehicle new-model)))
        (set-entity-param wander-vehicle :wander-circle-model new-model)))))

(defun.ps+ make-wander-avoiding-vehicle (&key (display-search-dist nil)
                                              (vehicle-width #lx20)
                                              (min-search-dist #lx50)
                                              (max-search-dist #lx100))
  (let* ((vehicle (make-wander-vehicle :display-wander-circle-p nil))
         (steering (get-ecs-component 'steering vehicle)))
    (add-entity-tag vehicle :avoid-obstacle)
    (add-ecs-component-list
     vehicle
     (init-entity-params :vehicle-width vehicle-width
                         :min-search-dist min-search-dist
                         :max-search-dist max-search-dist))
    (when display-search-dist
      (add-ecs-component-list
       vehicle
       (make-script-2d
        :func (lambda (entity)
                (update-search-dist-model
                 entity
                 (get-entity-param entity :vehicle-width)
                 (get-entity-param entity :min-search-dist)
                 (get-entity-param entity :max-search-dist))))))
    (setf-with (get-ecs-component 'vehicle-component vehicle)
      max-speed #lx2
      max-force #lx0.1)
    (update-avoiding-obstacle-using-param vehicle)
    vehicle))

(defun.ps+ update-avoiding-obstacle-param (vehicle
                                           &key vehicle-width min-search-dist max-search-dist)
  (check-entity-tags vehicle :avoid-obstacle)
  (flet ((update-a-param (key value)
           (when value
             (set-entity-param vehicle key value))))
    (update-a-param :vehicle-width vehicle-width)
    (update-a-param :min-search-dist min-search-dist)
    (update-a-param :max-search-dist max-search-dist))
  (update-avoiding-obstacle-using-param vehicle))

(defun.ps+ update-avoiding-obstacle-using-param (vehicle)
  (check-entity-tags vehicle :avoid-obstacle)
  (set-avoid-obstacle (get-ecs-component 'steering vehicle)
                      :vehicle-width (get-entity-param vehicle :vehicle-width)
                      :min-search-dist (get-entity-param vehicle :min-search-dist)
                      :max-search-dist (get-entity-param vehicle :max-search-dist)))

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

;; others

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

;; --- utils --- ;;

;; Note: (ps (random))     -> Math.random()
;;       (ps (random 1.0)) -> Math.floor(1.0 * Math.random());
(defun.ps random1 ()
  (random))
(defun random1 ()
  (random 1.0))

(defun.ps+ get-steering (entity)
  (get-ecs-component 'steering entity))

(defun.ps+ warp-when-over-edge (entity)
  (with-ecs-components (point-2d) entity
    (setf-with point-2d
      x (mod x #lx1000)
      y (mod y #ly1000))))
