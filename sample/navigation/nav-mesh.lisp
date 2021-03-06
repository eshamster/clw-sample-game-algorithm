(defpackage clw-sample-game-algorithm/sample/navigation/nav-mesh
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :init-nav-mesh
           :destroy-nav-mesh
           :setf-nav-mesh-display-p
           :update-nav-mesh

           :do-nav-mesh-grid
           :get-nav-mesh-piece-state
           :nav-mesh-2d-num-x
           :nav-mesh-2d-num-y
           :get-nav-mesh-piece-point))
(in-package :clw-sample-game-algorithm/sample/navigation/nav-mesh)

(defstruct.ps+ nav-mesh-2d
    rect num-x num-y (piece-entities (list)) grid-state)

(defun.ps+ init-nav-mesh (&key rect num-x num-y)
  (check-type rect rect-2d)
  (let ((result (make-nav-mesh-2d
                 :rect rect
                 :num-x num-x
                 :num-y num-y
                 :grid-state (make-array (* num-x num-y) :initial-element t))))
    (init-grid-entity result)
    result))

(defun.ps+ destroy-nav-mesh (mesh)
  (declare (ignore mesh))
  (do-tagged-ecs-entities (grid :nav-mesh-grid)
    (delete-ecs-entity grid)))

(defmacro.ps+ do-nav-mesh-grid (((var-x var-y) nav-mesh) &body body)
  `(dotimes (,var-y (nav-mesh-2d-num-y ,nav-mesh))
     (dotimes (,var-x (nav-mesh-2d-num-x ,nav-mesh))
       ,@body)))

(defmacro.ps+ do-nav-mesh-piece ((var-piece nav-mesh) &body body)
  `(dolist (,var-piece (nav-mesh-2d-piece-entities ,nav-mesh))
     ,@body))

(defun.ps+ update-nav-mesh (nav-mesh)
  (check-type nav-mesh nav-mesh-2d)
  (let ((obstacles (list)))
    (do-tagged-ecs-entities (obstacle :obstacle)
      (push obstacle obstacles))
    (do-nav-mesh-piece (piece nav-mesh)
      (if (find-if (lambda (obstacle)
                     (collide-entities-p piece obstacle))
                   obstacles)
          (disable-piece piece nav-mesh)
          (enable-piece piece nav-mesh)))))

(defun.ps+ setf-nav-mesh-display-p (nav-mesh value)
  (do-nav-mesh-piece (piece nav-mesh)
    (set-entity-param piece :display-p value)
    (if value
        (enable-model-2d piece)
        (disable-model-2d piece))))

(defun.ps+ calc-piece-id (x y nav-mesh)
  (+ (* y (nav-mesh-2d-num-x nav-mesh))
     x))

(defun.ps+ init-grid-entity (nav-mesh)
  (check-type nav-mesh nav-mesh-2d)
  (with-slots (rect num-x num-y piece-entities) nav-mesh
    (let ((grid (make-ecs-entity))
          (piece-width (/ (rect-2d-width rect) num-x))
          (piece-height (/ (rect-2d-height rect) num-y)))
      (add-entity-tag grid :nav-mesh-grid)
      (add-ecs-component-list
       grid
       (make-point-2d :x (rect-2d-x rect)
                      :y (rect-2d-y rect)))
      (with-ecs-entity-parent (grid)
        (do-nav-mesh-grid ((x y) nav-mesh)
          (push
           (init-piece (calc-piece-id x y nav-mesh)
                       (make-point-2d :x (* piece-width x)
                                      :y (* piece-height y))
                       piece-width
                       piece-height)
           piece-entities)))
      grid)))

(defmacro.ps+ get-piece-state (piece nav-mesh)
  `(aref (nav-mesh-2d-grid-state ,nav-mesh)
         (get-entity-param ,piece :id)))

(defun.ps+ get-nav-mesh-piece-state (x y nav-mesh)
  (aref (nav-mesh-2d-grid-state nav-mesh)
        (calc-piece-id x y nav-mesh)))

(defun.ps+ get-nav-mesh-piece-point (x y nav-mesh)
  "Get center point of the specifed piece"
  (unless (and (<= 0 x (1- (nav-mesh-2d-num-x nav-mesh)))
               (<= 0 y (1- (nav-mesh-2d-num-y nav-mesh))))
    (error "Invalid (x, y) index"))
  (with-slots ((offset-x x) (offset-y x) width height)
      (nav-mesh-2d-rect nav-mesh)
    (make-point-2d
     :x (+ offset-x (* (+ 1/2 x)
                       (/ width 1.0 (nav-mesh-2d-num-x nav-mesh))))
     :y (+ offset-y (* (+ 1/2 y)
                       (/ height 1.0 (nav-mesh-2d-num-y nav-mesh)))))))

(defun.ps+ enable-piece (piece nav-mesh)
  (setf (get-piece-state piece nav-mesh) t)
  (when (get-entity-param piece :display-p)
    (enable-model-2d piece)))

(defun.ps+ disable-piece (piece nav-mesh)
  (setf (get-piece-state piece nav-mesh) nil)
  (when (get-entity-param piece :display-p)
    (disable-model-2d piece)))

(defvar.ps+ *grid-piece-depth* -100)

(defun.ps+ init-piece (id left-down-point width height)
  (let ((piece (make-ecs-entity)))
    (add-entity-tag piece :nav-mesh-piece)
    (add-ecs-entity piece)
    (add-ecs-component-list
     piece
     (clone-point-2d left-down-point)
     (make-physic-polygon
      :target-tags (list :--nothing)
      :pnt-list (list (make-point-2d :x 0 :y 0)
                      (make-point-2d :x width :y 0)
                      (make-point-2d :x width :y height)
                      (make-point-2d :x 0 :y height)))
     (make-model-2d :model (make-solid-rect :width width :height height
                                            :color #xddffdd)
                    :depth *grid-piece-depth*)
     (make-model-2d :model (make-wired-rect :width width :height height
                                            :color #x22ff22)
                    :depth (1+ *grid-piece-depth*))
     (init-entity-params :id id
                         :display-p t))
    piece))
