(defpackage clw-sample-game-algorithm/sample/navigation/node/grid-node
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :init-grid-mesh
           :make-grid-mesh-node)
  (:import-from :clw-sample-game-algorithm/sample/navigation/node/interface
                :calc-heuristic-cost
                :calc-real-cost
                :convert-node-to-point
                :get-around-node-list
                :get-node-id)
  (:import-from :clw-sample-game-algorithm/sample/navigation/nav-mesh
                :do-nav-mesh-grid
                :get-nav-mesh-piece-point
                :get-nav-mesh-piece-state
                :nav-mesh-2d-num-x
                :nav-mesh-2d-num-y)
  (:import-from :ps-experiment/common-macros
                :with-slots-pair))
(in-package :clw-sample-game-algorithm/sample/navigation/node/grid-node)

(defstruct.ps+ grid-mesh num-x num-y enable-slant-p nav-mesh-2d)
(defstruct.ps+ grid-mesh-node x y)

(defun.ps+ init-grid-mesh (&key nav-mesh-2d enable-slant-p)
  (make-grid-mesh :enable-slant-p enable-slant-p
                  :num-x (nav-mesh-2d-num-x nav-mesh-2d)
                  :num-y (nav-mesh-2d-num-y nav-mesh-2d)
                  :nav-mesh-2d nav-mesh-2d))

(defmethod.ps+ calc-heuristic-cost ((mesh grid-mesh)
                                    (node1 grid-mesh-node) (node2 grid-mesh-node))
  (with-slots-pair ((enable-slant-p) mesh
                    ((x1 x) (y1 y)) node1
                    ((x2 x) (y2 y)) node2)
    (if enable-slant-p
        (max (abs (- x2 x1))
             (abs (- y2 y1)))
        (+ (abs (- x2 x1))
           (abs (- y2 y1))))))

(defmethod.ps+ calc-real-cost ((mesh grid-mesh)
                               (node1 grid-mesh-node) (node2 grid-mesh-node))
  1)

(defmethod.ps+ convert-node-to-point ((mesh grid-mesh) (node grid-mesh-node))
  (get-nav-mesh-piece-point
   (grid-mesh-node-x node)
   (grid-mesh-node-y node)
   (grid-mesh-nav-mesh-2d mesh)))

(defmethod.ps+ get-around-node-list ((mesh grid-mesh) (node grid-mesh-node))
  (let ((result (list)))
    (with-slots-pair ((num-x num-y nav-mesh-2d) mesh
                      ((base-x x) (base-y y)) node)
      (labels ((valid-point-p (x y)
                 (and (<= 0 x (1- num-x))
                      (<= 0 y (1- num-y))))
               (push-if-required (x y)
                 (let ((new-node (make-grid-mesh-node :x x :y y)))
                   (when (and (valid-point-p x y)
                              (get-nav-mesh-piece-state x y nav-mesh-2d))
                     (push new-node result)))))
        (push-if-required (1- base-x) base-y)
        (push-if-required (1+ base-x) base-y)
        (push-if-required base-x (1- base-y))
        (push-if-required base-x (1+ base-y))
        (when (grid-mesh-enable-slant-p mesh)
          (push-if-required (1- base-x) (1- base-y))
          (push-if-required (1+ base-x) (1- base-y))
          (push-if-required (1+ base-x) (1+ base-y))
          (push-if-required (1- base-x) (1+ base-y)))))
    result))

(defmethod.ps+ get-node-id ((mesh grid-mesh) (node grid-mesh-node))
  (with-slots (x y) node
    (+ x (* y (grid-mesh-num-x mesh)))))
