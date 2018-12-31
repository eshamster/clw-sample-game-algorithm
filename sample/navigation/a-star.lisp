(defpackage clw-sample-game-algorithm/sample/navigation/a-star
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :search-path)
  (:import-from :clw-sample-game-algorithm/sample/navigation/nav-mesh
                :do-nav-mesh-grid
                :get-nav-mesh-piece-point
                :get-nav-mesh-piece-state
                :nav-mesh-2d-num-x
                :nav-mesh-2d-num-y)
  (:import-from :ps-experiment/common-macros
                :with-slots-pair))
(in-package :clw-sample-game-algorithm/sample/navigation/a-star)

;; r: real, h: heuristic
(defstruct.ps+ a-star-node
    node parent
    r-score h-score)

(defstruct.ps+ a-star
    (opened-nodes (list))
  mesh
  goal-node
  grid-state)

(defun.ps+ get-a-star-grid-state (node a-star)
  (gethash (get-node-id (a-star-mesh a-star) node)
           (a-star-grid-state a-star)))

(defun.ps+ setf-a-star-grid-state (node state a-star)
  (setf (gethash (get-node-id (a-star-mesh a-star) node)
                 (a-star-grid-state a-star))
        state))

(defun.ps+ init-a-star (&key start-node goal-node mesh)
  (let ((result
         (make-a-star :goal-node goal-node
                      :grid-state (make-hash-table)
                      :mesh mesh)))
    (open-a-node start-node nil result)
    result))

(defun.ps+ search-path (&key nav-mesh start-x start-y goal-x goal-y (enable-slant-p t))
  "Returns searched path as list of point-2d (start to goal order).
If path is not found, returns nil."
  (let ((a-star (init-a-star :start-node (make-grid-mesh-node :x start-x :y start-y)
                             :goal-node (make-grid-mesh-node :x goal-x :y goal-y)
                             :mesh (make-grid-mesh :enable-slant-p enable-slant-p
                                                   :num-x (nav-mesh-2d-num-x nav-mesh)
                                                   :num-y (nav-mesh-2d-num-y nav-mesh)
                                                   :nav-mesh-2d nav-mesh))))
    (search-path-by-a-star a-star)))

(defun.ps+ search-path-by-a-star (a-star)
  (labels ((search-rec ()
             (let ((node (select-next-node a-star)))
               (unless node ; path not found
                 (return-from search-rec))
               (when (goal-node-p node a-star)
                 (return-from search-rec node))
               (open-around-nodes node a-star)
               (search-rec)))
           (extract-path (goal-node)
             (assert (goal-node-p goal-node a-star))
             (labels ((rec (node result)
                        (push node result)
                        (let ((parent (a-star-node-parent node)))
                          (if parent
                              (rec parent result)
                              result))))
               (rec goal-node (list)))))
    (let ((goal-as-node (search-rec)))
      (when goal-as-node
        (mapcar (lambda (as-node)
                  (convert-node-to-point
                   (a-star-mesh a-star)
                   (a-star-node-node as-node)))
                (extract-path goal-as-node))))))

(defun.ps+ goal-node-p (as-node a-star)
  (let ((mesh (a-star-mesh a-star)))
    (eq (get-node-id mesh (a-star-node-node as-node))
        (get-node-id mesh (a-star-goal-node a-star)))))

(defun.ps+ calc-score (as-node)
  (+ (a-star-node-r-score as-node)
     (a-star-node-h-score as-node)))

(defun.ps+ open-around-nodes (as-node-parent a-star)
  (with-slots (node) as-node-parent
    (setf-a-star-grid-state node :closed a-star)
    (with-slots (opened-nodes) a-star
      (setf opened-nodes (remove as-node-parent opened-nodes)))
    (dolist (around-node (get-around-node-list (a-star-mesh a-star) node))
      (unless (get-a-star-grid-state around-node a-star)
        (open-a-node around-node as-node-parent a-star)))))

(defun.ps+ open-a-node (node parent a-star)
  (with-slots (goal-node mesh) a-star
    (let ((as-node (make-a-star-node
                    :node node
                    :parent parent
                    :r-score (if parent
                                 (+ (a-star-node-r-score parent)
                                    (calc-real-cost
                                     mesh node (a-star-node-node parent)))
                                 0)
                    :h-score (calc-heuristic-cost
                              mesh node goal-node))))
      (push as-node (a-star-opened-nodes a-star))
      (setf-a-star-grid-state node :opened a-star))))

(defun.ps+ select-next-node (a-star)
  (let (result min-score)
    (dolist (node (a-star-opened-nodes a-star))
      (when (goal-node-p node a-star)
        (return-from select-next-node node))
      (let ((score (calc-score node)))
        (when (or (null min-score)
                  (< score min-score))
          (setf result node
                min-score score))))
    result))

;; TODO: move to another package
;; --- To generalize node --- ;;

(defgeneric.ps+ calc-heuristic-cost (mesh node1 node2))
(defgeneric.ps+ calc-real-cost (mesh node1 node2))
(defgeneric.ps+ convert-node-to-point (mesh node))
(defgeneric.ps+ get-around-node-list (mesh node))
(defgeneric.ps+ get-node-id (mesh node))

(defstruct.ps+ grid-mesh num-x num-y enable-slant-p nav-mesh-2d)
(defstruct.ps+ grid-mesh-node x y)

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
