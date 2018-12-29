(defpackage clw-sample-game-algorithm/sample/navigation/a-star
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :search-path)
  (:import-from :clw-sample-game-algorithm/sample/navigation/nav-mesh
                :do-nav-mesh-grid
                :get-nav-mesh-piece-state
                :nav-mesh-2d-num-x
                :nav-mesh-2d-num-y)
  (:import-from :ps-experiment/common-macros
                :with-slots-pair))
(in-package :clw-sample-game-algorithm/sample/navigation/a-star)

;; Note: "x" and "y" are represented as indexes in nav-mesh grid.

;; r: real, h: heuristic
(defstruct.ps+ a-star-node
    node parent
    r-score h-score)

(defstruct.ps+ a-star
    (opened-nodes (list))
  mesh
  goal-node
  grid-state)

(defun.ps+ calc-a-star-index (node a-star)
  ;; TODO: (Note: this assumes node is grid-mesh-node)
  (with-slots (x y) node
    (+ x (* y (grid-mesh-num-x (a-star-mesh a-star))))))

(defun.ps+ get-a-star-grid-state (node a-star)
  (aref (a-star-grid-state a-star)
        (calc-a-star-index node a-star)))

(defun.ps+ setf-a-star-grid-state (node state a-star)
  (setf (aref (a-star-grid-state a-star)
              (calc-a-star-index node a-star))
        state))

(defun.ps+ init-a-star (&key nav-mesh start-node goal-node mesh)
  (let (result)
    (setf result
          (make-a-star :goal-node goal-node
                       :grid-state (make-array (* (nav-mesh-2d-num-x nav-mesh)
                                                  (nav-mesh-2d-num-y nav-mesh))
                                               :initial-element nil)
                       :mesh mesh))
    (do-nav-mesh-grid ((x y) nav-mesh)
      (unless (get-nav-mesh-piece-state x y nav-mesh)
        (setf-a-star-grid-state (make-grid-mesh-node :x x :y y) :closed result)))
    (open-a-node start-node nil result)
    result))

(defun.ps+ search-path (&key nav-mesh start-x start-y goal-x goal-y (enable-slant-p t))
  "Returns searched path as list like '((x1 y1) ... (xn yn)).
If path is not found, returns nil."
  (let ((a-star (init-a-star :nav-mesh nav-mesh
                             :start-node (make-grid-mesh-node :x start-x :y start-y)
                             :goal-node (make-grid-mesh-node :x goal-x :y goal-y)
                             :mesh (make-grid-mesh :enable-slant-p enable-slant-p
                                                   :num-x (nav-mesh-2d-num-x nav-mesh)
                                                   :num-y (nav-mesh-2d-num-y nav-mesh)))))
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
    (let ((goal-node (search-rec)))
      (when goal-node
        (mapcar (lambda (as-node)
                  (let ((node (a-star-node-node as-node)))
                    (list (grid-mesh-node-x node)
                          (grid-mesh-node-y node))))
                (extract-path goal-node))))))

(defun.ps+ goal-node-p (as-node a-star)
  ;; TODO: This assumes that these nodes are grid-mesh-nodes.
  ;;       Should be (eq (a-star-node-node as-node) goal-node) after refactoring.
  (with-slots-pair (((base-x x) (base-y y)) (a-star-node-node as-node)
                    ((goal-x x) (goal-y y)) (a-star-goal-node a-star))
    (and (= base-x goal-x)
         (= base-y goal-y))))

(defun.ps+ calc-score (as-node)
  (+ (a-star-node-r-score as-node)
     (a-star-node-h-score as-node)))

(defun.ps+ get-around-node-list (node a-star)
  (let ((result (list)))
    (with-slots-pair ((grid-state mesh) a-star
                      (num-x num-y) mesh
                      ;; TODO: (Note: this assumes node is grid-mesh-node)
                      ((base-x x) (base-y y)) node)
      (labels ((valid-point-p (x y)
                 (and (<= 0 x (1- num-x))
                      (<= 0 y (1- num-y))))
               (push-if-required (x y)
                 (let ((new-node (make-grid-mesh-node :x x :y y)))
                   (when (and (valid-point-p x y)
                              (not (get-a-star-grid-state new-node a-star)))
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

(defun.ps+ open-around-nodes (as-node-parent a-star)
  (with-slots (node) as-node-parent
    (setf-a-star-grid-state node :closed a-star)
    (with-slots (opened-nodes) a-star
      (setf opened-nodes (remove as-node-parent opened-nodes)))
    (dolist (around-node (get-around-node-list node a-star))
      (open-a-node around-node as-node-parent a-star))))

(defun.ps+ open-a-node (node parent a-star)
  (with-slots (goal-node mesh) a-star
    (let ((as-node (make-a-star-node
                    :node node
                    :parent parent
                    :r-score (if parent (1+ (a-star-node-r-score parent)) 0)
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

(defstruct.ps+ grid-mesh num-x num-y enable-slant-p)

(defstruct.ps+ grid-mesh-node x y)

(defgeneric.ps+ calc-heuristic-cost (mesh node1 node2))

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
