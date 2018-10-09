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
                :nav-mesh-2d-num-y))
(in-package :clw-sample-game-algorithm/sample/navigation/a-star)

;; Note: "x" and "y" are represented as indexes in nav-mesh grid.

;; r: real, h: heuristic
(defstruct.ps+ a-star-node
    x y parent
    r-score h-score)

(defstruct.ps+ a-star
    (opened-nodes (list))
  num-x num-y
  goal-x goal-y
  enable-slant-p
  grid-state)

(defun.ps+ calc-a-star-index (x y a-star)
  (+ x (* y (a-star-num-x a-star))))

(defun.ps+ get-a-star-grid-state (x y a-star)
  (aref (a-star-grid-state a-star)
        (calc-a-star-index x y a-star)))

(defun.ps+ setf-a-star-grid-state (x y state a-star)
  (setf (aref (a-star-grid-state a-star)
              (calc-a-star-index x y a-star))
        state))

(defun.ps+ init-a-star (&key nav-mesh start-x start-y goal-x goal-y (enable-slant-p t))
  (let (result)
    (setf result
          (make-a-star :num-x (nav-mesh-2d-num-x nav-mesh)
                       :num-y (nav-mesh-2d-num-y nav-mesh)
                       :goal-x goal-x
                       :goal-y goal-y
                       :grid-state (make-array (* (nav-mesh-2d-num-x nav-mesh)
                                                  (nav-mesh-2d-num-y nav-mesh))
                                               :initial-element :none)
                       :enable-slant-p enable-slant-p))
    (do-nav-mesh-grid ((x y) nav-mesh)
      (unless (get-nav-mesh-piece-state x y nav-mesh)
        (setf-a-star-grid-state x y :closed result)))
    (open-a-node start-x start-y nil result)
    result))

(defun.ps+ search-path (&key nav-mesh start-x start-y goal-x goal-y (enable-slant-p t))
  "Returns searched path as list like '((x1 y1) ... (xn yn)).
If path is not found, returns nil."
  (let ((a-star (init-a-star :nav-mesh nav-mesh
                             :start-x start-x :start-y start-y
                             :goal-x goal-x :goal-y goal-y
                             :enable-slant-p enable-slant-p)))
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
        (mapcar (lambda (node)
                  (list (a-star-node-x node)
                        (a-star-node-y node)))
                (extract-path goal-node))))))

(defun.ps+ goal-node-p (node a-star)
  (with-slots (goal-x goal-y) a-star
    (and (= (a-star-node-x node) goal-x)
         (= (a-star-node-y node) goal-y))))

(defun.ps+ calc-score (node)
  (+ (a-star-node-r-score node)
     (a-star-node-h-score node)))

(defun.ps+ calc-heuristic-cost (base-x base-y goal-x goal-y enable-slant-p)
  (if enable-slant-p
      (max (abs (- goal-x base-x))
           (abs (- goal-y base-y)))
      (+ (abs (- goal-x base-x))
         (abs (- goal-y base-y)))))

(defun.ps+ get-around-node-list (base-x base-y a-star)
  (let ((result (list)))
    (with-slots (num-x num-y grid-state enable-slant-p) a-star
      (labels ((valid-point-p (x y)
                 (and (<= 0 x (1- num-x))
                      (<= 0 y (1- num-y))))
               (push-if-required (x y)
                 (when (and (valid-point-p x y)
                            (eq (get-a-star-grid-state x y a-star) :none))
                   (push (list x y) result))))
        (push-if-required (1- base-x) base-y)
        (push-if-required (1+ base-x) base-y)
        (push-if-required base-x (1- base-y))
        (push-if-required base-x (1+ base-y))
        (when enable-slant-p
          (push-if-required (1- base-x) (1- base-y))
          (push-if-required (1+ base-x) (1- base-y))
          (push-if-required (1+ base-x) (1+ base-y))
          (push-if-required (1- base-x) (1+ base-y)))))
    result))

(defun.ps+ open-around-nodes (parent a-star)
  (with-slots (x y) parent
    (setf-a-star-grid-state x y :closed a-star)
    (with-slots (opened-nodes) a-star
      (setf opened-nodes (remove parent opened-nodes)))
    (dolist (node (get-around-node-list x y a-star))
      (open-a-node (car node) (cadr node) parent a-star))))

(defun.ps+ open-a-node (x y parent a-star)
  (with-slots (goal-x goal-y enable-slant-p) a-star
    (let ((node (make-a-star-node
                 :x x :y y :parent parent
                 :r-score (if parent (1+ (a-star-node-r-score parent)) 0)
                 :h-score (calc-heuristic-cost x y goal-x goal-y enable-slant-p))))
      (push node (a-star-opened-nodes a-star))
      (setf-a-star-grid-state x y :opened a-star))))

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
