(defpackage clw-sample-game-algorithm/sample/navigation/a-star
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :search-path)
  (:import-from :clw-sample-game-algorithm/sample/navigation/node/interface
                :calc-heuristic-cost
                :calc-real-cost
                :convert-node-to-point
                :get-around-node-list
                :get-node-id)
  (:import-from :ps-experiment/common-macros
                :with-slots-pair))
(in-package :clw-sample-game-algorithm/sample/navigation/a-star)

;; r: real, h: heuristic
(defstruct.ps+ a-star-node
    node parent
    r-score h-score)

(defstruct.ps+ a-star
    (opening-nodes (list))
  (visited-nodes (make-hash-table))
  mesh
  goal-node)

(defun.ps+ register-node-as-visited (node a-star)
  (setf (gethash (get-node-id (a-star-mesh a-star) node)
                 (a-star-visited-nodes a-star))
        t))

(defun.ps+ visited-node-p (node a-star)
  (gethash (get-node-id (a-star-mesh a-star) node)
           (a-star-visited-nodes a-star)))

(defun.ps+ init-a-star (&key start-node goal-node mesh)
  (let ((result
         (make-a-star :goal-node goal-node
                      :mesh mesh)))
    (open-a-node start-node nil result)
    result))

(defun.ps+ search-path (&key mesh start-node goal-node)
  "Returns searched path as list of point-2d (start to goal order).
If path is not found, returns nil."
  (let ((a-star (init-a-star :start-node start-node
                             :goal-node goal-node
                             :mesh mesh)))
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
    (with-slots (opening-nodes) a-star
      (setf opening-nodes (remove as-node-parent opening-nodes)))
    (dolist (around-node (get-around-node-list (a-star-mesh a-star) node))
      (unless (visited-node-p around-node a-star)
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
      (push as-node (a-star-opening-nodes a-star))
      (register-node-as-visited node a-star))))

(defun.ps+ select-next-node (a-star)
  (let (result min-score)
    (dolist (node (a-star-opening-nodes a-star))
      (when (goal-node-p node a-star)
        (return-from select-next-node node))
      (let ((score (calc-score node)))
        (when (or (null min-score)
                  (< score min-score))
          (setf result node
                min-score score))))
    result))
