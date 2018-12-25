(defpackage clw-sample-game-algorithm/sample/SAT/visualizer
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :init-visualizer)
  (:import-from :clw-sample-game-algorithm/sample/SAT/axis
                :get-intersected-projection
                :init-axis-sat
                :axis-sat-base-point
                :axis-sat-unit-vector
                :projection-sat
                :projection-sat-axis
                :projection-sat-min
                :projection-sat-max
                :project-polygon-to-axis)
  (:import-from :clw-sample-game-algorithm/sample/SAT/collision
                :init-sat-object
                :sat-object-axis-list
                :sat-object-point-list)
  (:import-from :clw-sample-game-algorithm/sample/SAT/sat-system
                :sat-component
                :sat-component-point-list
                :extract-point-list)
  (:import-from :ps-experiment/common-macros
                :setf-with))
(in-package :clw-sample-game-algorithm/sample/SAT/visualizer)

(defvar.ps+ *color-list* (list #xff0000 #x00ffff))
(defvar.ps+ *intersected-color* #xffff00)

(defun.ps+ update-visualizer (entity)
  (let ((axis-list (get-axis-list))
        (target-point-lists (get-target-point-lists)))
    (delete-ecs-component-type 'model-2d entity)
    (dolist (axis axis-list)
      (add-projection-models axis target-point-lists entity))))

(defun.ps+ add-projection-models (axis point-lists entity)
  "For specified axis"
  (let ((proj-list (mapcar (lambda (point-list)
                             (project-polygon-to-axis point-list axis))
                           point-lists))
        (color-list *color-list*)
        (count 0))
    ;; lines for each point-list
    (dolist (proj proj-list)
      (let ((proj-model (make-projection-line-model
                         proj
                         (nth (mod count (length color-list))
                              color-list))))
        (add-ecs-component proj-model entity)
        (incf count)))
    ;; intersected line
    ;; XXX: only for 2 point-lists
    (let ((proj1 (nth 0 proj-list))
          (proj2 (nth 1 proj-list)))
      (when (and proj1 proj2)
        (let ((intersected-proj (get-intersected-projection proj1 proj2)))
          (when intersected-proj
            (let ((proj-model (make-projection-line-model
                               intersected-proj
                               *intersected-color*
                               :depth 1000)))
              (add-ecs-component proj-model entity))))))))

(defun.ps+ init-visualizer ()
  (let ((vis (make-ecs-entity)))
    (add-ecs-component-list
     vis
     (make-point-2d)
     (make-script-2d :func #'update-visualizer)
     (init-entity-params))
    (add-ecs-entity vis)))

(defun.ps+ adjustf-axis-base-point (axis)
  "Set axis as a tangent line of a oval"
  (let* ((axis-unit-vector (axis-sat-unit-vector axis))
         (axis-x (vector-2d-x axis-unit-vector))
         (axis-y (vector-2d-y axis-unit-vector))
         (oval-len-x (* 0.4 (get-screen-width))) ;; half length
         (oval-len-y (* 0.4 (get-screen-height))))
    (flet ((set-result (x-result y-result)
             (setf-with (axis-sat-base-point axis)
               x (+ (* 1/2 (get-screen-width))
                    (* (abs x-result)
                       (if (> axis-y 0) 1 -1)))
               y (+ (* 1/2 (get-screen-height))
                    (* (abs y-result)
                       (if (> axis-x 0) -1 1))))))
      (cond ((= axis-x 0) (set-result oval-len-x 0))
            ((= axis-y 0) (set-result 0 oval-len-y))
            (t (let* ((slope (/ axis-y axis-x))
                      (;; s a^2 / (s^2 a^2 + b^2)^1/2
                       x (/ (* slope (expt oval-len-x 2))
                            (expt (+ (* (expt slope 2)
                                        (expt oval-len-x 2))
                                     (expt oval-len-y 2))
                                  1/2)))
                      (;; b (1 - x^2/a^2)^1/2
                       y (* oval-len-y
                            (expt (- 1 (/ (expt x 2)
                                          (expt oval-len-x 2)))
                                  1/2))))
                 (set-result x y)))))))

(defun.ps+ get-axis-list ()
  (let* ((entity (find-a-entity-by-tag :sat-object))
         (result (sat-object-axis-list
                  (init-sat-object (calc-global-point-list entity)))))
    (dolist (axis result)
      (adjustf-axis-base-point axis))
    result))

(defun.ps+ get-target-point-lists ()
  (let ((result (list)))
    (do-tagged-ecs-entities (entity :sat-object)
      (push (calc-global-point-list entity) result))
    (nreverse result)))

(defun.ps+ calc-global-point-list (entity)
  (let* ((global-point (calc-global-point entity))
         (global-point-list (list)))
    (dolist (point (extract-point-list entity))
      (push (transformf-point (clone-point-2d point) global-point)
            global-point-list))
    global-point-list))

(defun.ps+ make-projection-line-model (proj color &key (depth 100))
  (check-type proj projection-sat)
  (with-slots (axis min max) proj
    (flet ((make-a-point (len)
             (with-slots (base-point unit-vector) axis
               (list (+ (vector-2d-x base-point) (* (vector-2d-x unit-vector) len))
                     (+ (vector-2d-y base-point) (* (vector-2d-y unit-vector) len))))))
      (make-model-2d 
       :depth depth
       :model (make-line :pos-a (make-a-point min)
                         :pos-b (make-a-point max)
                         :color color)))))
