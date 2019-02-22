(defpackage :clw-sample-game-algorithm/sample/vehicle/test-state-manager
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :init-test-state-manager
           :register-test-state
           :switch-test-state)
  (:import-from :ps-experiment/common-macros
                :setf-with))
(in-package :clw-sample-game-algorithm/sample/vehicle/test-state-manager)

;; --- data structure --- ;;

(defstruct.ps+ test-state-manager
    ;; ((name1 fn-make-state1) (name2 fn-make-state2) ...)
    ;; Note: Use list instead of hash-table to keep order.
    (state-list (list))
  state-manager
  current-state-name
  (on-add-state (list))
  (on-switch-state (list)))

;; --- interface --- ;;

(defun.ps+ init-test-state-manager ()
  (let ((manager-entity (make-ecs-entity)))
    (add-ecs-component-list
     manager-entity
     (make-script-2d :func (lambda (entity)
                             (let ((manager (get-internal-state-manager entity)))
                               (when manager
                                 (process-game-state manager)
                                 (process-state-change-input entity)))))
     (init-entity-params :manager (make-test-state-manager)))
    (add-ecs-entity manager-entity)
    (init-state-visualizer manager-entity)
    manager-entity))

(defun.ps+ register-test-state (manager name fn-make-state)
  (push (list name fn-make-state)
        (get-state-list manager))
  (dolist (callback (test-state-manager-on-add-state
                     (get-test-state-manager manager)))
    (funcall callback manager name)))

(defun.ps+ switch-test-state (manager name)
  (let* ((test-state-manager (get-test-state-manager manager))
         (internal-state-manager (get-internal-state-manager manager))
         (state (funcall (find-state-maker manager name)))
         (before-name (test-state-manager-current-state-name test-state-manager)))
    (setf (test-state-manager-current-state-name test-state-manager) name)
    (if internal-state-manager
        (interrupt-game-state state internal-state-manager)
        (setf (get-internal-state-manager manager)
              (init-game-state-manager state)))
    (dolist (callback (test-state-manager-on-switch-state test-state-manager))
      (funcall callback manager before-name name))))

;; - (not yet exported) - ;;

(defmacro.ps+ do-test-state ((var manager) &body body)
  (let ((state-info (gensym)))
    `(dolist (,state-info (get-state-list ,manager))
       (let ((,var (list :name (car ,state-info))))
         ,@body))))

(defun.ps+ add-on-switch-state-event (manager callback)
  "The callback should receive (manager before-state-name after-state-name) as arguments"
  (let ((test-state-manager (get-test-state-manager manager)))
    (push callback (test-state-manager-on-switch-state test-state-manager))))

(defun.ps+ add-on-add-state-event (manager callback)
  "The func should receive (manager new-state-name) as arguments"
  (let ((test-state-manager (get-test-state-manager manager)))
    (push callback (test-state-manager-on-add-state test-state-manager))))

;; --- utils --- ;;

(defun.ps+ get-test-state-manager (manager-entity)
  (get-entity-param manager-entity :manager))

(defmacro.ps+ get-state-list (manager-entity)
  `(test-state-manager-state-list
    (get-test-state-manager ,manager-entity)))

(defmacro.ps+ get-internal-state-manager (manager-entity)
  `(test-state-manager-state-manager
    (get-test-state-manager ,manager-entity)))

(defun.ps+ find-state-maker (manager-entity name)
  (let ((pair (find-if (lambda (pair) (eq (car pair) name))
                       (get-state-list manager-entity))))
    (assert pair)
    (cadr pair)))

;; --- controller --- ;;

(defun.ps+ process-state-change-input (manager-entity)
  (when (> (get-mouse-wheel-delta-y) 0)
    (goto-next-state manager-entity nil))
  (when (< (get-mouse-wheel-delta-y) 0)
    (goto-next-state manager-entity t)))

(defun.ps+ goto-next-state (manager-entity next-p)
  (with-slots (current-state-name state-list)
      (get-test-state-manager manager-entity)
    (let* ((cur-index (find-index-if
                       (lambda (pair) (eq (car pair) current-state-name))
                       state-list))
           (max-index (1- (length state-list)))
           (next-index (cond ((and next-p (= cur-index max-index)) 0)
                             ((and (not next-p) (= cur-index 0)) max-index)
                             (next-p (1+ cur-index))
                             (t (1- cur-index)))))
      (switch-test-state manager-entity
                         (car (nth next-index state-list))))))

;; --- visualizer --- ;;

(defvar.ps+ *visualizer-color* #xaaaaaa)

(defun.ps+ init-state-visualizer (manager-entity)
  (let* ((font-size #lx15)
         (margin #lx5)
         (visualizer (make-text-area :font-size font-size
                                     :x #lx-1000 ; re-calc after
                                     :y #ly-1000 ; re-calc after
                                     :margin margin))
        (cursor (make-visualizer-cursor)))
    (add-entity-tag visualizer :state-visualizer)
    (add-ecs-component-list
     visualizer
     (make-script-2d
      :func (lambda (entity)
              (let ((size (get-text-area-size entity))
                    (margin-from-edge #lx10))
                (setf-with (get-ecs-component 'point-2d entity)
                  x (- #lx1000 (getf size :width) margin-from-edge)
                  y (+ (getf size :height) margin-from-edge)))))
     (init-entity-params :name-list (list)
                         :cursor cursor))
    ;; register callbacks
    (add-on-add-state-event
     manager-entity
     (lambda (manager name)
       (declare (ignore manager))
       (add-state-to-visualizer visualizer name)))
    (add-on-switch-state-event
     manager-entity
     (lambda (manager before-name after-name)
       (declare (ignore manager before-name))
       (switch-state-in-visualizer visualizer after-name
                                   :margin margin :height font-size)))
    ;; add existing states
    (do-test-state (info manager-entity)
      (let ((name (getf info :name)))
        (assert name)
        (add-state-to-visualizer visualizer name)))
    ;; add entities
    (add-ecs-entity visualizer)
    (add-ecs-entity cursor visualizer)))

(defun.ps+ add-state-to-visualizer (visualizer name)
  (check-entity-tags visualizer :state-visualizer)
  (let ((name-list (get-entity-param visualizer :name-list)))
    (push name name-list)
    (add-text-to-area visualizer :text name :color *visualizer-color*)))

(defun.ps+ switch-state-in-visualizer (visualizer name &key margin height)
  (check-entity-tags visualizer :state-visualizer)
  (assert (and margin height))
  (let* ((cursor (get-entity-param visualizer :cursor))
         (name-list (get-entity-param visualizer :name-list))
         (index (find-index-if (lambda (name-in-list)
                                 (eq name name-in-list))
                               name-list))
         (offset-y (+ (* (+ margin height) (length name-list))
                      (* margin 2))))
    (assert cursor)
    (when (null index)
      (error "The name \"~A\" is not found in the list \"~A\""
             name name-list))
    (move-visualizer-cursor
     cursor
     (+ (* -1 offset-y)
        margin (* 1/2 height)
        (* (+ margin height) index)))))

;; cursor

(defun.ps+ make-visualizer-cursor ()
  (let ((cursor (make-ecs-entity))
        (width #lx10)
        (height #lx20))
    (add-ecs-component-list
     cursor
     (make-point-2d :x #x-1000)
     (make-model-2d :model (make-solid-polygon
                            :pnt-list (list (list (* -1 width) (* -1/2 height))
                                            (list 0 0)
                                            (list (* -1 width) (* 1/2 height)))
                            :color *visualizer-color*)))
    cursor))

(defun.ps+ move-visualizer-cursor (cursor rel-y)
  (let ((margin-x #lx10))
    (setf-with (get-ecs-component 'point-2d cursor)
      x (* -1 margin-x)
      y rel-y)))

;; --- aux --- ;;

(defun.ps+ find-index-if (test sequence)
  (dotimes (i (length sequence))
    (when (funcall test (nth i sequence))
      (return-from find-index-if i))))

