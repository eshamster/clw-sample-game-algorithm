(defpackage :clw-sample-game-algorithm/sample/vehicle/test-state-manager
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :init-test-state-manager
           :register-test-state
           :switch-test-state))
(in-package :clw-sample-game-algorithm/sample/vehicle/test-state-manager)

;; --- data structure --- ;;

(defstruct.ps+ test-state-manager
    ;; ((name1 fn-make-state1) (name2 fn-make-state2) ...)
    ;; Note: Use list instead of hash-table to keep order.
    (state-list (list))
  state-manager
  current-state-name)

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
    manager-entity))

(defun.ps+ register-test-state (manager name fn-make-state)
  (push (list name fn-make-state)
        (get-state-list manager)))

(defun.ps+ switch-test-state (manager name)
  (let ((test-state-manager (get-test-state-manager manager))
        (internal-state-manager (get-internal-state-manager manager))
        (state (funcall (find-state-maker manager name))))
    ;; TODO: Visualize state
    (add-to-event-log (+ "Change state to: " name))
    (setf (test-state-manager-current-state-name test-state-manager) name)
    (if internal-state-manager
        (interrupt-game-state state internal-state-manager)
        (setf (get-internal-state-manager manager)
              (init-game-state-manager state)))))

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

(defun.ps+ find-index-if (test sequence)
  (dotimes (i (length sequence))
    (when (funcall test (nth i sequence))
      (return-from find-index-if i))))

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
