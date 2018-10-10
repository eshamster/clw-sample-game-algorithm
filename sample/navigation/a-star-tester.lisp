(defpackage clw-sample-game-algorithm/sample/navigation/a-star-tester
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:import-from :clw-sample-game-algorithm/sample/navigation/a-star
                :search-path)
  (:import-from :clw-sample-game-algorithm/sample/navigation/nav-mesh
                :get-nav-mesh-piece-point)
  (:export :test-a-star
           :init-test-a-star))
(in-package :clw-sample-game-algorithm/sample/navigation/a-star-tester)

(defvar.ps+ *enable-slant-path-p* t)

(defun.ps+ init-test-a-star ()
  (add-panel-bool "Enable slant path" *enable-slant-path-p*
                  :on-change (lambda (value)
                               (setf *enable-slant-path-p* value))))

(defun.ps+ test-a-star (nav-mesh)
  (let ((path (search-path :nav-mesh nav-mesh
                            :start-x 1 :start-y 2
                            :goal-x 10 :goal-y 10
                            :enable-slant-p *enable-slant-path-p*)))
    (register-next-frame-func
     (lambda ()
       (let ((pre-line (find-a-entity-by-tag :path-line)))
         (when pre-line
           (delete-ecs-entity pre-line)))
       (when path
         (add-ecs-entity (make-line-entity path nav-mesh)))))))

(defun.ps+ make-line-entity (path nav-mesh)
  (let ((line (make-ecs-entity))
        (line-points (list)))
    (add-entity-tag line :path-line)
    (add-ecs-component-list line (make-point-2d))
    (dolist (node path)
      (let ((point (get-nav-mesh-piece-point
                    (car node) (cadr node) nav-mesh)))
        (push (list (point-2d-x point) (point-2d-y point))
              line-points)))
    (add-ecs-component-list
     line
     (make-model-2d :model (make-lines :pnt-list line-points
                                       :color #xff11ff)
                    :depth 0))
    line))
