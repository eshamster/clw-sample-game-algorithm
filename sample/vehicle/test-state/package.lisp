(defpackage :clw-sample-game-algorithm/sample/vehicle/test-state/package
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game
        ;; To Load
        :clw-sample-game-algorithm/sample/vehicle/test-state/basic-behaviors
        :clw-sample-game-algorithm/sample/vehicle/test-state/pursuit
        :clw-sample-game-algorithm/sample/vehicle/test-state/wander
        :clw-sample-game-algorithm/sample/vehicle/test-state/avoid-obstacle
        :clw-sample-game-algorithm/sample/vehicle/test-state/interpose
        :clw-sample-game-algorithm/sample/vehicle/test-state/hide
        :clw-sample-game-algorithm/sample/vehicle/test-state/follow-path
        :clw-sample-game-algorithm/sample/vehicle/test-state/offset-pursuit
        :clw-sample-game-algorithm/sample/vehicle/test-state/flocking)
  (:export :init-vehicle-test-state)
  (:import-from :clw-sample-game-algorithm/sample/vehicle/test-state/utils
                :get-test-state-manager)
  (:import-from :clw-sample-game-algorithm/sample/vehicle/test-state-manager
                :init-test-state-manager
                :switch-test-state))
(in-package :clw-sample-game-algorithm/sample/vehicle/test-state/package)

(eval-when (:execute :compile-toplevel :load-toplevel)
  (defun find-all-registering-functions (this-package)
    (mapcan (lambda (package)
              (let ((target-func (find-symbol "REGISTER-THIS-TEST-STATE" package)))
                (when target-func
                  (list target-func))))
            (package-use-list this-package))))

;; Define as macro to solve symbol in Common Lisp environment
(defmacro.ps+ register-all-test-states ()
  `(progn
     ,@(mapcar (lambda (func)
                 `(,func))
               (find-all-registering-functions #.*package*))))

(defun.ps+ init-vehicle-test-state ()
  (register-all-test-states)
  (switch-test-state (get-test-state-manager) :seek))
