#|
  This file is a part of clw-sample-game-algorithm project.
  Copyright (c) 2018 eshamster (hamgoostar@gmail.com)
|#

#|
  Samples to try some game algorithms

  Author: eshamster (hamgoostar@gmail.com)
|#

(defsystem clw-sample-game-algorithm
  :version "0.1"
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :author "eshamster"
  :license "LLGPL"
  :depends-on (:parenscript
               :ps-experiment
               :cl-ps-ecs
               :cl-web-2d-game
               :ningle
               :cl-markup
               :clack
               :clw-sample-game-algorithm/main)
  :description "Samples to try some game algorithms"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op clw-sample-game-algorithm/t))))

(defsystem clw-sample-game-algorithm/t
  :class :package-inferred-system
  :depends-on (:ps-experiment
               :ps-experiment/t
               :rove
               :alexandria
               :cl-js
               "ps-experiment/t/test-utils"
               "clw-sample-game-algorithm/t/sat/axis")
  :perform (test-op (o c) (symbol-call :rove '#:run c)))
