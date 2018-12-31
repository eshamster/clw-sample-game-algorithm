(defpackage clw-sample-game-algorithm/sample/navigation/node/interface
  (:use :cl
        :ps-experiment)
  (:export :calc-heuristic-cost
           :calc-real-cost
           :convert-node-to-point
           :get-around-node-list
           :get-node-id))
(in-package :clw-sample-game-algorithm/sample/navigation/node/interface)

(defgeneric.ps+ calc-heuristic-cost (mesh node1 node2))
(defgeneric.ps+ calc-real-cost (mesh node1 node2))
(defgeneric.ps+ convert-node-to-point (mesh node))
(defgeneric.ps+ get-around-node-list (mesh node))
(defgeneric.ps+ get-node-id (mesh node))
