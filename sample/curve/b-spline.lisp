(defpackage :clw-sample-game-algorithm/sample/curve/b-spline
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :calc-b-spline-point
           :make-knots-uniform-generator
           :make-knots-open-uniform-generator
           :make-knots-bezier-generator))
(in-package :clw-sample-game-algorithm/sample/curve/b-spline)

(defun.ps+ calc-b-spline-point (control-points knots-generator alpha)
  (let* ((num-control-points (length control-points))
         (knots (funcall knots-generator num-control-points))
         (result-point (make-point-2d)))
    (dotimes (i num-control-points)
      (let ((basis (calc-b-spline-basis num-control-points knots i alpha))
            (x (point-2d-x (nth i control-points)))
            (y (point-2d-y (nth i control-points))))
        (incf (point-2d-x result-point) (* x basis))
        (incf (point-2d-y result-point) (* y basis))))
    result-point))

(defun.ps+ calc-b-spline-basis (num-control-points knots index-control-point alpha)
  "So-called B-spline basis function"
  (assert (>= index-control-point 0))
  (labels ((div-non-0 (x y)
             (if (= y 0) 0 (/ x y)))
           (rec (i-knot cur-dim)
             (if (= cur-dim 0)
                 ;; knots_ik <= alpha < knots_ik+1
                 (if (and (<= (aref knots i-knot) alpha)
                          (< alpha (aref knots (1+ i-knot))))
                     1
                     0)
                 ;;   {(alpha - knots_ik) / (knots_ik+d - knots_ik)} * rec_ik,d-1 +
                 ;;   {(knots_ik+d+1 - alpha) / (knots_ik+d+1 - knots_ik+1)} * rec_ik+1,d-1
                 ;; Note: If the denominator is 0, the term can be concluded as 0.
                 ;;       This is because (knots_ik+m[+1] = knots_ik+n[+1]) holds for
                 ;;       arbitrary m and n (1<=m<=d, 1<=n<=d),
                 ;;       so when d = 0 the "rec" should return 0.
                 (+ (* (div-non-0 (- alpha
                                     (aref knots i-knot))
                                  (- (aref knots (+ i-knot cur-dim))
                                     (aref knots i-knot)))
                       (rec i-knot (1- cur-dim)))
                    (* (div-non-0 (- (aref knots (+ i-knot cur-dim 1))
                                     alpha)
                                  (- (aref knots (+ i-knot cur-dim 1))
                                     (aref knots (+ i-knot 1))))
                       (rec (1+ i-knot) (1- cur-dim)))))))
    (let ((dim (- (length knots) num-control-points 1)))
      (assert (>= dim 0))
      (rec index-control-point dim))))

;; --- make knots --- ;;

(defun.ps+ make-knots-uniform (dim num-control-points)
  "Ex. #(0 0.2 0.4 0.6 0.8 1)"
  (let* ((num-knots (+ dim num-control-points 1))
         (knots (make-array num-knots)))
    (dotimes (i num-knots)
      (setf (aref knots i)
            (* 1.0 (/ i (1- num-knots)))))
    knots))

(defun.ps+ make-knots-uniform-generator (dim)
  (lambda (num-control-points)
    (make-knots-uniform dim num-control-points)))

(defun.ps+ make-knots-open-uniform (dim num-control-points)
  "Ex. #(0 0 0 1/3 2/3 1 1 1) <- dim = 2"
  (let* ((num-knots (+ dim num-control-points 1))
         (knots (make-array num-knots))
         (num-uniform (- num-knots (* 2 (1+ dim)))))
    (dotimes (i (1+ dim))
      (setf (aref knots i) 0)
      (setf (aref knots (+ i (1+ dim) num-uniform)) 1))
    (dotimes (i num-uniform)
      (setf (aref knots (+ i (1+ dim)))
            (* 1.0 (/ (1+ i) (1+ num-uniform)))))
    knots))

(defun.ps+ make-knots-open-uniform-generator (dim)
  (lambda (num-control-points)
    (make-knots-open-uniform dim num-control-points)))

(defun.ps+ make-knots-bezier (num-control-points)
  (make-knots-open-uniform (1- num-control-points) num-control-points))

(defun.ps+ make-knots-bezier-generator ()
  (lambda (num-control-points)
    (make-knots-bezier num-control-points)))
