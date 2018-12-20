;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                                                                                      ;;;;
;;;; Artificial Intelligence Laboratoy, CIC-IPN                                           ;;;;
;;;; Professor: Dr. Salvador Godoy Calderón                                               ;;;;
;;;; Author: Mariano Orozco García                                                        ;;;;
;;;;                                                                                      ;;;;
;;;; starAlgorithm.lisp: Basic star algorithm for induction learning                      ;;;;
;;;;                                                                                      ;;;;
;;;; Updated: November, 2016                                                              ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ------------------------------ Initial State ------------------------------

;; Package:
(export (defvar *MLC_directory* "~/Documents/MachineLearningCourseMaterial") :cl-user) ; Change if needed
(load (concatenate 'string cl-user::*MLC_directory* "/pkg/Reinforcement.lisp"))
(load (concatenate 'string cl-user::*MLC_directory* "/pkg/Auxiliar.lisp"))

(defpackage :qlearningAlgorithm
	(:use
		:common-lisp
		:pkgReinforcement
		:pkgAuxiliar)
	(:export
		:qlearning))

(in-package :qlearningAlgorithm)

(defun qlearning(inputFile &key (rewards t) (gamma t) (type t) (extra t))
	(let ((problem (make-hash-table :test #'equal)) Q Qant poly (n 0) (end nil) m)

		;; Read file and print:
		(setf problem (problemRead (concatenate 'string cl-user:*MLC_directory* "/input/" inputFile)))
		(problemPrint problem :rewards rewards :gamma gamma :type type :extra extra)

		;; Find Q values:
		(setf Q (loop for i in (gethash 'rewards problem) collect (loop for j in i collect 0)))
		(setf Qant (loop for i in Q collect (loop for j in i collect j)))

		(loop until end do
			(loop for i from 0 below (length Q) do
				(loop for j from 0 below (length (nth i Q)) if
					(not (= (nth j (nth i (gethash 'rewards problem))) -1)) do
					(setf (nth j (nth i Q))
						(+ (nth j (nth i (gethash 'rewards problem)))
							(* (gethash 'gamma problem) (listMax (nth j Q)))))
				)
			)
			(setf n (1+ n))
			(setf end (listAnd (loop for i from 0 below (length Q) collect
				(listAnd (loop for j from 0 below (length (nth i Q)) collect
					(< (abs (- (nth j (nth i Q)) (nth j (nth i Qant)))) 1e-9))))))
			(setf Qant (loop for i in Q collect (loop for j in i collect j))))

		(setf m (listMax (loop for i in Q collect (listMax (loop for j in i collect j)))))
		(setf Q (loop for i in Q collect (loop for j in i collect (round (* (/ j m) 100)))))

		(when extra
			(format t "Q:~%")
			(loop for i in Q do (format t "    ~a~%" i)))
		(format t "Archived in ~a epochs!~%" n)

		;; Find solution from Q values:
		(setf poly (policy Q (gethash 'start problem) (gethash 'goal problem)))
		(when (string= (gethash 'type problem) "MAZE")
			(format t "Solution:~%")
			(loop for i in (mazeSolution problem poly) do (format t "    ~a~%" i)))
		poly))

;; Use:
(defparameter *file* "rooms.rt")
(format t "qLearning for problem in: ~a~%" *file*)
(format t "policy: ~a~%" (qlearning *file* :type nil))
