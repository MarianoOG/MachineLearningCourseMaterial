;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                                                                                      ;;;;
;;;; Artificial Intelligence Laboratoy, CIC-IPN                                           ;;;;
;;;; Professor: Dr. Salvador Godoy Calderón                                               ;;;;
;;;; Author: Mariano Orozco García                                                        ;;;;
;;;;                                                                                      ;;;;
;;;; starAlgorithm.lisp: Basic star algorithm for induction learning                      ;;;;
;;;;                                                                                      ;;;;
;;;; Updated: August, 2016                                                                ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ------------------------------ Initial State ------------------------------

;; Package:
(export (defvar *MLC_directory* "~/Documents/MachineLearningCourseMaterial") :cl-user) ; Change if needed
(load (concatenate 'string cl-user::*MLC_directory* "/pkg/Induction.lisp"))
(load (concatenate 'string cl-user::*MLC_directory* "/pkg/Logic.lisp"))
(load (concatenate 'string cl-user::*MLC_directory* "/pkg/Auxiliar.lisp"))

(defpackage :starAlgorithm
	(:use
		:common-lisp
		:pkgAuxiliar
		:pkgInduction
		:pkgLogic)
	(:export
		:starAlgorithm))

(in-package :starAlgorithm)

;; Auxiliary functions:
(defun starAlgorithm (inputFile POS &optional (visible t))
	(let ((table (make-hash-table :test #'equal)) (NEG '()) (filter '()) (seed '())
			(PS '()) (posCount '()) (negCount '()) (size '()) (star '()) (Solutions '()))

		;; Read .cet file and print:
		(setf table (exRead (concatenate 'string cl-user:*MLC_directory* "/input/" inputFile)))

		;; Define NEG as the rest of clases:
		(setf NEG (loop for h in (gethash (intern (gethash 'class table)) table) when (not (string= h (string-upcase (string POS)))) collect h))

		(if visible (format t "POS: ~a~%NEG: ~a~%" POS NEG))

		;; Define Filter
		(setf filter (loop for i from 0 below (nth (exFindClass table POS) (gethash 'count table)) collect t))

		;;; ------------------------------ Loop ------------------------------
		(loop for k from 0 do

			;; Generate seed:
			; (setf seed (exSelect table POS 0))
			(setf seed (exRandSelect table POS filter))
			(setf seed (exToSet table seed))

			;; Generate PS:
			(setf PS (setCombinations seed))

			;; Select consistent descriptions:
			(setf negCount (loop for item in PS collect
				(apply #'+ (loop for i from 0 below (length NEG) collect (exCount item table (nth i NEG))))))
			(setf PS (loop for i from 0 below (length PS) when (= (nth i negCount) 0) collect (nth i PS)))

			;; Select maximum coberture:
			(setf posCount (loop for item in PS collect (exCount item table POS)))
			(setf PS (loop for i from 0 below (length PS) when (= (nth i posCount) (apply #'max posCount)) collect (nth i PS)))

			;; Select the smaller descriptions:
			(setf size (loop for item in PS collect	(expSize item)))
			(setf PS (loop for i from 0 below (length PS) when (= (nth i size) (apply #'min size)) collect (nth i PS)))

			;; Generate Star:
			(setf star (concatenate 'list (list (nth (random (length PS)) PS)) star))

			;; Analize Star:
			(setf posCount (loop for item in star collect (exCount item table POS)))

			;; Reduce POS examples:
			(setf filter (mapAnd filter
				(loop for i in (exPositions (nth 0 star) table POS) collect (not i))))

			(if visible
				(format t "Seed: ~a~%PS(~a): ~a~%star: ~a~%filter: ~a -> ~a~%"
					seed (length PS) PS star filter (not (listOr filter))))

			(if (not (listOr filter))
				(setf Solutions (if (= (length star) 1)
					star
					(cons 'or (loop for i in star collect i))))
				(defparameter PS '()))

		until (> (length Solutions) 0))
		Solutions))

;; Use:
(defparameter *cet_file* "Ballons4.cet")
(defparameter *class* "T")
(format t "Star for ~a in ~a~%Solutions:~%" *class* *cet_file*)
(expPrint (starAlgorithm *cet_file* *class* t) pkgLogic::'tree)
