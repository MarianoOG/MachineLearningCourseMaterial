;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                                                                                      ;;;;
;;;; Artificial Intelligence Laboratoy, CIC-IPN                                           ;;;;
;;;; Professor: Dr. Salvador Godoy Calderón                                               ;;;;
;;;; Author: Mariano Orozco García                                                        ;;;;
;;;;                                                                                      ;;;;
;;;; pkgInduction.lisp: Functions to work with induction algorithms.                      ;;;;
;;;;                                                                                      ;;;;
;;;; Updated: November, 2016                                                             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ------------------------------ Package definition ------------------------------

(load (concatenate 'string cl-user::*MLC_directory* "/pkg/Auxiliar.lisp"))

(defpackage :pkgReinforcement
	(:use
		:common-lisp
		:pkgAuxiliar)
	(:export
		:problemRead
		:problemPrint
		:policy
		:mazeSolution
		:type
		:rewards
		:goal
		:start
		:gamma))

;;; ------------------------------ Package Description ------------------------------

;; The definition of the problem will be extrated from .re files
;; el simbolo # será usado para comentarios y secciones:
;;     	# Class indica el nombre de la clase seguida de un guion y las etiquetas de las clases separadas por espacios.

(in-package :pkgReinforcement)

(defun node2pos(size node)
	(when (< node (* (nth 0 size) (nth 1 size)))
		(list (1+ (/ (- node (mod node (nth 1 size))) (nth 1 size))) (1+ (mod node (nth 1 size))))))

(defun pos2node(size pos)
	(when (and (<= (nth 0 pos) (nth 0 size)) (<= (nth 1 pos) (nth 1 size)))
		(+ (* (1- (nth 0 pos)) (nth 1 size)) (1- (nth 1 pos)))))

(defun problemPrint(problem &key (rewards t) (gamma t) (type t) (extra t))
	(format t "*********************************************~%")
	(format t "START: ~a, GOAL: ~a~%" (gethash 'start problem) (gethash 'goal problem))
	(when rewards
		(format t "REWARDS:~%")
		(loop for i in (gethash 'rewards problem) do (format t "    ~a~%" i))
	)
	(when gamma (format t "GAMMA: ~a~%" (gethash 'gamma problem)))
	(when type (format t "TYPE: ~a~%" (gethash 'type problem)))
	(when (not (string= (gethash 'type problem) "REWARDS"))
			(format t "NodeDistance: ~a~%" (gethash 'ndist problem)))
	(when (and (string= (gethash 'type problem) "TRANSITION") extra)
			(format t "TRANSITION:~%")
			(loop for i in (gethash 'transition problem) do (format t "    ~a~%" i)))
	(when (string= (gethash 'type problem) "MAZE")
		(format t "Size: ~a~%" (gethash 'size problem)))
	(when (and (string= (gethash 'type problem) "MAZE") extra)
			(format t "MAZE:~%")
			(loop for i in (gethash 'maze problem) do (format t "    ~a~%" i))
			(format t "TRANSITION:~%")
			(loop for i in (gethash 'transition problem) do (format t "    ~a~%" i)))
	(format t "*********************************************~%"))

(defun policy(Q start goal &optional (operator #'listMax))
	(let ((poly (list start)) line max)
		; (format t "~a~%" poly)
		(loop do
			(setf line (nth (nth 0 poly) Q))
			; (format t "~a~%" line)
			(setf max (funcall operator line))
			; (format t "~a~%" max)
			(setf poly (cons
				(pick-random-element (loop for i from 0 below (length line) if (= (nth i line) max) collect i))
				poly))
			; (format t "~a~%" poly)
			until (= goal (nth 0 poly)))
		(reverse poly)))

(defun maze2transition(maze size)
	(let (transition p)
		(setf transition (loop for i from 0 below (* (nth 0 size) (nth 1 size)) collect
			(loop for j from 0 below (* (nth 0 size) (nth 1 size)) collect -1)))
		(loop for i from 0 below (nth 0 size) do
			(loop for j from 0 below (nth 1 size) do
				(setf p (pos2node size (list (1+ i) (1+ j))))
	           	(when (and (> j 0) (= (nth (1- j) (nth i maze)) 0))
	           		(setf (nth (pos2node size (list (1+ i) j)) (nth p transition)) 0))
				(when (and (> i 0) (= (nth j (nth (1- i) maze)) 0))
	           		(setf (nth (pos2node size (list i (1+ j))) (nth p transition)) 0))
				(when (and (< (1+ j) (nth 1 size)) (= (nth (1+ j) (nth i maze)) 0))
	           		(setf (nth (pos2node size (list (1+ i) (+ j 2))) (nth p transition)) 0))
				(when (and (< (1+ i) (nth 0 size)) (= (nth j (nth (1+ i) maze)) 0))
	           		(setf (nth (pos2node size (list (+ i 2) (1+ j))) (nth p transition)) 0))))
	transition))

(defun transition2rewards(transition ndist goal)
	(let (rewards (nodes (list goal)) (c 0))
		(setf rewards (loop for i in transition collect (loop for j in i collect j)))
		(loop for i from 0 below ndist do
			(loop for j from c below (length nodes) do
				; (format t "i: ~a, c: ~a, (nth ~a nodes): ~a~%" i c j (nth j nodes))
				(loop for k from 0 below (length rewards) if
					(= (nth (nth j nodes) (nth k rewards)) 0) do
					(setf (nth (nth j nodes) (nth k rewards)) (round (* (- ndist i) (/ 100 ndist))))
					(when (not (position k nodes))
						(setf nodes (concatenate 'list nodes (list k)))))
				(setf c (1+ c))))
	rewards))

(defun mazeSolution(problem policy)
	(let (solution pos (n 1))
		(setf solution (loop for i in (gethash 'maze problem) collect (loop for j in i collect j)))
		(loop for i in policy do
			(setf pos (node2pos (gethash 'size problem) i))
			(setf (nth (1- (nth 1 pos)) (nth (1- (nth 0 pos)) solution)) n)
			(setf n (1+ n)))
		solution))

;; La funcion reRead leerá un archivo externo file, regresara la tabla de ejemplos generada.
(defun problemRead(file)
	(let ((problem (make-hash-table :test #'equal)) l (op -1) (flag t))
		(with-open-file (i file)
	    	(loop for line = (read-line i nil) while line do
				(when (not (string= line ""))
					(progn
						(setf l (split-str (string-upcase line)))
						(if (string= (nth 0 l) "#")
						 	(progn
								(setf op
									(cond
										((string= (nth 1 l) "REWARDS") 0)
										((string= (nth 1 l) "TRANSITION") 1)
										((string= (nth 1 l) "MAZE") 2)
										((string= (nth 1 l) "START") 3)
										((string= (nth 1 l) "GOAL") 4)
										((string= (nth 1 l) "GAMMA") 5)
										((string= (nth 1 l) "NODEDISTANCE") 6)
										(t -1))))
							(progn
								(case op
									(0
										(setf (gethash 'type problem) "REWARDS")
										(setf (gethash 'rewards problem)
											(if flag
												(progn
													(setf flag nil)
													(list (loop for i from 0 below (length l)
														collect (with-input-from-string (in (nth i l)) (read in)))))
												(cons
													(loop for i from 0 below (length l)
														collect (with-input-from-string (in (nth i l)) (read in)))
													(gethash 'rewards problem)))))
									(1
										(setf (gethash 'type problem) "TRANSITION")
										(setf (gethash 'transition problem)
											(if flag
												(progn
													(setf flag nil)
													(list (loop for i from 0 below (length l)
														collect (with-input-from-string (in (nth i l)) (read in)))))
												(cons
													(loop for i from 0 below (length l)
														collect (with-input-from-string (in (nth i l)) (read in)))
													(gethash 'transition problem)))))
									(2
										(setf (gethash 'type problem) "MAZE")
										(setf (gethash 'maze problem)
											(if flag
												(progn
													(setf flag nil)
													(list (loop for i from 0 below (length l)
														collect (with-input-from-string (in (nth i l)) (read in)))))
												(cons
													(loop for i from 0 below (length l)
														collect (with-input-from-string (in (nth i l)) (read in)))
													(gethash 'maze problem))))
										(setf (gethash 'size problem)
											(list (length (gethash 'maze problem)) (length (nth 0 (gethash 'maze problem))))))
									(3 (setf (gethash 'start problem)
										(if (string= (gethash 'type problem) "MAZE")
											(pos2node (gethash 'size problem)
												(list (with-input-from-string (in (nth 0 l)) (read in))
													(with-input-from-string (in (nth 1 l)) (read in))))
											(with-input-from-string (in (nth 0 l)) (read in)))))
									(4 (setf (gethash 'goal problem)
										(if (string= (gethash 'type problem) "MAZE")
											(pos2node (gethash 'size problem)
												(list (with-input-from-string (in (nth 0 l)) (read in))
													(with-input-from-string (in (nth 1 l)) (read in))))
											(with-input-from-string (in (nth 0 l)) (read in)))))
									(5 (setf (gethash 'gamma problem) (with-input-from-string (in (nth 0 l)) (read in))))
									(6 (setf (gethash 'ndist problem) (with-input-from-string (in (nth 0 l)) (read in)))))))))))
		(if (string= (gethash 'type problem) "REWARDS")
			(setf (gethash 'rewards problem) (reverse (gethash 'rewards problem)))
			(when (or (not (gethash 'ndist problem)) (< (gethash 'ndist problem) 1)) (setf (gethash 'ndist problem) 1)))
		(when (string= (gethash 'type problem) "TRANSITION")
			(setf (gethash 'transition problem) (reverse (gethash 'transition problem)))
			(setf (gethash 'rewards problem)
				(transition2rewards (gethash 'transition problem) (gethash 'ndist problem) (gethash 'goal problem))))
		(when (string= (gethash 'type problem) "MAZE")
			(setf (gethash 'maze problem) (reverse (gethash 'maze problem)))
			(setf (gethash 'transition problem) (maze2transition (gethash 'maze problem) (gethash 'size problem)))
			(setf (gethash 'rewards problem)
				(transition2rewards (gethash 'transition problem) (gethash 'ndist problem) (gethash 'goal problem))))
	    problem))
