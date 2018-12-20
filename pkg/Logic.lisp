;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                                                                                      ;;;;
;;;; Artificial Intelligence Laboratoy, CIC-IPN                                           ;;;;
;;;; Professor: Dr. Salvador Godoy Calderón                                               ;;;;
;;;; Author: Mariano Orozco García                                                        ;;;;
;;;;                                                                                      ;;;;
;;;; Logic.lisp: Logic functions to use with boolean expresions                           ;;;;
;;;;                                                                                      ;;;;
;;;; Updated: August, 2016                                                                ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ------------------------------ Package definition ------------------------------

(load (concatenate 'string cl-user::*MLC_directory* "/pkg/Auxiliar.lisp"))

; <PENDING> :setDifference :setIntersection :setSubsElem :setUnion :forall :exist
(defpackage :pkgLogic
	(:use
		:common-lisp
		:pkgAuxiliar)
	(:export
		:binCount
		:binDifference
		:binJoin
		:binTable
		:expBuild
		:expEq
		:expEquivalent
		:expFindSimple
		:expImply
		:expNot
		:expPrint
		:expSimplify
		:expSize
		:expSubstitute
		:expValidate
		:setAddElem
		:setCombinations
		:setEq
		:setPosition
		:setSubstitute))

(in-package :pkgLogic)

;;; ------------------------------ Boolean functions ------------------------------
;; Work with list of boolean values (t, "-" or nil values).

;;; ------------------------------ Logic Expresions ------------------------------
;; The format of logic expresions is the following: '(relation name value)
;;     relation can be any of the following operators: =, !=, <, >, <= y >=.
;;     '(nil) and '(T) are valid expresions.
;;     Basic operators and & or can be used to generate valid expresions,
;;         e. g., (and A B) and (or A B) where A & B are valid expresions.

;;; ------------------------------ Sets of Logic Expresions ------------------------------
;; Sets of Logic Expresions are lists of valid logic expresions,
;;     e. g., '(exp1 exp2 ... expN), where expi is a valid logic expresion.

;; FUNCTION: expNot
;;     Description: Is the logic operator not
;;     Input parameters:
;;         exp: a valid logical expresion
;;     Returns: A valid logic expresion.
(defun expNot(exp)
	(if (typep (nth 0 exp) 'boolean)
		(list (not (nth 0 exp)))
		(case (nth 0 exp)
			(= (cons '≠ (cdr exp)))
			(≠ (cons '= (cdr exp)))
			(< (cons '>= (cdr exp)))
			(> (cons '<= (cdr exp)))
			(<= (cons '> (cdr exp)))
			(>= (cons '< (cdr exp)))
			(and (cons 'or (loop for i in (cdr exp) collect (expNot i)))) ; De Morgan's law 1
			(or (cons 'and (loop for i in (cdr exp) collect (expNot i)))) ; De Morgan's law 2
		 	(otherwise (format t "WARNING: ~a is not a valid expresion! (expNot)~%" (nth 0 exp)) nil))))

;; FUNCTION: binCount
;;     Description: Counts the number of T in a list l of boolean variables
;;     Input parameters:
;;         l: is a list of boolean variables (t, "-" or nil values)
;;     Returns: The number of T in the list l (or nil if l is not a list)
(defun binCount(l)
	(let ((c 0))
		(loop for i from 0 below (length l) when
			(eq t (nth i l)) do (setf c (+ c 1)))
		c))

;; FUNCTION: binDifference
;;     Description: Counts the number of differences between two lists of boolean variables
;;     Input parameters:
;;         a & b: are lists of boolean variables (t, "-" or nil values)
;;     Returns: Returns the number of differences (or nil if the lists are not of equal size)
(defun binDifference(a b)
	(let ((c 0))
		(if (= (length a) (length b))
			(progn
				(loop for i from 0 below (length a) when
					(not (eq (nth i a) (nth i b))) do (setf c (+ c 1)))
				c)
			nil)))

;; FUNCTION: binJoin
;;     Description: Joins the a and b list of boolean variables with "-" (no matter condition) when
;;					they differ
;;     Input parameters:
;;         a & b: are lists of boolean variables (t, "-" or nil values)
;;     Returns: A list of boolean expresions with t, nil or "-" (or nil if the lists are not of equal size)
(defun binJoin(a b)
	(if (= (length a) (length b))
		(loop for i from 0 below (length a) collect
			(if (eq (nth i a) (nth i b))
				(nth i a)
				'-))
		nil))

;; FUNCTION: binTable
;;     Description: Generates all the posible combinations for n boolean variables
;;     Input parameters:
;;         n: the number of boolean variables
;;     Returns: A list of lists with boolean variables (t, "-" or nil values).
(defun binTable(n)
	(labels ((auxBinTable (m s)
		(case m
			(0 '())
			(1 '((nil) (t)))
			(otherwise (setf s (auxBinTable (- m 1) s))
				(concatenate 'list
					(loop for i in s collect (cons nil i))
					(loop for i in s collect (cons t i)))))))
		(auxBinTable n nil)))

;; FUNCTION: expBuild
;;     Description: Buids a valid expresion using a set of logical expresions and a list of boolean variables
;;     Input parameters:
;;         n: the number of boolean variables
;;     Returns: A list of lists with boolean variables (t, "-" or nil values)
(defun expBuild(set bin &optional (conector 'and))
	(let (out)
		(if (= (length set) (length bin))
			(case (length set)
				(0 '(nil))
				(1 (if (typep (nth 0 bin) 'boolean)
						(if (nth 0 bin)
							(nth 0 set)
							(expNot (nth 0 set)))
						(case (nth 0 bin)
						('- '-))))
				(otherwise (setf out (cons conector (loop for i from 0 below (length set) when (not (eq (nth i bin) '-))
					collect (if (nth i bin) (nth i set) (expNot (nth i set))))))
					(case (length out)
						(1 '-)
						(2 (nth 1 out))
						(otherwise out))))
			nil)))

;; FUNCTION: expEq
;;     Description: Compares if expresion expA is equal to expresion expB
;;     Input parameters:
;;         expA & expB: are valid logical expresions
;;     Returns: t or nil
(defun expEq(expA expB)
	(if (and (listp expA) (listp expB) (= (length expA) (length expB)))
		(if (equal (nth 0 expA) (nth 0 expB))
			(case (length expA)
				(0 t)
				(1 t)
				(otherwise (case (nth 0 expA)
					(= (equal expA expB))
					(≠ (equal expA expB))
					(< (equal expA expB))
					(> (equal expA expB))
					(<= (equal expA expB))
					(>= (equal expA expB))
					(and (set-equal (cdr expA) (cdr expB) :test #'expEq))
					(or (set-equal (cdr expA) (cdr expB) :test #'expEq))
					(otherwise (format t "WARNING: ~a is not a valid expresion! (expEq)~%" (nth 0 expA)))
				))
			)
		)
		(equal expA expB)))

;; FUNCTION: expImply
;;     Description: Is the logic operator implication bewteen expA & expB (expA -> expB)
;;     Input parameters:
;;         expA & expB: are valid logical expresions
;;     Returns: A valid logic expresion.
(defun expImply(expA expB)
	(if (nth 0 expA)
		(if (eq (nth 0 expA) 't)
			expB
			(if (nth 0 expB)
				(if (eq (nth 0 expB) 't)
					t
					(cons 'or (list (expNot expA) expB)))
				(expNot expA)))
		t))

;; FUNCTION: expEquivalent
;;     Description: Is the logic operator equivalent between expA & expB (expA <-> expB)
;;     Input parameters:
;;         expA & expB: are valid logical expresions
;;     Returns: A valid logic expresion
(defun expEquivalent(expA expB)
	(if (nth 0 expA)
		(if (eq (nth 0 expA) 't)
			expB
			(if (nth 0 expB)
				(if (eq (nth 0 expB) 't)
					expA
					(cons 'and (list (expImply expA expB) (expImply expB expA))))
				(expNot expA)))
		(expNot expB)))

;; FUNCTION: setPosition
;;     Description: Checks if logic expresion exp is in set set.
;;     Input parameters:
;;         set: a set of valid logic expresions
;;         exp: a valid logic expresion
;;     Returns: A number of the index where exp is or nil if is not in the set
(defun setPosition(set exp)
	(position exp set :test #'expEq))

;; FUNCTION: expFindSimple
;;     Description: Finds the simplest unique logic expresions and generates a set with them.
;;     Input parameters:
;;         exp: a valid logical expresion
;;     Returns: A set of valid simple logic expresions.
(defun expFindSimple(exp)
	(labels ((auxFindSimple(s e)
			(if (or (setPosition s e) (setPosition s (expNot e)))
				s
				(if (= (length e) 0)
					nil
					(if (typep (nth 0 e) 'boolean)
						(list (list (nth 0 e)))
						(case (nth 0 e)
							(= (cons e s))
							(≠ (cons (expNot e) s))
							(< (cons e s))
							(> (cons e s))
							(<= (cons (expNot e) s))
							(>= (cons (expNot e) s))
							(and (loop for i in (cdr e) do (setf s (auxFindSimple s i))) s)
							(or (loop for i in (cdr e) do (setf s (auxFindSimple s i))) s)
					 		(otherwise (format t "WARNING: ~a is not a valid expresion! (expFindSimple)~%" (nth 0 exp)) nil)))))))
	(auxFindSimple nil exp)))

;; FUNCTION: expPrint
;;     Description: Prints in a pretty way the expresion.
;;     Input parameters:
;;         exp: a valid logical expresion
;;         type: can be 'tree or 'math for direfent formats.
;;     Returns: nil
(defun expPrint(exp &optional (type 'tree))
	(labels (
		(print-level(n) (loop for i from 0 below n do (format t "  ")))
		(print-tree(e n)
			(print-level n)
			(case (nth 0 e)
				(and (format t "AND~%") (loop for i in (cdr e) do (print-tree i (+ n 1))))
				(or (format t "OR~%") (loop for i in (cdr e) do (print-tree i (+ n 1))))
				(otherwise (format t "~a~%" e))))
		(print-math(e n)
			(case (nth 0 e)
				(and (when (≠ n 0) (format t "["))
					(loop for i from 1 below (length e) do (print-math (nth i e) (+ n 1))
					when (≠ (+ i 1) (length e)) do (format t " AND "))
					(when (≠ n 0) (format t "]~%")))
				(or (when (≠ n 0) (format t "["))
					(loop for i from 1 below (length e) do (print-math (nth i e) (+ n 1))
					when (≠ (+ i 1) (length e)) do (format t " OR "))
					(when (≠ n 0) (format t "]~%")))
				(otherwise (format t "~a" e)))))
		(case type
			(tree (print-tree exp 0))
			(math (print-math exp 0) (format t "~%"))
			(otherwise
	 			(format t "WARNING: ~a is not defined, printed as a list instead! (expPrint)~%" type)
	 			(format t "~a~%" exp)))))

;; FUNCTION: expSubstitute
;;     Description: Substitutes the term for the value in the logical expresion exp
;;     Input parameters:
;;         exp: a valid logical expresion
;;         term: a simple logical expresion (of size 1)
;;         value: the new value that will be replaced (t or nil)
;;     Returns: An valid logical expresion with the substitutions been made
(defun expSubstitute(exp term value)
	(if (expEq exp term)
		value
		(if (listp exp)
			(case (nth 0 exp)
				(≠ (expSubstitute exp (expNot term) (not value)))
				(<= (expSubstitute exp (expNot term) (not value)))
				(>= (expSubstitute exp (expNot term) (not value)))
				(and (loop for i from 0 below (length exp) collect
					(expSubstitute (nth i exp) term value)))
				(or (loop for i from 0 below (length exp) collect
					(expSubstitute (nth i exp) term value)))
				(otherwise exp))
			exp)))

;; FUNCTION: setSubstitute
;;     Description: Is similar to expSubstitute but terms and values are now lists.
;;     Input parameters:
;;         exp: a valid logic expresion
;;         terms: a list of valid simple logic expresions (of size 1)
;;         values: the values that will replace the terms (it can be anything but usually t or nil)
;;     Returns: An valid logical expresion with the substitutions made
(defun setSubstitute(exp terms values)
	(if (= (length terms) (length values))
		(loop for i from 0 below (length terms) do
			(setf exp (expSubstitute exp (nth i terms) (nth i values))))
		(format t "WARNING: the number of terms and values are not the same! (expSubstitute)~%"))
	exp)

;; FUNCTION: expSimplify
;;     Description: Uses the Quine - McCluskey algorithm to simplify a logical expesión
;;     Input parameters:
;;         exp: a valid logical expresion
;;     Returns: A valid logic expresion
;;     <PENDING>
(defun expSimplify(exp)
	(let (set bin e (table (make-hash-table :test #'equal)) (used (make-hash-table :test #'equal)) group)
		(setf set (expFindSimple exp))
		(format t "expFindSimple ~a~%" set)
		(setf bin (binTable (length set)))
		(format t "binTable ~a~%" bin)
		(setf e (loop for i from 0 below (length bin) collect (setSubstitute exp set (nth i bin)) do (format t "~a~%" (setSubstitute exp set (nth i bin)))))
		(format t "----- Table -----~%")
		(loop for i from 0 below (length e) do (format t "~a - ~a~%" (nth i bin) (nth i e)))
		(setf bin (loop for i from 0 below (length bin) when (eval (nth i e)) do
			(setf (gethash (list i) table) (nth i bin))
			(setf (gethash (list i) used) nil))) ; truth table and used markers initialized
		; external loop for group creation
		(loop for iext downfrom (+ (length set) 1) to 1 do
			(format t "iext: ~a~%" iext)
			(setf group (loop for i from 0 below iext collect '()))
			(format t "----- Table -----~%")
			(loop for i being each hash-key of table do
				(format t "~a - ~a - Used: ~a~%" i (gethash i table) (gethash i used)))
			(loop for i being each hash-key of table when (not (gethash i used)) do
				(setf (nth (binCount (gethash i table)) group) (cons i (nth (binCount (gethash i table)) group))))
			(format t "----- Groups -----~%")
			(loop for i from 0 below (length group) do (format t "~a~%" (nth i group)))
			; (format t "----- Relations -----~%")
			(loop for i from 0 below (length group) do
				(loop for j from 0 below (length (nth i group)) do
					(loop for k from 0 below (length (nth (+ i 1) group)) do
						(if (= (binDifference (gethash (nth j (nth i group)) table) (gethash (nth k (nth (+ i 1) group)) table)) 1)
							(progn
								; (format t "1 Diff:~%~a~%~a~%" (gethash (nth j (nth i group)) table) (gethash (nth k (nth (+ i 1) group)) table))
								; Generate new init on table and bin
								(setf (gethash (stable-sort (concatenate 'list (nth j (nth i group)) (nth k (nth (+ i 1) group))) #'<) table)
									(binJoin (gethash (nth j (nth i group)) table) (gethash (nth k (nth (+ i 1) group)) table)))
								(setf (gethash (stable-sort (concatenate 'list (nth j (nth i group)) (nth k (nth (+ i 1) group))) #'<) used)
									nil)
								; set already used
								(setf (gethash (nth j (nth i group)) used) t)
								(setf (gethash (nth k (nth (+ i 1) group)) used) t))))
					(if (gethash (nth j (nth i group)) used)
						(progn ; remove already used
							(remhash (nth j (nth i group)) used)
							(remhash (nth j (nth i group)) table))
						; set primes not to be used again
						(setf (gethash (nth j (nth i group)) used) t)))))
		; The remaining elements on table are primes
		(format t "----- Prime -----~%")
		(loop for i being each hash-key of table do
			(format t "~a - ~a - ~a~%" i (gethash i table) (gethash i used)))
		(expBuild (loop for i being each hash-key of table collect (expBuild set (gethash i table)))
			(loop for i being each hash-key of table collect t) 'or)))

;; FUNCTION: expSize
;;     Description: Counts every single simple expresion in the complex expresion exp
;;                  even if they are repeated or negated
;;     Input parameters:
;;         exp: a valid logical expresion
;;     Returns: A number with the size of the expresion (or nil if its not a valid expresion)
(defun expSize(exp)
	(if (typep (nth 0 exp) 'boolean)
		1
		(case (nth 0 exp)
			(= 1)
			(≠ 1)
			(< 1)
			(> 1)
			(<= 1)
			(>= 1)
			(and (if (= (length exp) 3)
				(+ (expSize (nth 1 exp)) (expSize (nth 2 exp)))
				(+ (expSize (nth 1 exp)) (expSize (cons 'and (cddr exp))))))
			(or (if (= (length exp) 3)
				(+ (expSize (nth 1 exp)) (expSize (nth 2 exp)))
				(+ (expSize (nth 1 exp)) (expSize (cons 'or (cddr exp))))))
		 	(otherwise (format t "WARNING: ~a is not a valid expresion! (expSize)~%" exp)))))

;; FUNCTION: expValidate
;;     Description: Validates if exp is a valid logical expresions
;;     Input parameters:
;;         exp: a valid logical expresion
;;     Returns: t or nil
(defun expValidate(exp)
	(if (listp exp)
		(if (typep (nth 0 exp) 'boolean)
			t
			(case (nth 0 exp)
				(= (= (length exp) 3))
				(≠ (= (length exp) 3))
				(< (= (length exp) 3))
				(> (= (length exp) 3))
				(<= (= (length exp) 3))
				(>= (= (length exp) 3))
				(and (listAnd (loop for i in (cdr exp) collect (expValidate i))))
				(or (listAnd (loop for i in (cdr exp) collect (expValidate i))))
		 		(otherwise nil)))
		nil))

;; FUNCTION: setAddExp
;;     Description: Adds the logic expresion exp in the set set unless it is already in it
;;     Input parameters:
;;         set: a set of valid logic expresions
;;         exp: a valid logical expresion
;;     Returns: A set of valid logic expresions
(defun setAddElem(set exp)
	(if exp
		(if (setPosition set exp)
			set
			(if (car set)
				(cons exp set)
				(list exp)))
		set))

;; FUNCTION: setCombinations
;;     Description: Generate a set of logic expresions with the disyunctions of combinations of original set s
;;     Input parameters:
;;         set: a set of valid logic expresions
;;         operator: the conector for the logic expresions combinations
;;     Returns: A set of valid logic expresions
(defun setCombinations(set &optional (operator 'and))
	(let (res)
		(loop for exp in set do
			(loop for r in res do
				(setf res
					(concatenate 'list res
						(if (eq (nth 0 r) operator)
							(list (concatenate 'list r (list exp)))
							(list (list operator r exp))))))
			(setf res (cons exp res)))
		res))

;; FUNCTION: setEq
;;     Description: Checks if a set of logical expresions is equal to another using #'expEq
;;     Input parameters:
;;         set1 & set2: are sets of valid logic expresions
;;     Returns: t or nil
(defun setEq(set1 set2)
	(set-equal set1 set2 :test #'expEq))

(provide "pkgLogic")
