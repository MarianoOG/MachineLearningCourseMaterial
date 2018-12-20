;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                                                                                      ;;;;
;;;; Artificial Intelligence Laboratoy, CIC-IPN                                           ;;;;
;;;; Professor: Dr. Salvador Godoy Calderón                                               ;;;;
;;;; Author: Mariano Orozco García                                                        ;;;;
;;;;                                                                                      ;;;;
;;;; Auxiliar.lisp: Auxiliar functions to work with other packages.                       ;;;;
;;;;                                                                                      ;;;;
;;;; Updated: September, 2016                                                                ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ------------------------------ Package definition ------------------------------

(defpackage :pkgAuxiliar
	(:use
		:common-lisp)
	(:export
		:split-str
		:listAnd
		:listMax
		:listOr
		:mapAnd
		:mapOr
		:pick-random-element
		:set-equal
		:unordered-equal
		:≠))

(in-package :pkgAuxiliar)

;;; ------------------------------ Strings ------------------------------

;; FUNCTION: split-str
;;     Description: Separates strings using a separator
;;     Input parameters:
;;         string: its the string to be separated
;;         separator: its is the string that will be used to separate
;;     Returns: List of strings with the elements
;;     Extracted from: https://gist.github.com/siguremon/1174988
(defun split-str (string &optional (separator " "))
	(labels ((split-str-1 (string &optional (separator " ") (r nil))
  		(let ((n (position separator string
		     :from-end t
		     :test #'(lambda (x y)
			       (find y x :test #'string=)))))
    		(if n
				(split-str-1 (subseq string 0 n) separator (cons (subseq string (1+ n)) r))
      			(cons string r)))))
		(split-str-1 string separator)))


;;; ------------------------------ Auxiliar and & or functions ------------------------------

;; FUNCTION: listAnd
;;     Description: Evaluates boolean and operator along each couple of the list l of boolean expresions.
;;     Input parameters:
;;         l: a list of boolean expresions (t or nil)
;;     Returns: A boolean expresion.
(defun listAnd(l)
	(case (length l)
		(1 (nth 0 l))
		(2 (and (nth 0 l) (nth 1 l)))
		(otherwise (and (nth 0 l) (listAnd (cdr l))))))

;; FUNCTION: listMax
;;     Description: Finds the maximum number on a list
;;     Input parameters:
;;         l: a list of numbers
;;     Returns: The maximum of a list of numbers
(defun listMax(l)
	(apply #'max l))

;; FUNCTION: listOr
;;     Description: Evaluates boolean or operator along each couple of the list l of boolean expresions.
;;     Input parameters:
;;         l: a list of boolean expresions (t or nil)
;;     Returns: A boolean expresion.
(defun listOr(l)
	(case (length l)
		(1 (nth 0 l))
		(2 (or (nth 0 l) (nth 1 l)))
		(otherwise (or (nth 0 l) (listOr (cdr l))))))

;; FUNCTION: mapAnd
;;     Description: Maps boolean and operator between two lists a and b of boolean expresions.
;;     Input parameters:
;;         a & b: a list of boolean expresions (t or nil)
;;     Returns: A boolean expresion.
(defun mapAnd(a b)
	(labels ((auxAnd(x y) (and x y)))
		(mapcar #'auxAnd a b)))

;; FUNCTION: mapOr
;;     Description: Maps boolean or operator between two lists a and b of boolean expresions.
;;     Input parameters:
;;         a & b: a list of boolean expresions (t or nil)
;;     Returns: A boolean expresion.
(defun mapOr(a b)
	(labels ((auxOr(x y) (or x y)))
		(mapcar #'auxOr a b)))

;; FUNCTION: pick-random-element
;;     Description: picks a random element from a list
;;     Input parameters: l - a list of elements
;;     Returns: Element of a list
(defun pick-random-element(l)
	(nth (random (length l)) l))

;;; ------------------------------ Equality functions ------------------------------

;; FUNCTION: set-equal
;;     Description: equal operator for sets
;;     Input parameters:
;;         l1 & l2: two lists
;;         test: the boolean function for equality to be used
;;     Returns: true if every element of l1 is an element of l2 and vice versa or nil otherwise
;;     Extracted from: lisp-unit
(defun set-equal (l1 l2 &key (test #'equal))
  (and (listp l1)
       (listp l2)
       (subsetp l1 l2 :test test)
       (subsetp l2 l1 :test test)))

;; FUNCTION: unordered-equal
;;     Description: unordered equal operator between two lists
;;     Input parameters:
;;         l1 & l2: two lists
;;         test: the boolean function for equality to be used
;;     Returns: true is l1 is a permuation of l2 or nil otherwise.
;;     Extracted from: lisp-unit
(defun unordered-equal(l1 l2)
  (and (listp l1)
       (listp l2)
       (= (length l1) (length l2))
       (every #'(lambda (x1) (= (count x1 l1) (count x1 l2))) l1)))

;;; ------------------------------ Numeric inequality ------------------------------

;; FUNCTION: ≠
;;     Description: Is the inequality operator between numbers
;;     Input parameters:
;;         x & y: two numbers
;;     Returns: T or nil
(defun ≠(x y)
	(not (= x y)))

(provide "pkgAuxiliar")
