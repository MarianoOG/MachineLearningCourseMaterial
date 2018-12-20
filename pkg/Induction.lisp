;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                                                                                      ;;;;
;;;; Artificial Intelligence Laboratoy, CIC-IPN                                           ;;;;
;;;; Professor: Dr. Salvador Godoy Calderón                                               ;;;;
;;;; Author: Mariano Orozco García                                                        ;;;;
;;;;                                                                                      ;;;;
;;;; pkgInduction.lisp: Functions to work with induction algorithms.                      ;;;;
;;;;                                                                                      ;;;;
;;;; Updated: September, 2016                                                             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ------------------------------ Package definition ------------------------------

(load (concatenate 'string cl-user::*MLC_directory* "/pkg/Auxiliar.lisp"))
(load (concatenate 'string cl-user::*MLC_directory* "/pkg/Logic.lisp"))

; <PENDING> :exSubtable
(defpackage :pkgInduction
	(:use
		:common-lisp
		:pkgAuxiliar
		:pkgLogic)
	(:export
		:exAdd
		:exCount
		:exEval
		:exFindClass
		:exFormat
		:exPositions
		:exPrint
		:exRandSelect
		:exRead
		:exRemove
		:exSelect
		:exToSet
		:exValidate
		:exWrite))

;;; ------------------------------ Package Description ------------------------------

;; Los ejemplos serán extraidos de archivos de texto externos con extension .cet (class example table) donde
;; el simbolo # será usado para comentarios y secciones:
;;     	# Class indica el nombre de la clase seguida de un guion y las etiquetas de las clases separadas por espacios.
;;     	# Headers indica que la siguiente linea son los valores de los encabezados separados por un espacio.
;;     	# Domains indica los dominios de cada comluna acomodados en orden que aparecen en headers.
;;     	# Examples (clase) indica que cada una de lineas siguientes serán los ejemplos de dicha clase.
;;      # End es necesario al final para realizar el conteo de los ultimos ejemplos.

;; La tablas hash contiene los ejemplos del problema, leidos del archivo externo:
;; 		En la llave 'class se encuentra el nombre de la clase.
;; 		En la llave 'headers se encuentra una lista con los encabezados.
;; 		En la llave 'length se encuentra el tamaño de la lista de los encabezados.
;; 		En la llave 'size se encuentra el numero de ejemplos que contiene la tabla.
;; 		En la llave 'count se guarda una lista con el conteo por clase de los ejemplos.
;; 		En las llaves '[columna] donde columna es el nombre de la cabecera de dicha columna, se encuentran los dominios de cada atributo.
;; 		En llaves con celdas de construccion (Clase . #Ejemplo) estan acomodados los ejemplos en el mismo orden que los encabezados.
;;			El numero de ejemplo se reinicia por clase haciendo cuentas independientes.

(in-package :pkgInduction)

;;; ------------------------------ Example Interaction ------------------------------

;; La funcion exFindClass encuentra el numero que corresponde a dicha función
(defun exFindClass (table class)
	(setf class (typecase class
			(number (if (and (>= class 0) (< class (length (gethash 'count table)))) class nil))
			(symbol
				(position (string class)
					(gethash (intern (gethash 'class table)) table) :test #'string=))
			(otherwise
				(position (string-upcase class)
					(gethash (intern (gethash 'class table)) table) :test #'string=))))
	class)

;; La funcion exFormat convierte los ejemplos a mayusculas y listas de cadenas, que es el formato utilizado.
(defun exFormat(ex)
	(loop for i from 0 below (length ex) do
		(setf (nth i ex) (typecase (nth i ex)
			(number (write-to-string (nth i ex)))
			(symbol (string (nth i ex)))
			(otherwise (nth i ex)))))
	(mapcar #'string-upcase ex))

;; La funcion exValidate comprueba si el ejemplo nuevo esta dentro del dominio en cada uno de sus parametros
(defun exValidate (table ex)
	(setf ex (exFormat ex))
	(let ((a t))
		(if (eq (length ex) (gethash 'length table))
			(loop for i from 0 below (gethash 'length table) do
				(setf a (and a (position (nth i ex) (gethash (intern (nth i (gethash 'headers table))) table) :test #'string=))))
			(setf a nil))
		(numberp a)))
; <pte> agregar * y -

;; La funcion exAdd agrega un ejemplo ex en la posicion n a la tabla table.
;; Se puede elegir un valor donde agregar el nuevo ejemplo, con esto se modifica el anterior.
(defun exAdd (table class ex &optional n)
	(setf class (exFindClass table class))
	(setf ex (exFormat ex))
	(if (and class (exValidate table ex) (eq (gethash 'length table) (length ex)))
		(if (and n (>= n 0) (< n (nth class (gethash 'count table))))
			(progn
				(setf (gethash (cons class n) table) ex))
			(progn
				(setf (gethash (cons class (nth class (gethash 'count table))) table) ex)
				(setf (nth class (gethash 'count table)) (+ (nth class (gethash 'count table)) 1))
				(setf (gethash 'size table) (+ (gethash 'size table) 1))))
		(format t "WARNING: request couldn't be finished!~%"))
	table)

;; La funcion exEval evalua la expresion logica exp en el ejemplo n de la clase class de la tabla table.
(defun exEval(exp table class n)
	(setf class (exFindClass table class))
	(if class
		(if (typep (nth 0 exp) 'boolean)
			(nth 0 exp)
			(case (nth 0 exp)
				(= (string= (nth 2 exp)
						(nth (position (nth 1 exp) (gethash 'headers table) :test #'string=) (gethash (cons class n) table))))
				(!= (not (exEval (expNot exp) table class n)))
				(< nil) ; <pte>
				(> nil) ; <pte>
				(<= nil) ; <pte>
				(>= nil) ; <pte>
				(and (and (exEval (nth 1 exp) table class n)
					(if (= (length exp) 3)
					 	(exEval (nth 2 exp) table class n)
					 	(exEval (cons 'and (cddr exp)) table class n))))
				(or (or (exEval (nth 1 exp) table class n)
					(if (= (length exp) 3)
					 	(exEval (nth 2 exp) table class n)
					 	(exEval (cons 'or (cddr exp)) table class n))))
				(otherwise (format t "WARNING: ~a is not a valid expresion!~%" exp) nil)))
		(format t "WARNING: Class doesn't exist in the table!~%")))
; <pte> agregar simbolos especiales: *, cualquiera o no impota y - sin infomacion.
; <pte> completar para los casos restantes.

;; La funcion exCount contará cuantos elementos existen en una tabla table que cumplen con la expresion logica exp
(defun exCount(exp table &optional (class nil))
	(setf class (exFindClass table class))
	(let ((a 0))
		(if class
			(loop for i from 0 below (nth class (gethash 'count table)) do
					(if (exEval exp table class i) (setf a (+ a 1))))
			; (loop for c from 0 below (length (gethash 'count table)) do
			; 	(if (> c 0) (cons a '(0)))
			; 	(loop for i from 0 below (nth c (gethash 'count table)) do
			; 		(if (exEval exp table c i) (setf (nth c a) (+ (nth c a) 1)))))
		)
		a))

;; La funcion exPositions muestra las posiciones en las cuales existen los elemntos que cumplen con la expresion exp
(defun exPositions(exp table &optional (class nil))
	(setf class (exFindClass table class))
	(let (a)
		(if class
			(setf a (loop for i from 0 below (nth class (gethash 'count table))
				collect (exEval exp table class i))))
			; (loop for c from 0 below (length (gethash 'count table)) do
			; 	(if (> c 0) (cons a '(0)))
			; 	(loop for i from 0 below (nth c (gethash 'count table)) do
			; 		(if (exEval exp table c i) (setf (nth c a) (+ (nth c a) 1)))))
		a))
	; (if class
	; 	(loop for i from 0 below (gethash 'size table) if (exEval exp table i) collect i))
	; )

;; La funcion exprint imprime la tabla y su informacion.
(defun exPrint (table &key (class t) (headers t) (examples t))
	(format t "*********************************************~%")
	(if class
		(format t "CLASS - ~a: ~a~%" (gethash 'class table)
			(gethash (intern (gethash 'class table)) table)))
	(if headers
		(progn
			(format t "~a - HEADERS: (DOMAINS)~%" (gethash 'length table))
			(loop for i from 0 below (gethash 'length table) do
				(format t "  ~a - ~a: ~a~%" i (nth i (gethash 'headers table))
				(gethash (intern (nth i (gethash 'headers table))) table)))))
	(if examples
		(progn
			(format t "~a - EXAMPLES:~%" (gethash 'size table))
			(loop for i from 0 below (length (gethash 'count table)) do
				(format t "  ~a - ~a~%" (nth i (gethash 'count table))
					(nth i (gethash (intern (gethash 'class table)) table)))
				(loop for j from 0 below (nth i (gethash 'count table)) do
					(format t "    ~a: ~a~%" j (gethash (cons i j) table))))))
	(format t "*********************************************~%"))
; <pte> Agregar opcionales para ocultar secciones.

;; La funcion exRandSelect selecciona un elemento de la clase class al azar
(defun exRandSelect(table class &optional filter)
	(setf class (exFindClass table class))
	(if (not filter) (setf filter (loop for i from 0 below (nth class (gethash 'count table)) collect t)))
	(let (a)
		(loop for i from 0 do (setf a (random (nth class (gethash 'count table))))
		 	until (nth a filter))
		(if class
			(gethash (cons class a) table)
			(format t "WARNING: Class doesn't exist in the table!~%"))))

;; La funcion exRead leerá un archivo externo file, regresara la tabla de ejemplos generada.
(defun exRead (file)
	(let ((table (make-hash-table :test #'equal)) (a -1) (b -1) (c 0) l (op -1))
		(with-open-file (i file)
	    	(loop for line = (read-line i nil) while line do
				(if (not (string= line ""))
					(progn
						(setf l (split-str (string-upcase line)))
						(if (string= (nth 0 l) "#")
							(progn
								(if (= op 3) (setf (gethash 'count table) (cons (+ a 1) (gethash 'count table))))
								(setf op
									(cond
										((string= (nth 1 l) "CLASS") 0)
										((string= (nth 1 l) "HEADERS") 1)
										((string= (nth 1 l) "DOMAINS") 2)
										((string= (nth 1 l) "EXAMPLES") 3)
										(t -1)))
								(setf a -1)
								(if (= op 3) (setf b
									(position (nth 2 l) (gethash (intern (gethash 'class table)) table) :test #'string=))))
							(progn
								(setf a (+ a 1))
								(case op
									(0  (setf (gethash 'class table) (nth 0 l))
										(setf (gethash (intern (gethash 'class table)) table) (cddr l)))
									(1  (setf (gethash 'headers table) l)
										(setf (gethash 'length table) (length l)))
									(2  (setf (gethash (intern (nth a (gethash 'headers table))) table) l))
									(3  (if (exValidate table l)
											(progn
												(setf (gethash (cons b a) table) l)
												(setf c (+ c 1)))
											(progn
												(setf a (- a 1))
											 	(format t "WARNING: Line ~a was ignored~%" line)))))))))))
		(setf (gethash 'size table) c)
		(setf (gethash 'count table) (reverse (gethash 'count table)))
		table))
; <pte> Dominios continuos

;; La funcion exRemove elimina el ejemplo de la clase class de la posicion n de la tabla table.
;; Regresa la misma tabla si no es posible retirar esa posicion y la nueva tabla si lo es.
(defun exRemove (table class n)
	(setf class (exFindClass table class))
	(if class
		(progn
			(if (or (< n 0) (>= n (nth class (gethash 'count table))))
				(format t "WARNING: invalid n number, the same table is returned!~%")
				(progn
					(remhash (cons class n) table)
					(loop for i from n below (nth class (gethash 'count table)) do
						(setf (gethash (cons class i) table) (gethash (cons class (+ i 1)) table)))
					(remhash (- (nth class (gethash 'count table)) 1) table)
					(setf (gethash 'size table) (- (gethash 'size table) 1))
					(setf (nth class (gethash 'count table)) (- (nth class (gethash 'count table)) 1)))))
		(format t "WARNING: invalid class number, the same table is returned!~%"))
	table)

;; La funcion exSelect selecciona el elemento n de la clase class
(defun exSelect (table class n)
	(setf class (exFindClass table class))
	(if (and class (>= n 0) (< n (nth class (gethash 'count table))))
		(gethash (cons class n) table)
		(format t "WARNING: Example ~a in class doesn't exist in the table!~%" n)))

;; La funcion exSubtable crea una subtabla que cumple con alguna condicion expresada con una
; (defun exSubtable () ) ; <pte>

;; La funcion exToSet convierte un ejemplo en un conjunto de expesiones logicas
(defun exToSet(table ex)
	(loop for i from 0 below (gethash 'length table) collect
		(list '= (nth i (gethash 'headers table)) (nth i ex))))

;; La funcion exWrite escribirá sobre un archivo los datos de una tabla de ejemplos y sus respectivos encabezados.
(defun exWrite (file table)
	(with-open-file (out file
						:direction :output
						:if-exists :supersede
						:if-does-not-exist :create)
		(format out "# CLASS~%~a - " (gethash 'class table))
		(loop for i from 0 below (length (gethash (intern (gethash 'class table)) table)) do
			(if (eq i (- (length (gethash (intern (gethash 'class table)) table)) 1))
				(format out "~a~%" (nth i (gethash (intern (gethash 'class table)) table)))
				(format out "~a " (nth i (gethash (intern (gethash 'class table)) table)))))
		(format out "~%# HEADERS~%")
  		(loop for i from 0 below (gethash 'length table) do
  			(if (eq i (- (gethash 'length table) 1))
  				(format out "~a~%" (nth i (gethash 'headers table)))
  				(format out "~a " (nth i (gethash 'headers table)))))
  		(format out "~%# DOMAINS~%")
		(loop for j from 0 below (gethash 'length table) do
			(loop for i from 0 below (length (gethash (intern (nth j (gethash 'headers table))) table)) do
				(if (eq i (- (length (gethash (intern (nth j (gethash 'headers table))) table)) 1))
					(format out "~a~%" (nth i (gethash (intern (nth j (gethash 'headers table))) table)))
					(format out "~a " (nth i (gethash (intern (nth j (gethash 'headers table))) table))))))
  		(loop for j from 0 below (length (gethash 'count table)) do
  			(format out "~%# EXAMPLES ~a~%" (nth j (gethash (intern (gethash 'class table)) table)))
  			(loop for i from 0 below (nth j (gethash 'count table)) do
  				(loop for k from 0 below (gethash 'length table) do
  					(if (eq k (- (gethash 'length table) 1))
  						(format out "~a~%" (nth k (gethash (cons j i) table)))
  						(format out "~a " (nth k (gethash (cons j i) table)))))))
  		(format out "~%# END~%")))
; <pte> Dominios continuos

(provide "pkgInduction")
