;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: RACER; Base: 10 -*-

;;; Copyright (c) 1998-2014, 
;;; Volker Haarslev, Ralf Moeller, Michael Wessel.  
;;; All rights reserved.

;;; Racer is distributed under the following BSD 3-clause license

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are
;;; met:

;;; Redistributions of source code must retain the above copyright notice,
;;; this list of conditions and the following disclaimer.

;;; Redistributions in binary form must reproduce the above copyright
;;; notice, this list of conditions and the following disclaimer in the
;;; documentation and/or other materials provided with the distribution.

;;; Neither the name Racer nor the names of its contributors may be used
;;; to endorse or promote products derived from this software without
;;; specific prior written permission.

;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
;;; CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,
;;; BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
;;; FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL
;;; VOLKER HAARSLEV, RALF MOELLER, NOR MICHAEL WESSEL BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES, LOSS OF USE, DATA, OR PROFITS, OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
;;; IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
;;; OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
;;; ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :racer)

(defvar *cd-simplex-verbose* nil)

(defun copy-solver-state (old-state &optional (row-dim nil) (col-dim nil))
  "Erzeugt einen neuen solver-state mit den Dimensionen row-dim (Anzahl der Zeilen)
   und col-dim (Anzahl der Spalten). Dieser uebernimmt alle Werte aus old-state."
  #+:debug (assert (or (null (or row-dim col-dim)) (and row-dim col-dim)))
  (when old-state
    (let ((new-state (copy-solver-state-internal old-state)))
      (if row-dim
        (progn
          (if (eql (array-dimension (solver-state-basic-vars new-state) 0) row-dim)
            (setf (solver-state-basic-vars new-state)
                  (copy-seq (solver-state-basic-vars old-state)))
            (let ((array (make-array row-dim :initial-element nil)))
              (setf (subseq array 0 (array-dimension (solver-state-basic-vars new-state) 0))
                    (copy-seq (solver-state-basic-vars old-state)))
              (setf (solver-state-basic-vars new-state) array)))
          (if (eql (array-dimension (solver-state-nonbasic-vars new-state) 0) col-dim)
            (setf (solver-state-nonbasic-vars new-state)
                  (copy-seq (solver-state-nonbasic-vars old-state)))
            (let ((array (make-array col-dim :initial-element nil)))
              (setf (subseq array 0 (array-dimension (solver-state-nonbasic-vars new-state) 0))
                    (copy-seq (solver-state-nonbasic-vars old-state)))
              (setf (solver-state-nonbasic-vars new-state) array)))
          (if (and (eql (sparse-array-dimension (solver-state-matrix new-state) 0) row-dim)
                   (eql (sparse-array-dimension (solver-state-matrix new-state) 1) col-dim))
            (setf (solver-state-matrix new-state)
                  (copy-sparse-array (solver-state-matrix old-state)))
            (setf (solver-state-matrix new-state)
                  (copy-sparse-array (solver-state-matrix old-state) (list row-dim col-dim))))
          (if (eql (array-dimension (solver-state-constant-col new-state) 0) row-dim)
            (setf (solver-state-constant-col new-state)
                  (copy-seq (solver-state-constant-col old-state)))
            (let ((array (make-array row-dim :initial-element 0)))
              (setf (subseq array 0 (array-dimension (solver-state-constant-col new-state) 0))
                    (copy-seq (solver-state-constant-col old-state)))
              (setf (solver-state-constant-col new-state) array)))
          (when (solver-state-symbolic-col new-state)
            (if (eql (array-dimension (solver-state-symbolic-col new-state) 0) row-dim)
              (setf (solver-state-symbolic-col new-state)
                    (copy-seq (solver-state-symbolic-col old-state)))
              (let ((array (make-array row-dim :initial-element 0)))
                (setf (subseq array 0 (array-dimension (solver-state-symbolic-col new-state) 0))
                      (copy-seq (solver-state-symbolic-col old-state)))
                (setf (solver-state-symbolic-col new-state) array)))))
        (progn
          (setf (solver-state-basic-vars new-state)
                (copy-seq (solver-state-basic-vars old-state)))
          (setf (solver-state-nonbasic-vars new-state)
                (copy-seq (solver-state-nonbasic-vars old-state)))
          (setf (solver-state-matrix new-state)
                (copy-sparse-array (solver-state-matrix old-state)))
          (setf (solver-state-constant-col new-state)
                (copy-seq (solver-state-constant-col old-state)))
          (setf (solver-state-symbolic-col new-state)
                (copy-seq (solver-state-symbolic-col old-state)))))
      (when (solver-state-string-var-values-mapping old-state)
        (let* ((var-values-mapping (solver-state-string-var-values-mapping old-state))
               (new-var-values-mapping (make-hash-table :size (hash-table-count var-values-mapping))))
          (loop for entry being the hash-values of var-values-mapping using (hash-key var) do
                      (setf (gethash var new-var-values-mapping) (copy-var-entry entry)))
          (setf (solver-state-string-var-values-mapping new-state)              
                new-var-values-mapping)))
      new-state)))

(defun add-constraint (state row-dim col-dim new-basic-var new-nonbasic-vars 
                                new-slack-var new-free-vars new-coeffs new-rs new-symbolic)
  (setf (solver-state-free-vars state) 
        (append new-free-vars (solver-state-free-vars state)))
  (push new-slack-var (solver-state-slack-vars state))
  (if (eql (array-dimension (solver-state-basic-vars state) 0) row-dim)
    (setf (aref (solver-state-basic-vars state) (1- row-dim)) new-basic-var)
    (setf (solver-state-basic-vars state)
          (adjust-array (solver-state-basic-vars state) row-dim :initial-element new-basic-var)))
  (unless (eql (array-dimension (solver-state-nonbasic-vars state) 0) col-dim)
    (setf (solver-state-nonbasic-vars state)
          (adjust-array (solver-state-nonbasic-vars state) col-dim :initial-element nil)))
  (loop for new-nbv in new-nonbasic-vars
        and col from (- (array-dimension (solver-state-nonbasic-vars state) 0)
                        (length new-nonbasic-vars))
        do (setf (aref (solver-state-nonbasic-vars state) col) new-nbv))
  (unless (and (eql (sparse-array-dimension (solver-state-matrix state) 0) row-dim)
               (eql (sparse-array-dimension (solver-state-matrix state) 1) col-dim))
    (setf (solver-state-matrix state)
          (adjust-sparse-array (solver-state-matrix state) (list row-dim col-dim))))
  (loop for col from 0 below col-dim 
        and coeff in new-coeffs do
        (setf (saref (solver-state-matrix state) (1- row-dim) col) coeff))
  (if (eql (array-dimension (solver-state-constant-col state) 0) row-dim)
    (setf (aref (solver-state-constant-col state) (1- row-dim)) new-rs)
    (setf (solver-state-constant-col state)
          (adjust-array (solver-state-constant-col state)
                        row-dim
                        :initial-element new-rs)))
  (if (solver-state-symbolic-col state)
    (if (eql (array-dimension (solver-state-symbolic-col state) 0) row-dim)
      (when new-symbolic
        (setf (aref (solver-state-symbolic-col state) (1- row-dim)) new-symbolic))
      (setf (solver-state-symbolic-col state)
            (adjust-array (solver-state-symbolic-col state)
                          row-dim
                          :initial-element (or new-symbolic 0))))
    (when new-symbolic
      (setf (solver-state-symbolic-col state) (make-array row-dim :initial-element 0))
      (setf (aref (solver-state-symbolic-col state) (1- row-dim)) new-symbolic)))
  state)

(defun compute-cd-vars (vars coeffs var-coeff)
  (loop for v in vars
        and index from 0 
        for value = (aref coeffs index)
        unless (zerop value)
        collect (progn
                  (setf (gethash v var-coeff) value)
                  v)))

(defun replace-vars (basic-vars
                       var-coeff
                       nonbasic-vars
                       matrix
                       rs
                       constant-col
                       symbolic-col
                       constr-symbolic)
  ;(print (mapcar #'length (list basic-vars nonbasic-vars)))
  (loop for bv across basic-vars
        and row from 0 do
        (let ((fac (gethash bv var-coeff)))
          (when fac
            (remhash bv var-coeff)
            (loop for nbv across nonbasic-vars
                  and col from 0 do
                  (let ((old (gethash nbv var-coeff))
                        (elem (saref matrix row col)))
                    (if old
                      (unless (zerop elem)
                        (decf (gethash nbv var-coeff) (* fac elem)))
                      (setf (gethash nbv var-coeff) (- (* fac elem))))))
            (decf rs (* fac (aref constant-col row)))
            (when symbolic-col
              (decf constr-symbolic (* fac (aref symbolic-col row)))))))
  (values rs constr-symbolic))


(defun added-cd-predicate-satisfiable-p (state
                                               pred
                                               vars
                                               &optional
                                               (copy-concrete-domain-state nil)
                                               (new-or-dependencies nil))
  #+:debug (assert (linear-predicate-p pred))
  (let* ((state (or state (make-solver-state)))
         (coeffs (predicate-coefficients pred))
         (rs (predicate-right-side pred))
         (op (predicate-operator pred))
         (constr-symbolic 0)
         (var-coeff (make-hash-table))	
         (basic-vars (solver-state-basic-vars state))
         (nonbasic-vars (solver-state-nonbasic-vars state))
         (free-vars (solver-state-free-vars state))
         (slack-vars (solver-state-slack-vars state))
         (matrix (solver-state-matrix state))
         (constant-col (solver-state-constant-col state))
         (symbolic-col (solver-state-symbolic-col state)))
    (when (eql op '<)
      (setf constr-symbolic -1))
    ; zu jeder Variable, die in constraint vorkommt, den Wert
    ; des Koeffizienten in var-coeff eintragen falls
    ; der Wert ungleich Null ist, sonst die Variable aus
    ; vars entfernen
    (setf vars (compute-cd-vars vars coeffs var-coeff))
    ; sind in vars und damit auch in var-coeff Basisvariablen enthalten? Falls ja ersetzen: 
    (multiple-value-bind (rs constr-symbolic)
                         (replace-vars basic-vars
                                       var-coeff
                                       nonbasic-vars
                                       matrix
                                       rs
                                       constant-col
                                       symbolic-col
                                       constr-symbolic)
      (let* ((new-free-vars 
              (loop for var in vars
                    unless (or (member var free-vars)
                               (member var slack-vars))
                    collect var))
             (constr-nonbasic-vars nil)
             new-basic-var
             (new-nonbasic-vars new-free-vars) 
             (new-slack-var (gensym))
             (old-row-dim (array-dimension basic-vars 0))
             (old-col-dim (array-dimension nonbasic-vars 0))
             new-coeffs
             new-state)
        ; In constr-nonbasic-vars alle Variablen einsammeln, die
        ; im neuen constraint einen von Null verschiedenen Koeffizienten haben:
        (loop for var being the hash-key of var-coeff using (hash-value val) do
              (unless (zerop val)
                (push var constr-nonbasic-vars)))
        (cond ((consp new-free-vars) ; ==> es gibt neue freie Variable 
               (setf new-basic-var (pop new-nonbasic-vars))
               (push new-slack-var new-nonbasic-vars)
               (setf (gethash new-slack-var var-coeff) 1)
               (setf constr-nonbasic-vars (remove new-basic-var constr-nonbasic-vars))
               (push new-slack-var constr-nonbasic-vars)
               ; gesamten constraint durch den Koeffizienten der neuen 
               ; Basisvariable dividieren:
               (let ((fac (/ 1 (gethash new-basic-var var-coeff))))
                 (dolist (c-var constr-nonbasic-vars)
                   (setf (gethash c-var var-coeff)
                         (* fac (gethash c-var var-coeff)))) 
                 (setf rs (* fac rs))
                 (setf constr-symbolic (* fac constr-symbolic)))
               (setf new-coeffs (nconc
                                 (loop for nbv across nonbasic-vars 
                                       collect
                                       (if (member nbv constr-nonbasic-vars)
                                         (gethash nbv var-coeff)
                                         0))
                                 (loop for nbv in new-nonbasic-vars
                                       collect (gethash nbv var-coeff))))
               (setf new-state
                     (add-constraint (if copy-concrete-domain-state 
                                       (copy-solver-state state
                                                          (1+ old-row-dim)
                                                          (+ old-col-dim (length new-nonbasic-vars)))
                                       state)
                                     (1+ old-row-dim)
                                     (+ old-col-dim (length new-nonbasic-vars))
                                     new-basic-var
                                     new-nonbasic-vars
                                     new-slack-var
                                     new-free-vars
                                     new-coeffs
                                     rs
                                     (if (zerop constr-symbolic)
                                       nil
                                       constr-symbolic))))
              ((and (lists-disjoint-p constr-nonbasic-vars free-vars) ; ==> enthaelt nur slack-vars
                    (or (plusp rs) ; Konstante ist positiv
                        (and (zerop rs) (<= 0 constr-symbolic)))) 
               (setf new-basic-var new-slack-var)
               (setf new-coeffs (loop for nbv across nonbasic-vars 
                                      collect
                                      (if (member nbv constr-nonbasic-vars)
                                        (gethash nbv var-coeff)
                                        0)))
               (setf new-state
                     (add-constraint (if copy-concrete-domain-state 
                                       (copy-solver-state state
                                                          (1+ old-row-dim)
                                                          (+ old-col-dim (length new-nonbasic-vars)))
                                       state)
                                     (1+ old-row-dim)
                                     (+ old-col-dim (length new-nonbasic-vars))
                                     new-basic-var
                                     new-nonbasic-vars
                                     new-slack-var
                                     new-free-vars
                                     new-coeffs
                                     rs
                                     (if (zerop constr-symbolic)
                                       nil
                                       constr-symbolic))))
              (t (setf new-basic-var new-slack-var) ; sonstige Faelle
                 (setf new-coeffs (loop for nbv across nonbasic-vars 
                                        collect
                                        (if (member nbv constr-nonbasic-vars)
                                          (gethash nbv var-coeff)
                                          0)))		   
                 (setf new-state
                       (simplex (add-constraint (if copy-concrete-domain-state 
                                                  (copy-solver-state state
                                                                     (1+ old-row-dim)
                                                                     (+ old-col-dim (length new-nonbasic-vars)))
                                                  state) 
                                                (1+ old-row-dim)
                                                (+ old-col-dim (length new-nonbasic-vars))
                                                new-basic-var
                                                new-nonbasic-vars
                                                new-slack-var
                                                new-free-vars
                                                new-coeffs
                                                rs
                                                (if (zerop constr-symbolic)
                                                  nil
                                                  constr-symbolic))))))
        (when (and new-state new-or-dependencies)
          (setf (solver-state-or-dependencies new-state) new-or-dependencies))
        (if new-state
          (progn
            (incf-statistics *successful-cd-predicate-tests*)
            (values t new-state))
          (progn
            (incf-statistics *failed-cd-predicate-tests*)
            nil))))))
				 

(defun print-cd-tableau (A b symb Zeile Spalte m n)
    (format t "~%~10,T|")
    (dotimes (j n)
      (format t "~12,12T~A" (aref Spalte j)))
    (format t "~12,12T|")
    (if symb
	(format t "~12,12T|~%")
	(terpri))
    (dotimes (j (+ 3 n))
      (format t "----------" ))
    (if symb 
	(format t "----------" ))
    (terpri)
    (dotimes (i m)
      (format t "~A~10,T|" (aref Zeile i))
      (dotimes (j n)
	(format t "~12,12T~A" (aref A i j)))
      (format t "~12,12T|~A" (aref b i))
      (if symb 
	  (format t "~12,12T|~A~%" (aref symb i))
	  (terpri)))
    )


(defun simplex (state)
  "Fuehrt Simplex-Iterationen durch, bis eine Loesung gefunden
   wird. Dann wird ein neuer solver-state zurueckgegeben, der
   das Ungleichungssystem in geloester Form enthaelt. Falls es
   keine Loesung gibt, wird nil zurueckgegeben"
  ; check input:
  (let* ((basic-vars (solver-state-basic-vars state))
	 (nonbasic-vars (solver-state-nonbasic-vars state))
	 (free-vars (solver-state-free-vars state))
	 (slack-vars (solver-state-slack-vars state))
	 (matrix (solver-state-matrix state))
	 (constant-col (solver-state-constant-col state))
	 (symbolic-col (solver-state-symbolic-col state))
	 (vars (let ((v (make-hash-table)))
		 (loop for bv across basic-vars
		     and pos from 0 do
		       (setf (gethash bv v) (list 'bv pos)))
		 (loop for nbv across nonbasic-vars
		     and pos from 0 do
		       (setf (gethash nbv v) (list 'nbv pos)))
		 v))
	 (row-dim (sparse-array-dimension matrix 0))
	 (col-dim (sparse-array-dimension matrix 1)))
    ; row-dim = height of the matrix we are working in.
    ; col-dim = width of the matrix we are working in.
    (when *cd-simplex-verbose*
      (format t "~%~%Initialisierung:~%")
      (print-cd-tableau matrix constant-col symbolic-col basic-vars nonbasic-vars row-dim col-dim))
    (flet 
	((pivot (k l)
	   ; pivots the tableau around the element matrix[k,l] with 0<=k<row-dim, 0<=l<col-dim.
	   (let ((r (/ (saref matrix k l))))
	     ; column l :
             (let ((factor (- r)))
	       (dotimes (i row-dim)
	         (unless (eql i k)
                   (let ((val (saref matrix i l)))
                     (unless (zerop val)
		       (setf (saref matrix i l) (* factor val)))))))
	     ; everything except row k and column l :
	     (dotimes (j col-dim)
	       (unless (eql j l)
		 (let ((s (saref matrix k j)))
                   (unless (zerop s)
		     (dotimes (i row-dim)
		       (unless (eql i k)
                         (let ((val (saref matrix i l)))
                           (unless (zerop val)
		             (incf (saref matrix i j) (* s val))))))))))
	     (let ((s (aref constant-col k)))
               (unless (zerop s)
	         (dotimes (i row-dim)
		   (unless (eql i k)
                     (let ((val (saref matrix i l)))
                       (unless (zerop val)
		         (incf (aref constant-col i) (* s val))))))))
	     (if symbolic-col 
               (let ((s (aref symbolic-col k)))
                 (unless (zerop s)
		   (dotimes (i row-dim)
		     (unless (eql i k)
                       (let ((val (saref matrix i l)))
                         (unless (zerop val)
		           (incf (aref symbolic-col i) (* s val)))))))))
	     ; row k : 
	     (dotimes (j col-dim)
	       (unless (eql j l)
                 (let ((val (saref matrix k j)))
                   (unless (zerop val)
		     (setf (saref matrix k j) (* val r))))))
	     (setf (aref constant-col k) (* (aref constant-col k) r))
	     (if symbolic-col
		 (setf (aref symbolic-col k) (* (aref symbolic-col k) r)))
	     ; element (k,l) :
	     (setf (saref matrix k l) r)
	     )
	   ; swap labels:
	   (rotatef (aref basic-vars k) (aref nonbasic-vars l))
	   (rotatef (gethash (aref basic-vars k) vars)
		    (gethash (aref nonbasic-vars l) vars))
	   ))
      (flet
	  ((SuchePivotZeile (l)
	     (let ((k nil)
		   (hmin nil)
                   (slack-vars-table (make-hash-table :size (length slack-vars)))
                   )
               (loop for var in slack-vars do
                     (setf (gethash var slack-vars-table) t))
	       ; Zeilen mit kleinstem Quotienten constant-col[i]/matrix[i,l] suchen
	       (loop for bv across basic-vars
		     and row from 0 do
		     (when (and (gethash bv slack-vars-table)
				(if (eql (1+ row) row-dim) ; letzte Zeile?
				    (minusp (saref matrix row l)) ; dann neg koeff
				  (plusp (saref matrix row l)))) ; sonst pos koeff
		       (let ((h (/ (aref constant-col row) (saref matrix row l))))
			 (if (or (not hmin) (< h hmin)) 
			     (setq hmin h k (list row))
			   (if (eql h hmin)
			       (push row k))))))
	       ;(format t "k: ~A~%" k)
	       ; falls symbolic-col, bei mehreren Zeilen die mit kleinstem Quotienten 
	       ; symbolic-col[i]/matrix[i,l] waehlen:
	       (when (and symbolic-col (> (length k) 1))
		 (setf k (let ((h2min nil)
			       (k2 nil))
			   (loop for row in k do
				 (let ((h (/ (aref symbolic-col row) (saref matrix row l))))
				   ;(format t "h: ~A   row: ~A   h2min: ~A~%" h row h2min)
				   (if (or (not h2min) (< h h2min)) 
				       (setq h2min h k2 (list row))
				     (if (eql h h2min)
					 (push row k2)))))
			   k2)))
	       ;(format t "k: ~A~%" k)
	       ; falls mehrere Zeilen, diejenige mit der juengsten Basisvariable waehlen:
	       (if (< 1 (length k))
		   (let ((bvs (mapcar #'(lambda (x) (list x (aref basic-vars x))) k)))
		     (loop for sv in slack-vars do
			   ;(format t "sv: ~A~%" sv)
			   (when (member sv bvs :key #'second)
			     (return (first (first (member sv bvs :key #'second)))))))
		   (first k))
	       )))
        ; search feasible solution
	(when *cd-simplex-verbose*
          (format t "~%~%Zulaessige Loesung suchen:~%"))
	(loop
	  (let (l)
	    ; Pivotspalte k waehlen:
	    ; falls moeglich: juengste freie Variable in der letzten Zeile
	    (setf l (loop for fv in free-vars do
			  (let ((var (gethash fv vars)))
			    (when (and (eql (first var) 'nbv)
				       (not (zerop (saref matrix (1- row-dim) (second var)))))
			      (return (second var))))))
	    (if l
		(progn
		  (pivot (1- row-dim) l)
		  (when *cd-simplex-verbose*
		    (print-cd-tableau matrix constant-col symbolic-col basic-vars nonbasic-vars row-dim col-dim))
		  (return)); loesbar ==> neuen Zustand erzeugen 
	      ; sonst: juengste waehlbare slack-variable (negativer Koeffizient)
	      ; in der letzten Zeile als Pivotspalte
	      (progn
		(setf l (loop for sv in slack-vars do
			      (let ((var (gethash sv vars)))
				(when (and (eql (first var) 'nbv)
					   (minusp (saref matrix (1- row-dim) (second var))))
				  (return (second var))))))
		(if l
		    (let ((k (SuchePivotZeile l)))
		      (pivot k l)
		      (when *cd-simplex-verbose*
			(print-cd-tableau matrix constant-col symbolic-col basic-vars nonbasic-vars row-dim col-dim))
		      (when (eql k (1- row-dim))
			(return))); loesbar ==> neuen Zustand erzeugen
	          ; falls keine Pivotspalte gefunden wurde ==> unloesbar
		  (return-from simplex nil))))))
        ; Solvable! Build new state:
#|
        (setf (solver-state-basic-vars state) basic-vars
              (solver-state-nonbasic-vars state) nonbasic-vars
              (solver-state-slack-vars state) slack-vars
              (solver-state-matrix state) matrix
              (solver-state-constant-col state) constant-col
              (solver-state-symbolic-col state) symbolic-col)
        (make-solver-state :basic-vars basic-vars
			   :nonbasic-vars nonbasic-vars
			   :slack-vars slack-vars
			   :free-vars free-vars
			   :matrix matrix
			   :constant-col constant-col
			   :symbolic-col symbolic-col)
|#
	state))))




