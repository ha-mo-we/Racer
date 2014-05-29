;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: RACER; Base: 10 -*-

;;; Copyright (c) 1998-2014, 
;;; Volker Haarslev, Ralf Moeller, Michael Wessel,
;;; with contributions from Martina Timmann.
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

(defparameter *simplex-verbose* nil)
(defparameter *gomory-verbose* nil)

;; variablen fuer die statistik:
(defvar *simplex-number-of-variables*)
(defvar *simplex-number-of-equations*)
(defvar *simplex-number-of-iterations*)

;; lexicographical dual variant of gomory's first cutting-plane-
;; algorithm for integer solutions to linear programs
;; martina timmann 01.12.2000 

; type of a number: here rational numbers
(deftype num () 'rational) ; abbreviation: r
(deftype vector-of-nums () '(simple-array num 1)) ; abbreviation: r^n
(deftype matrix-of-nums () '(simple-array num 2)) ; abbreviation: r^mxn

; notation:
; indices   1-based in mathematical theory, 0-based in this implementation
; x[i]      (aref x i)
; a[i,j]    (aref a i j)
; c`        the transpose of c
; <c,x>     c`*x = sum(i>=1, c[i]*x[i])
; b>=0      every component of the vector b is >= 0

; input: a, an mxn matrix of numbers,
;        b, an m vector of numbers,
;        c, an n vector of numbers.
; requirements on the input: 
;        every column vector (c[j],a[0,j],a[1,j],...,a[m,j])`, j=0,...,m-1
;        is lexicographically positive.
;        (a vector v is lexicographically positive if the first nonzero
;        component of v is positive)
; solves a dual linear optimisation problem with a lexicographical dual
; variant of gomory's first cutting-plane-method:
; ( c ) among the x in r^n with a*x<=b, x>=0, x integer, find those 
;       with <c,x> = min
; output: 1st value: flag if solvable, nil or t,
;         if solvable:
;           2nd value: solution of ( c ), the linear problem lp:
;                      list containing
;                      - the solution x
;                      - the optimal value of <c,x>


(defun gomory1 (a b c)
  (with-race-trace-sublevel ("gomory1"
                             :expanded t
                             :arguments (list a b c)
                             :trace-result t)
    (multiple-value-prog1
      (gomory-internal a b c)
      (when *simplex-verbose*
        (format t "~%anzahl der variablen: ~a~%" *simplex-number-of-variables*)
        (format t "~%anzahl der gleichungen: ~a~%" *simplex-number-of-equations*)
        (format t "~%anzahl der iterationen: ~a~%" *simplex-number-of-iterations*))
      (race-trace ("~&simplex: variables = ~d, equations = ~d, iterations = ~d~%"
                   *simplex-number-of-variables*
                   *simplex-number-of-equations*
                   *simplex-number-of-iterations*)))))

(defun gomory-internal (a b c)
  ; check input:
  (let* ((m (length b))
         (n (length c))
         (v (+ m n))) ; total number of variables 
    (declare (type fixnum m n v))
    ;;(assert (typep a `(array * (,m ,n))))
    ; m = height of the input-matrix.
    ; n = width of the matrix we are working in.
    ; v = height of the matrix we are working in (input matrix completed
    ;     by unit rows (-1 0 ... 0) ... (0 0 ... -1))
    ; check if every column vector (c[j],a[0,j],a[1,j],...,a[m,j])`
    ; is lexicographically positive:
    (dotimes (j n)
      (let ((non-zero 0))
        (if (not (eql (aref c j) 0))
          (setq non-zero (aref c j))
          (dotimes (i m)
            (let ((aij (saref a i j)))
              (when (not (eql aij 0))
                (setq non-zero aij)
                (return)))))
        (when (<= non-zero 0)
          #+:debug (error "columns are not lexicographically positive")
          (return-from gomory-internal
            (values nil "columns are not lexicographically positive")))))
    ; initialization:
    (let ((aa (let ((h (make-sparse-array `(,(+ v 1) ,n) :initial-element 0)))
                (dotimes (i m)
                  (dotimes (j n)
                    (setf (saref h i j) (rational (saref a i j)))
                    ) )
                ; add unit rows
                (dotimes (j n)
                  (let ((i (+ m j)))
                    (setf (saref h i j) -1)))
                h
                )   )
          (ab (let ((h (make-array (+ v 1) :initial-element 0)))
                (dotimes (i m) (setf (aref h i) (rational (elt b i))))
                h
                )   )
          (ac (let ((h (make-array n)))
                (dotimes (j n) (setf (aref h j) (rational (elt c j))))
                h
                )   )
          (ad 0)
          ; aa in r^mxn : the tableau we work in
          ; ab in r^m : the right margin
          ; ac in r^n : the bottom margin
          ; ad in r : the bottom right corner
          (row (let ((h (make-array (+ v 1))))
                 (dotimes (i m) (setf (aref h i) (cons 'u i)))
                 (dotimes (i n) (setf (aref h (+ i m)) (cons 'x i)))
                 h
                 )      )
          (col (let ((h (make-array n)))
                 (dotimes (j n) (setf (aref h j) (cons 'x j)))
                 h
                 )       )
          ; row in cons^v : left labels
          ; col in cons^n : top labels
          ;
          ; the starting tableau:
          ;
          ;             | col[0]    col[1]    ... col[n-1]    |
          ; ------------+-------------------------------------+---------
          ; row[0]      |  aa[0,0]   aa[0,1]  ... aa[0,n-1]   |  ab[0]
          ; row[1]      |  aa[1,0]   aa[1,1]  ... aa[1,n-1]   |  ab[1]
          ;   ...       |    ...       ...          ...       |   ...
          ; row[m-1]    | aa[m-1,0] aa[m-1,1] ... aa[m-1,n-1] | ab[m-1]
          ; row[m]      |    -1         0     ...    0        |    0
          ; row[m+1]    |     0        -1     ...    0        |    0
          ;   ...       |    ...       ...          ...       |   ...
          ; row[v-1]    |     0         0     ...   -1        |    0	  
          ; ------------+-------------------------------------+---------
          ;             |   ac[0]     ac[1]   ...   ac[n-1]   |   ad
          ;
          ;
          ; for all i=0,...,m-1:
          ;   u[i] + sum(j=0,...,n-1; aa[i,j]*x[j]) = ab[i]
          ; for all i=m,...,v-1:
          ;   x[i-m] - x[i-m] = 0 
          ; sum(j=0,...,n-1; ac[j]*x[j]) - ad = <c,x>
          ; these are to be considered as equations in the unknowns x,u.
          (x (make-array n))
          (u (make-array m))
          (cutnr 0) ; number of already added cuts	  	  
          )
      ;;(declare (type matrix-of-nums aa) (type vector-of-nums ab ac) 
      ;;         (type (or null num) ad))
      ;; Declarations removed, rm 10/20/08: they are wrong because of sparse matrices being used.
      (when *gomory-verbose*
        (format t "~%~%initialization:~%")
        (print-tableau aa ab ac ad row col v n))
      (race-trace ("~&starting simplex with aa=~s ab=~s ac=~s ad=~s ~
                    row=~d col=~d v=~d n=~d~%"
                   aa ab ac ad row col v n))
      ; until the solution is integer: solve the problem, without
      ; caring for the integer-condition, add a new equation, solve
      ; this new problem...
      (if *gomory-verbose*
        (format t "search feasible solution:~%"))
      (setf ad (gomory1-iteration aa ab ac ad col row n v cutnr))
      (unless ad
        (return-from gomory-internal nil))
      ; solvable in integers! build solution:
      (dotimes (i m)
        (let ((s (aref ab i)))	    
          (setf (aref u i) s)))
      (dotimes (i n)
        (let ((s (aref ab (+ i m))))
          (setf (aref x i) s)))
      (values t (list x u (- ad))))))

(defun multiply-column (aa column column-length factor)
  (loop for i from 0 to (1- column-length)
        for val = (saref aa i column) do
        (unless (zerop val)
          (setf (saref aa i column) (* factor val)))))

(defun add-columns (ab aa column column-length factor)
  (loop for i from 0 to (1- column-length)
        for val = (saref aa i column) do
        (unless (zerop val)
          (incf (aref ab i) (* factor val)))))

(defun inc-multiply-column (aa inc-column factor-column factor)
  (if (sparse-array-p aa)
    (loop for (index . val) in (svref (sparse-array-table aa) factor-column)
          do (incf (saref aa index inc-column) (* factor val)))
    (loop for i from 0 to (1- (array-dimension aa 0))
          for val = (saref aa i factor-column) do
          (unless (zerop val)
            (incf (saref aa i inc-column) (* factor val))))))

(defun pivot (k l aa ab ac ad row col v n)
  (declare (type fixnum k l))
  ; pivots the tableau around the element aa[k,l] with 0<=k<v, 0<=l<n.
  ; before and after pivoting the columns (ac[j],aa[0,j],...,aa[v-1,j])
  ; are lexicographically positive.
  ;;(declare (type matrix-of-nums aa) (type vector-of-nums ab ac) 
  ;;         (type num ad))
  ;; Declarations removed, rm 10/20/08: they are wrong because of sparse matrices being used.
  (let ((r (/ (saref aa k l))))
    (declare (type num r))
    ; column l :
    (multiply-column aa l v (- r))
    (setf (aref ac l) (- (* r (aref ac l))))
    ; everything except column l :
    (loop for j from 0 to (1- n) do
          (unless (eql j l)
            (let ((s (saref aa k j)))
              (unless (zerop s)
                (inc-multiply-column aa j l s)
                (incf (aref ac j) (* s (aref ac l))))
              ) ) )
    (let ((s (aref ab k)))
      (unless (zerop s)
        (add-columns ab aa l v s)
        (incf ad (* s (aref ac l))))
      ) )
  ; change label for col[l]
  (setf (aref col l) (aref row k))
  ad)

(defun choosepivotcol (k aa ac n)
  ; for a row k choose the col l such that:
  ; among the j with aa[k,j]<0 l is the one for which the vector 
  ; (-ac[j]/aa[k,j], -aa[0,j]/aa[k,j],..., -aa[v-1,j]/aa[k,j])
  ; is lexicographically minimal
  ; if aa[k,j]>=0 for all j, nil is returned.
  ;;(declare (type matrix-of-nums aa) (type vector-of-nums ac))
  ;; Declarations removed, rm 10/20/08: they are wrong because of sparse matrices being used.
  (let ((l nil))		
    (let ((hmin nil))
      ; find all the j for which aa[k,j]<0 and 
      ; the quotient -ac[j]/aa[k,j] is minimal
      (dotimes (j n)
        (let ((akj (saref aa k j))
              h)
          (when (< akj 0)
            (setq h (- (/ (aref ac j) akj)))
            ;;(if (not hmin) (setq hmin h))
            (if (null hmin) (setq hmin h))  ; changed rm 27.5.03
            (if (eql h hmin)
              (setq l (cons j l))
              (when (< h hmin) (setq hmin h l (list j))))))))
    ; among the j with -ac[j]/aa[k,j] minimal find the one
    ; for which the j-th column divided by -aa[k,j] is minimal.
    (loop for i upfrom 0
          while (> (length l) 1) do
          (let ((hmin nil)
                (lh nil))
            (loop for j in l do
                  (let ((akj (saref aa k j))
                        h)
                    (setq h (- (/ (saref aa i j) akj)))
                    ;;(if (not hmin) (setq hmin h)) ;; changed rm 27.5.03
                    (if (null hmin) (setq hmin h))
                    (if (eql h hmin)
                      (setq lh (cons j lh))
                      (when (< h hmin) (setq hmin h lh (list j))))
                    ))
            (setq l lh))
          finally (setq l (car l)))
    l))

(defun gomory1-iteration (aa ab ac ad col row n v cutnr)
  (loop 
    ; search feasible solution
    (loop
      ; search minimal ab[k]<0 :
      (let (k)
        (let ((hmin 0))
          (dotimes (i v) 
            (let ((h (aref ab i)))
              (when (< h hmin) (setq hmin h k i))
              ))
          (if (eql hmin 0)
            (return) ; every ab[i]>=0 ==> feasible
            ))
        (let ((l (choosepivotcol k aa ac n)))
          (if (null l)
            ; every aa[k,j]>=0 and ab[k]<0 ==> row makes dp inconsistent
            (progn ;;(warn "case 1: i am not sure if gomory1-iteration is correct.")
                   (return-from gomory1-iteration nil))
            (setf ad (pivot k l aa ab ac ad row col v n)))
          ; still every column (ac[j],aa[0,j],...,aa[v-1,j])
          ; is lexicographically positive, the column
          ; (ad,ab[0],ab[1],...,ab[v-1]) has decreased, because
          ; a negative vector has been added to it.
          ; statistik-zaehler fuer anzahl der iterationen inkrementieren:
          (incf *simplex-number-of-iterations*)
          (if *gomory-verbose*
            (print-tableau aa ab ac ad row col v n))
          ) ) )
    ; find first row with ad or ab[i] not integer
    (let ((index nil)
          (r-frac))
      (if (not (integerp ad))
        ; use ac to generate the new restriction:
        (setq index -1 r-frac (second (multiple-value-list (floor ad))))
        (dotimes (i v)
          (let ((s (aref ab i)))
            (when (not (integerp s))
              (setq index i r-frac (second (multiple-value-list (floor s))))
              (return ad)))))
      (if (not index) (return ad))	; ad and every ab[i] are integer 
      ; ==> build solution
      ; add new equation and store it temporary in 
      ; (ab[v], aa[v,0],..., aa[v,n-1]). after the first
      ; simplex-iteration this row is not needed anymore.
      (setf (aref row v) (cons 'c cutnr))
      (incf cutnr)
      (dotimes (j n)
        (let (s frac)
          (if (eql index -1) ; ac is generating row
            (setq s (aref ac j)
                  frac (second (multiple-value-list (floor s))))
            (setq s (saref aa index j)
                  frac (second (multiple-value-list (floor s)))))
          (setf (saref aa v j)  (- frac))))
      (setf (aref ab v) (- r-frac))
      )
    (when *gomory-verbose*
      (format t "add new equation:~%")
      (print-tableau aa ab ac ad row col (+ v 1) n)
      (format t "search feasible solution:~%"))
    (let ((l (choosepivotcol v aa ac n)))
      (if (null l)
        (progn ;;(warn "case 2:i am not sure if gomory1-iteration is correct.")
               (return-from gomory1-iteration nil))   ; new: rm 27.5.03 unsolvable if l = nil????
        (setf ad (pivot v l aa ab ac ad row col v n))))
    ; statistik-zaehler fuer anzahl der iterationen inkrementieren:
    (incf *simplex-number-of-iterations*)
    (if *gomory-verbose*
      (print-tableau aa ab ac ad row col v n))
    ))

(defun print-tableau (a b c q zeile spalte m n)
    (format t "~%~6,t|")
    (dotimes (j n)
      (format t "~8,8t~a~a" (first (aref spalte j)) (rest (aref spalte j))))
    (format t "~8,8t|~%")
    (dotimes (j (+ 2 n))
      (format t "--------" ))
    (format t "~%z~6,t|")
    (dotimes (j n)
      (format t "~8,8t~a" (aref c j)))
    (format t "~8,8t|~a~%" q)
    (dotimes (j (+ 2 n))
      (format t "--------" ))
    (terpri)
    (dotimes (i m)
      (format t "~a~a~6,t|" (first (aref zeile i)) (rest (aref zeile i)))
      (dotimes (j n)
	(format t "~8,8t~a" (saref a i j)))
      (format t "~8,8t|~a~%" (aref b i)))
    (terpri)
    (terpri)
  )

	
		      
