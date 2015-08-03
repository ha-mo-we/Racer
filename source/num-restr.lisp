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

(defvar *roles*)
(defvar *qualified-at-most-roles*)
(defvar *individuals*)
(defvar *variables*)

(defstruct srole
  (name nil)
  (at-most nil)
  (at-least nil)
  (ancestors nil) ;inkl der Rolle selber
  (descendants nil) ;inkl der Rolle selber
  (all-qualifications nil)
  (qualified-at-most nil) ; Liste mit Eintraegen der Art (zahl konzeptterm)
  (qualified-at-least nil);
  (individuals nil) ; Liste von Individuennamen
  (exclusion-list nil)
  (partitions nil)
  (qual-at-most-roles nil)
  (new nil) ; wurde die Rolle selbst erzeugt?
  (constraints nil) ; constraints as dependencies
  )

(defun sort-role-names (role-list)
  "Sortiert die Namen der Rollen aus role-list in die Liste sorted-role-names so ein, 
   dass der Name jeder Rolle hinter den Namen der Oberrollen dieser Rolle in 
   sorted-role-names steht."
  (let ((sorted-role-names nil))
    (loop until (null role-list) do 
	  (let ((level nil))
	    (dolist (r role-list)
	      (let ((name (srole-name r)))
		; Sind alle Vorfahren bereits in sorted-role-names?
		(when (dolist (anc-name (srole-ancestors r) t)
			(when (not (or (eql anc-name name) 
				       (member anc-name sorted-role-names)))
			  (return nil)))
		  (setf role-list (remove r role-list)) 
		  (setf level (adjoin name level)))))
	    (setf sorted-role-names (append sorted-role-names level))))
    sorted-role-names))
		
(defun satisfiable-p (concept-list role-list individual unused-1 unused-2)
  (declare (ignore unused-1 unused-2))
  (role-successor-satisfiable-p role-list concept-list individual nil nil))

(defun negate-qualification (qualification)
  (concept-negated-concept qualification))

(defun get-roles (role-names)
  "Liefert zu einer Liste von Rollennamen die zugehoerige Rollen.
   Falls eine Rolle im Slot new einen Eintrag enthaelt, werden stattdessen die
   zu diesem Eintrag gehoerigen Rollen zurueckgegeben."
  (let ((roles nil))
    (dolist (name role-names)
      (let ((r (gethash name *roles*)))
	(if (srole-new r)
	    (dolist (anc (srole-new r))
	      (pushnew (gethash anc *roles*) roles))
	    (pushnew r roles))))
    roles)
  )

(defun get-qualifications (role-names)
  "Liefert zu einer Liste von Rollennamen alle zugehoerige All-Qualifikationen."
  (let ((qualifications nil))
    (dolist (name role-names)
      (let ((r (gethash name *roles*)))
	(setf qualifications 
	      (append (srole-all-qualifications r) qualifications))
	))
    (list qualifications))
  )

(defun eliminate-qualified-at-least (name role)
  "Erzeugt zu einer Rolle fuer jede qualifizierte at-least-Restriktion 
   eine neue Unterrolle mit entsprechenden at-least und all-Restriktionen,
   falls die all-Restriktionen erfuellbar sind. Sonst wird NIL
   zurueckgeliefert und keine Rollen erzeugt.
   Die neuen Rollen werden in *roles* eingetragen und ihre Namen werden
   in die Listen von descendants aller ancestors der Rolle eingetragen."
  (dolist (qualified-at-least (srole-qualified-at-least role) t)
    (let ((qualifications (list (srole-all-qualifications role)
				(list (second qualified-at-least))))
	  (number (first qualified-at-least)))
      (when (not (satisfiable-p qualifications (list role) nil nil nil))
	(return-from eliminate-qualified-at-least nil))
      (let ((new-name (gensym (concatenate 'string (symbol-name name) "-AT-LEAST-"))))
	; Neue Rolle erzeugen:
	(setf (gethash new-name *roles*)
	  (make-srole :name new-name
		      :at-least number
		      :ancestors (cons new-name (srole-ancestors role))
		      :descendants (list new-name)
		      :all-qualifications (reduce #'append qualifications :initial-value nil)
		      :new (list name)
                      :constraints (srole-constraints role)))
	; Neue Rolle in srole-descendants von role und deren vorfahren eintragen:
	(dolist (n (srole-ancestors role))
	  (let ((r (gethash n *roles*)))
		(setf (srole-descendants r) (adjoin new-name (srole-descendants r)))))
	))))

(defun eliminate-qualified-at-most (name role)
  "Erzeugt zu einer Rolle fuer jede qualifizierte at-most-Restriktion 
   eine neue Unterrolle mit entsprechenden at-most und all-Restriktionen,
   falls die all-Restriktionen erfuellbar sind und eine weitere Unterrolle,
   die die negierte at-most-Qualifikation zusammen mit den all-Restriktionen
   der Oberrolle alls all-Qualifikationen enthaelt, falls diese zusammen
   erfuellbar sind. Es wird in jedem Fall mindestens eine der beiden Rollen
   erzeugt, da nicht fuer beide die Qualifikationen unerfuellbar sein
   koennen (die all-qualifikationen der Oberrolle wurden bereits auf
   Erfuellbarkeit geprueft).
   Die neuen Rollen werden in *qualified-at-most-roles* eingetragen und ihre 
   Namen werden in die Listen von descendants aller ancestors der Rolle 
   eingetragen. Ausserdem wird in den Slot qual-at-most-roles der Rolle 
   und all ihrer Nachkommen fuer jede qualifizierte at-most-Restriktion
   eine Liste mit den erzeugten Rollen (bzw. der erzeugten Rollen) eingetragen."
  (dolist (qualified-at-most (srole-qualified-at-most role) t)
    (let ((qualifications (list (srole-all-qualifications role)
				(list (second qualified-at-most))))
	  (neg-role-qualifications (list (srole-all-qualifications role)
				         (list (negate-qualification (second qualified-at-most)))))
	  (number (first qualified-at-most))
	  (at-most-roles nil))
      (when (satisfiable-p qualifications (list role) nil nil nil)
	(let ((new-name (gensym (concatenate 'string (symbol-name name) "-AT-MOST-"))))
          ; Neue Rolle erzeugen:
	  (setf (gethash new-name *roles*)
		(make-srole :name new-name
			    :at-most number
			    :ancestors (cons new-name (srole-ancestors role))
			    :descendants (list new-name)
			    :all-qualifications (reduce #'append qualifications :initial-value nil)
			    :new (list name)
                            :constraints (srole-constraints role)))
	  (push new-name at-most-roles)))
      (when (satisfiable-p neg-role-qualifications (list role) nil nil nil)
	(let ((neg-new-name (gensym (concatenate 'string "NEG-" (symbol-name name) "-AT-MOST-"))))
	  ; Neue Rolle erzeugen:
	  (setf (gethash neg-new-name *roles*)
		(make-srole :name neg-new-name
			    :ancestors (cons neg-new-name (srole-ancestors role))
			    :descendants (list neg-new-name)
			    :all-qualifications (reduce #'append neg-role-qualifications :initial-value nil)
			    :new (list name)
                            :constraints (srole-constraints role)))
	  (push neg-new-name at-most-roles)))
      ; bei r und nachkommen von r neue Rollen
      ; in srole-qual-at-most-roles eintragen:
      (dolist (n (srole-descendants role))
	(let ((r (gethash n *roles*)))
	  (push at-most-roles (srole-qual-at-most-roles r)))) 
      ; Neue Rollen in srole-descendants von role und deren vorfahren eintragen:
      (dolist (n (srole-ancestors role))
	(let ((r (gethash n *roles*)))
	  (setf (srole-descendants r ) (append at-most-roles (srole-descendants r)))))
      ; Neue Rollen in *qualified-at-most-roles* eintragen:
      (setf *qualified-at-most-roles* (append at-most-roles *qualified-at-most-roles*))
      )))

(defun eliminate-individuals (name role)
  (dolist (ind (srole-individuals role))
    (if (member ind *individuals*)
	; Fuer dieses Individuum existiert bereits eine Rolle
	(let ((ind-role (gethash ind *roles*)))
	  (setf (srole-ancestors ind-role) 
	    (union (srole-ancestors role) (srole-ancestors ind-role)))
	  (setf (srole-all-qualifications ind-role) 
	    (append (srole-all-qualifications role)
		    (srole-all-qualifications ind-role)))
	  (push name (srole-new ind-role)))
      ; andernfalls existiert noch kein Rolle, also muss sie angelegt werden:
      (progn      
	;neue Rolle erzeugen:
	(setf (gethash ind *roles*)
	  (make-srole :name ind
		      :at-least 1
		      :at-most 1
		      :ancestors (cons ind (srole-ancestors role))
		      :descendants (list ind)
		      :all-qualifications (srole-all-qualifications role)
		      :new (list name)
                      :constraints (srole-constraints role)))
        ; Neue Rolle in *individuals* eintragen:
	(push ind *individuals*)))
     ; (Neue) Rolle in srole-descendants von role und deren vorfahren eintragen:
    (dolist (n (srole-ancestors role))
      (let ((r (gethash n *roles*)))
	(setf (srole-descendants r) (adjoin ind (srole-descendants r)))))
  ))

(defun intersect-with-qualified-at-most-roles (start-partition at-most-roles)
  (let ((partitions (list start-partition))
	(new-at-most-roles nil)
	(ind (first (intersection *individuals* start-partition))))
    ; at-most-roles, die schon in der start-partition
    ; beruecksichtigt sind, aussortieren
    (dolist (role-list at-most-roles)
      (when (dolist (name role-list t)
	      (when (member name start-partition)
		(return nil)))
	(push role-list new-at-most-roles)))
    ; fuer verbleibende at-most-roles: alle partitionen jeweils mit
    ; beiden Rollen zu schneiden versuchen
    (dolist (qual-roles new-at-most-roles )
      (let ((new-partitions nil))
	(dolist (q-role qual-roles)
	  (dolist (p partitions)
	    (push q-role p)
	    (when (satisfiable-p (get-qualifications p) (get-roles p) ind nil nil)
	      (push p new-partitions))))
	(setf partitions new-partitions)))
    ; falls keine partitionen erzeugt wurden:
    (if (null partitions)
	(return-from intersect-with-qualified-at-most-roles nil))
    ; neue Variablen erzeugen
    (let ((new-vars (loop for p in partitions 
			  collect *simplex-number-of-variables*
			  do 
			  (setf (gethash *simplex-number-of-variables* *variables*) p)
			  (incf *simplex-number-of-variables*))))
      ; Variablen in srole-partitions der Vorfahren aller Rollen aus
      ; der start-partition eintragen
      (add-vars-to-srole-partitions start-partition new-vars)
      ; Variablen in srole-partitions der at-most-roles eintragen, falls
      ; diese in der partition vorkommen
      (dolist (q-name (reduce #'append new-at-most-roles :initial-value nil))
	(let ((q-role (gethash q-name *roles*)))
	  (dolist (v new-vars)
	    (let ((p (gethash v *variables*)))
	      (when (member q-name p)
		;(pushnew v (srole-partitions q-role))
                (add-to-srole-partition q-role (list v))
                )))))
      new-vars))
  )

(defun add-to-srole-partition (role vars)
  (loop with partition = (srole-partitions role)
        for var in vars
        do
        (cond ((null partition) (push var partition))
              ((consp (first partition))
               (if (eql (car (first partition)) (1- var))
                 (setf (car (first partition)) var)
                 (push var partition)))
              ((numberp (first partition))
               (if (eql (first partition) (1- var))
                 (setf (first partition) (cons var (first partition)))
                 (push var partition))))
        finally (setf (srole-partitions role) partition)))

(defun add-vars-to-srole-partitions (start-partition new-vars)
  (add-new-vars-to-srole-ancestors (get-srole-ancestors start-partition) new-vars))

(defun get-srole-ancestors (start-partition)
  (loop with ancestors = nil
        for name in start-partition
        do (setf ancestors (union (srole-ancestors (gethash name *roles*)) ancestors))
        finally (return ancestors)))

(defun add-new-vars-to-srole-ancestors (ancestors new-vars)
  (loop for a-name in ancestors
        for anc = (gethash a-name *roles*)
        do
        ;(setf (srole-partitions anc) (append new-vars (srole-partitions anc)))
        (add-to-srole-partition anc new-vars)))

(defun intersection-in-pairs (sorted-role-names roles individuals)
  "Bildet alle Schnitte von bis zu zwei Rollen aus sorted-role-names, die Fueller
   enthalten koennen und nicht Vorfahren bzw. Nachkommen voneinander sind.
   Durch qualifizierte at-most-Restriktionen muessen evtl. noch weitere (at-most-roles)
   an den Schnitten beteiligt sein."
  (let ((role-list nil))
    (dolist (name sorted-role-names)
      (let ((r (gethash name roles))
	    (ind nil))
	; falls name in individuals enthalten ist, alle uebrigen namen
	; aus individuals zur exclusion-list hinzufuegen:
	(when (member name individuals)
	  (setf ind name)
	  (setf (srole-exclusion-list r) (remove name individuals)))
	; Partition(en) "nur r" (evtl inkl at-most-rollen) fuer die Rolle r erzeugen 
	; und zu partitions von r und den Vorfahren von r hinzufuegen:
	(intersect-with-qualified-at-most-roles (list name) (srole-qual-at-most-roles r))
	; Inhalte der exclusion-lists der Vorfahren uebernehmen:
	(dolist (a-name (srole-ancestors r))
	  (let ((anc (gethash a-name roles)))
	    (setf (srole-exclusion-list r) (union (srole-exclusion-list anc) (srole-exclusion-list r)))))
	; r mit jeder Rolle aus role-list schneiden (falls erlaubt) und zu partitions
	; von diesen beiden Rollen und deren Vorfahren hinzufuegen:
	(dolist (n role-list)
	  (let ((ind2 ind))
	    (when (not (or (member n (srole-ancestors r)) (member n (srole-exclusion-list r))))
	      (if (member n individuals) ; darf nur auftreten, wenn name kein individuum ist
		  (setf ind2 n))
	      (let* ((r2 (gethash n roles))
		     (qual-at-most-roles (union (srole-qual-at-most-roles r) 
						(srole-qual-at-most-roles r2) :test #'equal)))
		(if (satisfiable-p (get-qualifications (list name n)) (get-roles (list name n)) ind2 nil nil)
		    (intersect-with-qualified-at-most-roles (list name n) qual-at-most-roles)
		  (setf (srole-exclusion-list r) (union (srole-descendants r2) (srole-exclusion-list r))
			(srole-exclusion-list r2) (union (srole-descendants r) (srole-exclusion-list r2)))
		  ))))))
      (setf role-list (cons name role-list))
      )))

(defun generate-all-intersections (sorted-role-names
                                       roles
                                       individuals-list
                                       variables-list
                                       number-of-variables
                                       qualified-at-most-roles)
  "Bildet aufbauend auf den von intersection-in-pairs gebildeten Schnitten alle 
   moeglichen Schnitte der Rollen, die Fueller haben koennen."
  (let ((variables nil))
    (dotimes (n number-of-variables)
      (let ((p (gethash n variables-list)))
	(if (> (length (set-difference p qualified-at-most-roles)) 1) ; Schnitt von mind. 2 Rollen
	    (setf variables (cons n variables)))))
    (dolist (name sorted-role-names)
      ; alle partitionen entfernen, die die Rolle mit Namen name enthalten:
      (let ((var-new nil))
	(dolist (n variables)
	  (if (not (member name (gethash n variables-list)))
	      (push n var-new)))
	(setf variables var-new))
      ; jede Partition mit der Rolle schneiden (falls erlaubt): 
      (let ((r (gethash name roles))
	    (ind nil))
	(when (member name individuals-list)
	  (setf ind name))
	(dolist (n variables)
	  (let ((p (gethash n variables-list)))
	    (when (not (or (not (lists-disjoint-p p (srole-ancestors r)))
			   (not (lists-disjoint-p p (srole-descendants r)))
			   (not (lists-disjoint-p p (srole-exclusion-list r)))))
	      (setf ind (first (intersection p individuals-list)))
	      (let ((p-new (cons name p)))
		; Kann der neue Schnitt Fueller enthalten?
		(when (satisfiable-p (get-qualifications p-new) (get-roles p-new) ind nil nil)
		  ; dann Schnitt bilden
		  (setf variables 
		    (append (intersect-with-qualified-at-most-roles p-new (srole-qual-at-most-roles r))
			    variables)))
		))))))
      ))
  
(defun generate-lop (sorted-role-names
                       roles
                       qualified-at-most-roles
                       no-of-variables)
  "Erzeugt aus den at-most und at-least-Qualifikationen und den partitions der
   Rollen ein lineares Optimierungsproblem der Form Ax<=b, mit dem Zielfuntionsvektor
   c, der aus lauter Einsen besteht."
  (let ((at-most-list nil)
	(at-least-list nil)
	(restr-count 0))
    ; Rollen mit at-most und at-least-Restriktionen heraussuchen 
    ; und Anzahl der Restriktionen zaehlen:
    (dolist (name sorted-role-names)
      (let ((r (gethash name roles)))
	(when (srole-at-most r)
	  (setf at-most-list (cons name at-most-list))
	  (incf restr-count))
	(when (srole-at-least r)
	  (setf at-least-list (cons name at-least-list))
	  (incf restr-count))))
    (dolist (name qualified-at-most-roles)
      (let ((r (gethash name roles)))
	(when (srole-at-most r)
	  (setf at-most-list (cons name at-most-list))
	  (incf restr-count))))
    ; Statistikvariable fuer Anzahl der Gleichungen aktualisieren:
    (setf *simplex-number-of-equations* restr-count)
    (let ((A (make-sparse-array (list restr-count no-of-variables) :element-type 'rational :initial-element 0))
	  (b (make-array restr-count :element-type 'rational :initial-element 0))
	  (c (make-array no-of-variables :element-type 'rational :initial-element 1))
	  (row 0))
      ; at-most-Ungleichungen erzeugen
      (dolist (name at-most-list)
	(let ((r (gethash name roles)))
	  (dolist (var (srole-partitions r))
            (if (consp var)
              (loop for nvar from (car var) downto (cdr var) do
	            (setf (saref A row nvar) 1))
              (setf (saref A row var) 1)))
	  (setf (aref b row) (srole-at-most r))
	  (incf row)))
      ; at-least-Ungleichungen erzeugen
      (dolist (name at-least-list)
	(let ((r (gethash name roles)))
	  (dolist (var (srole-partitions r))
            (if (consp var)
              (loop for nvar from (car var) downto (cdr var) do
	            (setf (saref A row nvar) -1))
              (setf (saref A row var) -1)))
	  (setf (aref b row) (- (srole-at-least r)))
	  (incf row)))
      (values A b c))))
      
(defun generate-model (solution variables individuals)
  "Liefert zu einer Loesung aus gomory1 ein Modell: Liefert eine Liste, die
   fuer jede (Struktur-)Variable, die eine Wert groesser Null hat, eine Liste
   mit folgendem Inhalt enthaelt: Liste aller beteiligten Rollen, Loesungswert
   (entspricht Anzahl der Individuen), Liste aller geforderten Qualifikationen,
   nil falls es sich nicht um ein ABox-Individuum handelt, 
   (fehlt noch: sonst der Name des Individuums)"
  (let ((sol (first solution))
	(model nil))
    (dotimes (i (array-dimension sol 0))
      (let ((val (aref sol i)))
	(when (> val 0)
	  (let* ((role-names (gethash i variables))
		 (ind (intersection role-names individuals)))
	    (push (list (get-roles role-names) 
			val 
			(get-qualifications role-names) 
			ind) 
		  model)))))
    (when *simplex-verbose* 
      (format t "~%Modell: ~A" model))
    model)
  )

(defun get-all-roles-in-table (table)
  (loop for role being the hash-value of table
        collect role))
    
(defun check-number-restrictions (roles unused-1 unused-2 unused-3 unused-4)
  (declare (ignore unused-1 unused-2 unused-3 unused-4))
  (with-race-trace-sublevel ("check-number-restrictions"
                             :expanded nil
                             :arguments (list roles)
                             :trace-result t)
    (let ((*roles* (make-hash-table))
          (*variables* (make-hash-table))
	  (*qualified-at-most-roles* nil)
	  (*individuals* nil))	
      (dolist (r roles) ; alle Rollen in die hash-tabelle eintragen 
        (setf (gethash (srole-name r) *roles*) r))
      ; ueberfluessige Ober- und Unterrollen entfernen:
      (loop for role in (get-all-roles-in-table *roles*)
            do (let ((anc nil)
                     (desc nil)) 
                 (dolist (a (srole-ancestors role))
                   (when (gethash a *roles*)
                     (push a anc)))
                 (dolist (d (srole-descendants role))
                   (when (gethash d *roles*)
                     (push d desc)))
                 (setf (srole-ancestors role) anc)
                 (setf (srole-descendants role) desc)))
      ; ueberpruefen, ob alle Rollen Fueller haben koennen
      (dolist (r roles)
        (when (not (satisfiable-p (list (srole-all-qualifications r)) (list r) nil nil nil)) ; Rolle r kann keine Fueller haben 
          (return-from check-number-restrictions  nil)))
      ; at-least- und at-most-Terme und Individuen eliminieren:
      (loop for role in (get-all-roles-in-table *roles*) do
            (if (srole-qualified-at-least role)
              (when (not (eliminate-qualified-at-least (srole-name role) role))
                ; at-least-Term nicht erfuellbar
                (return-from check-number-restrictions nil))))
      (loop for role in (get-all-roles-in-table *roles*) do
            ; (alte) Individuen eliminieren
            (if (srole-individuals role)
              (eliminate-individuals (srole-name role) role)))
      (loop for role in (get-all-roles-in-table *roles*) do
            (if (srole-qualified-at-most role)
              (eliminate-qualified-at-most (srole-name role) role)))
      ; Rollen sortieren
      (let ((role-list nil))
        (loop for role in (get-all-roles-in-table *roles*)
              do
              (if (not (member (srole-name role) *qualified-at-most-roles*)) 
                (push role role-list)))
        (let ((sorted-role-names (sort-role-names role-list)))
          ; Paarweise Schnittbildung
          (intersection-in-pairs sorted-role-names *roles* *individuals*)
          (when *simplex-verbose*
	    (format t "~%Rollen: ~A~%" sorted-role-names)
	    (dotimes (i *simplex-number-of-variables*)
	      (format t "~A = ~A ~A~%"  i (gethash i *variables*) (get-qualifications (gethash i *variables*))))
	    (maphash #'(lambda (n r) (format t "~A: ~A~%" n r)) *roles*))
          (race-trace ("~&Calling Simplex with pairwise intersections ~
                        *roles*=~S *variables*=~S *qualified-at-most-roles*=~S ~
                        *individuals*=~S sorted-role-names=~S~%"
                       *roles* *variables* *qualified-at-most-roles*
                       *individuals* sorted-role-names))
          ; Konstruktion und Loesung des LOPs:
          (let ((solvable-p nil)
	        (solution nil))
	    (multiple-value-bind (A b c)
                                 (generate-lop sorted-role-names
                                               *roles*
                                               *qualified-at-most-roles*
                                               *simplex-number-of-variables*)
	      (multiple-value-bind (sol-p sol) (gomory1 A b c)
	        (setf solvable-p sol-p solution sol)))
	    (if solvable-p 
              (let ((model (generate-model solution *variables* *individuals*)))
                (race-trace ("~&Simplex found solution ~S using pairwise intersections~%"
                             model))
	        (values t model))
              ; Falls vereinfachtes Problem nicht loesbar: alle Schnitte bilden
	      (let ((count *simplex-number-of-variables*))
                (race-trace ("~&Simplex found NO solution using pairwise intersections~%"))
	        (generate-all-intersections sorted-role-names
                                            *roles*
                                            *individuals*
                                            *variables*
                                            *simplex-number-of-variables*
                                            *qualified-at-most-roles*)
                (if (eql count *simplex-number-of-variables*)
                  (progn
                    (race-trace ("~&No additional intersections possible: NO solution found~%"))
                    nil)
	          (progn
                    (race-trace ("~&Calling Simplex with all intersections~%"))
		    (if *simplex-verbose*
                      (dotimes (i *simplex-number-of-variables*)
                        (format t "~A = ~A ~A~%"  i (gethash i *variables*) (get-qualifications (gethash i *variables*)))))
	            ; Konstruktion und Loesung des LOPs:
		    (multiple-value-bind (A b c)
                                         (generate-lop sorted-role-names
                                                       *roles*
                                                       *qualified-at-most-roles*
                                                       *simplex-number-of-variables*)
		      (multiple-value-bind (sol-p sol) (gomory1 A b c)
		        (setf solvable-p sol-p solution sol)))
		    ; Falls Loesung existiert: erzeugen eines Modells:
		    (if solvable-p 
                      (let ((model (generate-model solution *variables* *individuals*)))
                        (race-trace ("~&Simplex found solution ~S using all intersection~%"
                                     model))
	                (values t model))
                      (progn
                        (race-trace ("~&Simplex found NO solution using all intersections~%"))
                        nil))))))))))))



