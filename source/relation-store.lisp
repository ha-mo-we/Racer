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

(defparameter *relation-store-virtual-removal* nil)

(defstruct (relation-store
            (:copier copy-relation-store-internal)
            (:constructor make-relation-store (is-empty-p
                                               left-table
                                               right-table
                                               contains-only-old-inds-p
                                               &optional
                                               added-relation-constraints
                                               removed-individuals)))
  (is-empty-p t)                        ; is the store completely empty?
  (left-table nil)                      ; left individual name indexes corresponding role assertions
  (right-table nil)                     ; right individual name indexes corresponding role assertions
  (contains-only-old-inds-p t)          ; only old (Abox) individuals are indexing the tables
                                        ; if T no clean-up or copying is required
  (added-relation-constraints nil)      ; contains dynamically added relation constraints not indexed by the tables
                                        ; avoids copying of changed tables
  (removed-individuals nil)             ; list of virtually removed old individuals still contained in the tables
  (protected-p nil)                     ; if T a copy of the store needs to be created before changing it
  )

(defmethod print-object ((object relation-store) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A ~A ~A"
            (relation-store-is-empty-p object)
            (when (relation-store-left-table object)
              (hash-table-count (relation-store-left-table object)))
            (when (relation-store-right-table object)
              (hash-table-count (relation-store-right-table object))))))

(defstruct (kernel-filler
            (:constructor make-kernel-filler (role elements)))
                                        ; describes a set of relation constraints with the same left or
                                        ; right individual and role
  (role nil)                            ; role used in all relation constraints
  (count 1)                             ; number of fillers
  (elements nil)                        ; list of relation constraints
                                        ; length identical to count
  )

(defmethod print-object ((object kernel-filler) stream)
  (print-unreadable-object (object stream :type nil :identity nil)
    (format stream "~A:~D"
            (kernel-filler-role object)
            (kernel-filler-count object))))

(defun protect-relation-store (store)
  (when store
    (setf (relation-store-protected-p store) t)
    store))

(defun relation-store-empty-p (relation-store)
  (or (null relation-store)
      (relation-store-is-empty-p relation-store)))

(defun copy-relation-store (store copy-tables-p)
  (if (or (null store) (relation-store-empty-p store))
    store
    (if copy-tables-p 
      (make-relation-store nil
                           (copy-hash-table (relation-store-left-table store)
                                            0
                                            #'copy-fillers)
                           (copy-hash-table (relation-store-right-table store)
                                            0
                                            #'copy-fillers)
                           (relation-store-contains-only-old-inds-p store)
                           (relation-store-added-relation-constraints store)
                           (relation-store-removed-individuals store))
     (copy-relation-store-internal store))))

(defun copy-fillers (filler-list)
  (loop for filler in filler-list
        collect (copy-kernel-filler filler)))

(defun safe-string< (item1 item2)
  (let ((item1 (if (numberp item1)
                 (princ-to-string item1)
                 item1))
        (item2 (if (numberp item2)
                 (princ-to-string item2)
                 item2)))
    (string< item1 item2)))

#-:debug
(defun sort-relation-constraints (list)
  list)

#+:debug
(defun sort-relation-constraints (list)
  (if (rest list)
    (sort list #'safe-string< :key #'constraint-ind-1)
    list))

(race-inline (set-kernel-ind-fillers
              get-kernel-ind-fillers
              remove-kernel-ind-fillers
              add-to-kernel-ind-table
              add-to-kernel-filler))

(defun set-kernel-ind-fillers (ind-name ind-table fillers)
  (setf (gethash ind-name ind-table) fillers))

(defun get-kernel-ind-fillers (ind-name ind-table)
  (gethash ind-name ind-table))

(defun remove-kernel-ind-fillers (ind-name ind-table)
  (remhash ind-name ind-table))

(defun get-kernel-ind-filler-elements (ind-name ind-table)
  (loop for filler in (get-kernel-ind-fillers ind-name ind-table)
        append (kernel-filler-elements filler)))

(defun add-to-kernel-ind-table (ind-name filler ind-table)
  (push filler (gethash ind-name ind-table)))

(defun add-to-kernel-filler (filler constraint)
  (push constraint (kernel-filler-elements filler)))

(defun remove-from-kernel-filler (filler constraint)
  (setf (kernel-filler-elements filler)
        (remove constraint (kernel-filler-elements filler)
                :test 'rel-constraint-equal-test))
  (setf (kernel-filler-count filler) (length (kernel-filler-elements filler))))

(defun rel-constraint-equal-test (rel-constraint-1 rel-constraint-2)
  (and (eql (constraint-ind-1 rel-constraint-1)
            (constraint-ind-1 rel-constraint-2))
       (eql (constraint-ind-2 rel-constraint-1)
            (constraint-ind-2 rel-constraint-2))
       (eq (constraint-term rel-constraint-1)
           (constraint-term rel-constraint-2))))

(defun create-kernel-filler (constraint filler-table ind-table)
  (let* ((role (constraint-term constraint))
         (ind-1 (constraint-ind-1 constraint))
         (key (list ind-1 role))
         (old-filler (gethash key filler-table))
         (filler (or old-filler (make-kernel-filler role (list constraint)))))
    (if old-filler
      (add-to-kernel-filler filler constraint)
      (progn
        (setf (gethash key filler-table) filler)
        (add-to-kernel-ind-table ind-1 filler ind-table)))
    #+:debug filler))

(defun create-kernel-predecessor (constraint filler-table ind-table)
  (let* ((role (constraint-term constraint))
         (ind-2 (constraint-ind-2 constraint))
         (key (list role ind-2))
         (old-filler (gethash key filler-table))
         (filler (or old-filler (make-kernel-filler role (list constraint)))))
    (if old-filler
      (add-to-kernel-filler filler constraint)
      (progn
        (setf (gethash key filler-table) filler)
        (add-to-kernel-ind-table ind-2 filler ind-table)))
    #+:debug filler))

(defun update-kernel-filler (removed-inds ind constraint ind-table)
  (let* ((role (constraint-term constraint))
         (new-fillers
          (if removed-inds
            (remove-obsolete-filler-elements (lambda (ind)
                                               (member ind removed-inds))
                                             ind
                                             ind-table)
            (get-kernel-ind-fillers ind ind-table)))
         (old-filler
          (find role new-fillers :key #'kernel-filler-role))
         (filler (or old-filler (make-kernel-filler role (list constraint)))))
    (if old-filler
      (unless (member constraint (kernel-filler-elements filler))
        (remove-from-kernel-filler filler constraint)
        (add-to-kernel-filler filler constraint)
        (incf (kernel-filler-count filler)))
      (add-to-kernel-ind-table ind filler ind-table))
    #+:debug filler))

(defun remove-obsolete-filler-elements (obsolete-fn ind ind-table)
  (loop with ind-changed-p = nil
        with old-fillers = (get-kernel-ind-fillers ind ind-table)
        for filler in old-fillers
        for filler-changed-p = nil
        for new-filler =
        (loop with removed-count = 0
              with old-filler-elements = (kernel-filler-elements filler)
              for rel-constraint in old-filler-elements
              when (or (funcall obsolete-fn (constraint-ind-1 rel-constraint))
                       (funcall obsolete-fn (constraint-ind-2 rel-constraint)))
              do
              (unless filler-changed-p
                (setf filler-changed-p t))
              (incf removed-count)
              and collect rel-constraint into removed-filler-elements
              finally
              (when filler-changed-p
                (setf ind-changed-p t)
                (let ((filler-elements
                       (stable-set-difference old-filler-elements
                                              removed-filler-elements)))
                  (when filler-elements
                    (let ((new-filler (copy-kernel-filler filler)))
                      (setf (kernel-filler-elements new-filler) filler-elements)
                      (decf (kernel-filler-count new-filler) removed-count)
                      (return new-filler))))))
        if new-filler
        collect new-filler into new-fillers
        and collect filler into removed-fillers
        when filler-changed-p
        collect filler into removed-fillers
        finally
        (if ind-changed-p
          (let ((remaining-fillers 
                 (nconc new-fillers
                        (stable-set-difference old-fillers removed-fillers))))
            (if remaining-fillers
              (set-kernel-ind-fillers ind ind-table remaining-fillers)
              (remove-kernel-ind-fillers ind ind-table))
            (return remaining-fillers))
          (return old-fillers))))

(defun generate-relation-store (relation-constraints &optional (symmetric-p t))
  (let ((left-ind-table (racer-make-hash-table))
        (right-ind-table (racer-make-hash-table))
        (filler-table (racer-make-hash-table :test 'equal :structure-p t))
        (only-old-inds-p t))
    (loop for constraint in relation-constraints
          do
          (when (and only-old-inds-p
                     (not (and (old-individual-p (constraint-ind-1 constraint))
                               (old-individual-p (constraint-ind-2 constraint)))))
            (setf only-old-inds-p nil))
          (create-kernel-filler constraint filler-table left-ind-table)
          (when symmetric-p
            (create-kernel-predecessor constraint filler-table right-ind-table)))
    (loop for filler being the hash-value of filler-table
          for elements = (kernel-filler-elements filler)
          when (rest elements)
          do (setf (kernel-filler-count filler) (length elements)))
    (make-relation-store (null relation-constraints)
                         left-ind-table
                         right-ind-table
                         only-old-inds-p)))

(defun get-all-rel-constraints (store)
  (when store
    (sort-relation-constraints
     (append (relation-store-added-relation-constraints store)
             (loop with removed-inds = (relation-store-removed-individuals store)
                   for fillers being the hash-value of (relation-store-left-table store)
                   using (hash-key individual)
                   unless (and removed-inds (member individual removed-inds))
                   nconc
                   (loop for filler in fillers
                         append (kernel-filler-elements filler)))))))

(defmacro loop-over-all-rel-constraints ((constraint store) &body body)
  (let ((fillers-sym (gensym))
        (filler-sym (gensym))
        (ind-sym (gensym))
        (removed-inds-sym (gensym)))
    `(when ,store
       (loop with ,removed-inds-sym = (relation-store-removed-individuals ,store)
             for ,fillers-sym being the hash-value of (relation-store-left-table ,store)
             using (hash-key ,ind-sym)
             unless (and ,removed-inds-sym (member ,ind-sym ,removed-inds-sym))
             do
             (loop for ,filler-sym in ,fillers-sym
                   do
                   (loop for ,constraint in (kernel-filler-elements ,filler-sym)
                         . ,body)))
       (loop for ,constraint in (relation-store-added-relation-constraints ,store)
             . ,body))))

(defun get-all-individuals (store)
  (when store
    (constraint-set-difference
     (stable-union (racer-remove-duplicates
		    (loop for constraint in (relation-store-added-relation-constraints store)
		          collect (constraint-ind-1 constraint)
		          collect (constraint-ind-2 constraint)))
		   (loop for individual being the hash-key of (relation-store-left-table store)
		         collect individual))
     (relation-store-removed-individuals store))))

(defun add-to-relation-store (removed-inds relation-constraints added-constraints store)
  (when added-constraints
    (setf (relation-store-added-relation-constraints store)
          (racer-merge-remove-rel-constraint-duplicates
           (append added-constraints
                   (relation-store-added-relation-constraints store))))
    #+:debug (assert (eql (length (relation-store-added-relation-constraints store))
                          (length (racer-remove-rel-constraint-duplicates
                                   (relation-store-added-relation-constraints store))))))
  (when relation-constraints
    (loop with left-table = (relation-store-left-table store)
          with right-table = (relation-store-right-table store)
          with only-old-inds-p = (relation-store-contains-only-old-inds-p store)
          for constraint in relation-constraints
          do
          (unless (or (not only-old-inds-p)
                      (and (old-individual-p (constraint-ind-1 constraint))
                           (old-individual-p (constraint-ind-2 constraint))))
            (setf only-old-inds-p nil))
          (update-kernel-filler removed-inds
                                (constraint-ind-1 constraint)
                                constraint
                                left-table)
          (update-kernel-filler removed-inds
                                (constraint-ind-2 constraint)
                                constraint
                                right-table)
          finally
          (setf (relation-store-contains-only-old-inds-p store) only-old-inds-p))))

(defun remove-inds-from-relation-store (removed-inds store)
  (let ((left-table (relation-store-left-table store))
        (right-table (relation-store-right-table store)))
    (remove-kernel-filler-2 removed-inds left-table)
    (remove-kernel-filler-1 removed-inds right-table)
    (loop for ind in removed-inds do
          (remove-kernel-ind-fillers ind left-table)
          (remove-kernel-ind-fillers ind right-table))))

(defun remove-kernel-filler-1 (removed-inds ind-table)
  (loop for ind in removed-inds do
        (loop for filler in (get-kernel-ind-fillers ind ind-table) do
              (loop for rel-constraint in (kernel-filler-elements filler)
                    for filler-ind-1 = (constraint-ind-1 rel-constraint) do
                    (remove-obsolete-filler-elements (lambda (ind)
                                                       (member ind removed-inds))
                                                     filler-ind-1
                                                     ind-table)))))
(defun remove-kernel-filler-2 (removed-inds ind-table)
  (loop for ind in removed-inds do
        (loop for filler in (get-kernel-ind-fillers ind ind-table) do
              (loop for rel-constraint in (kernel-filler-elements filler)
                    for filler-ind-2 = (constraint-ind-2 rel-constraint) do
                    (remove-obsolete-filler-elements (lambda (ind)
                                                       (member ind removed-inds))
                                                     filler-ind-2
                                                     ind-table)))))

(defun number-of-relation-fillers (ind role store)
  (if store
    (if (and (true-old-individual-p ind)
             (member ind (relation-store-removed-individuals store)))
      0
      (progn
        #+:debug (assert (not (member ind (relation-store-removed-individuals store))))
        (+ (loop for constraint in (relation-store-added-relation-constraints store)
	         when (and (eql ind (constraint-ind-1 constraint))
		           (member role (role-ancestors-internal (constraint-term constraint))))
	         sum 1)
           (loop for filler in (get-kernel-ind-fillers ind (relation-store-left-table store))
	         when (member role (role-ancestors-internal (kernel-filler-role filler)))
	         sum (kernel-filler-count filler)))))
    0))

(defun relation-filler-individuals (ind role store)
  (when store
    (unless (and (true-old-individual-p ind)
                 (member ind (relation-store-removed-individuals store)))
      #+:debug (assert (not (member ind (relation-store-removed-individuals store))))
      (loop with first-rel-constraint = nil
	    for filler in (get-kernel-ind-fillers ind (relation-store-left-table store))
	    when (member role (role-ancestors-internal (kernel-filler-role filler)))
	    nconc (mapcar #'constraint-ind-2 (kernel-filler-elements filler))
	    into inds
	    and
	    unless first-rel-constraint
	    do (setf first-rel-constraint (first (kernel-filler-elements filler)))
	    finally
	    (loop for constraint in (relation-store-added-relation-constraints store)
	          when (and (eql ind (constraint-ind-1 constraint))
			    (member role (role-ancestors-internal (constraint-term constraint))))
	          do (push (constraint-ind-2 constraint) inds)
	          and
	          unless first-rel-constraint
	          do (setf first-rel-constraint constraint))
	    (return (values nil
			    first-rel-constraint
			    (if (rest inds)
                                (racer-remove-duplicates inds)
                              inds)))))))

(defun role-successor-exists-p (ind
                                role
                                qualification
                                expanded-constraints
                                expanded-store
                                expanded-store-index
                                relation-store)
  (flet ((selector (constraint)
	   (let ((concept 
		  (if (constraint-negated-p constraint)
                    (concept-negated-concept (constraint-term constraint))
		    (constraint-term constraint))))
	     (or (eq qualification concept)
		 (and (and-concept-p concept)
		      (member qualification (concept-term concept))
		      t)))))
    (when (and relation-store
               (not (member ind (relation-store-removed-individuals relation-store))))
      (or
       (loop with top-concept = *top-concept*
             for constraint in (relation-store-added-relation-constraints relation-store)
	     thereis (and (eql ind (constraint-ind-1 constraint))
			  (member role (role-ancestors-internal (constraint-term constraint)))
			  (or (eq qualification top-concept)
			      (ind-selected-constraints-p
			       (constraint-ind-2 constraint)
			       #'selector
			       expanded-constraints
			       expanded-store
                               expanded-store-index))))
       (loop with top-concept = *top-concept*
             for filler in (get-kernel-ind-fillers ind
						   (relation-store-left-table relation-store))
	     thereis 
	     (and (member role (role-ancestors-internal (kernel-filler-role filler)))
		  (or (eq qualification top-concept)
		      (indset-selected-constraints-p
		       (racer-remove-duplicates (mapcar #'constraint-ind-2
							(kernel-filler-elements filler)))
		       #'selector
		       expanded-constraints
		       expanded-store
                       expanded-store-index))))))))

(defun relation-successor-individuals (ind role store)
  (when store
    (unless (and (true-old-individual-p ind)
                 (member ind (relation-store-removed-individuals store)))
      #+:debug (assert (not (member ind (relation-store-removed-individuals store))))
      (loop for filler in (get-kernel-ind-fillers ind (relation-store-left-table store))
	    when (member role (role-ancestors-internal (kernel-filler-role filler)))
	    nconc (mapcar #'constraint-ind-2 (kernel-filler-elements filler))
	    into inds
	    finally
	    (loop for constraint in (relation-store-added-relation-constraints store)
	          when (and (eql ind (constraint-ind-1 constraint))
			    (member role (role-ancestors-internal (constraint-term constraint))))
	          do (push (constraint-ind-2 constraint) inds))
            (if (rest inds)
                (return (racer-remove-duplicates inds))
              (return inds))))))

(defun relation-predecessor-individuals (ind role store)
  (when store
    (unless (and (true-old-individual-p ind)
                 (member ind (relation-store-removed-individuals store)))
      #+:debug (assert (not (member ind (relation-store-removed-individuals store))))
      (loop for filler in (get-kernel-ind-fillers ind (relation-store-right-table store))
            when (member role (role-ancestors-internal (kernel-filler-role filler)))
            nconc (mapcar #'constraint-ind-1 (kernel-filler-elements filler))
            into inds
	    finally 
            (loop for constraint in (relation-store-added-relation-constraints store)
	          when (and (eql ind (constraint-ind-2 constraint))
			    (member role (role-ancestors-internal (constraint-term constraint))))
	          do (push (constraint-ind-1 constraint) inds))
            (if (rest inds)
                (return (racer-remove-duplicates inds))
              (return inds))))))

(defun relation-predecessor-relation-constraints (ind role store)
  (when store
    (unless (and (true-old-individual-p ind)
                 (member ind (relation-store-removed-individuals store)))
      #+:debug (assert (not (member ind (relation-store-removed-individuals store))))
      (loop for filler in (get-kernel-ind-fillers ind (relation-store-right-table store))
            when (member role (role-ancestors-internal (kernel-filler-role filler)))
            append (kernel-filler-elements filler) into relation-constraints
	    finally 
            (loop for constraint in (relation-store-added-relation-constraints store)
	          when (and (eql ind (constraint-ind-2 constraint))
			    (member role (role-ancestors-internal (constraint-term constraint))))
	          do (push constraint relation-constraints))
            (if (rest relation-constraints)
                (return (racer-remove-rel-constraint-duplicates relation-constraints))
              (return relation-constraints))))))

(defun predecessor-individuals (ind store)
  (when store
    #+:debug (assert (not (member ind (relation-store-removed-individuals store))))
    (unless (and (true-old-individual-p ind)
                 (member ind (relation-store-removed-individuals store)))
      (loop for filler in (get-kernel-ind-fillers ind (relation-store-right-table store))
            nconc (mapcar #'constraint-ind-1 (kernel-filler-elements filler))
            into inds
	    finally
            (loop for constraint in (relation-store-added-relation-constraints store)
	          when (eql ind (constraint-ind-2 constraint))
	          do (push (constraint-ind-1 constraint) inds))
            (if (rest inds)
                (return (racer-remove-duplicates inds))
              (return inds))))))

(defun is-left-individual-p (ind store)
  (when store
    (unless (and (true-old-individual-p ind)
                 (member ind (relation-store-removed-individuals store)))
      #+:debug (assert (not (member ind (relation-store-removed-individuals store))))
      (or (loop for constraint in (relation-store-added-relation-constraints store)
	        thereis (eql ind (constraint-ind-1 constraint)))
	  (get-kernel-ind-fillers ind (relation-store-right-table store))))))

(defun successor-individuals (ind store)
  (when store
    (unless (and (true-old-individual-p ind)
                 (member ind (relation-store-removed-individuals store)))
      #+:debug (assert (not (member ind (relation-store-removed-individuals store))))
      (loop for filler in (get-kernel-ind-fillers ind (relation-store-left-table store))
            nconc (mapcar #'constraint-ind-2 (kernel-filler-elements filler))
            into inds
	    finally
            (loop for constraint in (relation-store-added-relation-constraints store)
	          when (eql ind (constraint-ind-1 constraint))
	          do (push (constraint-ind-2 constraint) inds))
            (if (rest inds)
                (return (racer-remove-duplicates inds))
              (return inds))))))

(defun collect-relation-fillers (indset
                                     role
                                     store
                                     &optional (filler-predicate-fn nil))
  (when store
    #+:debug
    (assert (if (listp indset)
              (lists-disjoint-p indset (relation-store-removed-individuals store))
              (not (member indset (relation-store-removed-individuals store)))))
    (loop with result = nil
	  with new-indset = (if (listp indset)
			      indset
			      (list indset))
          for ind in new-indset
          for rel-constraints = 
          (loop for filler in (get-kernel-ind-fillers ind (relation-store-left-table store))
                for filler-role = (kernel-filler-role filler)
                when (if filler-predicate-fn
                       (funcall filler-predicate-fn filler-role)
                       (member role (role-ancestors-internal filler-role)))
                append (kernel-filler-elements filler))
          do (setf result (constraint-set-union rel-constraints result))
	  finally
	  (loop for constraint in (relation-store-added-relation-constraints store)
	        for filler-role = (constraint-term constraint)
	        when (and (member (constraint-ind-1 constraint) new-indset)
			  (if filler-predicate-fn
			    (funcall filler-predicate-fn filler-role)
			    (member role (role-ancestors-internal filler-role))))
	        collect constraint into rel-constraints
	        finally (setf result (constraint-set-union rel-constraints result)))
	  (return result))))

(defun collect-relation-filler-constraints (ind store)
  (when store
    #+:debug (assert (not (member ind (relation-store-removed-individuals store))))
    (unless (and (true-old-individual-p ind)
                 (member ind (relation-store-removed-individuals store)))
      (racer-remove-duplicates
       (nconc (loop for constraint in (relation-store-added-relation-constraints store)
	            when (eql ind (constraint-ind-1 constraint))
	            collect constraint)
	      (loop for filler in (get-kernel-ind-fillers ind (relation-store-left-table store))
		    append (kernel-filler-elements filler)))))))

(defun collect-same-ind-pair-disjoint-relation-filler-constraints (abox ind store)
  (when store
    #+:debug (assert (not (member ind (relation-store-removed-individuals store))))
    (when (dl-disjoint-roles (abox-language abox))
      (unless (and (true-old-individual-p ind)
                   (member ind (relation-store-removed-individuals store)))
        (let ((table (smart-clrhash *stable-set-difference-table* nil '*stable-set-difference-table*))
              (reflexive-roles-p (dl-reflexive-roles (abox-language abox))))
          (loop for constraint in (relation-store-added-relation-constraints store)
                when (and (eql ind (constraint-ind-1 constraint))
                          (some #'role-disjoint-roles (role-ancestors-internal (constraint-term constraint))))
                do (push constraint (gethash (constraint-ind-2 constraint) table)))
          (loop for filler in (get-kernel-ind-fillers ind (relation-store-left-table store))
                do
                (loop for constraint in (kernel-filler-elements filler)
                      when (some #'role-disjoint-roles (role-ancestors-internal (constraint-term constraint)))
                      do (push constraint (gethash (constraint-ind-2 constraint) table))))
          (loop for same-pair-list being the hash-value of table
                when (or (rest same-pair-list)
                         (and reflexive-roles-p
                              (let ((constraint (first same-pair-list)))
                                (eql (constraint-ind-1 constraint) (constraint-ind-2 constraint)))))
                collect same-pair-list))))))

(defun reverse-ind-pair-asymmetric-relation-filler-constraints-p (abox ind store)
  (when store
    #+:debug (assert (not (member ind (relation-store-removed-individuals store))))
    (when (dl-asymmetric-roles (abox-language abox))
      (unless (and (true-old-individual-p ind)
                   (member ind (relation-store-removed-individuals store)))
        (let ((asymmetric-fillers nil))
          (loop for constraint in (relation-store-added-relation-constraints store)
                when (and (eql ind (constraint-ind-1 constraint))
                          (some #'role-asymmetric-p (role-ancestors-internal (constraint-term constraint))))
                do (push constraint asymmetric-fillers))
          (loop for filler in (get-kernel-ind-fillers ind (relation-store-left-table store))
                do
                (loop for constraint in (kernel-filler-elements filler)
                      when (some #'role-asymmetric-p (role-ancestors-internal (constraint-term constraint)))
                      do (push constraint asymmetric-fillers)))
          (loop with culprit-1 = nil
                with culprit-2 = nil
                for constraint-1 in asymmetric-fillers
                for ind-2 = (constraint-ind-2 constraint-1)
                for role = (constraint-term constraint-1)
                when (or (loop for constraint-2 in (relation-store-added-relation-constraints store)
                               when (and (eql (constraint-ind-1 constraint-2) ind-2)
                                         (eql (constraint-ind-2 constraint-2) ind)
                                         (member role (role-ancestors-internal (constraint-term constraint-2))))
                               do
                               (setf culprit-1 constraint-1)
                               (setf culprit-2 constraint-2)
                               (return t))
                         (loop for filler in (get-kernel-ind-fillers ind-2 (relation-store-left-table store))
                               thereis
                               (loop for constraint-2 in (kernel-filler-elements filler)
                                     when (and (eql (constraint-ind-2 constraint-2) ind)
                                               (member role
                                                       (role-ancestors-internal (constraint-term constraint-2))))
                                     do
                                     (setf culprit-1 constraint-1)
                                     (setf culprit-2 constraint-2)
                                     (return t))))
                do (return (values t culprit-1 culprit-2))))))))

(defun collect-ind-fillers (ind store)
  (when store
    #+:debug (assert (not (member ind (relation-store-removed-individuals store))))
    (unless (and (true-old-individual-p ind)
                 (member ind (relation-store-removed-individuals store)))
      (nconc (loop for constraint in (relation-store-added-relation-constraints store)
	           when (eql ind (constraint-ind-1 constraint))
	           collect constraint)
             (get-kernel-ind-filler-elements ind (relation-store-left-table store))))))

(defun collect-all-fillers (ind-set store)
  (when store
    (loop with result = nil
	  with left-table = (relation-store-left-table store)
	  with right-table = (relation-store-right-table store)
	  for ind in (stable-set-difference ind-set (relation-store-removed-individuals store))
	  do
          (setf result (constraint-set-union (get-kernel-ind-filler-elements ind left-table) result))
          (setf result (constraint-set-union (get-kernel-ind-filler-elements ind right-table) result))
	  finally
	  (return
           (constraint-set-union (loop for constraint in (relation-store-added-relation-constraints store)
                                       when (or (member (constraint-ind-1 constraint) ind-set)
                                                (member (constraint-ind-2 constraint) ind-set))
                                       collect constraint)
                                 result)))))

(defun remove-from-relation-store (removed-inds new-store)
  (let ((only-old-inds-p (relation-store-contains-only-old-inds-p new-store)))
    (if *relation-store-virtual-removal*
      (setf (relation-store-removed-individuals new-store)
            (stable-union removed-inds (relation-store-removed-individuals new-store)))
      (remove-inds-from-relation-store removed-inds new-store))
    (loop for constraint in (relation-store-added-relation-constraints new-store)
          when (or (member (constraint-ind-1 constraint) removed-inds)
                   (member (constraint-ind-2 constraint) removed-inds))
          collect constraint into obsolete-constraints
          finally
          (when obsolete-constraints
            (setf (relation-store-added-relation-constraints new-store)
                  (constraint-set-difference (relation-store-added-relation-constraints new-store)
                                             obsolete-constraints))))
    (unless only-old-inds-p
      (loop for ind being the hash-key of (relation-store-left-table new-store)
            unless (and (old-individual-p ind) (not (member ind removed-inds)))
            do (return)
            finally (setf only-old-inds-p t))
      (when only-old-inds-p
        (setf (relation-store-contains-only-old-inds-p new-store) t)))))
 
(defun update-relation-store (removed-inds new-rel-constraints state &optional (copy-p t))
  (let* ((store (state-relation-store state)))
    #+:debug (assert store)
    (multiple-value-bind (relation-constraints added-constraints)
                         (when new-rel-constraints
                           (loop for constraint in new-rel-constraints
                                 if (and (old-individual-p (constraint-ind-1 constraint))
                                         (old-individual-p (constraint-ind-2 constraint)))
                                 collect constraint into relation-constraints
                                 else collect constraint into added-constraints
                                 finally (return (values relation-constraints added-constraints))))
      (let* ((new-store
              (if (or (relation-store-protected-p store)
                      (and copy-p (state-copy-relation-store-p state)))
                (progn
                  (setf (state-copy-relation-store-p state) nil)
                  (copy-relation-store store
                                       (or removed-inds
                                           (not (null relation-constraints)))))
                store))
             (left-table (relation-store-left-table new-store))
             (right-table (relation-store-right-table new-store)))
        (when removed-inds
          (remove-from-relation-store removed-inds new-store))
        (when new-rel-constraints
          (add-to-relation-store removed-inds relation-constraints added-constraints new-store))
        (if (and (eql (hash-table-count left-table) 0)
                 (eql (hash-table-count right-table) 0)
                 (null (relation-store-added-relation-constraints new-store)))
          (setf (relation-store-is-empty-p new-store) t)
          (setf (relation-store-is-empty-p new-store) nil))
        new-store))))

(defun clean-relation-store (live-fn state)
  (let* ((store (state-relation-store state))
         (only-old-inds-p (relation-store-contains-only-old-inds-p store)))
    #+:debug (assert store)
    (if only-old-inds-p
      (if (null (relation-store-added-relation-constraints store))
        store
        (let* ((new-store (if (state-copy-relation-store-p state)
                            (copy-relation-store store nil)
                            store))
               (left-table (relation-store-left-table new-store))
               (right-table (relation-store-right-table new-store)))
          (loop for constraint in (relation-store-added-relation-constraints new-store)
                for ind-1 = (constraint-ind-1 constraint)
		unless (or (old-individual-p ind-1) (funcall live-fn ind-1))
		collect constraint into obsolete-constraints
		finally
                (when obsolete-constraints
                  (setf (relation-store-added-relation-constraints new-store)
		        (constraint-set-difference (relation-store-added-relation-constraints new-store)
					           obsolete-constraints))))
          (if (and (eql (hash-table-count left-table) 0)
                   (eql (hash-table-count right-table) 0)
                   (null (relation-store-added-relation-constraints new-store)))
            (progn
              (setf (relation-store-is-empty-p store) t)
              (setf (relation-store-contains-only-old-inds-p new-store) t)))
          new-store))
      (let* ((new-store (if (state-copy-relation-store-p state)
                          (progn
                            (setf (state-copy-relation-store-p state) nil)
                            (copy-relation-store store t))
                          store))
             (left-table (relation-store-left-table new-store))
             (right-table (relation-store-right-table new-store))
             (obsolete-fn (complement live-fn)))
	(loop for constraint in (relation-store-added-relation-constraints new-store)
                for ind-1 = (constraint-ind-1 constraint)
		unless (or (old-individual-p ind-1) (funcall live-fn ind-1))
              collect constraint into obsolete-constraints
              finally
              (when obsolete-constraints
                (setf (relation-store-added-relation-constraints new-store)
		      (constraint-set-difference (relation-store-added-relation-constraints new-store)
					         obsolete-constraints))))
        (loop for individual being the hash-key of left-table
              do
              (if (funcall live-fn individual)
                (remove-obsolete-filler-elements obsolete-fn individual left-table)
                (remove-kernel-ind-fillers individual left-table)))
        (loop for individual being the hash-key of right-table
              do
              (if (funcall live-fn individual)
                (remove-obsolete-filler-elements obsolete-fn individual right-table)
                (remove-kernel-ind-fillers individual right-table)))
        (when *relation-store-virtual-removal*
          (setf (relation-store-removed-individuals new-store)
                (loop for individual in (relation-store-removed-individuals new-store)
                      when (funcall live-fn individual)
                      collect individual)))
        (if (and (eql (hash-table-count left-table) 0)
                 (eql (hash-table-count right-table) 0)
		 (null (relation-store-added-relation-constraints store)))
          (progn
            (setf (relation-store-is-empty-p new-store) t)
            (setf (relation-store-contains-only-old-inds-p new-store) t))
          (unless only-old-inds-p
            (loop for ind being the hash-key of left-table
                  unless (old-individual-p ind) do
                  (return)
                  finally (setf only-old-inds-p t))
            (when only-old-inds-p
              (setf (relation-store-contains-only-old-inds-p new-store) t))))
        ;(break)
        new-store))))

(defun is-filler-of-ind-p (ind-1 ind-2 role store)
  (when store
    (unless (or (and (true-old-individual-p ind-1)
                     (member ind-1 (relation-store-removed-individuals store)))
                (and (true-old-individual-p ind-2)
                     (member ind-2 (relation-store-removed-individuals store))))
      #+:debug (assert (not (member ind-1 (relation-store-removed-individuals store))))
      #+:debug (assert (not (member ind-2 (relation-store-removed-individuals store))))
      (or (loop for constraint in (relation-store-added-relation-constraints store)
	        thereis (and (eql ind-1 (constraint-ind-1 constraint))
			     (eql ind-2 (constraint-ind-2 constraint))
			     (member role (role-ancestors-internal (constraint-term constraint)))))
	  (loop for filler in (get-kernel-ind-fillers ind-1 (relation-store-left-table store))
	        thereis (and (member role (role-ancestors-internal (kernel-filler-role filler)))
			     (member ind-2 (kernel-filler-elements filler)
				     :key #'constraint-ind-2)))))))

(defun get-filler-roles-of-ind (ind store &optional (features-only-p t))
  (when store
    (unless (and (true-old-individual-p ind)
                 (member ind (relation-store-removed-individuals store)))
      #+:debug (assert (not (member ind (relation-store-removed-individuals store))))
      (racer-remove-duplicates
       (nconc (loop for constraint in (relation-store-added-relation-constraints store)
		    for role = (constraint-term constraint)
		    when (and (eql ind (constraint-ind-1 constraint))
			      (or (not features-only-p) (role-feature-p role)))
		    collect (constraint-term constraint))
	      (loop for left-filler in (get-kernel-ind-fillers ind (relation-store-left-table store))
                    for right-filler in (get-kernel-ind-fillers ind (relation-store-right-table store))
		    for left-role = (kernel-filler-role left-filler)
		    for right-role = (kernel-filler-role right-filler)
		    when (or (not features-only-p) (role-feature-p left-role))
		    collect left-role
		    when (or (not features-only-p) (role-feature-p right-role))
		    collect right-role))))))

(defun role-find-if-in-relation-store (ind role store)
  (when (and store (not (member ind (relation-store-removed-individuals store))))
    (or (loop for constraint in (relation-store-added-relation-constraints store)
	      thereis (and (eql ind (constraint-ind-1 constraint))
			   (member role (role-ancestors-internal (constraint-term constraint)))))
        (loop for filler in (get-kernel-ind-fillers ind (relation-store-left-table store))
              thereis (member role (role-ancestors-internal (kernel-filler-role filler)))))))

(defun clash-in-relation-store-p (abox store)
  (when store
    (let ((use-unique-name-assumption *use-unique-name-assumption*))
      (or (loop with ind-table = (when use-unique-name-assumption
                                   (racer-make-hash-table))
                for relation-constraint in (relation-store-added-relation-constraints store)
                for ind-1 = (constraint-ind-1 relation-constraint)
                for ind-2 = (constraint-ind-2 relation-constraint)
                for role = (constraint-term relation-constraint)
                do
                (when (clash-in-relation-constraint-p-1 abox
                                                        ind-1
                                                        ind-2
                                                        (when (true-old-individual-p ind-2)
                                                          (find-individual abox ind-2))
                                                        role)
                  (return t))
                (when use-unique-name-assumption
                  (let* ((ind-1-feature-set (gethash ind-1 ind-table))
                         (old-entry (find role ind-1-feature-set :key #'constraint-term)))
                    (if old-entry
                        (return t)
                      (when (role-feature-p role)
                        (pushnew relation-constraint (gethash ind-1 ind-table)))))))
          (loop for fillers being the hash-value of (relation-store-left-table store)
                thereis
                (loop for filler in fillers
                      for role = (kernel-filler-role filler)
                      thereis 
                      (or (is-bottom-object-role-p role)
                          (is-bottom-datatype-role-p role)
                          (and (role-datatype role)
                               (loop for constraint in (kernel-filler-elements filler)
                                     for ind-2 = (find-individual abox (constraint-ind-2 constraint) nil)
                                     thereis
                                     (and ind-2 
                                          (not (or (individual-attribute-assertions ind-2)
                                                   (and (individual-encoded-axioms ind-2)
                                                        (member-if #'cd-concept-p
                                                                   (individual-encoded-axioms ind-2)
                                                                   :key #'second)))))))
                          (and use-unique-name-assumption
                               (role-feature-p role)
                               (> (kernel-filler-count filler) 1))
                          (and (role-irreflexive-p role)
                               (loop for constraint in (kernel-filler-elements filler)
                                     thereis (eql (constraint-ind-1 constraint)
                                                  (constraint-ind-2 constraint)))))))
          (loop with tbox = (abox-tbox abox)
                with reflexive-roles-p = (dl-reflexive-roles (abox-language abox))
                for ind-1 being the hash-key of (relation-store-left-table store)
                thereis
                (or (let ((same-ind-pair-constraints
                           (collect-same-ind-pair-disjoint-relation-filler-constraints abox ind-1 store)))
                      (loop for constraint-set in same-ind-pair-constraints
                            for all-roles = (mapcar #'constraint-term constraint-set)
                            thereis
                            (or (disjoint-roles-in-role-conjunction-clash-p tbox all-roles)
                                (loop for constraint in constraint-set
                                      thereis
                                      (or (and reflexive-roles-p
                                               (eql (constraint-ind-1 constraint) (constraint-ind-2 constraint))
                                               (member-if (lambda (roles)
                                                            (some #'role-reflexive-p roles))
                                                          (role-ancestors-internal (constraint-term constraint))
                                                          :key #'role-disjoint-roles))
                                          (loop for ancestor in (role-ancestors-internal (constraint-term constraint))
                                                thereis
                                                (not (role-set-disjoint-p (role-disjoint-roles ancestor)
                                                                          all-roles))))))))
                    (reverse-ind-pair-asymmetric-relation-filler-constraints-p abox ind-1 store)))))))

(defun asymmetric-role-clash-in-extended-relation-store-p (abox store added-relation-constraints)
  (let ((affected-inds
         (racer-remove-duplicates
          (loop for constraint in added-relation-constraints
                collect (constraint-ind-1 constraint)
                collect (constraint-ind-2 constraint)))))
    (loop for ind in affected-inds do
          (multiple-value-bind (clash-p culprit-1 culprit-2)
              (reverse-ind-pair-asymmetric-relation-filler-constraints-p abox ind store)
            (when clash-p
              (return (values clash-p culprit-1 culprit-2)))))))

(defun collect-role-domain-range-restrictions (store)
  (when store
    (let ((rel-table (racer-make-hash-table :test 'equal :structure-p t)))
      (nconc 
       (loop for constraint in (relation-store-added-relation-constraints store)
             for role = (constraint-term constraint)
             for domain = (role-domain-restriction role)
             for range = (role-range-restriction role)
             for rel-constraint = (when (or domain range)
                                    constraint)
             for ind-1 = (when rel-constraint
                           (constraint-ind-1 rel-constraint))
             for ind-2 = (when rel-constraint
                           (constraint-ind-2 rel-constraint))
             for key = (when rel-constraint
                         (cons ind-1 role))
             for found = (when rel-constraint
                           (gethash key rel-table))
             when (and domain (not found) (not (gethash (list ind-1 domain) rel-table)))
             collect (concluded-concept-constraint ind-1 domain rel-constraint)
             and do (setf (gethash (list ind-1 domain) rel-table) t)
             when (and range (not found) (not (gethash (list ind-2 range) rel-table)))
             collect (concluded-concept-constraint ind-2 range rel-constraint)
             and do (setf (gethash (list ind-2 range) rel-table) t)
             when (and (or domain range) (not found)) do 
             (setf (gethash (cons ind-2 (role-inverse-internal role)) rel-table) t))
       (loop with left-table = (relation-store-left-table store)
	     for fillers being the hash-value of left-table
	     nconc
             (loop for filler in fillers
	           for role = (kernel-filler-role filler)
	           for domain = (role-domain-restriction role)
	           for range = (role-range-restriction role)
	           for rel-constraint = (when (or domain range)
				          (first (kernel-filler-elements filler)))
	           for ind-1 = (when rel-constraint
			         (constraint-ind-1 rel-constraint))
	           for ind-2 = (when rel-constraint
			         (constraint-ind-2 rel-constraint))
	           for key = (when rel-constraint
			       (cons ind-1 role))
	           for found = (when rel-constraint
			         (gethash key rel-table))
                   when (and domain (not found) (not (gethash (list ind-1 domain) rel-table)))
                   collect (concluded-concept-constraint ind-1 domain rel-constraint)
                   and do (setf (gethash (list ind-1 domain) rel-table) t)
                   when (and range (not found) (not (gethash (list ind-2 range) rel-table)))
                   collect (concluded-concept-constraint ind-2 range rel-constraint)
                   and do (setf (gethash (list ind-2 range) rel-table) t)
	           when (and (or domain range) (not found)) do 
                   (setf (gethash (cons ind-2 (role-inverse-internal role)) rel-table) t)))))))
