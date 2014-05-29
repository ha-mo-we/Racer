;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: THEMATIC-SUBSTRATE; Base: 10 -*-

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

(in-package :thematic-substrate)

(defun delete-all-dboxes ()
  (setf *all-dboxes* nil))

(defpersistentclass defined-query ()
  ((name :reader name :initarg :name)
   (head :reader head :initarg :head)
   (body :reader body :initarg :body)
   (query :reader query :initarg :query)
   (activated-p :reader activated-p :initarg :activated-p :initform t)))

(defpersistentclass dbox ()
  ((name-to-def-hash :accessor name-to-def-hash 
                     :initform (mht :size 30))
   (for-tbox :accessor for-tbox 
             :initarg :for-tbox)))

;;;
;;;
;;;

(defun create-dbox (&optional (tbox (current-tbox)))
  (let ((dbox (make-instance 'dbox :for-tbox tbox)))
    (push dbox *all-dboxes*)
    dbox))

(defmethod create-alias-dbox (of-tbox &optional (for-tbox (current-tbox)))
  (let* ((dbox (find-dbox of-tbox)))
    (cond ((find-dbox for-tbox :error-p nil)

           (nrql-error "DBOX for TBox ~A already exists" for-tbox))

          (t 
           (let ((new-dbox (create-dbox for-tbox)))
             (setf (name-to-def-hash new-dbox)
                   (name-to-def-hash dbox))

             new-dbox)))))


(defmethod create-copy-dbox (of-tbox &optional (for-tbox (current-tbox)))
  (let* ((dbox (find-dbox of-tbox)))
    (cond ((find-dbox for-tbox :error-p nil)

           (nrql-error "DBOX for TBox ~A already exists" for-tbox))

          (t 
           (let ((new-dbox (create-dbox for-tbox)))

             (maphash #'(lambda (name def)
                          (declare (ignorable name))
                          (dolist (def def)
                            (register-defined-query def new-dbox)))
                      (name-to-def-hash dbox))

             new-dbox)))))

(defun find-dbox (for-tbox &key (error-p t))
  (let ((for-tbox (or for-tbox 
                      *nrql-tbox*
                      (current-tbox))))
    (or (find for-tbox *all-dboxes* :key #'for-tbox)
        (when error-p 
          (nrql-error "Can't find DBOX for TBox ~A" for-tbox)))))

(defun delete-dbox (for-tbox)
  (let ((dbox (find-dbox for-tbox)))
    (when dbox
      (setf *all-dboxes*
            (delete dbox *all-dboxes*)))))

(defmethod register-defined-query ((def defined-query) (dbox dbox))
  (if (gethash (name def)
               (name-to-def-hash dbox))
      (setf (gethash (name def)
                     (name-to-def-hash dbox))
            (append (gethash (name def)
                             (name-to-def-hash dbox))
                    (list def)))
    (setf (gethash (name def)
                   (name-to-def-hash dbox))
          (list def))))

(defmethod delete-defined-query ((def defined-query) (dbox dbox))
  (setf (gethash (name def)
                 (name-to-def-hash dbox))
        (delete def 
                (gethash (name def)
                         (name-to-def-hash dbox))))
  (unless (gethash (name def)
                   (name-to-def-hash dbox))
    (remhash (name def)
             (name-to-def-hash dbox))))

(defmethod delete-defined-query ((def list) (dbox dbox))
  (mapc #'(lambda (def) 
            (delete-defined-query def dbox))
        def))

;;;
;;;
;;;

(defmethod all-names-of-defined-queries ((dbox dbox))
  (loop as name being the hash-key of (name-to-def-hash dbox)
        collect name))

;;;
;;;
;;;

(defmethod delete-all-definitions1 ((dbox dbox))
  (clrhash (name-to-def-hash dbox)))

;;;
;;;
;;; 

(defmethod get-definition1 ((substrate substrate) name arity &key (error-p t))
  (declare (ignorable name arity))
  (when error-p 
    (nrql-error "No definitions possible for substrate ~A" substrate))
  nil)

(defmethod get-definition1 ((substrate racer-substrate) name arity &key (error-p t))
  (get-definition1 (dbox substrate) name arity :error-p error-p))

(defmethod get-definition1 ((dbox dbox) name arity &key (error-p t) also-deactivated-p)
  (or
   ;;; gibt nur eine pro Arity! 
   (if (and arity 
            (numberp arity))
       (let ((defs
              (remove-if-not #'(lambda (x)
                                 (and (or (activated-p x) also-deactivated-p)
                                      (= (length (head x)) arity)))
                             (gethash name (name-to-def-hash dbox)))))
         (if (cdr defs)
             defs
           (first defs)))
     (gethash name (name-to-def-hash dbox)))
   (when error-p 
     (nrql-error "Can't find definition ~A of arity ~A in DBox for TBox ~A" name arity (for-tbox dbox)))))
  
;;;
;;;
;;;

(defmethod describe-definition1 ((substrate racer-substrate) name arity &key (error-p t))
  (describe-definition1 (dbox substrate) name arity :error-p error-p))

(defmethod describe-definition1 ((dbox dbox) (name symbol) arity  &key (error-p t))
  (let ((def (get-definition1 dbox name arity  :error-p error-p)))
    (if (listp def)
        (mapcar #'(lambda (def) 
                    (describe-definition1 dbox def nil))
                def)
      (describe-definition1 dbox def nil))))

(defmethod describe-definition1 ((dbox dbox) (def defined-query) arity &key (error-p t))
  (declare (ignore arity error-p))
  (if (activated-p def)
      `(defquery ,(name def) ,(head def) ,(body def))
    `(deactivated (defquery ,(name def) ,(head def) ,(body def)))))

;;;
;;;
;;;

(defmethod check-for-name-clash  ((substrate racer-substrate) name args &key warn-p)
  (let* ((def (get-definition1 substrate name (length args)))
	 (old-tbox (tbox substrate))
	 (tbox (or old-tbox (mirror-of-tbox substrate))))
    
    (unless old-tbox
      (setf (slot-value substrate 'tbox) tbox))
    
    (unwind-protect
        
	(when def
          (when (and (not (cdr args)) ; (?x C) ?
                     (find name
                           (dl-all-atomic-concepts substrate)
                           :key #'ensure-list
                           :test #'member))
            (when warn-p 
              (nrql-warning "Concept ~A exists in TBox ~A. Assuming you are referring to the concept ~A" 
                            name tbox name))
            (return-from check-for-name-clash t))

          (when (and (not (cddr args)) ; (?x ?y R) ?
                     (or (find name
                               (dl-all-roles substrate))
                         (find name
                               +equal-roles+)
                         (find name
                               +different-from-roles+)))
            (when warn-p 
              (nrql-warning "Role ~A exists in TBox ~A. Assuming you are referring to the role ~A" 
                            name tbox name))
            (return-from check-for-name-clash t)))
      
      (setf (slot-value substrate 'tbox) old-tbox))))

;;;
;;;
;;;

(defmethod get-variable-substituted-query ((substrate racer-substrate) name args
                                           &key parser)
  (let ((def (get-definition1 substrate name (length args)))
        (ano-counter *ano-counter*))

    (labels ((do-it (def)
               (substitute-vois-in 
                (body def)
                (append (mapcar #'(lambda (old new) 
                                    (list old new))
                                (head def)
                                args) 
                                                       
                        (mapcar #'(lambda (old new) 
                                    (list (get-corresponding-voi parser old)
                                          (get-corresponding-voi parser new)))
                                (head def)
                                args)
                                                       
                        (mapcar #'(lambda (old) 
                                    (list old 
                                          #| (if (substrate-thing-p parser old)
                                              (if (aux-var-p parser old)
                                                  (intern (format nil "$?*INT-ANO~A" (incf *ano-counter*)))
                                                (intern (format nil "?*INT-ANO~A" (incf *ano-counter*))))
                                            (if (aux-var-p parser old)
                                                (intern (format nil "$?INT-ANO~A" (incf *ano-counter*)))
                                              (intern (format nil "?INT-ANO~A" (incf *ano-counter*))))) |# 
                                          (get-ano-var old)))
                                (set-difference (get-vois-from parser (body def))
                                                (append (head def)
                                                        (mapcar #'(lambda (x) 
                                                                    (get-corresponding-voi parser x))
                                                                (head def))))))
                :parser parser
                :substitution-list-only-p nil
                :dont-anonymize-inds-p t)))
                           

      (when def

        (check-for-name-clash substrate name args)

        (let ((def (ensure-list def)))
              
          (if (cdr def)
              
              `(union ,@(mapcar #'(lambda (def)
                       
                                    ;;; wichtig, verhindert folgenden Effekt: 
                                    ;;; (DEFQUERY R (?X ?Y) (AND (?X ?Z R1) (?Z ?Y R2)))
                                    ;;; (DEFQUERY R (?X ?Y) (AND (?Y ?X S1) (?X ?Z R2)))
                                    ;;; 
                                    ;;; (retrieve-with-explanation () (?x ?y r))
                                    ;;; 
                                    ;;; ohne "festhalten" von *ano-counter*: 
                                    ;;; 
                                    ;;; (RETRIEVE (?X-ANO5 ?Y-ANO4 ?Z-ANO1-ANO6 ?Z-ANO2-ANO3)
                                    ;;;       (UNION (AND (?X-ANO5 ?Z-ANO1-ANO6 R2) (?Y-ANO4 ?X-ANO5 S1) (TOP ?Z-ANO2-ANO3))
                                    ;;;              (AND (?Z-ANO2-ANO3 ?Y-ANO4 R2) (?X-ANO5 ?Z-ANO2-ANO3 R1) (TOP ?Z-ANO1-ANO6)))
                                    ;;;
                                    ;;; mit Bindung von *ano-counter*: 
                                    ;;; 
                                    ;;; (RETRIEVE (?X-ANO2 ?Y-ANO1 ?Z-ANO1-ANO3)
                                    ;;;       (UNION (AND (?X-ANO2 ?Z-ANO1-ANO3 R2) (?Y-ANO1 ?X-ANO2 S1))
                                    ;;;                   (AND (?Z-ANO1-ANO3 ?Y-ANO1 R2) (?X-ANO2 ?Z-ANO1-ANO3 R1)))
                                    ;;; 
                                    ;;; also 1 Variable weniger! Die BOEMIES erzeugen UNION-Queries mit 10 Disjunkten (!)
                                    ;;; erzeugen bis zu 20 neue Variablen sonst, da kommt nRQL sonst nicht durch
                                    ;;; 
                                    ;;; dieses "Sharing" von anonymen Variablen ist nat�rlich nur f�r UNION
                                    ;;; zul�ssig! 

                                    (let ((*ano-counter* ano-counter))
                                      (do-it def)))
                           
                                def))
                      
            (do-it (first def))))))))
                     
;;;
;;;
;;;

(defmethod describe-all-definitions1 ((dbox dbox))
  (let ((res nil))
    (maphash #'(lambda (name def)
                 (declare (ignore name))
                 (dolist (def def)
                   (push (describe-definition1 dbox def nil)
                         res)))
             (name-to-def-hash dbox))
    (reverse res)))

;;;
;;;
;;;

(defun define-query1 (name head body &rest args 
                           &key keep-p 
			   (tbox (or *nrql-tbox* (current-tbox)))
			   (consider-head-atom-for-consistency-check-p t)
			   (allow-multiple-definitions-p *allow-multiple-definitions-p*)
                           &allow-other-keys)
  
  (let* ((dbox (or (find-dbox tbox :error-p nil)
                   (create-dbox tbox)))
         (def (get-definition1 dbox name 
                               (length head)
                               :error-p nil)))

    (when (and *always-prefer-defined-queries-p*
               (find-if #'(lambda (x)
                            (equal (first (last x))  
                                   name))
                        (if (consp (first body))
                            body
                          (list body))))

      (unless *lazy-query-unfolding-p*
        (parser-error 
         "Cyclic definition ~A detected" name)))
    
    (if (and def (not allow-multiple-definitions-p))

        (nrql-error "Definition ~A of arity ~A already exists" name (length head))

      (if keep-p 
        
          (let* ((query
		  (apply #'racer-prepare-query head body args))
		 (query1 (when (consp query)
			   (find-query (first query)))))
	    
	    (when (and query1 *report-inconsistent-queries-p*
		       (query-inconsistent-p query1
					     :rule-con-pattern
					     (when consider-head-atom-for-consistency-check-p 
					       (cond ((= (length head) 1)
						      `((instance ,(first head) ,name)))
						     ((= (length head) 2)
						      `((related ,(first head) ,(second head) ,name)))))))
              (nrql-warning "Defined query ~S - inconsistency found:~%~S" 
                            `(,name ,@head) body))
	    
	    ;; (break "~S" (describe-query (first query)))

            (register-defined-query (make-instance 'defined-query 
                                                   :name name :head head :body body
                                                   :query query)
                                    dbox)
            query)
        
        (let ((*use-repository-p* nil)
              (*put-into-repository-p* nil)
	      ;; (*report-tautological-queries-p* nil)
	      ;; (*report-inconsistent-queries-p* nil)
              (*optimize-p* nil)
              (*generate-code-p* nil)
              (*rewrite-semantically-p* nil)
	      ;; (*rewrite-to-dnf-p* nil)
              )

          (let* ((query
		  (apply #'racer-prepare-query head body args))
		 (query1 (when (consp query)
			   (find-query (first query)))))
	    
            (unless (eq query ':timeout) ;;VH I am not sure whether this is a correct timeout recovery
              (when (and query1 *report-inconsistent-queries-p*
                         (query-inconsistent-p query1
                                               :rule-con-pattern
                                               (when consider-head-atom-for-consistency-check-p 
                                                 (cond ((= (length head) 1)
                                                        `((instance ,(first head) ,name)))
                                                       ((= (length head) 2)
                                                        `((related ,(first head) ,(second head) ,name)))))))
                (nrql-warning "Defined query ~S - inconsistency found:~%~S" 
                              `(,name ,@head) body))
              
              ;;; (break "~S" (describe-query (first query)))
              
              (register-defined-query (make-instance 'defined-query 
                                        :name name :head head :body body
                                        :query query)
                                      dbox)
              (delete-query (first query)))
            
            name))))))

;;;
;;;
;;;

(defun undefine-query1 (name arity &key (tbox (or *nrql-tbox* (current-tbox))))
  (let ((dbox (find-dbox tbox :error-p nil)))
    (when dbox
      (delete-defined-query 
       (get-definition1 dbox name arity :error-p t) dbox)
      (all-names-of-defined-queries dbox))))

;;;
;;;
;;;

(defun activate-defined-query1 (name arity &key pos (tbox (or *nrql-tbox* (current-tbox))))
  (let ((dbox (find-dbox tbox :error-p nil)))
    (when dbox
      (let ((query (get-definition1 dbox name arity :error-p t :also-deactivated-p t)))
        (when (consp query)
          (when pos
            (setf query (nth pos query))))
        (dolist (query (ensure-list query))
          (setf (slot-value query 'activated-p) t))
        t))))

(defun deactivate-defined-query1 (name arity &key pos (tbox (or *nrql-tbox* (current-tbox))))
  (let ((dbox (find-dbox tbox :error-p nil)))
    (when dbox
      (let ((query (get-definition1 dbox name arity :error-p t :also-deactivated-p t)))
        (when (consp query)
          (when pos
            (setf query (nth pos query))))
        (dolist (query (ensure-list query))
          (setf (slot-value query 'activated-p) nil))
        nil))))


