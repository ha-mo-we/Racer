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


(defmacro with-nrql-standard-settings (&body body)

  ;;; Racer! 

  `(let ((*multiprocess-queries* nil)
         (*add-role-assertions-for-datatype-properties-p* nil)

         (*tuple-at-a-time-p* nil)
         (*proactive-tuple-computation-p* t)

         (*how-many* nil)

         (*two-phase-processing-p* nil)
         (*deliver-phase-two-warning-tokens-p* nil)
         (*deliver-kb-has-changed-warning-tokens-p* nil)
         
         (*initial-abox-mirroring-p* nil)
         (*check-abox-consistency-p* t)
         
         (*optimize-p* t)
         (*warnings-p* t) 
         (*rewrite-to-dnf-p* t)
         
         (*told-information-reasoning-p* nil) 
         (*report-inconsistent-queries-p* nil)
         (*report-tautological-queries-p* nil)
         (*rewrite-semantically-p* nil)
         (*use-repository-p* nil)
         (*put-into-repository-p* nil)
         (*classify-concepts-in-instance-assertions-p* nil)
         (*ensure-tbox-classification-p* nil)
         (*add-rule-consequences-p* nil)
         (*dont-add-abox-duplicates-p* nil)
         (saved-id *iterator-id*)
         (*type-of-substrate* 'racer-dummy-substrate))

     (prog1 
         (progn ,@body)
       (when (> (- *iterator-id* saved-id) 1)
         (nrql-runtime-error "Bad use of with-nrql-settings, at most ONE call to racer-answer-query allowed"))
       (unless (= *iterator-id* saved-id)
         (delete-query *last-query*)))))

;;;
;;; Hilfsfunktion f. Racer
;;;

#|
(defmacro with-nrql-timeout (&rest body)
  `(if *server-timeout*
       (with-timeout (*server-timeout*

                      (let ((substrate 
                             (find-racer-substrate abox *type-of-substrate*)))
                        
                        (when substrate
                          (substrate-needs-reset substrate))

                        (nrql-warning "Timeout encountered!")

                        :timeout))
	 ,@body)
     (progn
       ,@body)))
     
|#


(defun racer-retrieve-individual-filled-roles (from to &key (abox (current-abox)) no-inverses-p negated-p
                                                    roles)
  (with-critical-section 
    (let ((saved-cur-substrate *cur-substrate*))
     
      (racer-prepare-substrate :abox abox :prepare-now-p t )
     
      (let* ((roles1 nil)
             (*running-substrate* *cur-substrate*)
             (*running-abox* (abox *running-substrate*))
             (*running-tbox* (tbox *running-substrate*)))
        
        (check-abox-consistency *running-substrate*)
         
        (dolist (role (or roles (dl-prover-all-roles *running-substrate*)))
          (when (and (=> no-inverses-p (symbolp role))
                     (=> (symbolp role)
                         (not (role-used-as-datatype-property-p role *running-tbox*))))
            (evaluate-dl-prover-check-individuals-related-p *running-substrate*
                                                            from to 
                                                            (if negated-p 
                                                                `(not ,role)
                                                              role)
                                                            #'(lambda (&rest args) 
                                                                (declare (ignorable args))
                                                                (push role roles1)))))
       
        (setf *cur-substrate* saved-cur-substrate)
       
        (remove-duplicates roles1)))))


(defun racer-retrieve-related-individuals (role &key (abox (current-abox)))
  (with-critical-section 
    (let ((saved-cur-substrate *cur-substrate*)
          (role (normalize-racer-role role)))

      (racer-prepare-substrate :abox abox :prepare-now-p t)

      (let* ((*running-substrate* *cur-substrate*)
             (*running-abox* (abox *running-substrate*))
             (*running-tbox* (tbox *running-substrate*))
             (pairs nil))

        (check-abox-consistency *running-substrate*)

        (dolist (from (dl-prover-all-individuals *running-substrate*))
          (evaluate-dl-prover-retrieve-individual-fillers *running-substrate*
                                                          from role
                                                          #'(lambda (&rest args &key to &allow-other-keys) 
                                                              (declare (ignorable args))
                                                              (push (list from to) pairs))))

        (setf *cur-substrate* saved-cur-substrate)
       
        (remove-duplicates pairs :test #'equal)))))


(defun racer-retrieve-individual-fillers (from role
                                               &key 
                                               only-one-p
                                               only-if-p
                                               (abox (current-abox)) 
                                               (remove-synonyms-p t)
                                               (show-synonyms-p remove-synonyms-p))
  
  ;;; auch fuer inverse und negierte
  
  (with-critical-section 
    (let* ((saved-cur-substrate *cur-substrate*)
           (fillers nil)
           (role (normalize-racer-role role))
           (negated-p (negated-dl-role-p role))
           (role (if negated-p 
                     (second role)
                   role))
           (inverse-p (consp role))
           (role (if (consp role)
                     (second role)
                   role)))        
                
      (racer-prepare-substrate :abox abox :prepare-now-p t)

      (let* ((*running-substrate* *cur-substrate*)
             (*running-abox* (abox *cur-substrate*))
             (*running-tbox* (tbox *cur-substrate*))
             (*use-individual-synonyms-p* t))

        (check-abox-consistency *running-substrate*)

        (evaluate-dl-prover-retrieve-individual-fillers *running-substrate*
                                                        from 
                                                        (if negated-p
                                                            `(not ,role)
                                                          role)
                                                        #'(lambda (&rest args &key to from &allow-other-keys) 
                                                            (declare (ignorable args))
                                                            (if from ;; das ist richtig so! inverse-p wird u.U. vom Algorithmus umgedreht
                                                                (push from fillers)
                                                              (push to fillers)))
                                                        :inverse-p inverse-p 
							:only-if-p only-if-p
							:only-one-p only-one-p)

        (setf *cur-substrate* saved-cur-substrate)
	       
        (let ((res
               (remove-duplicates fillers :test #'(lambda (x y)
                                                    (if remove-synonyms-p
                                                        (member x (dl-prover-individual-synonyms
                                                                   *running-substrate* y))
                                                      (eq x y))))))
	  
          (if show-synonyms-p 
              (mapcar #'(lambda (x) 
                          (dl-prover-individual-synonyms
                           *running-substrate* x))
                      res)
            res))))))
