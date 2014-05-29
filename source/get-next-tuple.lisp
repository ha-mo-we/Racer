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

;;;
;;;
;;;

(defmethod get-next-set-of-rule-consequences ((query query) &rest args &key &allow-other-keys)
  (apply #'get-next-tuple query args))

(defmethod get-next-tuple ((query query) &rest args &key &allow-other-keys)
  (declare (ignorable args))


  (with-slots (process get-next-tuple-p 
                       bindings-queue
                       current-bindings 
                       last-queue-item
                       query-satisfiable) query


    (cond ((not query-satisfiable)
               
           :inconsistent)

          (t

           (labels ((wait-for-next-tuple ()
		                  
                      (process-wait 
                       (or (not process)
                           bindings-queue))

                      (when (runtime-error query)
                        ;;; NICHT nrql-error (wegen doppelter nRQL-Error: Ausgaben)
                        (error (runtime-error query))))
	       	       
                    (return-no-more-tuples ()
                      (if (timeout-p query)
                          :timeout

                        (progn 
                          (last-tuple-has-been-delivered query)
                          :exhausted))))

             (wait-for-next-tuple)
             (with-queue-synchronization (query)                
                 
               (let ((res-tuple
             
                      (cond ((not bindings-queue)
          
                             (return-no-more-tuples))
               
                            (t 

                             (let ((tuple
                                    (pop bindings-queue)))
            
                               (unless bindings-queue
                                 (setf last-queue-item nil))
            
                               (cond ((not (proactive-tuple-computation-p query))
                              
                                      (setf get-next-tuple-p t)
                                      tuple)

                                     (t tuple)))))))

                 (setf current-bindings res-tuple)
              
                 res-tuple)))))))



(defmethod get-next-tuple ((query nrql-query) &rest args &key &allow-other-keys)
  (declare (ignorable args))
  (with-slots (process get-next-tuple-p  
                       deliver-phase-two-warning-tokens-p
                       deliver-kb-has-changed-warning-tokens-p
                       two-phase-processing-p
                       bindings-queue
                       current-bindings 
                       last-queue-item
                       kb-changed-token-delivered-p
                       query-satisfiable) query


    (release-queue-lock-if-already-mine (query) ; give register-bindings :before chance to register new tuple! yield

       (cond ((not query-satisfiable)
      
              :inconsistent)

             (t

              (labels ((wait-for-next-tuple ()

                         (process-wait
                          (or (not process)
                              bindings-queue))

                         (when (runtime-error query)
                           ;;; NICHT nrql-error (wegen doppelter nRQL-Error: Ausgaben)
                           (error (runtime-error query))))
	       	       
                       (return-no-more-tuples ()
                         (if (timeout-p query)
                             :timeout

                           (progn 
                             (last-tuple-has-been-delivered query) 
                             :exhausted))))

                ;;; (princ (bindings-queue query)) (terpri)

                (cond ((and (not kb-changed-token-delivered-p)
                            deliver-kb-has-changed-warning-tokens-p
                            (not (query-accurate-p query)))
          
                       (let ((token :warning-kb-has-changed))
                      
                         (setf kb-changed-token-delivered-p t)
                      
                         (setf current-bindings token)
                      
                         token))

                      (t

                       (let ((tuple nil)
                             (res-tuple nil)
                             (exhausted-p nil))

                         ;; (princ 1)

                         (wait-for-next-tuple)

                         ;; (princ 2)

                         (with-queue-synchronization (query)

                           ;; (princ 3)
                      
                           (cond ((not bindings-queue)
          
                                  (setf exhausted-p t
                                        res-tuple (return-no-more-tuples)))

                                 (t 
                               
                                  (setf tuple (pop bindings-queue))
            
                                  (unless bindings-queue
                                    (setf last-queue-item nil))))

                           (unless exhausted-p
                             (setf res-tuple
            
                                   (cond ((not (proactive-tuple-computation-p query))
                                                     
                                          ;;; nach Abholung wird immer der Auftrag zur Berechnung 
                                          ;;; des naechsten Tupels gegen (eines im Voraus) 
                                          ;;; get-next-tuple-p = T 

                                          (case tuple 

                                            (:warning-expensive-phase-two-starts
                                             (cond ((and deliver-phase-two-warning-tokens-p
                                                         two-phase-processing-p)
                                                    (with-queue-synchronization (query)
                                                      (push :warning-expensive-phase-two-starts-2 bindings-queue)
                                                      tuple))
                                                   (t
                                                    (setf get-next-tuple-p t)
                                                    (get-next-tuple query))))
                       
                                            (:warning-expensive-phase-two-starts-2
                                             (setf get-next-tuple-p t)
                                             (get-next-tuple query))
                       
                                            (otherwise 

                                             (setf get-next-tuple-p t)
                                             tuple)))
              
                                         (t 

                                          (case tuple
                                            (:warning-expensive-phase-two-starts
                              
                                             ;;; im eager mode wird das Token nicht geliefert!

                                             (get-next-tuple query))
			       
                                            (otherwise tuple)))))))
                         ;;(print 4)
                         (setf current-bindings res-tuple)
                         ;;(print 5)
                         res-tuple)))))))))
        
;;;
;;;
;;;

(defmethod next-tuple-available-p ((query query))
  (when (bindings-queue query)
    t))

(defmethod next-set-of-rule-consequences-available-p ((query query))
  (next-tuple-available-p query))

;;;
;;;
;;;

(defmethod get-current-tuple ((query query))
  (current-bindings query))

(defmethod get-current-set-of-rule-consequences ((query query))
  (current-bindings query))

  
