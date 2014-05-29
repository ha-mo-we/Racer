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

(defpersistentclass qbox (dag)
  ((time-stamp :reader time-stamp :initform 0)))

;;;
;;;
;;;

(defmethod clear-repository ((substrate substrate))
  (setf (slot-value substrate 'qbox) nil)
  'okay-repository-cleared)

(defmethod show-qbox ((substrate substrate) &optional definitions-p)
  (when (qbox substrate)
    (show-qbox (qbox substrate)
               definitions-p)))

(defmethod show-qbox ((qbox qbox) &optional definitions-p)
  (visualize-dag qbox
                 ;;; :view :textually
                 :printer 
                 (if definitions-p 
                     'dag-node-name
                   'show-node-name)))


(defmethod show-node-name ((x null))
  nil)

(defmethod show-node-name ((x query))
  (if (equivalents x)
      
      (format nil "~A:~A = ~A"
              (time-stamp x)
              (or (iterator-id x)
                  (format nil "SUBQUERY-~A-OF-~A"
                          (subquery-id x)
                          (iterator-id
                           (top-level-query x))))
              ;;; wichtig, nur so kann man sehen,
              ;;; ob eine Subsumptionsbeziehung dazu
              ;;; verwendet wurde, Kandidaten zu 
              ;;; enumerierten! Zeitliche Reihenfolge!
              (mapcar #'(lambda (x) 
                          (format nil "~A"
                                  (or (iterator-id x)
                                      (format nil "SUBQUERY-~A-OF-~A"
                                              (subquery-id x)
                                              (iterator-id
                                               (top-level-query x))))
                                  ;(time-stamp x)
                                  ))
                      (equivalents x)))

    (format nil "~A:~A"
            (time-stamp x)
            (or (iterator-id x)
                (format nil "SUBQUERY ~A OF ~A"
                        (subquery-id x)
                        (iterator-id
                         (top-level-query x)))))))
    

;;;
;;;
;;;

(defmethod make-qbox ((substrate substrate))
  (let* ((top (make-instance 'master-top-query 
                             :name 'master-top-query 
                             :iterator-id 'master-top-query
                             :time-stamp 0
                             :dont-initialize-p t))
         (bottom (make-instance 'master-bottom-query 
                                :name 'master-bottom-query 
                                :iterator-id 'master-bottom-query
                                :time-stamp 0
                                :dont-initialize-p t))
         (qbox (make-dag :type 'qbox
                         :name (format nil "QBox for Substrate ~A" (name substrate)))))
                   
    (insert-dag-node qbox top)   
    (setf (dag-node-parents bottom) (list top))
    (insert-dag-node qbox bottom)
    
    qbox))

;;;
;;;
;;;

(defmethod get-qbox ((query query))
  (qbox (substrate query)))

(defmethod get-qbox ((substrate substrate))
  (qbox substrate))

;;;
;;;
;;;

(defmethod delete-dag-node :before ((dag qbox) (query query))
  (dolist (equivalent-query (equivalents query))
    (setf (slot-value equivalent-query 'equivalents)
          (delete query (slot-value equivalent-query 'equivalents)))))

;;;
;;;
;;;


(defmethod remove-outdated-qbox-nodes ((substrate substrate))
  (remove-outdated-qbox-nodes (qbox substrate)))

(defmethod remove-outdated-qbox-nodes ((qbox qbox)) 
  (dolist (query (dag-nodes qbox))
    (when (or (not (valid-qbox-entry-p query))
              (not (query-accurate-p query)))
      (delete-dag-node qbox query))))

;;;
;;;
;;;

(defmethod classify ((query query))

  (let ((substrate (substrate query))
        (qbox (get-qbox query)))
          
    (unless qbox
      (setf qbox (make-qbox substrate))
      (setf (slot-value (substrate query) 'qbox) qbox))

    (unless (dag-node-parents query)

      (remove-outdated-qbox-nodes qbox)

      (setf (slot-value query 'time-stamp)
            (time-stamp qbox))

      (let ((*warnings-p* nil))

        (let ((parents (compute-node-parents query qbox))
              (children (compute-node-children query qbox)))

          (when *debug-p* 
            (format t "~%~% ~A -> Parents: ~A Children: ~A~%" query parents children))
          
          (if (and (set-equal parents children)
                   parents
                   (not (cdr parents))
                   (not (cdr children)))

              (let ((equi-node (car parents)))
                (unless (eq equi-node query)
                
                  (setf (slot-value query 'equivalents)
                        (list equi-node))
                  (pushnew query (slot-value equi-node 'equivalents))))
                                
            (setf (dag-node-parents query) parents 
                  (dag-node-children query) children))))))
    
  'classified)

;;;
;;;
;;;

(defmethod compute-node-parents ((query query) (qbox qbox) &rest args)
  (declare (ignorable args))

  (labels ((mark-all-descendants (node)
             (mark-dag-node node)
             (mapc #'mark-all-descendants (dag-node-children node)))               
           (do-it (nodes)
             (when nodes
               (let ((current (first nodes))
                     (nodes (rest nodes)))
                 (if (dag-node-marked-p current)
                     (do-it nodes)
                   (if (query-entails-p query current
					:enforce-same-arity-p t)
                       (progn
                         (do-it (append nodes (dag-node-children current))))
                     (progn 
                       (mark-all-descendants current)
                       (do-it nodes))))))))
    

    (if *syntactic-repository-p* 

        (or 
         (remove-if-not #'(lambda (q) 
                            ;;; syntaktischer Cache-Hit
                            (tree-equal (original-query q)
                                        (original-query query)))
                        (dag-nodes qbox))
         (list (dag-top qbox)))
        
      (when (dag-nodes qbox)    
        (unmark-all-dag-nodes qbox)      
        (do-it (list (dag-top qbox)))      
        (remove-duplicates 
         (remove-if-not #'(lambda (q) 
                            (and (not (dag-node-marked-p q))
                                 (every #'(lambda (child) 
                                            (dag-node-marked-p child))
                                        (dag-node-children q))))
                        (dag-nodes qbox)))))))
    
(defmethod compute-node-children ((query query) (qbox qbox) &rest args)
  (declare (ignorable args))

  (labels ((mark-all-ancestors (query)
             (mark-dag-node query)
             (mapc #'mark-all-ancestors (dag-node-parents query)))

           (unmark-relevant-dag-nodes (dag val)
             (dolist (query (dag-nodes dag))
               (if (and (dag-node-marked-p query)
                        (= (dag-node-marked-p query)
                           val))
                   (unmark-dag-node query)
                 (mark-dag-node query))))
           
           (mark-all-descendants (query val)
             (let ((node-val (dag-node-marked-p query)))
               (when (or (and (not node-val)
                              (= val 1))
                         (and node-val
                              (= node-val (1- val))))
                 (mark-dag-node query val))
               (mapc #'(lambda (x)
                         (mark-all-descendants x val))
                     (dag-node-children query))))
             
           (do-it (nodes)

             (when nodes
               (let ((current (first nodes))
                     (nodes (rest nodes)))

                 (if (dag-node-marked-p current)
                     ;;; richtig! s. unmark-relevant-... -> toggelt die markierung!
                     
                     (do-it nodes)
                   
                   (if (query-entails-p current query
					:enforce-same-arity-p t)
                       
                       (progn
                         (do-it (if (dag-node-parents current)
                                    (append nodes (dag-node-parents current))
                                  nodes)))
                     (progn 
                       (mark-all-ancestors current)
                       (do-it nodes))))))))

    
    (if *syntactic-repository-p* 
        (or (remove-if-not #'(lambda (q) 
                               ;;; syntaktischer Cache-Hit
                               (tree-equal (original-query q)
                                           (original-query query)))
                           (dag-nodes qbox))
            (list (dag-bottom qbox)))
            
      (when (dag-nodes qbox)
        (unmark-all-dag-nodes qbox)
      
        (let* ((parents (dag-node-parents query))
               (n (length parents)))
        
          (when parents          
                    
            (mark-all-descendants (first parents) 1)      
          
            (loop as parent in (rest parents) 
                  as i from 2 by 1 do
                  (mark-all-descendants parent i))

            (unmark-relevant-dag-nodes qbox n))

          (when (dag-node-marked-p (dag-bottom qbox))
            (nrql-error "Query repository: internal error"))
          
          (do-it (list (dag-bottom qbox))))
      
        (remove-duplicates 
         (remove-if-not #'(lambda (q) 
                            (and (not (dag-node-marked-p q))
                                 (every #'(lambda (parent) 
                                            (dag-node-marked-p parent))
                                        (dag-node-parents q))))
                        (dag-nodes qbox)))))))

;;;
;;;
;;;

(defmethod unregister-query ((query query))
  (if (in-dag query)
      (delete-dag-node (in-dag query) query)
    (dolist (equivalent-query (equivalents query))
      (setf (slot-value equivalent-query 'equivalents)
            (delete query (slot-value equivalent-query 'equivalents))))))

(defmethod register-query ((query query))
  (let* ((query-stamp
          (time-stamp query))
         (qbox-stamp 
          (when (get-qbox query)
            (time-stamp (get-qbox query)))))

    (labels ((register (query) 
               (let ((equivalents (equivalents query))
                     (parents (dag-node-parents query))
                     (children (dag-node-children query))
                     (qbox (get-qbox query)))

                 (incf (slot-value qbox 'time-stamp))
                 (setf (slot-value query 'time-stamp)
                       (time-stamp qbox))

                 (when *debug-p* (format t "REGISTER ~A: ~A ~A ~A~%" query equivalents parents children))
                 
                 (unless equivalents 
                   ;;; es wird immer nur der erste Knoten einer Aequivalenzklasse registriert!
                   (insert-dag-node qbox query)))))

      (cond ((or (not qbox-stamp)
                 (not query-stamp))

             (when *debug-p* (format t  "*** Classify ~A~%" query))

             (classify query)

             (register query))

            ((and qbox-stamp query-stamp
                  (not (= qbox-stamp query-stamp)))
             
             (let ((qbox (get-qbox query)))

               (delete-dag-node qbox query)
               
               (when *debug-p* (format t  "*** RE-Classify ~A~%" query))
               
               (classify query) 
               (register query)))

            (t 

             (when *debug-p* (format t "**** REGISTER ~A~%" query))
             
             (unless (in-dag query)
               (register query)))))))

;;;
;;; Interface 
;;;

(defun get-substrate-for-abox (abox &optional (type-of-substrate *type-of-substrate*))
  (declare (ignorable type-of-substrate))
  #-:midelora (find-racer-substrate abox *type-of-substrate*)
  #+:midelora (find-midelora-substrate abox *type-of-substrate*))

;;;
;;;
;;;

(defmethod get-nodes-in-qbox-for-abox1 ((abox null) &optional (type-of-substrate *type-of-substrate*))
  (declare (ignorable type-of-substrate))
  :not-found)

(defmethod get-nodes-in-qbox-for-abox1 ((abox symbol) &optional (type-of-substrate *type-of-substrate*))
  (get-nodes-in-qbox-for-abox1 (get-substrate-for-abox abox type-of-substrate)))

(defmethod get-nodes-in-qbox-for-abox1 ((substrate dl-prover-substrate) &optional type-of-substrate)
  (declare (ignorable type-of-substrate))
  (if (qbox substrate) 
      (mapcar #'iterator-id 
              (remove (dag-top (qbox substrate))
                      (remove (dag-bottom (qbox substrate))
                              (dag-nodes (qbox substrate)))))
    :not-found))

;;;
;;;
;;; 

(defmethod show-qbox-for-abox1 ((abox null) &optional definitions-p  (type-of-substrate *type-of-substrate*))
  (declare (ignorable definitions-p type-of-substrate))
  :not-found)

(defmethod show-qbox-for-abox1 ((abox symbol) &optional definitions-p  (type-of-substrate *type-of-substrate*))
  (show-qbox-for-abox1
   (get-substrate-for-abox abox type-of-substrate)
   definitions-p))

(defmethod show-qbox-for-abox1 ((substrate dl-prover-substrate) &optional definitions-p type-of-substrate)
  (declare (ignorable type-of-substrate))
  (if (qbox substrate) 
      (progn
        (format t "~%~%;;;~%;;; QBOX FOR ~A FOR ABOX ~A~%;;; ~%" (type-of substrate) (abox substrate))
        (show-qbox substrate definitions-p)
        :see-output-on-stdout)
    :not-found))

;;;
;;;
;;;


(defmethod get-dag-of-qbox-for-abox1 ((abox null) &optional (type-of-substrate *type-of-substrate*))
  (declare (ignorable type-of-substrate))
  :not-found)

(defmethod get-dag-of-qbox-for-abox1 ((abox symbol) &optional (type-of-substrate *type-of-substrate*))
  (get-dag-of-qbox-for-abox1 (get-substrate-for-abox abox type-of-substrate)))

(defmethod get-dag-of-qbox-for-abox1 ((substrate dl-prover-substrate) &optional type-of-substrate)
  (declare (ignorable type-of-substrate))

  (let* ((qbox
          (qbox substrate)))

    (if qbox
        (let ((top (dag-top qbox))
              (bot (dag-bottom qbox)))
          (mapcar #'(lambda (x) 
                      (tree-map #'(lambda (x)
                                    (cond ((eq x top)
                                           'master-top-query)
                                          ((eq x bot)
                                           'master-bottom-query)
					  ((null x) x)
                                          (t
                                           (or (iterator-id x)
                                               (intern (format nil "~A-~A-~A-~A"
                                                               (string-transform "subquery")
                                                               (subquery-id x)
                                                               (string-transform "of")
                                                               (iterator-id
                                                                (top-level-query x))))))))
				(list (cons x (equivalents x))
				      (dag-node-parents x)
				      (dag-node-children x))))
                  (remove-duplicates
                   (cons top
                         (append
                          (remove top 
                                  (remove bot (dag-nodes qbox)))
                          (list bot))))))
      :not-found)))
