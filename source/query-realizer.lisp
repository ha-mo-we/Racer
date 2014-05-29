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

(defmethod semantically-rewrite-query ((query atomic-query) (parser simple-parser) 
                                       &key &allow-other-keys)

  (first (semantically-rewrite-conjuncts query (list query))))

    
(defmethod semantically-rewrite-query ((query or-query) (parser simple-parser) 
                                       &rest args &key &allow-other-keys)

  (declare (ignorable args))

  (let* ((subqueries (mapcar #'(lambda (sq) 
                                 (apply #'semantically-rewrite-query sq parser args))
                             (subqueries query)))
         
         (subqueries 
          (remove-if #'query-inconsistent-p subqueries)))
    
    (cond ((not subqueries)
           
           query)

          ((not (cdr subqueries))
           
           (make-top-level-query (first subqueries))
           
           (first subqueries))

          (t (make-description (type-of query) 
                               nil
                               :subqueries subqueries 
                               :parser parser)))))


(defmethod semantically-rewrite-query ((query and-query) (parser simple-parser)
                                       &rest args)
  (declare (ignorable args))

  (let ((new-conjuncts 
         (semantically-rewrite-conjuncts query (subqueries query))))
    
    (make-description (type-of query) 
                      nil
                      :subqueries new-conjuncts
                      :parser parser)))

;;;
;;;
;;;

(defmethod semantically-rewrite-conjuncts ((query nrql-query) (conjuncts list))
  
  (let* ((parser (parser query))
         (old-conjuncts (remove-if-not #'(lambda (x) 
                                           (or (not (is-instance-retrieval-query-p x))
                                               (negated-p x)))
                                       conjuncts))
         (new-conjuncts
          (realize-abox-query-conjuncts conjuncts
                                        :tbox (tbox (substrate query))))
         (new-conjuncts
          (append old-conjuncts
                  (if (eq new-conjuncts 'inconsistent)
                      (list (make-description 'nrql-bottom-query 'bottom
                                              :parser parser
                                              :vois (list (first (vois parser)))))
                    (let ((cluster nil))
                      
                      (loop while new-conjuncts do 
                            (let* ((entry (first new-conjuncts))
                                   (voi (first entry))
                                   (concept (second entry))
                                   (others (loop as other in (rest new-conjuncts) 
                                                 when (eq (first other) voi)
                                                 collect other)))
                              (push (list voi (cons concept (mapcar #'second others))) 
                                    cluster)

                              (setf new-conjuncts (rest new-conjuncts))
                              (setf new-conjuncts 
                                    (set-difference new-conjuncts others))))

                      (dolist (entry cluster)
                        (unless (cdr (second entry))
                          (setf (cdr entry)  
                                (second entry))))

                      (mapcar #'(lambda (entry)
                                  (make-description 'nrql-instance-retrieval-query
                                                    (second entry)
                                                    :parser parser
                                                    :vois (list (first entry))))
                              cluster))))))

    new-conjuncts))



