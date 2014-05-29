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

(defun ensure-precompletion-materialization (abox)
  (setf abox (find-abox abox))
  (abox-consistent-p abox)
  (loop for subgraph in (abox-subgraphs abox)
        do
        (loop for constraint in (state-expanded-constraints 
                                 (precompletion-state (subgraph-precompletion subgraph)))
              do (let ((ind (constraint-ind constraint))
                       (concept-term (constraint-term constraint)))
                   (typecase concept-term
                     (some-concept (let ((role (concept-role concept-term))
                                         (filler-concept (concept-term concept-term))
                                         (new-ind (gentemp "ANON-IND-")))
                                     (print (list (list ind new-ind) role))
                                     (print (list ind (decode-concept filler-concept)))))
                     (otherwise (print (list ind concept-term))))))))

(defun rewrite-concept (concept-name tbox var)
  (check-type concept-name symbol)
  (setf tbox (find-tbox tbox))
  (ensure-knowledge-base-state ':tbox-prepared tbox)
  (rewrite-concept-0 concept-name tbox var (make-hash-table)))

(defun rewrite-concept-0 (concept-name tbox var expanded-concepts)
  (let ((concept-entry (gethash concept-name (tbox-concept-axioms-index tbox))))
    (cond ((or (null concept-entry)
               (not (null (second concept-entry))) 
               (tbox-meta-constraint-concepts tbox))
           concept-name)
          (t (setf (gethash concept-name expanded-concepts) t)
             (catch 'not-rewritable
               (rewrite-concept-1
                concept-name
                (first concept-entry)
                tbox 
                var
                expanded-concepts))))))

(defun rewrite-concept-1 (concept-name concept tbox var expanded-concepts)
  (if (symbolp concept)
    `(and (,var ,concept))
    (case (first concept)      
      (some (let ((filler-var (gentemp "$?ANON-IND-")))
              `(and (,var ,filler-var ,(second concept))
                    ,(let ((rewritten (rewrite-concept-0 (third concept) tbox filler-var 
                                                         expanded-concepts)))
                       (if (symbolp rewritten)
                         `(,filler-var ,rewritten)
                         rewritten)))))
      #|
      (all 
       (let ((filler-var (gentemp "$?ANON-IND-"))) 
         `(neg (project-to (,var) (and (,var ,filler-var ,(second concept))
                                       (neg ,(rewrite-concept-1 concept-name 
                                                               (third concept) tbox filler-var
                                                               expanded-concepts)))))))
      |#
      (and 
       `(and ,@(mapcan #'rest (loop for concept-1 in (rest concept) 
                                    collect
                                    (rewrite-concept-1 concept-name concept-1 tbox var
                                                       expanded-concepts)))))
      (otherwise (throw 'not-rewritable concept-name)))))

#|
(ensure-precompletion-materialization (current-abox))

(rewrite-concept 'c (current-tbox) `?x)
|#





  
