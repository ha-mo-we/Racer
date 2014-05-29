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

(defgeneric get-parser-class-for-substrate (substrate))   

;;;
;;; Interface: Substrate <-> Query
;;;

#+:dlmaps (defmethod get-parser-class-for-substrate ((substrate substrate))
  'substrate-parser)

#+:dlmaps (defmethod get-parser-class-for-substrate ((substrate racer-substrate))
  'racer-substrate-parser)

#+:dlmaps (defmethod get-parser-class-for-substrate ((substrate racer-descriptions-substrate))
  'racer-descriptions-substrate-parser)

(defmethod get-parser-class-for-substrate ((substrate racer-dummy-substrate))
  'nrql-abox-query-parser)

(defmethod get-parser-class-for-substrate ((substrate racer-tbox-mirror-substrate))
  'nrql-tbox-query-parser)

#+:midelora
(defmethod get-parser-class-for-substrate ((substrate midelora-substrate))
  'midelora-abox-query-parser)

#+:midelora
(defmethod get-parser-class-for-substrate ((substrate midelora-tbox-mirror-substrate))
  'midelora-tbox-query-parser)

;;;
;;; Dispatcher-Methoden
;;;

(defmethod make-dispatcher ((parser simple-parser) sym &key expression)
  (declare (ignorable expression))

  ;;; dieser Parser "versteht" alles, was get-expression-type
  ;;; zurueckliefert...
  (make-instance sym :dont-initialize-p t))

#+:dlmaps (defmethod make-dispatcher ((parser substrate-parser) sym &key expression)
  (declare (ignorable expression))
            
  ;;; dieser Parser versteht nur simple-substrate and 
  ;;; predicate-queries, sowie and/or -> homogeneous complex queries!
  (make-instance 
   (case sym

     ((substrate-simple-and-node-query 
       substrate-simple-or-node-query 
       
       substrate-simple-and-edge-query 
       substrate-simple-or-edge-query 
       
       substrate-predicate-edge-query
       substrate-predicate-node-query

       same-as-query
       
       top-query
       bottom-query)

      sym)

     (and-query 'complex-substrate-and-query)
     
     (or-query 'complex-substrate-or-query)
     
     (otherwise (return-from make-dispatcher nil)))
   
   :dont-initialize-p t))

(defmethod make-dispatcher ((parser nrql-abox-query-parser) sym &key expression)
  (declare (ignorable expression))
  ;;; dieser Parser versteht nur instance/edge-retrieval, und and/or
  (make-instance 
   (case sym    
     
     (and-query 'nrql-and-query)     

     (or-query 'nrql-or-query)

     (instance-retrieval-query 'nrql-instance-retrieval-query)
     
     (edge-retrieval-query 'nrql-edge-retrieval-query)
     
     (has-known-successor-retrieval-query 'nrql-has-known-successor-retrieval-query)
     
     (cd-edge-retrieval-query 'nrql-cd-edge-retrieval-query)
     
     (top-query 'nrql-top-query)
     
     (bottom-query 'nrql-bottom-query)
     
     (same-as-query 'nrql-same-as-query)

     (true-query 'nrql-true-query)
     
     (false-query 'nrql-false-query)
     
     (minilisp-query 'nrql-minilisp-query)

     (otherwise (return-from make-dispatcher nil)))
   
   :dont-initialize-p t))


#+:midelora
(defmethod make-dispatcher ((parser midelora-abox-query-parser) sym &key expression)
  (declare (ignorable expression))
  (make-instance 
   (case sym    
     
     (and-query 'midelora-nrql-and-query)     

     (or-query 'midelora-nrql-or-query)

     (instance-retrieval-query 'midelora-nrql-instance-retrieval-query)
     
     (edge-retrieval-query 'midelora-nrql-edge-retrieval-query)
     
     (has-known-successor-retrieval-query 'midelora-nrql-has-known-successor-retrieval-query)
          
     (top-query 'midelora-nrql-top-query)
     
     (bottom-query 'midelora-nrql-bottom-query)
     
     (same-as-query 'midelora-nrql-same-as-query)

     (true-query 'midelora-nrql-true-query)
     
     (false-query 'midelora-nrql-false-query)

     (otherwise (return-from make-dispatcher nil)))
   
   :dont-initialize-p t))



#+:dlmaps (defmethod make-dispatcher ((parser racer-substrate-parser) sym &key expression)
 (declare (ignorable expression))
 
  ;;; das RACER-Substrate ist "hybrid" -> Substrate und ABox
  ;;; befragbar 
  ;;; Knotenbeschreibungen im Substrate koennen beliebig sein!
  ;;; z.B. simple-description, oder auch racer-description! 
  (make-instance 
   (case sym
     (and-query 'hybrid-and-query)
     (or-query 'hybrid-or-query)
     (otherwise sym))
   :dont-initialize-p t))


#+:dlmaps (defmethod make-dispatcher ((parser racer-descriptions-substrate-parser) sym &key expression)
 (declare (ignorable expression))
  ;;; dieser Parser versteht nur RACER-Descriptions
  ;;; Annahme: ein racer-descriptions-substrate hat NUR RACER-descriptions!!!
  ;;; -> simple-substrate-queries ausschliessen!
  (make-instance 
   (case sym

     ((substrate-racer-node-query 
       substrate-racer-edge-query
       same-as-query

       top-query
       bottom-query)
      
      sym)

     (and-query 'complex-substrate-and-query)
     
     (or-query 'complex-substrate-or-query)
     
     (otherwise (return-from make-dispatcher nil)))
   
   :dont-initialize-p t))
