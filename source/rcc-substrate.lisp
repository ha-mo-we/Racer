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

(defconstant +inconsistent-marker+ :rcc-bottom-relation)

(defpersistentclass rcc-substrate (data-substrate)
  ((rbox :reader rbox)
   (rcc-type :reader rcc-type)
   (rcc-synonyms :reader rcc-synonyms :initform (make-hash-table))

   (edge-consistent-p :initform :dont-known)

   (minimal-label-computed-p :initform nil)))

(defpersistentclass rcc-mirror-substrate (rcc-substrate mirror-data-substrate ))

(defmethod describe-substrate-int ((substrate rcc-substrate))
  `((:type ,(type-of substrate))
    (:abox ,(abox substrate))
    (:rcc-type ,(rcc-type substrate))
    (:rcc-consistent ,(slot-value substrate 'edge-consistent-p))
    (:rcc-synonyms ,(let ((res nil))
                      (maphash #'(lambda (key value)
                                   (push (list key value) res))
                               (rcc-synonyms substrate))
                      res))
    (:no-of-nodes ,(length (get-nodes substrate)))
    (:no-of-edges ,(length (get-edges substrate)))))


(defmethod edge-description-int2 ((substrate rcc-substrate) from to)
  (let ((edge (get-edge-info substrate from to)))
    (if (not (eq edge +no-descr-marker+))
        `((:from-node ,from)
          (:to-node ,to)
          (:rcc-relation ,(or (edge-info-aux-type edge)
			      (list (roles (rbox substrate)))))
          (:edge-label ,(edge-info-type edge)))
      :not-found)))


(defmethod initialize-instance :after ((substrate rcc-substrate) &rest initargs)
  (declare (ignorable initargs))
  (with-slots (rbox rcc-type rcc-synonyms) substrate
    (setf rcc-type *rcc-type*)
    (setf rbox
          (ecase *rcc-type*
            ((:rcc1 #+:mlisp :RCC1) +rcc1-rolebox+)
            ((:rcc2 #+:mlisp :RCC2) +rcc2-rolebox+)
            ((:rcc3 #+:mlisp :RCC3) +rcc3-rolebox+)
            ((:rcc5 #+:mlisp :RCC5) +rcc5-rolebox+)
            ((:rcc8 #+:mlisp :RCC8) +rcc8-rolebox+)))
    
    (let ((syns
	   (remove-duplicates *rcc-synonyms*
			      :test #'equal)))
      
      (dolist (syn syns)
	(let ((from (first syn))
	      (to (to-keyword-big (second syn))))
	  (setf (gethash from rcc-synonyms) to)))
      
      (dolist (role (roles rbox))
	(setf (gethash (to-keyword-big role) rcc-synonyms) (to-keyword-big role))
	(setf (gethash (to-keyword-small role) rcc-synonyms) (to-keyword-big role))))))
	


(defmethod resolve-rcc-synonym ((substrate rcc-substrate) role)
  (with-slots (rcc-synonyms) substrate
    (gethash role rcc-synonyms)))

(defmethod resolve-rcc-synonyms ((substrate rcc-substrate) descr)

  ;; CNF
  ;; descr = :pp, or descr = ((:tpp :ntpp) :ec) 
  ;; Zurueck: Disjunktion ((:tppi :nttp))
  
  (if descr
      (let* ((res
	      (cons (roles (rbox substrate))
		    (mapcar #'(lambda (x) 
				(if (symbolp x)
				    (ensure-list 
				     (resolve-rcc-synonym substrate x))
				  (apply #'append
					 (mapcar #'(lambda (x) 
						     (when (symbolp x)
						       (ensure-list (resolve-rcc-synonym substrate x))))
						 x))))
			    (ensure-list descr))))
	     (res2 (if (cdr res)
		       (reduce #'intersection res)
		     (first res))))
	
	(if res2
	    (list res2)
	  +inconsistent-marker+))
    +inconsistent-marker+))
    

;;;
;;;
;;;

(defpersistentclass nrql-rcc-query-parser (nrql-data-query-parser) nil)

(defmethod get-parser-class-for-substrate ((substrate rcc-substrate))
  'nrql-rcc-query-parser)

;;;
;;;
;;;

(defpersistentclass rcc-substrate-query (nrql-query) nil)

;; (defpersistentclass rcc-substrate-node-query (data-substrate-node-query rcc-substrate-query) nil)

(defpersistentclass rcc-substrate-edge-query (data-substrate-edge-query rcc-substrate-query) nil)

;;;
;;;
;;;

(defmethod initialize-description :after ((query rcc-substrate-edge-query))
  (with-slots (textual-description) query
    (setf textual-description 
          (let ((descr (ensure-list textual-description)))
            (mapcar #'(lambda (x) 
                        (if (consp x)
                            (mapcar #'to-keyword x)
                          (to-keyword x)))
                    descr)))))

;;;
;;;
;;;

(defmethod is-rcc-query-p ((substrate rcc-substrate) expression)
  (not (eq (resolve-rcc-synonyms substrate (third expression))
	   +inconsistent-marker+)))

(defmethod make-dispatcher ((parser nrql-rcc-query-parser) sym &key expression)
  (case sym    
    
    #| (substrate-simple-and-node-query 
        (make-instance 'rcc-substrate-node-query :dont-initialize-p t)) |# 
    
    (substrate-simple-or-edge-query
     (if (is-rcc-query-p (substrate parser) expression)
         (make-instance 'rcc-substrate-edge-query :dont-initialize-p t)
       (call-next-method)))
    
    (otherwise 

     (call-next-method))))


(defmethod syntactically-rewrite-atomic-query (query (parser nrql-rcc-query-parser)
                                                     &rest args)
  (declare (ignorable args))
  
  (cond ((is-rcc-query-p (substrate parser) query)         
         
         (apply #'call-next-method 
                `(,(first query)
                  ,(second query)
                  ,(resolve-rcc-synonyms (substrate parser) (third query)))
                parser
                args))

        (t (call-next-method))))

;;;
;;;
;;;

(defmethod valid-node-or-description-p ((parser nrql-rcc-query-parser) expr)
  (declare (ignorable expr))
  (call-next-method))

(defmethod valid-edge-and-description-p ((parser nrql-rcc-query-parser) expr)
  (declare (ignorable expr))
  (call-next-method))

(defmethod valid-edge-or-description-p ((parser nrql-rcc-query-parser) expr)
  (declare (ignorable expr))
  (call-next-method))


;;;
;;;
;;;

(defmacro loop-over-all-rcc-successors ((substrate from &optional descr) (var) &rest body)
  `(loop-over-all-rcc-successors1 ,substrate ,from 
                                  #'(lambda (,var)
                                      ,@body)
                                  ,descr))


(defmacro loop-over-all-rcc-predecessors ((substrate from &optional descr) (var) &rest body)
  `(loop-over-all-rcc-predecessors1 ,substrate ,from 
                                    #'(lambda (,var)
                                        ,@body)
                                    ,descr))


(defmethod loop-over-all-rcc-successors1 ((substrate rcc-substrate) from fn &optional descr)
  (let ((succs nil))
    (loop-over-successors (substrate from descr)
                          (to) 
                          (progn 
                            (push to succs)
                            (funcall fn to)))
    
    (loop-over-nodes (substrate to)
      (unless (member to succs)
        (when (rcc-related-p substrate from to descr)
          (funcall fn to))))))


(defmethod loop-over-all-rcc-predecessors1 ((substrate rcc-substrate) to fn &optional descr)
  (let ((preds nil))
    (loop-over-predecessors (substrate to descr)
                            (from) 
                            (progn 
                              (push from preds)
                              (funcall fn from)))
    
    (loop-over-nodes (substrate from)
      (unless (member from preds)
        (when (rcc-related-p substrate from to descr)
          (funcall fn from))))))

;;;
;;; 
;;;


(defmethod node-instance :before ((substrate rcc-substrate) name &key &allow-other-keys)
  (declare (ignorable name))
  (with-slots (minimal-label-computed-p  edge-consistent-p) substrate
    (when edge-consistent-p		; also :unkown oder T!
      (setf minimal-label-computed-p nil
            edge-consistent-p :unknown))))

(defun tree-remove (fn tree)
  (if (consp tree)
      (remove nil 
	      (mapcar #'(lambda (x) 
			  (tree-remove fn x))
		      tree))
    (if (funcall fn tree)
	nil 
      tree)))

(defmethod nodes-related ((substrate rcc-substrate) from to description &key &allow-other-keys)
  (with-slots (data-edges data-nodes minimal-label-computed-p edge-consistent-p) substrate

    (let* ((description (ensure-list description))
           (rcc-part 
	    (tree-remove #'(lambda (x)
			     (eq (resolve-rcc-synonyms substrate x)
				 +inconsistent-marker+))
			 description)))
      
      (let* ((rcc-part 
	      (if rcc-part 
		  (resolve-rcc-synonyms substrate rcc-part)
		;;; (list (roles (rbox substrate)))
		;;; nicht notwendig
		))
	     (inconsistent-p 
	      (eq rcc-part +inconsistent-marker+)))

	
        (apply #'call-next-method substrate from to description nil)
      
        (when rcc-part
        
          (when edge-consistent-p		; also :unkown oder T!
            (setf edge-consistent-p :unknown)
            (setf minimal-label-computed-p nil))
	
          (unless (node-p substrate from)
            (node-instance substrate from))
	
          (unless (node-p substrate to)
            (node-instance substrate to))
	
          (pushnew to (node-info-successors (gethash from data-nodes)) :test #'equalp)
          (pushnew from (node-info-predecessors (gethash to data-nodes)) :test #'equalp)
	
          (let* ((key (list from to))
                 (info (gethash key data-edges))

                 (old-rcc-part (when info (edge-info-aux-type info))))

            ;;; 
            (let* ((new 
                    (cond (inconsistent-p
                           +inconsistent-marker+)
                          ((eq old-rcc-part +inconsistent-marker+)
                           +inconsistent-marker+)
                          (t 
                           (if old-rcc-part
                               (or (intersection (first old-rcc-part) (first rcc-part))
                                   +inconsistent-marker+)
                             (first rcc-part))))))
	      
	      (setf inconsistent-p 
                    (eq new +inconsistent-marker+))
	    
              (unless inconsistent-p
                (setf new (list new)))

              (if info
                  (setf (second info) new)
                (setf (gethash key data-edges)
                      (list nil new)))
	    
              (when inconsistent-p
                (setf edge-consistent-p nil))))))

      (values))))


;;;
;;;
;;;

(defmethod delete-node :after ((substrate rcc-substrate) name &rest args)
  (declare (ignorable name args))
  (with-slots (edge-consistent-p minimal-label-computed-p) substrate
    (setf minimal-label-computed-p nil 
          edge-consistent-p :unknown)))

(defmethod delete-edge :after ((substrate rcc-substrate) from to &rest args)
  (declare (ignorable from to args))
  (with-slots (edge-consistent-p minimal-label-computed-p) substrate
    (setf minimal-label-computed-p nil
          edge-consistent-p :unknown)))

;;;
;;;
;;;

(defmethod get-edge-between ((substrate rcc-substrate) from to) 
  (with-slots (data-edges
               default-edge-label) substrate

    
    (multiple-value-bind (fromto found1p)
        (gethash (list from to) data-edges)

      (cond (found1p 
             (values 
              (list (first fromto)
                    (if (second fromto)
                        (second fromto)
                      (list (roles (rbox substrate)))))
              t))

            ((eq from to)
             (values (copy-tree (list nil (list (reflexive-roles (rbox substrate)))))
                     nil))

            (t 
            
             (multiple-value-bind (tofrom found2p)
                 (gethash (list to from) data-edges)

               (cond (found2p ; inverse edge present!

                      (values (list nil
                                    (if (second tofrom)
                                        (inv-role (rbox substrate) (second tofrom))
                                      (list (roles (rbox substrate)))))
                              t))

                     (default-edge-label 

                      ;; note - there is NO inverse edge present! 
                      ;; default relation must symmetric (e.g., :DC) 

                      (values (copy-tree (list nil (first default-edge-label)))
                              nil))

                     (t 

                      (values (copy-tree (list nil (list (roles (rbox substrate)))))
                              nil)))))))))

;;;
;;;
;;;


(defmethod compute-minimal-label ((substrate rcc-substrate))
  (let* ((iter t)
         (rbox (rbox substrate))
         (consistent-p t)
         (nodes (get-nodes substrate)))

    (with-slots (edge-consistent-p minimal-label-computed-p) substrate

      (when (and (not minimal-label-computed-p)
                 edge-consistent-p)

        (block loop

          (loop while iter do
                (setf iter nil)

                (mapl #'(lambda (nodes1) 

                          (let ((from (first nodes1)))

                            (mapl #'(lambda (nodes2) 

                                      (let* ((to (first nodes2)))

                                        (multiple-value-bind (from-to foundp)
                                            (get-edge-between substrate from to)

                                          (declare (ignorable foundp))
					  
                                          (dolist (over nodes)
                                            
                                            (multiple-value-bind (from-over foundp)
                                                (get-edge-between substrate from over)

                                              (declare (ignorable foundp))
					      
                                              (multiple-value-bind (over-to foundp)
                                                  (get-edge-between substrate over to)
						
                                                (declare (ignorable foundp))
                                                
						
                                                (let* ((r (edge-info-aux-type from-over))
                                                       (s (edge-info-aux-type over-to))
                                                       (rs (edge-info-aux-type from-to)))
						  
						  (when (member +inconsistent-marker+
								(list r s rs))
						    (setf consistent-p nil)
						    (return-from loop))
						  
						  (let ((comp 
							 (list (lookup rbox (first r) (first s)))))
						    
						    #|
						    (terpri)
						    (format t "(~A,~A) : ~A~%" from over r)
						    (format t "(~A,~A) : ~A~%" over to s)
						    (format t "(~A,~A) : ~A /\\ ~A~%" from to rs comp)
						    |# 
						    
						    (when (eq (first comp)
							      +inconsistent-marker+)
						      (setf consistent-p nil)
						      (return-from loop))
						    
						    (unless (implies-p rs comp)

						      (setf iter t)
						      
						      (let ((res 
							     (intersection (first rs) (first comp))))
							
							(if res
							    (nodes-related substrate from to (list res))
							  (progn 
							    (nodes-related substrate from to +inconsistent-marker+)
							    (setf consistent-p nil)
							    (return-from loop)))))))))))))

					;(rest nodes1)
                                  nodes1)))

                      nodes)))

        (setf edge-consistent-p consistent-p)

        (when consistent-p
          (setf minimal-label-computed-p t))

        substrate))))

;;;
;;;
;;;

(defmethod rcc-related-p ((substrate rcc-substrate) from to &optional descr)
  (compute-minimal-label substrate)

  (multiple-value-bind (edge foundp) 
      (get-edge-between substrate from to)

    (declare (ignorable foundp))

    (let ((type (edge-info-aux-type edge)))

      (implies-p type descr))))


;;;
;;;
;;;


(defmethod check-abox-consistency ((substrate rcc-substrate) &key &allow-other-keys)
  (call-next-method) ;; loest einen Error aus wenn ABox inkonsistent
  
  (or (consistent-p substrate)
      (nrql-error "RCC substrate ~A is inconsistent! Querying denied" (abox substrate))))


(defmethod consistent-p ((substrate rcc-substrate))
  (when (substrate-needs-reset-p substrate) 
    (reset-substrate substrate))

  (if (not (slot-value substrate 'edge-consistent-p))
      nil
    (progn
      (compute-minimal-label substrate)
      (slot-value substrate 'edge-consistent-p))))

;;;
;;; API 
;;;

(defmethod reset-substrate :before ((substrate rcc-substrate) &key &allow-other-keys)
  (setf (slot-value substrate 'edge-consistent-p) :unknown))

;;;
;;;
;;; 

(nrql-defun (set-rcc-box
             :doc ((:doc-type :long)
                   (:category :substrate-layer)
                   (:description "Creates an RCC substrate for ABox \\argument{abox}")
                   (:syntax (set-rcc-box abox &optional rcc-type))
                   (:arguments 
                    (:first abox "the (name of the) ABox")
                    (:optional rcc-type :rcc8 "either {\\tt :rcc5} {\\tt :rcc8}"))
                   (:values "the \\argument{abox}")
                   (:remarks)
                   (:examples)
                   (:see-also set-data-box enable-data-substrate-mirroring 
                    set-substrate-type)))

  (name &optional (rcc-type :rcc8) (type 'rcc-substrate))

  (setf *type-of-substrate* type
        *rcc-type* rcc-type)

  (set-data-box name)

  name)

(nrql-defmacro (in-rcc-box :nrql-function set-rcc-box
                           :doc ((:doc-type :short)
                                 (:category :substrate-layer))))

;;;
;;;
;;;

(nrql-defun (create-rcc-node
             :doc ((:doc-type :short)
                   (:category :substrate-layer)
                   (:synonym create-data-node)))
  (&rest args)
  (apply #'create-data-node args))

(nrql-defun (create-rcc-edge
             :doc ((:doc-type :short)
                   (:category :substrate-layer)
                   (:synonym create-data-edge)))
  (&rest args)
  (apply #'create-data-edge args))

(nrql-defun (rcc-consistent-p
             :doc ((:doc-type :long)
                   (:category :substrate-layer)
                   (:description "Checks the RCC substrate for relational RCC consistency")
                   (:syntax (rcc-consistent-p &optional abox type-of-substrate))
                   (:arguments 
                    (:optional abox (current-abox) "the (name of the) ABox")
                    (:optional type-of-substrate "'rcc-substrate" 
                     "the type of the substrate"))
                   (:values t nil)
                   (:remarks)
                   (:examples)
                   (:see-also )))

  (&optional abox type-of-substrate)
  (with-data-box (abox type-of-substrate)
                 (consistent-p *cur-substrate*)))

(nrql-defmacro (rcc-consistent? :nrql-function rcc-consistent-p
                                :doc ((:doc-type :short)
                                      (:category :substrate-layer))))



(nrql-defun (register-rcc-synonym 
             :doc ((:doc-type :long)
                   (:category :substrate-layer)
                   (:description "Registers the role name \\argument{role} as a synonym for the RCC relation
\\argument{rcc-relation}")
                   (:syntax (register-rcc-synoym role rcc-relation))
                   (:arguments 
                    (:first role "the role")
                    (:second rcc-relation "the RCC relation"))
                   (:values "the synonym mapping function (a list)")
                   (:remarks)
                   (:examples)
                   (:see-also )))

  (role rcc-relation)
	    
  (when (assoc role *rcc-synonyms*)
    (setf *rcc-synonyms*
          (delete role *rcc-synonyms* :key #'first)))
  (push (list role rcc-relation) *rcc-synonyms*))

(nrql-defmacro (rcc-synonym :nrql-function register-rcc-synonym
                            :doc ((:doc-type :short)
                                  (:category :substrate-layer))))

(nrql-defun (delete-rcc-synonyms 
	     :doc ((:doc-type :short)
		   (:description "Deletes all registered RCC 
synonyms (see \\funref{register-rcc-synonym}")
		   (:category :substrate-layer)))
  ()
  (setf *rcc-synonyms* nil)
  :okay)

;;;
;;;
;;;

(nrql-defun (rcc-instance1
             :doc ((:doc-type :short)
                   (:category :substrate-layer)
                   (:synonym data-node1)))
  (&rest args)
  (apply #'data-node1 args))

(nrql-defmacro (rcc-instance
                :nrql-function rcc-instance1
                :doc ((:doc-type :short)
                      (:category :substrate-layer))))

(nrql-defun (rcc-related1
             :doc ((:doc-type :short)
                   (:category :substrate-layer)
                   (:synonym data-edge1)))
  (&rest args)
  (apply #'data-edge1 args))

(nrql-defmacro (rcc-related 
                :nrql-function rcc-related1
                :doc ((:doc-type :short)
                      (:category :substrate-layer))))

(nrql-defun (rcc-node1
             :doc ((:doc-type :short)
                   (:category :substrate-layer)
                   (:synonym data-node1)))
  (&rest args)
  (apply #'data-node1 args))

(nrql-defmacro (rcc-node :nrql-function rcc-node1
                         :doc ((:doc-type :short)
                               (:category :substrate-layer))))

(nrql-defun (rcc-edge1
             :doc ((:doc-type :short)
                   (:category :substrate-layer)
                   (:synonym data-edge1)))
  (&rest args)
  (apply #'data-edge1 args))

(nrql-defmacro (rcc-edge :nrql-function rcc-edge1
                         :doc ((:doc-type :short)
                               (:category :substrate-layer))))

;;;
;;;
;;;

(nrql-defun (rcc-node-label1
             :doc ((:doc-type :short)
                   (:category :substrate-layer)
                   (:synonym node-label1)))
  (&rest args)
  (apply #'node-label1 args))

(nrql-defmacro (rcc-node-label :nrql-function rcc-node-label1
                               :doc ((:doc-type :short)
                                     (:category :substrate-layer))))

(nrql-defun (rcc-edge-label1
             :doc ((:doc-type :short)
                   (:category :substrate-layer)
                   (:synonym node-label1)))
  (&rest args)
  (apply #'edge-label1 args))

(nrql-defmacro (rcc-edge-label :nrql-function rcc-edge-label1
                               :doc ((:doc-type :short)
                                     (:category :substrate-layer))))


;;;
;;;
;;; 

(nrql-defun (rcc-node-description1
             :doc ((:doc-type :short)
                   (:category :substrate-layer)
                   (:synonym node-description1)))
  (&rest args)
  (apply #'node-description1 args))

(nrql-defmacro (rcc-node-description :nrql-function rcc-node-description1
                                     :doc ((:doc-type :short)
                                           (:category :substrate-layer))))

(nrql-defun (rcc-edge-description1
             :doc ((:doc-type :short)
                   (:category :substrate-layer)
                   (:synonym node-description1)))
  (&rest args)
  (apply #'edge-description1 args))

(nrql-defmacro (rcc-edge-description :nrql-function rcc-edge-description1
                                     :doc ((:doc-type :short)
                                           (:category :substrate-layer))))

;;;
;;;
;;;

(nrql-defun (del-rcc-node1
             :doc ((:doc-type :short)
                   (:category :substrate-layer)
                   (:synonym del-data-node1)))
  (&rest args)
  (apply #'del-data-node1 args))

(nrql-defun (del-rcc-edge1
             :doc ((:doc-type :short)
                   (:category :substrate-layer)
                   (:synonym del-data-edge1)))
  (&rest args)
  (apply #'del-data-edge1 args))

(nrql-defmacro (del-rcc-node :nrql-function del-rcc-node1
                             :doc ((:doc-type :short)
                                   (:category :substrate-layer))))

(nrql-defmacro (del-rcc-edge :nrql-function del-rcc-edge1
                             :doc ((:doc-type :short)
                                   (:category :substrate-layer))))

;;;
;;;
;;;

(nrql-defun (enable-rcc-substrate-mirroring 
             :doc ((:doc-type :short)
                   (:category :substrate-layer)
                   (:description "Like
              \\funref{enable-data-substrate-mirroring}, but substrates of type
              {\\tt rcc-mirror-substrate} are created. This is a {\\tt
              mirror-data-substrate} as well as a {\\tt rcc-substrate}")
                   (:syntax (enable-rcc-substrate-mirroring))
                   (:inverse-of disable-rcc-substrate-mirroring)
                   (:arguments )
                   (:values :okay-rcc-substrate-mirroring-enabled)
                   (:remarks "Note that you can register a set of RCC
              relation synonyms using \\funref{register-rcc-synonym}. This allows
              you, for example, to declare a set of OWL object properties to be used
              as RCC relations in the RCC substrate")
                   (:examples)
                   (:see-also disable-rcc-substrate-mirroring 
                    register-rcc-synonym
                    describe-query-processing-mode)))
  ()
  (setf *type-of-substrate* 'rcc-mirror-substrate)
  (setf *initial-abox-mirroring-p* t)	;;; wichtig!!!

  :okay-rcc-substrate-mirroring-enabled)


(nrql-defun (disable-rcc-substrate-mirroring 
             :doc ((:doc-type :short)
                   (:category :substrate-layer)
                   (:inverse-of enable-rcc-substrate-mirroring)))
  ()
  (setf *type-of-substrate* 'racer-dummy-substrate)
  :okay-rcc-substrate-mirroring-disabled)


;;;
;;;
;;;

(defquery-code (rcc-substrate-edge-query)
  (:tester
   (:runtime
    (continuation &rest args &key from to &allow-other-keys)
    (with-slots (negated-p substrate voi-from voi-to) query
      (let ((from (or from (bound-to voi-from)))
            (to (or to (bound-to voi-to))))
        (if negated-p
            (unless (rcc-related-p substrate from to query)
              (apply continuation :from from :to to args))
          (when (rcc-related-p substrate from to query)
            (apply continuation :from from :to to args)))))))
  
  (:from-bound-enumerator 
   (:runtime 
    (continuation &rest args &key &allow-other-keys)
    (with-slots (negated-p substrate voi-from) query    
      (if negated-p
          (loop-over-nodes (substrate var)
            (unless (rcc-related-p (bound-to voi-from) var query)
              (apply continuation :to var args)))

        (loop-over-all-rcc-successors (substrate (bound-to voi-from) query)
                                      (var)
                                      (apply continuation :to var args))))))

  (:to-bound-enumerator
   (:runtime 
    (continuation &rest args &key &allow-other-keys)
    (with-slots (negated-p substrate voi-to) query    
      (if negated-p
          (loop-over-nodes (substrate var)
            (unless (rcc-related-p var (bound-to voi-to) query)
              (apply continuation :from var args)))

        (loop-over-all-rcc-predecessors (substrate (bound-to voi-to) query)
                                        (var)
                                        (apply continuation :from var args))))))


  (:enumerator
   (:runtime
    (continuation &rest args &key &allow-other-keys)
    (with-slots (negated-p substrate) query
      (if negated-p
          (if (eq (voi-from query)
                  (voi-to query))
              (loop-over-nodes (substrate from)
                (unless (rcc-related-p substrate from from query)
                  (apply continuation :from from :to from args)))
            (loop-over-nodes (substrate from)
              (loop-over-nodes (substrate to)
                (unless (rcc-related-p substrate from to query)
                  (apply continuation :from from :to to args)))))
        (if (eq (voi-from query)
                (voi-to query))        
            (loop-over-nodes (substrate from)
              (when (rcc-related-p substrate from from query)
                (apply continuation :from from :to from args)))
          (loop-over-nodes (substrate from)
            (loop-over-nodes (substrate to)
              (when (rcc-related-p substrate from to query)
                (apply continuation :from from :to to args))))))))))

