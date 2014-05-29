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

(declaim (special *socket* *server-control-socket* 
                  *server-control-process* *exit-server-requested*))

;;;
;;; 
;;; 

(defun map-over-queries-for-aboxes-of-type (fn queries-or-rules &key abox (type-of-substrate *type-of-substrate*) &allow-other-keys)
  (remove nil 
          (mapcar #'(lambda (q)
                      (when (or (not abox)
                                (and (eq abox (abox (substrate q)))
                                     (or (eq (type-of (substrate q)) type-of-substrate)
					 (eq type-of-substrate :all))))
                        (funcall fn q)))
                  queries-or-rules)))

(defun map-over-substrates-of-type-for-abox (fn &key abox (type-of-substrate *type-of-substrate*) &allow-other-keys)
  (remove nil 
          (mapcar #'(lambda (s)
                      (when (or (not abox)
                                (and (eq abox (abox s))
                                     (or (eq (type-of s) type-of-substrate)
					 (eq type-of-substrate :all))))
                        (funcall fn s)))
                  *all-substrates*)))

;;;
;;; Racer Parameters 
;;; 


(nrql-defun (set-racer-parameter)
  (name value)
  
  (setf (symbol-value (intern (symbol-name name) :racer)) value)
  (list name value))


;;;
;;; MiniLisp 
;;; 

(nrql-defun (evaluate1 
             :doc ((:doc-type :long)
                   (:category :basic-commands)
                   (:description "Evaluates a MiniLisp expression on the server")
                   (:syntax (evaluate minilisp-body))
                   (:arguments
                    (:first minilisp-body "a MiniLisp lambda body"))
                   (:values "The value returned by the lambda body")
                   (:examples (evaluate (1+ 3)))))
	    (&rest args)
	    (let ((*package* (find-package :racer-user)))
	      ;;; MiniLisp funktioniert nur in racer-user!
	      (eval-expr nil (cons 'progn args) nil)))


(nrql-defmacro (evaluate :nrql-function evaluate1
			 :doc ((:doc-type :short)
			       (:category :basic-commands))))


;;;
;;;
;;;

(nrql-defun (define1 
             :doc ((:doc-type :long)
                   (:category :basic-commands)
                   (:description "Defines a MiniLisp function on the server. Functions can be called using \\funref{fcall}")
                   (:syntax (define function-name parameter-list minilisp-body))
                   (:arguments
                    (:first minilisp-bodies "a (list of) MiniLisp lambda bodies, see \\funref{evaluate}")
                    (:second function-name "name of the function")
                    (:third parameter-list "parameter list of the function"))
                   (:values function-name)
                   (:examples (define twice (a) (+ a a)))))
  (name arglist &rest body)
  (apply #'register-function (change-package-of-description name :racer-user) arglist body)
  name)

(nrql-defmacro (define :nrql-function define1
			 :doc ((:doc-type :short)
			       (:category :basic-commands))))


(nrql-defun (undefine1
             :doc ((:doc-type :long)
                   (:category :basic-commands)
                   (:description "Delete a MiniLisp function from the server")
                   (:syntax (undefine function-name))
                   (:arguments
                    (:first function-name "name of the function"))
                   (:values :okay)
                   (:examples (define twice (a) (+ a a)))))
  (name)
  
  (if (minilisp-function-p name)
      (progn 
        (unregister-function name)
        :okay)
    :not-found))

(nrql-defmacro (undefine :nrql-function undefine1
			 :doc ((:doc-type :short)
			       (:category :basic-commands))))


(nrql-defun (undefine-all
             :doc ((:doc-type :long)
                   (:category :basic-commands)
                   (:description "Delete all MiniLisp functions from the server")
                   (:syntax (undefine-all))
                   (:arguments)
                   (:values :okay)
                   (:examples (undefine-all))))
  ()
  
  (clrhash *registered-functions*)
  
  :okay)


(nrql-defun (fcall :doc ((:doc-type :long)
                         (:category :basic-commands)
                         (:description "Calls a MiniLisp function previously defined with \\funref{define}. Recursive {\\tt fcall}s are aborted at runtime to ensure termination")
                         (:syntax (fcall function-name &rest args))
                         (:arguments
                          (:first function-name "name of the function to call")
                          (:args  args "arguments for the function"))
                         (:values "The value returned by the function \\argument{function-name}")
                         (:examples (fcall twice 3))))
  (name &rest args)
  
  (apply #'call-function name args))

(nrql-defun (get-all-functions :doc ((:doc-type :long)
                                     (:category :basic-commands)
                                     (:description "Returns all MiniLisp function previously defined with \\funref{define}")
                                     (:syntax (get-all-functions))
                                     (:arguments)
                                     (:values "The list of all function definitions")
                                     (:examples (get-all-functions))))
  ()
  
  (get-minilisp-functions))


(nrql-defun (get-all-server-functions :doc ((:doc-type :long)
                                     (:category :basic-commands)
                                     (:description "Returns all MiniLisp server functions")
                                     (:syntax (get-all-functions))
                                     (:arguments)
                                     (:values "The list of all server function definitions")
                                     (:examples (get-all-server-functions))))
  ()
  
  (get-minilisp-server-functions))


(nrql-defun (server-function  
             :doc ((:doc-type :long)
                   (:category :basic-commands)
                   (:description "Registers MiniLisp function as a server function")
                   (:syntax (server-function name))
                   (:arguments
                    (:first name "a MiniLisp function"))
                   (:values :okay)
                   (:examples (server-function test))
                   (:see-also define undefine get-all-functions)))
  (name)
  (register-as-server-function name))


;;;
;;;
;;;


(nrql-defun (defcon1 
             :doc ((:doc-type :long)
                   (:category :basic-commands)
                   (:description "Defines a MiniLisp constant")
                   (:syntax (defcon1 constant-name literal))
                   (:arguments
                    (:first constant-name "the name of the constant (a symbol)")
                    (:second literal "a MiniLisp literal"))
                   (:values literal)
                   (:examples (defcon1 pi 3.141))))
  (name value)
  (register-value (change-package-of-description name :racer-user) value))


(nrql-defmacro (defcon :nrql-function defcon1
                       :doc ((:doc-type :short)
                             (:category :basic-commands))))



(nrql-defun (defpar1 
             :doc ((:doc-type :long)
                   (:category :basic-commands)
                   (:description "Defines a MiniLisp parameter")
                   (:syntax (defcon1 constant-name expression))
                   (:arguments
                    (:first constant-name "the name of the constant (a symbol)")
                    (:second expression "a MiniLisp expression which is evaluted to yield the parameter value"))
                   (:values "The evaluated \\argument{expression}")
                   (:examples (defpar1 pi (+ 1 2)))))
  (name value)
  (register-value (change-package-of-description name :racer-user) 
                  (evaluate1 value)))


(nrql-defmacro (defpar :nrql-function defpar1
                       :doc ((:doc-type :short)
                             (:category :basic-commands))))


(nrql-defun (unbind1
             :doc ((:doc-type :long)
                   (:category :basic-commands)
                   (:description "Delete a MiniLisp constant or parameter")
                   (:syntax (unbind1 name))
                   (:arguments
                    (:first name "name of the constant or parameter"))
                   (:values :okay)
                   (:examples (unbind1 pi))))
  (name)
  
  (if (minilisp-value-p name)
      (progn 
        (unregister-value name)
        :okay)
    :not-found))


(nrql-defmacro (unbind :nrql-function unbind1
			 :doc ((:doc-type :short)
			       (:category :basic-commands))))



(nrql-defun (unbind-all
             :doc ((:doc-type :long)
                   (:category :basic-commands)
                   (:description "Deletes all MiniLisp constants and parameters")
                   (:syntax (unbind-all))
                   (:arguments)
                   (:values :okay)
                   (:examples (unbind-all))))
  ()
  
  (clrhash *registered-values*)
  
  :okay)


(nrql-defun (get-all-values :doc ((:doc-type :long)
                                     (:category :basic-commands)
                                     (:description "Returns all MiniLisp parameters and constants previously defined with \\funref{defcon} or \\funref{defpar}")
                                     (:syntax (get-all-values))
                                     (:arguments)
                                     (:values "The list of all function definitions")
                                     (:examples (get-all-values))))
  ()
  
  (get-minilisp-values))

(nrql-defun (get-all-server-values :doc ((:doc-type :long)
                                     (:category :basic-commands)
                                     (:description "Returns all MiniLisp server parameters and constants")
                                     (:syntax (get-all-server-values))
                                     (:arguments)
                                     (:values "The list of all server value definitions")
                                     (:examples (get-all-server-values))))
  ()
  
  (get-minilisp-server-values))


(nrql-defun (server-value  
             :doc ((:doc-type :long)
                   (:category :basic-commands)
                   (:description "Registers MiniLisp parameter or constant as a server parameter or constant")
                   (:syntax (server-value name))
                   (:arguments
                    (:first name "a MiniLisp parameter or constant"))
                   (:values :okay)
                   (:examples (server-value pi))
                   (:see-also defpar defcon get-all-value)))
  (name)
  (register-as-server-value name))


;;;
;;;
;;; 


(defun server-hook (fn)
  (cond ((and (symbolp fn)
              (fboundp fn))
         (add-server-hook-function fn)
         (push (symbol-function fn) *server-hooks*))
        (t (nrql-error "~A is not a function" fn))))

;;;
;;; reset-all-substrate, reset-nrql-engine, full-reset, prepare-nrql-engine 
;;; 

(nrql-defun (reset-all-substrates 
             :doc ((:doc-type :long)
                   (:category :basic-commands)
                   (:description "Resets all substrates")
                   (:syntax (reset-all-substrates))
                   (:arguments
                    (:key abox (current-abox) "consider substrates for ABox {\\tt abox} only")
                    (:key type-of-substrate  racer-dummy-substrate "consider substrates of 
                                                 specified type only, one of:
                                                  \\argument{racer-dummy-substrate,
                                                  data-substrate, mirror-data-substrate, rcc-substrate,
                                                  racer-tbox-mirror-substrate}"))
                   (:values :okay-all-substrates-reset)
                   (:remarks "Does not delete anything from the server")
                   (:examples)
                   (:see-also delete-all-substrates reset-nrql-engine all-substrates describe-all-substrates)))
  
  (&rest args)
  
  (apply #'map-over-substrates-of-type-for-abox
         #'substrate-needs-reset 
         args)

  :okay-all-substrates-reset)


(nrql-defun (set-substrate-type 
             :doc ((:doc-type :long)
                   (:category :basic-commands)
                   (:description "Determines the type (class) of the substrates that
nRQL creates internally on request")
                   (:syntax (set-substrate-type type-of-substrate))
                   (:arguments 
                    (:first type-of-substrate "a substrate type (class), one of: {\\tt data-substrate, 
mirror-data-substrate, rcc-substrate, rcc-mirror-substrate}"))
                   (:values :okay :ignored)
                   (:remarks)
                   (:examples)
                   (:see-also get-substrate-type describe-query-processing-mode)))
  (type)
  (let ((type (change-package-of-description type :ts t)))
    (if (member type '(data-substrate mirror-data-substrate rcc-substrate rcc-mirror-substrate))
        (progn (setf *type-of-substrate* type)
          :okay)
      :ignored)))


(nrql-defun (get-substrate-type 
             :doc ((:doc-type :long)
                   (:category :basic-commands)
                   (:description "Returns the type (class) of the substrates that 
nRQL creates internally on request")
                   (:syntax (get-substrate-type))
                   (:arguments )
                   (:values "the type (class), a symbol")
                   (:remarks)
                   (:examples)
                   (:see-also set-substrate-type describe-query-processing-mode)))
  ()

  *type-of-substrate*)


(nrql-defun (delete-all-substrates 
             :doc ((:doc-type :long)
                   (:category :basic-commands)
                   (:description "Deletes all substrates")
                   (:syntax (delete-all-substrates))
                   (:arguments
                    (:key abox (current-abox) "consider substrates for ABox {\\tt abox} only")
                    (:key type-of-substrate  racer-dummy-substrate "consider substrates of 
                                                 specified type only, one of:
                                                  \\argument{racer-dummy-substrate,
                                                  data-substrate, mirror-data-substrate, rcc-substrate,
                                                  racer-tbox-mirror-substrate}"))
                   (:values :okay-all-substrates-deleted)
                   (:remarks )
                   (:examples)
                   (:see-also reset-all-substrates reset-nrql-engine all-substrates describe-all-substrates)))
  
  (&rest args)
  
  (setf *all-substrates*
        (set-difference *all-substrates*
                        (apply #'map-over-substrates-of-type-for-abox 
                               #'identity 
                               args)))

  (unless (member *cur-substrate* *all-substrates*)
    (setf *cur-substrate* nil))

  :okay-all-substrates-deleted)

(nrql-defun (all-substrates 
             :doc ((:doc-type :long)
                   (:category :basic-commands)
                   (:description "Returns a list of all substrates. A substrate
is the internal (ABox) representation that nRQL needs in order to answer queries. A substrate
has a type and a corresponding Racer ABox. For each ABox and type there is at most one substrate") 
                   (:syntax (all-substrate))
                   (:arguments
                    (:key abox (current-abox) "consider substrates for ABox {\\tt abox} only")
                    (:key type-of-substrate  racer-dummy-substrate "consider substrates of 
                                                 specified type only, one of:
                                                  \\argument{racer-dummy-substrate,
                                                  data-substrate, mirror-data-substrate, rcc-substrate,
                                                  racer-tbox-mirror-substrate}"))
                   (:values "A list of (abox type-of-substrate) entries, denoting the name of
the substrate (which is identical to the name of the associated ABox) as well as the type of the substrate")
                   (:remarks "")
                   (:examples)
                   (:see-also reset-all-substrates describe-all-substrates)))
  
  (&rest args)
  
  (apply #'map-over-substrates-of-type-for-abox 
         #'(lambda (x) (list (abox x) (type-of x)))
         args))


(nrql-defun (reset-nrql-engine 
  
             :doc ((:doc-type :long)
                   (:category :basic-commands)
                   (:description "Aborts all (active) queries and rules using
\\funref{abort-all-queries}, \\funref{abort-all-rules}, then resets the
internal caches of the nRQL engine using
\\funref{reset-all-substrates}), and finally calls
\\funref{restore-standard-settings}.

If \\argument{full-reset-p} = {\\tt t} is given, nRQL will delete all
TBoxes (as well as the associated ABoxes) using
\\funref{delete-all-tboxes}), delete all the queries and rules using
\\funref{delete-all-queries}, \\funref{delete-all-rules}, deletes all
substrates (as well as the associated QBoxes) and associated defined
queries")
                   (:syntax (reset-nrql-engine &key full-reset-p))
                   (:arguments  ; type, name, default, description 
                    (:key full-reset-p nil "pass {\\tt t} if you really want to
reset the nRQL engine fully - note that this will delete everything
from the RacerPro server"))
                   (:values :okay-full-reset :okay-engine-reset)
                   (:remarks)
                   (:examples)
                   (:see-also reset-nrql-engine restore-standard-settings)))

  (&key full-reset-p)
  
  (abort-all-queries)
  (abort-all-rules)
        
  (if full-reset-p
      (progn 
        #+:midelora
        (prover::delete-all-tboxes)
        (delete-prefix-mappings)
        (delete-all-tboxes)
        #+:racer-server
        (delete-rcc-synonyms)
	;;; (clear-mirror-table)
        (setf *iterator-id* 0)
        (delete-all-queries)
        (delete-all-rules)
        (delete-all-substrates)
        (delete-all-dboxes)
        ;;; (undefine-all)
        ;;; (unbind-all)
        (|OWLAPI-init|)
        (owl-syntaxes:owllink-init)
        )    
    (reset-all-substrates))

  (restore-standard-settings)
  
  (if full-reset-p 
      :okay-full-reset
    :okay-engine-reset))
      
(nrql-defun (full-reset 
             :doc ((:doc-type :short)
                   (:category :basic-commands)
                   (:description "Simply calls 
\\code{(reset-nrql-engine :full-reset-p t)}, see 
\\funref{reset-nrql-engine}")))

  ()
  
  (reset-nrql-engine :full-reset-p t))


(nrql-defun (exit-server 
             :doc ((:doc-type :short)
                   (:category :basic-commands)
                   (:description "Exists the current RacerPro server.")))
	    
  ()
  
  (setf *exit-server-requested* t)

  ;; (check-if-unsafe)
  
  #+:allegro (progn 
	       (close *socket*)
	       (close *server-control-socket* )
	       (mp:process-kill *server-control-process*)
	       (excl:exit 0 :quiet t :no-unwind t))
  
  #+:lispwork (lispworks:quit))

#+:racer-server
(nrql-defun (in-unsafe-mode? 
             :doc ((:doc-type :short)
                   (:category :basic-commands)
                   (:description "Check whether RacerPro is running in unsafe mode")))
	    
  ()
  
  *unsafe-mode*)


(nrql-defun (prepare-nrql-engine 
             :doc ((:doc-type :long)
                   (:category :basic-commands)
                   (:description "Prepares the internal index
structures of the nRQL engine for query answering on the ABox
\\argument{abox}.  Usually, there is no need to call this function. 
The function will be called automatically before the first query to 
\\argument{abox} is executed. Thus, answering the first query to 
\\argument{abox} might take considerably longer than subsequent queries
to that ABox. For benchmarking purposes, the nRQL engine should thus be 
prepared using this function (or with \\funref{prepare-racer-engine})
 before the first query is executed")
                   (:syntax (prepare-nrql-engine abox &rest args))
                   
                   (:arguments 
                    (:optional abox (current-abox) "the (name of the) ABox for which the
engine is prepared")
                    (:reference (args) (with-nrql-settings)))

                   (:values "The (name of the) ABox \\argument{abox} is returned") 
                   (:remarks)
                   (:examples)
                   (:see-also reset-nrql-engine prepare-racer-engine)))

  (&optional abox &rest args)

  (with-racer-timeout
    (apply #'eval-nrql-settings
           #'(lambda ()
               (apply #'racer-prepare-substrate 
                      :abox abox
                      :prepare-now-p t
                      :initial-abox-mirroring-p t
                      args))
           args)
    
    abox))


;;;
;;; speziell f. Sirius (ABox-Graph) 
;;; 

#+:racer-server
(nrql-defun (get-individual-successors)
             
  (ind &rest args 
       &key
       (no-inverses-p t) 
       only-inverses-p
       no-transitives-p
       (no-top-role-p t)
       (abox (current-abox))
       negated-p 
       roles 
       (remove-synonyms-p t) &allow-other-keys)
	    
  (let ((tbox (associated-tbox abox)))

    (ensure-knowledge-base-state :abox-prepared (find-abox abox))
	      
    (remove nil 
            (mapcar #'(lambda (role) 
                        (when (and (=> (consp role) (not no-inverses-p))
                                   (=> (symbolp role)
                                       (not (role-used-as-datatype-property-p role tbox)))
                                   (=> (transitive-p role tbox) (not no-transitives-p))
                                   (=> (eq role +top-object-role-symbol+)
                                       (not no-top-role-p)))        
                          (let* ((role 
                                  (if only-inverses-p ; nur fuer OWLlink 
                                      (if (consp role)
                                          (second role)
                                        (atomic-role-inverse role tbox))
                                    role))
                                 (succs 
                                  (when role
                                    (apply #'racer-retrieve-individual-fillers ind 
                                           (if negated-p 
                                               `(not ,role)
                                             role)
                                           :abox abox
                                           :remove-synonyms-p remove-synonyms-p 
                                           :allow-other-keys t
                                           args))))
                            (when succs
                              (list role succs)))))
				  
                    (or roles 
                        (all-roles (associated-tbox abox)))))))

#+:racer-server
(nrql-defun (get-individual-datatype-fillers)
   (individual-name &optional (abox (current-abox)) (with-types-p t))

   (let* ((abox (find-abox abox))	 
	  (tbox (associated-tbox abox))
	  (roles (all-roles tbox))
	  (fillers nil))
	      
    (ensure-knowledge-base-state :abox-prepared (find-abox abox))
    
     (loop for role in roles 
	 when (and (symbolp role)
		     (role-used-as-datatype-property-p role tbox)
		     (setf fillers
		       (retrieve-individual-told-datatype-fillers 
			individual-name role nil abox with-types-p)))
	 collect `(,role ,fillers))))

#+:racer-server
(nrql-defun (get-individual-annotation-datatype-fillers)
   (individual-name &optional (abox (current-abox)) (with-types-p t))

   (let* ((abox (find-abox abox))	 
	  (tbox (associated-tbox abox))
	  (roles (all-roles tbox))
	  (fillers nil))

     (ensure-knowledge-base-state :abox-prepared (find-abox abox))

     (loop for role in roles 
	 when (and (symbolp role)
		   (role-used-as-datatype-property-p role tbox)
		   (role-used-as-annotation-property-p role tbox)
		   (setf fillers
		     (retrieve-individual-annotation-property-fillers
		      individual-name role abox with-types-p)))
	 collect `(,role ,fillers))))
     
#+:racer-server
(nrql-defun (get-individual-annotation-fillers)
     (individual-name &optional (abox (current-abox)))

     (let* ((abox (find-abox abox))	 
	    (tbox (associated-tbox abox))
	    (roles (all-roles tbox))
	    (fillers nil))
	      
     (ensure-knowledge-base-state :abox-prepared (find-abox abox))

     (loop for role in roles 
	 when (and (symbolp role)
		   (not (role-used-as-datatype-property-p role tbox))
		   (role-used-as-annotation-property-p role tbox)
		   (setf fillers
		     (retrieve-individual-fillers individual-name role abox :told t)))
	 collect `(,role ,fillers))))

;;;
;;; 
;;; 

(nrql-defun (get-concept-properties) (concept 
				      &optional
				      (tbox (current-tbox))
				      &key
				      (for-roles (all-roles tbox :inverse-test (lambda (r) (declare (ignorable r)) nil)))
				      (qualifications (all-atomic-concepts tbox)))
	    
	    (let ((res nil)
		  (n (length for-roles))
		  (m 0))

              (set-progress-100% n)

	      (dolist (role for-roles)
		(incf m)
		(set-progress-value m)
		(unless (concept-satisfiable-p
			 `(and ,concept (not (some ,role top)))
			 tbox)
		  (let ((quals nil))
		    (dolist (qual qualifications)
		      (unless (concept-satisfiable-p `(and ,concept (not (some ,role ,qual)))
						     tbox)
			(push qual quals)))
		    (when quals
		      (push (list role (remove 'top quals))
			    res)))))
	      res))


	      
(nrql-defun (get-role-hierarchy)
	    (&optional (tbox (current-tbox)) 
             &key
             (for-roles (all-roles tbox :inverse-test (lambda (r) (declare (ignorable r)) nil))))
  
  (labels ((get-syns (r)
             (if (listp r)
                 (mapcar #'(lambda (r) 
                             (atomic-role-synonyms r tbox))
                         r)
               (atomic-role-synonyms r tbox))))
  
    (delete-duplicates
     (let* ((n (length for-roles))
	    (m 0)
	    
	    (rh
             (with-progress-range (n (0 100))
               (mapcar #'(lambda (r)
                           (incf m)
                           (set-progress-value m)
                           (let ((r (first (ensure-list r))))
                             `(,(get-syns r)
                               ,(or (atomic-role-parents  r tbox :synsets-p t) `((,+top-object-role-symbol+)))
                               ,(or (atomic-role-children r tbox :synsets-p t) `((,+bottom-object-role-symbol+))))))
                       for-roles)))
            (roots 
             (loop as (role parents) in rh
		 when (or (equal parents `((,+top-object-role-symbol+))) ;; ??? 
			  (equal parents `(,+top-object-role-symbol+)))
		 collect role)))
              
       (cons `((,+top-object-role-symbol+) nil ,roots)
             rh))
     :test #'equal
     :key #'first)))

#|
(nrql-defun (get-role-hierarchy)
	    (&optional (tbox (current-tbox)) 
             &key
             (for-roles (all-roles tbox :inverse-test (lambda (r) (declare (ignorable r)) nil))))
  
  (labels ((get-syns (r)
             (if (listp r)
                 (mapcar #'(lambda (r) 
                             (atomic-role-synonyms r tbox))
                         r)
               (atomic-role-synonyms r tbox))))
  
    (delete-duplicates
     (let* ((n (length for-roles))
	    (m 0)
	    
	    (rh
             (mapcar #'(lambda (r)
			 (incf m)
			 (set-progress-value m n)
                         (let ((r (first (ensure-list r))))
                           `(,(get-syns r)
                             ,(or (atomic-role-parents  r tbox :synsets-p t) '(top-role))
                             ,(or (atomic-role-children r tbox :synsets-p t) '(bottom-role)))))
                     for-roles))
            (roots 
             (loop as (role parents) in rh
                   when (equal parents '(top-role))
		 collect role)))
              
       (cons `((top-role) nil ,roots)
             rh))
     :test #'equal
     :key #'first)))
|#              

(nrql-defun (get-abox-graph)
  (&optional (abox (current-abox)) 
             &key 
             depth
             no-transitives-p 
	     (no-top-role-p t)
	     (browsing-mode-p (and (numberp depth) (= depth 1)))
             told-only-p
             (root-individuals (all-individuals abox))
	     (selected-individuals root-individuals)
	     only-successors-in-selected-individuals-p 	     
             (for-roles 
	      (let ((roles 
		     (remove-if (lambda (r)
				  (or (eq r +bottom-object-role-symbol+)
				      ;;(eq r +bottom-data-role-symbol+)
				      ))
				(all-roles 
				 (associated-tbox abox)
				 :inverse-test (lambda (r) (declare (ignorable r)) nil)))))
		(if no-top-role-p 
		    (remove-if (lambda (r) 
				 (or (eq r +top-object-role-symbol+)
				     ;;(eq r +top-data-role-symbol+)
				     ))
			       roles)
		  roles)))
             (for-datatype-properties
              (let ((tbox (associated-tbox abox)))
                (remove-if-not #'(lambda (role) 
                                   (and (role-used-as-datatype-property-p role tbox)
                                        (not (role-used-as-annotation-property-p role tbox))))
                               for-roles)))
             (for-annotation-properties
              (let ((tbox (associated-tbox abox)))
                (remove-if-not #'(lambda (role) 
                                   (role-used-as-annotation-property-p role tbox))
                               for-roles))))

  (let* ((tbox (associated-tbox abox))
         (tbox1 (find-tbox tbox))
         (n (length root-individuals))
         (m 0)
         (root-individuals (mapcar #'(lambda (ind) 
                                       (list ind 0))
                                   (remove-duplicates root-individuals)))
         (res nil)
         (hash (mht :size 1000))
         (for-attributes  (remove-if-not #'(lambda (x) 
					     (cd-attribute-p x tbox))
                               for-roles))
         (for-roles (remove-if #'(lambda (x) 
                                   (cd-attribute-p x tbox))
                               for-roles))
	 (role-hash (mht :size (length for-roles)))
	 (inds-hash (when only-successors-in-selected-individuals-p
		      (mht :size (length root-individuals))))
	 
	 (*check-abox-consistency-p* 
	  (if told-only-p 
	      nil
	    *check-abox-consistency-p*)))
    
    (when told-only-p
      (racer-prepare-substrate :abox abox 
			       :prepare-now-p t))
    
    (with-progress-range (n (0 100))
	      
      (dolist (role for-roles)
	(setf (gethash role role-hash) t))
    
      (when only-successors-in-selected-individuals-p
	(dolist (ind root-individuals)
	  (setf (gethash (first ind) inds-hash) t))
	(dolist (ind selected-individuals)
	  (setf (gethash ind inds-hash) t)))
    
      (labels ((get-syns (i)
		 (if told-only-p 
		     (list i)
		   (if (listp i)
		       (mapcar #'(lambda (i) 
				   (retrieve-individual-synonyms i told-only-p abox))
			       i)
		     (retrieve-individual-synonyms i told-only-p abox)))))

	(ensure-knowledge-base-state :abox-prepared (find-abox abox))
		
	(with-progress-range (n (0 100))
	  (loop while root-individuals do
		(let* ((ind-and-depth
			(pop root-individuals))
		       (ind (first (ensure-list (first ind-and-depth))))
		       (ind-depth (second ind-and-depth)))

		  (incf m)
		  (set-progress-value m)
			
		  (when (and (not (gethash ind hash))
			     (=> depth (< ind-depth depth)))
			  
		    (setf (gethash ind hash) t)
			  
		    (let* ((succs 
			    (if told-only-p
				(let* ((ras 
					(all-role-assertions-for-individual-in-domain ind abox))
				       (ras (if browsing-mode-p
						(list (first ras))
					      ras))
				       (res nil))
			     
				  (loop while ras do
					(let* ((ra (pop ras))
					       (succ (second (first ra)))
					       (role (second ra))
					       (entry (assoc role res))
					       (syns (get-syns succ)))
						     
					  (when (and (gethash role role-hash)
						     (=> only-successors-in-selected-individuals-p 
							 (some #'(lambda (syn)
								   (gethash syn inds-hash))
							       syns)))
					    (push (list (first syns) 
							(1+ ind-depth))
						  root-individuals)
					    (incf n)
					    (if entry
						(push syns (second entry))
					      (push (list role (list syns)) res)))))
			     
				  (mapcar #'(lambda (entry)
					      (list (first entry)
						    (remove-duplicates (second entry))))
					  res))
			 
			      (remove nil 
				      (mapcar 
				       #'(lambda (role)
					   (let ((role1
						  (get-tbox-role tbox1 role))
					    
						 (only-if-fn-p 
						  #'(lambda (succ)
						      (=> only-successors-in-selected-individuals-p 
							  (gethash succ inds-hash)))))
					     
					     (when (=> no-transitives-p
						       (not (some #'role-transitive-p 
								  (role-descendants-internal role1))))
					  
					       (let ((succs
						      (racer-retrieve-individual-fillers 
						       ind role 
						       :abox abox
						       :only-one-p browsing-mode-p
						       :only-if-p only-if-fn-p
						       :remove-synonyms-p t)))
					    
						 (when succs
					      
						   (dolist (succ succs)
						     (incf n)
						     (push (list succ (1+ ind-depth)) 
							   root-individuals))
					      
						   (list role succs))))))
				  
				       for-roles))))
		       
			   (data-fillers
			    (when (=> browsing-mode-p 
				      (not succs))
			      
			      (nconc
			       (remove nil 
				       (mapcar 
					#'(lambda (role)
					    (when (and (role-used-as-datatype-property-p role tbox)
						       (not (role-used-as-annotation-property-p role tbox)))
					      (let ((succs 
						     (retrieve-individual-told-datatype-fillers 
						      ind role told-only-p abox t)))
						(when succs
						  (list role (mapcar #'(lambda (succ)
									 (if (consp succ)
									     (list (second succ)
										   (second (third succ)))
									   succ))
								     succs))))))
					for-datatype-properties))
			       
			       (when *cur-substrate*
				 (remove nil
					 (mapcar #'(lambda (attribute)
						     (let ((succs
							    (nrql-head-get-told-value-if-exists 
							     *cur-substrate* 
							     ind attribute)))
						       (unless (eq succs :unknown)
							 (setf succs 
							   (remove-if #'symbolp succs)) 
							 (when succs
							   (list attribute (list succs))))))
						 for-attributes))))))
		       
			   (annotation-fillers
			    (when (=> browsing-mode-p
				      (not data-fillers))
			  
			      (remove nil
				      (mapcar #'(lambda (role)
						  (when 
						      (and (role-used-as-datatype-property-p role tbox)
							   (role-used-as-annotation-property-p role tbox))
						    (let ((succs
							   (retrieve-individual-annotation-property-fillers
							    ind role abox t)))
						      (when succs
							(list role 
							      (mapcar #'(lambda (succ)
									  (if (consp succ)
									      (list (second succ)
										    (second (third succ)))
									    succ))
								      succs))))))
					      for-annotation-properties)))))
		  
		      (push 
		   
		       (let ((syns (get-syns ind)))
		     
			 (if browsing-mode-p
		      
			     (if (or succs 
				     data-fillers
				     annotation-fillers)
			      
				 `(,syns :successors-found)
			
			       `(,syns nil nil nil))
		    
			   `(,syns 
			     ,succs 
			     ,data-fillers
			     ,annotation-fillers)))
		   
		       res))))))
  
	res))))

;;;
;;; 
;;; 


#+:racer-server
(nrql-defun (all-same-as-assertions)
  (&optional (abox (current-abox)) &key (count nil))

  (let* ((abox (find-abox abox)))
	      
     (ensure-knowledge-base-state :abox-prepared (find-abox abox))
     
     (let ((res
            (mapcar #'(lambda (x)
                        `(same-as ,@(rest x)))
                    (remove-if-not #'(lambda (x) 
                                       (eq (first x) 'same-individual-as))
                                   (abox-individual-identity-disjointness-assertions abox)))))

       (if count
           (length res)
         res))))


#+:racer-server
(nrql-defun (all-different-from-assertions)
  (&optional (abox (current-abox)) &key (count nil))

  (let* ((abox (find-abox abox)))
	      
     (ensure-knowledge-base-state :abox-prepared (find-abox abox))
     
     (let ((res
	    (remove-if-not #'(lambda (x) 
			       (member (first x) '(different-from all-different)))
			   (abox-individual-identity-disjointness-assertions abox))))

       (if count
           (length res)
         res))))
;;;
;;; Queries, Rules 
;;; 

(nrql-defun (all-queries
             :doc ((:doc-type :long)
                   (:category :basic-commands)
                   (:syntax (all-queries))
                   (:arguments
                    (:key abox (current-abox) "consider queries for substrates for ABox {\\tt abox} only")
                    (:key type-of-substrate  racer-dummy-substrate "consider queries for substrates of 
                                                 specified type only, one of:
                                                  \\argument{racer-dummy-substrate,
                                                  data-substrate, mirror-data-substrate, rcc-substrate,
                                                  racer-tbox-mirror-substrate}"))
                   (:description "Returns a list of (the IDs of) all queries")))
  (&rest args)
  (remove-if-not #'symbol-package ; gensym-Queries entfernen
                 (apply #'map-over-queries-for-aboxes-of-type 
                        #'iterator-id 
                        (with-access-to-lifecycle-lists
                          (copy-list *all-queries*))
                        args)))

(nrql-defun (all-rules 
             :doc ((:doc-type :short)
                   (:category :basic-commands)
                   (:rule-equivalent-of all-queries)))
  (&rest args)
  (remove-if-not #'symbol-package
		 (apply #'map-over-queries-for-aboxes-of-type 
                        #'iterator-id 
                        (with-access-to-lifecycle-lists
                          (copy-list *all-rules*))
                        args)))

;;;
;;;
;;;

(nrql-defmethod (describe-query-status
                 :doc ((:doc-type :long)
                       (:category :basic-commands) 
                       (:description "Describes the current status of the query \\argument{id} - whether the
query is ready (to run), running, waiting (sleeping), or terminated")
                       (:syntax (describe-query-status id))
                       (:arguments
                        (:first id "the ID of the query, or {\\tt :last} or {\\tt :last-query}"))
                       (:values "A list of status symbols describing the current status of the query")
                       (:remarks)
                       (:examples)
                       (:see-also describe-all-queries)))
  ((query null))
  :not-found)

(nrql-defmethod (describe-query-status) ((query symbol))
  (describe-query-status (find-query query)))

;;;
;;;
;;;

(nrql-defmethod (describe-rule-status 
                 :doc ((:doc-type :short)
                       (:category :basic-commands)
                       (:rule-equivalent-of describe-query-status)))
  ((query null))
  :not-found)

(nrql-defmethod (describe-rule-status) ((query symbol))
  (describe-rule-status (find-rule query)))

;;;
;;;
;;;

(nrql-defmethod (describe-query  
                 :doc ((:doc-type :long)
                       (:category :basic-commands) 
                       (:description "Returns a description of the query \\argument{id}")
                       (:syntax (describe-query id &optional rewritten-p))
                       (:arguments 
                        (:first id "the ID of the query, or {\\tt :last} or {\\tt :last-query}")
                        (:optional rewritten-p t " if {\\tt nil} is
specified, then the \emph{original} query will be returned, otherwise 
  the internally rewritten query"))
                       (:values "A list of three items: The query
identify, the current status description (see
                                          \\funref{describe-query-status}), and either the rewritten or original syntactic query")
                       (:remarks "This function uses
  \\funref{describe-query-status}, 
  \\funref{query-head} (or \\funref{original-query-head}) and 
  \\funref{query-body} (or \\funref{original-query-body}) to create the description")
                       (:examples)
                       (:see-also describe-all-queries)))
  
  ((query null) &optional (rewritten-p t))
 
  (declare (ignorable rewritten-p))
  :not-found)

(nrql-defmethod (describe-query)
  ((query symbol) &optional (rewritten-p t))
  (let ((*use-new-syntax-for-unparse-p* t))
    (describe-query (find-query query) rewritten-p)))

;;;
;;;
;;;

(nrql-defmethod (describe-rule
                 :doc ((:doc-type :short)
                       (:category :basic-commands)
                       (:rule-equivalent-of describe-all-queries)))
  ((query null) &optional (rewritten-p t))
  (declare (ignorable rewritten-p))
  :not-found)

(nrql-defmethod (describe-rule)
  ((query symbol) &optional (rewritten-p t))
  (describe-rule (find-rule query) rewritten-p))

;;;
;;;
;;;

(nrql-defun (describe-all-queries
             :doc ((:doc-type :long)
                   (:category :basic-commands)
                   (:syntax (describe-all-queries))
                   (:description "Applies \\funref{describe-query} to the result of \\funref{all-queries} and
returns it")
                   (:arguments 
                    (:optional rewritten-p t " if {\\tt nil} is
specified, then the \emph{original} queries will be returned, otherwise  
  the internally rewritten queries")
                    (:key abox (current-abox) "consider substrates for ABox {\\tt abox} only")
                    (:key type-of-substrate  racer-dummy-substrate "consider substrates of 
                                                 specified type only, one of:
                                                  \\argument{racer-dummy-substrate,
                                                  data-substrate, mirror-data-substrate, rcc-substrate,
                                                  racer-tbox-mirror-substrate}"))))
  
  (&optional (rewritten-p t) &rest args)

  (apply #'map-over-queries-for-aboxes-of-type 
         #'(lambda (x) 
             (when (symbol-package (iterator-id x))
               (describe-query x rewritten-p)))
         (with-access-to-lifecycle-lists
           (copy-list *all-queries*))
         args))

(nrql-defun (describe-all-rules
             :doc ((:doc-type :short)
                   (:category :basic-commands)
                   (:rule-equivalent-of describe-all-queries)))

  (&optional (rewritten-p t) &rest args)

  (apply #'map-over-queries-for-aboxes-of-type 
         #'(lambda (x) 
             (when (symbol-package (iterator-id x))
               (describe-rule x rewritten-p)))
         (with-access-to-lifecycle-lists
           (copy-list *all-rules*))
         args))

;;;
;;;
;;;

(nrql-defmethod (query-head
                 :doc ((:doc-type :long)
                       (:category :basic-commands)
                       (:description "Returns the (possibly rewritten) head of the query \\argument{id}")
                       (:syntax (query-head id))
                       (:arguments 
                        (:first id "the ID of the query, or {\\tt :last} or {\\tt :last-query}"))
                       (:values "The (possibly rewritten) head of the query")
                       (:remarks "Note that individuals in the original query head are usually yreplaced by 
  representative variables")
                       (:examples)
                       (:see-also original-query-head query-body)))
  ((query null))
  :not-found)

(nrql-defmethod (query-head)
  ((query query))
  (tree-map #'(lambda (x) 
                (if (is-voi-p x)
                    (textual-description x)
                  x))
            (answer-pattern query)))

(nrql-defmethod (query-head) ((query symbol))
  (query-head (find-query query)))

;;;
;;;
;;;

(nrql-defmethod (rule-consequence 
                 :doc ((:doc-type :long)
                       (:category :basic-commands)
                       (:rule-equivalent-of query-head)
                       (:description "Returns the (possibly rewritten) rule consequence of 
the rule \\argument{id}")
                       (:syntax (rule-consequence id))
                       (:arguments 
                        (:arguments 
                         (:first id "the ID of the rule, or {\\tt :last} or {\\tt :last-rule}")))
                       (:values "The (possibly rewritten) consequence of the rule")
                       (:remarks)
                       (:examples)
                       (:see-also original-rule-consequence rule-antecedence)))
  ((query null))
  :not-found)

(nrql-defmethod (rule-consequence) ((query query))
  (tree-map #'(lambda (x) 
                (if (is-voi-p x)
                    (textual-description x)
                  x))
            (rule-con-pattern query)))

(nrql-defmethod (rule-consequence) ((query symbol))
  (rule-consequence (find-rule query)))

;;;
;;;
;;;

(nrql-defmethod (original-query-head
                 :doc ((:doc-type :short)
                       (:category :basic-commands)
                       (:description "Like \\funref{query-head}, but the original head is returned")))
  ((query null))
  :not-found)

(nrql-defmethod (original-query-head) ((query query))
  (first (original-description (parser query))))

(nrql-defmethod (original-query-head) ((query symbol))
  (original-query-head (find-query query)))


;;;
;;;
;;;

(nrql-defmethod (original-rule-consequence 
                 :doc ((:doc-type :short)
                       (:rule-equivalent-of original-query-head)
                       (:category :basic-commands)
                       (:description "Like \\funref{rule-consequence}, but the original consequence is returned")))
  ((query null))
  :not-found)

(nrql-defmethod (original-rule-consequence) ((query query))
  (first (original-description (parser query))))

(nrql-defmethod (original-rule-consequence) ((query symbol))
  (original-rule-consequence (find-rule query)))

;;;
;;;
;;;

(nrql-defmethod (query-body
                 :doc ((:doc-type :long)
                       (:category :basic-commands)
                       (:description "Returns the (possibly rewritten) body of the query \\argument{id}")
                       (:syntax (query-body id))
                       (:arguments 
                        (:first id "the ID of the query, or {\\tt :last} or {\\tt :last-query}"))
                       (:values "The (possibly rewritten) body of the query")
                       (:examples)
                       (:see-also original-query-body query-head)))
  ((query null))
  :not-found)

(nrql-defmethod (query-body) ((query query))
  (let ((*use-new-syntax-for-unparse-p* t))
    (unparse-query query)))

(nrql-defmethod (query-body) ((query symbol))
  (query-body (find-query query)))

;;;
;;;
;;;

(nrql-defmethod (rule-antecedence 
                 :doc ((:doc-type :long)
                       (:category :basic-commands)
                       (:rule-equivalent-of query-body)                       
                       (:description "Returns the (possibly rewritten) antecedence of the rule \\argument{id}")
                       (:syntax (rule-antecedence id))
                       (:arguments 
                        (:first id "the ID of the rule, or {\\tt :last} or {\\tt :last-rule}"))
                       (:values "The (possibly rewritten) antecedence of the rule")
                       (:examples)
                       (:see-also original-rule-antecedence rule-consequence)))
  ((query null))
  :not-found)

(nrql-defmethod (rule-antecedence) ((query query))
  (let ((*use-new-syntax-for-unparse-p* t))
    (unparse-query query)))

(nrql-defmethod (rule-antecedence) ((query symbol))
  (rule-antecedence (find-rule query)))

;;;
;;;
;;;

(nrql-defmethod (original-query-body
                 :doc ((:doc-type :short)
                       (:category :basic-commands)
                       (:description "Like \\funref{query-body}, but the original body is returned")))
  ((query null))
  :not-found)

(nrql-defmethod (original-query-body) ((query query))
  (second (original-description (parser query))))

(nrql-defmethod (original-query-body) ((query symbol))
  (original-query-body (find-query query)))


;;;
;;;
;;;

(nrql-defmethod (original-rule-antecedence
                 :doc ((:doc-type :short)
                       (:category :basic-commands)
                       (:rule-equivalent-of original-query-body)
                       (:description "Like \\funref{rule-antecedence}, but the original antecedence is returned")))
  ((query null))
  :not-found)

(nrql-defmethod (original-rule-antecedence) ((query query))
  (second (original-description (parser query))))

(nrql-defmethod (original-rule-antecedence) ((query symbol))
  (original-rule-antecedence (find-rule query)))


;;;
;;; delete-query, delete-rule, delete-all-queries, delete-all-rules 
;;;

(nrql-defmethod (delete-query 
                 :doc ((:doc-type :long)
                       (:category :query-management)
                       (:description "Deletes the query \\argument{id}, enabling the
garbage collector to recycle some memory")
                       (:syntax (delete-query id))
                       (:arguments 
                        (:first id "the ID of the query to be
deleted, or {\\tt :last} or {\\tt :last-query}"))
                       (:values :OKAY-QUERY-DELETED :NOT-FOUND)
                       (:remarks)
                       (:examples)
                       (:see-also delete-all-queries)))
  
  ((query null))

  :not-found)

(nrql-defmethod (delete-query) ((query symbol))
  (delete-query (find-query query)))

;;; 
;;; 
;;; 

(nrql-defmethod (delete-rule
                 :doc ((:doc-type :long)
                       (:category :rule-management)
                       (:description "Deletes the query \\argument{id}, enabling the
garbage collector to recycle some memory")
                       (:syntax (delete-rule id))
                       (:arguments 
                        (:first id "the ID of the rule to be deleted, or {\\tt 
:last} or {\\tt :last-query}"))
                       (:values  :OKAY-RULE-DELETED :NOT-FOUND)
                       (:remarks)
                       (:examples)
                       (:see-also delete-all-rules)))

  ((query null))
                 
  :not-found)

(nrql-defmethod (delete-rule) ((query symbol))
  (delete-rule (find-rule query)))

;;;
;;; 
;;;

(nrql-defun (delete-all-queries
             :doc ((:doc-type :long)
                   (:category :query-management)
                   (:description "Aborts and deletes all queries")
                   (:syntax (delete-all-queries))
                   (:arguments
                    (:key abox (current-abox) "consider queries for substrates for ABox {\\tt abox} only")
                    (:key type-of-substrate  racer-dummy-substrate "consider queries for substrates of 
                                                 specified type only, one of:
                                                  \\argument{racer-dummy-substrate,
                                                  data-substrate, mirror-data-substrate, rcc-substrate,
                                                  racer-tbox-mirror-substrate}"))
                   (:values :okay-all-queries-deleted)
                   (:remarks)
                   (:examples)
                   (:see-also abort-query delete-query abort-all-queries)))

  (&rest args)
  
  (apply #'abort-all-queries args)
  
  (setf *all-queries* 
        (set-difference *all-queries* (apply #'map-over-queries-for-aboxes-of-type #'identity 
                                             (with-access-to-lifecycle-lists
                                               (copy-list *all-queries*))
                                             args))
        
        *ready-queries* 
        (set-difference *ready-queries* (apply #'map-over-queries-for-aboxes-of-type #'identity (with-access-to-lifecycle-lists
                                                                                                  (copy-list *ready-queries*)) args))
        
        *active-queries* 
        (set-difference *active-queries* (apply #'map-over-queries-for-aboxes-of-type #'identity (with-access-to-lifecycle-lists
                                                                                                   (copy-list *active-queries*)) args))
        
        *processed-queries* 
        (set-difference *processed-queries* (apply #'map-over-queries-for-aboxes-of-type #'identity (with-access-to-lifecycle-lists
                                                                                                      (copy-list *processed-queries*)) args)))
  
  :okay-all-queries-deleted)

(nrql-defun (delete-all-rules 
             :doc ((:doc-type :long)
                   (:category :rule-management)
                   (:description "Aborts and deletes all rules") 
                   (:syntax (delete-all-rules))
                   (:arguments
                    (:key abox (current-abox) "consider rules for substrates for ABox {\\tt abox} only")
                    (:key type-of-substrate  racer-dummy-substrate "consider rules for substrates of 
                                                 specified type only, one of:
                                                  \\argument{racer-dummy-substrate,
                                                  data-substrate, mirror-data-substrate, rcc-substrate,
                                                  racer-tbox-mirror-substrate}"))
                   (:values :okay-all-rules-deleted)
                   (:remarks)
                   (:examples)
                   (:see-also abort-rule delete-rule abort-all-rules)))

  (&rest args)
  
  (apply #'abort-all-rules args)
  
  (setf *all-rules* 
        (set-difference *all-rules* (apply #'map-over-queries-for-aboxes-of-type #'identity (with-access-to-lifecycle-lists
                                                                                              (copy-list *all-rules*)) args))
        
        *ready-rules* 
        (set-difference *ready-rules* (apply #'map-over-queries-for-aboxes-of-type #'identity (with-access-to-lifecycle-lists
                                                                                                (copy-list *ready-rules*)) args))
        
        *active-rules* 
        (set-difference *active-rules* (apply #'map-over-queries-for-aboxes-of-type #'identity (with-access-to-lifecycle-lists
                                                                                                 (copy-list *active-rules*)) args))
        
        *processed-rules* 
        (set-difference *processed-rules* (apply #'map-over-queries-for-aboxes-of-type #'identity (with-access-to-lifecycle-lists
                                                                                                    (copy-list *processed-rules*)) args)))
  
  
  :okay-all-rules-deleted)

;;; 
;;; Queries 
;;; 

;;; 
;;; Functional interface 
;;; Query preparation 
;;; 

(nrql-defun (racer-prepare-query 

             :doc ((:doc-type :long)
                   
                   (:category :abox-queries)
        
                   (:description "Prepares (i.e., parses and compiles) a nRQL
ABox query but does not execute (start) it yet")

                   (:syntax (racer-prepare-query head body 
                                                 &rest args 
                                                 &key id
                                                 abox 
                                                 execute-p
                                                 prepare-now-p
                                                 premise
                                                 type-of-substrate 
                                                 rewrite-defined-concepts-p))
                   
                   (:arguments
                    (:first head "the head of the query, see {\\tt
<query-head>}, Section 6.1.8 in the User Guide")
                    (:second body "the body of the query, see {\\tt
<query-body>}, Section 6.1.8 in the User Guide")
                    (:key id QUERY-XXX "the ID (name) of the query")
                    (:key abox (current-abox) "the (name of the) ABox to be queried")
                    (:key execute-p nil "if {\\tt t}, then the query is automatically
executed; the \\argument{args} are passed to \\funref{execute-query}")
                    (:key premise nil "the query premise, a list of ABox assertions, 
see {\\tt <query-premise>}, Section 6.1.8 in the User Guide")
                    (:key type-of-substrate "'racer-dummy-substrate" "a symbol
naming a substrate type, one of: \\argument{racer-dummy-substrate,
data-substrate, mirror-data-substrate, rcc-substrate,
racer-tbox-mirror-substrate}")
                    ;;; (:key rewrite-defined-concepts-p "by environment" "see ...")
                    (:key execute-p nil "if {\\tt t} is specfied,
the query is automatically started (executed); \\funref{execute-query} will be called. 
The \\argument{args} will be passed through to \\funref{execute-query}")
                        
                    (:key prepare-now-p nil "if {\\tt t}, then
the substrate will be prepared immediately (see
                                            \\funref{prepare-nrql-engine}), otherwise later when the query is
about to be executed")
                    (:reference args execute-query)
                    )

                   (:values  "A list like {\\tt (:QUERY-466 :READY-TO-RUN)}, where {\\tt :QUERY-466}
  is the query ID and {\\tt :READY-TO-RUN} indicates the current
  status of the query" )

                   (:remarks "To start the query, use \\funref{execute-query} 
(or \\argument{execute-p} = {\\tt t})")

                   (:examples (racer-prepare-query '(?x) '(and (?x woman) (?x ?y has-child))))

                   (:see-also execute-query get-answer)))

  (res-args query &rest args)

  (with-racer-timeout
    (if (ensure-deadlock-prevention)
        :denied-due-to-deadlock-prevention
      (progn
        (apply #'racer-prepare-substrate args)
        (apply #'answer-query-int *cur-substrate* query res-args (append args (list :execute-p nil)))))))


(nrql-defun (racer-prepare-query1 

             :doc ((:doc-type :short)
                   (:category :abox-queries)
                   (:description "Like \\funref{racer-prepare-query}, but with
flipped argument positions for \\argument{head} and \\argument{body}"))) 

  (query res-args &rest args) 
    
  (apply #'racer-prepare-query res-args query args))


(nrql-defun (prepare-query 
             :doc ((:doc-type :short)
                   (:category :abox-queries)
                   (:synonym racer-prepare-query)))
  (&rest args)

  (apply #'racer-prepare-query args))


(nrql-defun (prepare-query1 
             :doc ((:doc-type :short)
                   (:category :abox-queries)
                   (:synonym racer-prepare-query1)))
  (&rest args)


  (apply #'racer-prepare-query1 args))


;;;
;;; Functional interface 
;;; Query execution 
;;;

(nrql-defmethod (execute-query
                 
                 :doc ((:doc-type :long)
                       (:category :abox-queries)
                       (:description "Sets up and starts a query
answering process (thread) for the prepared (ready) query
\\argument{id}. The query answering process prepares the substrate for
query answering if it has not been prepared yet (see
                                                 \\funref{prepare-nrql-engine}) before query answering starts on that
substrate.  In set-at-a-time mode, automatically calls 
\\funref{get-answer} and returns the answer")
                       (:syntax (execute-query id 
                                               &rest args
                                               &key 
                                               how-many
                                               exclude-permutations-p
                                               use-individual-synonyms-p
                                               
                                               tuple-at-a-time-p
                                               deliver-kb-has-changed-warning-tokens-p
                                               proactive-tuple-computation-p
                                               told-information-reasoning-p
                                               
                                               check-abox-consistency-p
                                               ensure-tbox-classification-p
                                               initial-abox-mirroring-p
                                               initial-role-assertion-mirroring-p
                                               
                                               two-phase-processing-p
                                               deliver-phase-two-warning-tokens-p
                                               ))

                       (:arguments 
                        (:first id
                         "the ID of the query to be executed, or {\\tt
:last} or {\\tt :last-query}")
                        
                        (:key how-many "by environment" 
                         "the number of tuples to be computed; 
{\\tt nil} means unbounded; see also \\funref{set-max-no-of-tuples-bound}")
                        
                        (:key exclude-permutations-p "by environment" 
                         "if {\\tt t} is specified, then permutations
will be exluded from the query answer; see also \\
funref{exclude-permutations}")
                         
                        (:key tuple-at-a-time-p "by environment" 
                         "if {\\tt t} is specified, then the
tuple-at-a-time mode will be used, otherwise set-at-a-time mode; 
see also \\funref{process-tuple-at-a-time}")
                         
                        (:key deliver-kb-has-changed-warning-tokens-p "by environment" 
                         "if {\\tt t} is specified, and the query is execute in tuple-at-a-time mode, 
then a warning token will be delivered if the KB has changed during query
execution; see also \\funref{enable-kb-has-changed-warning-tokens}")

                        (:key proactive-tuple-computation-p "by environment" 
                         "if {\\tt t} is specified, then the eager
mode will be used in tuple-at-a-time mode, otherwise the lazy mode; in
set-at-a-time mode, tuple computation is always eager. 
See also \\funref{enable-eager-tuple-computation}")

                        (:key deliver-phase-two-warning-tokens-p "by environment" 
                         "if {\\tt t} is specified, then a warning
token will be delivered if the query is executed in two-phase
processing mode before phase two starts; see also
\\funref{enable-phase-two-starts-warning-tokens}")
                         
                        (:key told-information-reasoning-p "by environment"  
                         "if {\\tt t} is specified, then only the
information in the nRQL caches \(resp.  the current substrate\) is
used for query answering. Calls to RacerPro basic ABox retrieval
functions are avoided, but query answering is incomplete. Note that
the amount of information in the caches depends on the nRQL mode which
was active at the time the substrate was prepared (see
                                                   \\funref{prepare-nrql-engine}); 
see also \\funref{enable-told-information-querying}")

                        (:key check-abox-consistency-p "by environment" 
                         "if {\\tt t} is specified, the ABox to be
queried is checked for consistency before querying starts; see
also \\funref{check-abox-consistency-before-querying}")
                         
                        (:key ensure-tbox-classification-p "by environment" 
                         "if the substrate for the ABox has not been
prepared yet (see \\funref{prepare-nrql-engine}), then the new query
answering process will do that before query answering starts.  If
{\\tt t} is specified for this argument, then the substrate will be
set-up in nRQL mode 1. See also \\funref{enable-smart-abox-mirroring}, 
\\funref{set-nrql-mode}")
                         
                        (:key initial-abox-mirroring-p "by environment" 
                         "if {\\tt t} is specified, and the substrate
for the ABox has not been prepared yet (see
                                        \\funref{prepare-nrql-engine}), then the substrate will be prepared
before query answering starts, and the substrate will contain a
complete mirror of the (syntactic, told) ABox information. Otherwise
that information is acquired dynamically during query answering (if
                                                                    needed). For the incomplete modi, {\\tt t} \emph{must} be specified. See also
\\funref{enable-abox-mirroring},  
\\funref{set-nrql-mode}")
                         
                        (:key initial-role-assertion-mirroring-p "by environment" 
                         "like \\argument{initial-abox-mirroring-p},
but only the role assertions are initially mirrored if {\\tt t} is
specified, but not the instance assertions")

                        (:key ensure-tbox-classification-p "by environment" 
                         "if nRQL shall use incomplete mode 1, then
{\\tt t} must be specified. Note that this causes the TBox to be
classified before the substrate is prepared (created).  See also
\\funref{enable-smart-abox-mirroring}, \\funref{set-nrql-mode}")

                        (:key classify-concepts-in-instance-assertions  "by environment" 
                         "if nRQL shall use incomplete mode 2, then
{\\tt t} must be specified. See also
\\funref{enable-very-smart-abox-mirroring}, \\funref{set-nrql-mode}")
                         
                        (:key two-phase-processing-p "by environment" 
                         "if {\\tt t} is specified, then two-phase
processing is enabled for this query; see
also \\funref{enable-two-phase-query-processing-mode}")

                        (:key use-individual-synonyms-p "by environment" 
                         "if {\\tt t} is specified, then nRQL
considers two individuals as being the same if they are synonyms. Note
that this also changes the semantics of {\\tt same-as} from syntactic
name identiy to semantic individual equivalence. See also
\\funref{use-individual-synonym-equivalence-classes}")
                        
                        (:args args "in set-at-a-time mode,
\\funref{get-answer} is called automatically; thus, the keyword
arguments of \\funref{get-answer} are accepted here as well and passed
to \\funref{get-answer}"))

                       (:values "If the query is executed in set-at-a-time mode,  
then the answer to that query since \\funref{get-answer} is called automatically, 
otherwise a list like {\\tt (:QUERY-466 :RUNNING)}, where {\\tt :QUERY-466} 
  is the query ID and {\\tt :RUNNING} indicates the current 
  status of the query" )

                       (:remarks
                        "The query has to be prepared (ready) before it can
  be executed, see \\funref{query-ready-p},
\\funref{prepared-queries}. 

If no values for the listed keyword arguments are specified, then
either the lexical settings established by a surrounding
\\funref{with-nrql-settings} or the currently active global settings
(see \\funref{describe-query-processing-mode}) will be used; this is
documents as ``default by environment'' in the argument lists. 

 Note that also the keyword arguments accepted by \\funref{get-answer}
are accepted and passed through to \\funref{get-answer} with
\\argument{args}")

                       (:examples (racer-prepare-query '(?x) '(?x woman))
                        (execute-query :last))

                       (:see-also 
                        racer-prepare-query get-answer prepare-nrql-engine)))

  ((query null) &rest args &key &allow-other-keys)
  
  (declare (ignorable args))
  
  :not-found)

(nrql-defmethod (execute-query)
  ((query symbol) &rest args &key &allow-other-keys)
  (with-racer-timeout
    (apply #'eval-nrql-settings2
           #'(lambda ()
               (apply #'execute-query (find-query query *ready-queries*) args))
           :allow-other-keys t
           args)))

;;;
;;;
;;;

(nrql-defmethod (abort-query
                 :doc ((:doc-type :long)
                       (:category :execution-control)
                       (:description "Aborts the active query (see \\funref{query-active-p})
\\argument{id}. The query becomes processed (see \\funref{query-processed-p})") 
                       (:syntax (abort-query id))
                       (:arguments 
                        (:first id "the ID of the query, or {\\tt :last} or {\\tt :last-query}"))
                       (:values "Either {\\tt :okay-query-aborted} or {\\tt :not-found}")
                       (:remarks)
                       (:examples)
                       (:see-also abort-all-queries query-active-p)))
  ((query null))
  :not-found)

(nrql-defmethod (abort-query) ((query symbol))
  (abort-query (find-query query *active-queries*)))

;;;
;;;
;;;

(nrql-defmethod (abort-rule
                 :doc ((:doc-type :short)
                       (:category :execution-control)
                       (:rule-equivalent-of :abort-query)))
  ((query null))
  :not-found)

(nrql-defmethod (abort-rule) ((query symbol))
  (abort-rule (find-rule query *active-rules*)))

;;;
;;; Queries + Rules 
;;; Set-at-a-time: Get Answer
;;; 

(defun execute-if-needed (query &rest args &key execute-p &allow-other-keys)
  (cond ((and execute-p 
              (query-prepared-p query)
              (not (query-active-p query))
              (not (query-processed-p query)))
         (apply #'execute-query query args))
        ((and (subscribers (find-query query))
              (not (query-active-p query))
              (query-prepared-p query))
         (apply #'execute-query query :execute t args))
        ((and (subscribers (find-query query))
              (not (query-active-p query))
              (query-processed-p query))
         (apply #'reexecute-query query :execute t args))))
        

(nrql-defmethod (get-answer
                 :doc ((:doc-type :long)

                       (:category :getting-answers)
                       
                       (:description "Gets (resp. forces the
                                                  computation of) the complete answer set (result) of a query or the set
of conclusions of a rule, independently if the query resp. rule had
been in tuple- or set-at-a-time mode. 
 The query or rule must be active (see \\funref{query-active-p},
                                       \\funref{rule-active-p}, \\funref{active-queries}, 
                                       \\funref{active-rules}) or already processed (see 
                                                                                     \\funref{query-processed-p},
                                                                                     \\funref{rule-processed-p},
                                                                                     \\funref{processed-queries}, 
                                                                                     \\funref{processed-rules})")
                       
                       (:syntax (get-answer id 
                                            &rest args
                                            &key
                                            execute-p 
                                            verbose-p 
                                            dont-show-variables
                                            dont-show-head-projection-operators-p
                                            dont-show-lambdas-p))

                       (:arguments 
                        (:first id "the ID of the query or rule
(also {\\tt :last, :last-query, :last-rule} are possible)")
         
                        (:key execute-p nil "if {\\tt t} is specfied,
the query is automatically started (executed); \\funref{execute-query} will be called. 
The \\argument{args} will be passed through to \\funref{execute-query}")
                        
                        (:key verbose-p t "if {\\tt t} is specfied,
also head projection operators are shown literally in the result
tuples")
                        (:key dont-show-variables nil "a list of
variables. Usually, a variable binding is shown as a (variable value)
entry in a result tuple. If the variable is a member in the list
\\argument{dont-show-variables}, then only value will be included in
the result tuples")

                        (:key dont-show-head-projection-operators-p
                         nil "the results of projection operators are usually included in the
form (operator operator-result) in the result tuples. Specify {\\tt t}
if only the operator result shall appear in the result tuples")
                        
                        (:key dont-show-lambdas-p nil "same as
\\argument{dont-show-head-projection-operators-p}, but for solely for lambda operators")

                        (:reference args execute-query))
                        
                       (:values "A list of tuples, or {\\tt t} or {\\tt
nil}, or a list of lists of ABox assertions (the rule consequences),
or {\\tt :NOT-FOUND}")

                       (:remarks "Can be called an arbitrary number of
times on a query or rule.  The
  answer is stored in the query resp. rule object and is thus not
recomputed if {\\tt get-answer} is called. You can check with
\\funref{query-accurate-p} (resp.  \\funref{rule-accurate-p}) whether
the stored answer is still valid.  See also the value of
\\funref{describe-query}. In case of a rule, the rule consequences can
be added with \\funref{add-chosen-sets-of-rule-consequences}.

Note that the query or rule named \\argument{id} must be on the list
of active or processed queries (see \\funref{active-queries},
                                    \\funref{processed-queries}), otherwise {\\tt :NOT-FOUND} is
returned.

The tuples are actually computed by repeated calls to
\\funref{get-next-tuple}. Thus, also special tokens (markers) returned 
by \\funref{get-next-tuple} might appear in the answer")

                       (:examples)
                       
                       (:see-also get-next-tuple get-next-n-remaining-tuples
                        add-chosen-sets-of-rule-consequences 
                        active-queries processed-queries )))

  ((query null) &rest args &key &allow-other-keys)

  (declare (ignorable args))

  :not-found)


(nrql-defmethod (get-answer)
  ((query symbol) &rest args &key &allow-other-keys)

  (declare (ignorable args))
  
  (with-racer-timeout

    (apply #'execute-if-needed query args)

    (apply #'get-answer (or (find-query query *active-queries*)
                            (find-query query *processed-queries*)
                            
                            (find-rule query *active-rules*)
                            (find-rule query *processed-rules*))
           args)))

;;;
;;;
;;;

(nrql-defun (get-answer-size 
             :doc ((:doc-type :short)
                   (:category :getting-answers)
                   (:description "Like \\funref{get-answer}, but only returns the number
of result tuples (resp. number of sets of rule conclusions)")))
  
  (query &optional execute-p &rest args)
           
  (let ((answer (apply #'get-answer query :execute-p execute-p args)))
    (if (consp answer)
        (length answer)
      answer)))

;;;
;;; Queries 
;;; Incremental Query Answering 
;;; Tuple at a time
;;; 

(nrql-defmethod (get-current-tuple
                 :doc ((:doc-type :long)
                       (:category :getting-answers)
                       (:description "Returns the result of the last call to 
  \\funref{get-next-tuple} on the query \\argument{id}")
                       (:syntax (get-current-tuple id))
                       (:arguments 
                        (:first id "the ID of the query, or {\\tt :last}, 
                         or {\\tt :last-query}"))
                       (:values 
                        "See \\funref{get-next-tuple}. Moreover, {\\tt
nil} is returned if there
  was no previous call to {\\tt get-next-tuple}")
                       (:remarks)
                       (:examples)
                       (:see-also get-next-tuple)))

  ((query null))
 
  :not-found)

(nrql-defmethod (get-current-tuple)

  ((query symbol))
  
  (get-current-tuple (find-query query)))

;;;
;;; 
;;;


(nrql-defmethod (next-tuple-available-p 
                 :doc ((:doc-type :long)
                       (:category :getting-answers)
                       (:description "Checks for the availability of
the next answer tuple from a query")
                       (:syntax (next-tuple-available-p id))
                       (:arguments 
                        (:first id "the ID of the query, or {\\tt :last}, 
                         or {\\tt :last-query}"))
                       (:values "{\\tt t} or {\\tt nil} (or {\\tt :not-found})")
                       (:remarks "If this function returns {\\tt t},
then \\funref{get-next-tuple} returns that tuple immediately
 (without computation delay; the API does not block)")
                       (:examples)
                       (:see-also get-next-tuple)))
  ((query null))
  :not-found)

(nrql-defmethod (next-tuple-available-p)
  ((query symbol))
  (next-tuple-available-p (find-query query)))

;;;
;;;
;;;

(nrql-defmethod (get-next-tuple
                 :doc ((:doc-type :long)
                       (:category :getting-answers)
                       (:description "Gets the next tuple from query
          \\argument{id}. The query must be on the list of
          \\funref{active-queries} or \\funref{processed-queries}.  A ready
          (prepared) query can also be started (executed), see
          \\argument{execute-p} argument. For rules,
          \\funref{get-next-set-of-rule-consequences} must be used")
                       (:syntax (get-next-tuple id &rest args))
                       (:arguments 
                        (:first id "the ID of the query, or {\\tt :last}, 
          or {\\tt :last-query}")
                        (:key execute-p nil "specify {\\tt t} to start (execute) 
          a prepared (ready) query. The
          \\argument{args} are passed to
          \\funref{execute-query} (however, the query is always executed in
                                          tuple-at-a-time mode, this cannot be overridden)")
                        (:reference args (execute-query)))
                       (:values 
                        "The tuple, or {\\tt :exhausted} in case there
          are no more tuples, or special tokens such as {\\tt :not-found}, {\\tt
          :inconsistent}, etc")
                       (:remarks "If the query had been started in
          lazy tuple-at-a-time mode, then computation of the next tuple might
          eventually take some time and thus the function might not return
          immediately.

          However, if the query had been started in eager mode, then there is a
          chance that the next tuple (and probably some more tuples not yet
                                          requested) have already been pre-computed, and are thus already
          available.  The function \\funref{next-tuple-available-p} can be used
          to check for the availability of such (immediately available) tuples.

          Note that a query might still have tuples available, even if the query
          process (thread) has already terminated and thus the query is no
          longer active (the query already appears on the list of
                             \\funref{processed-queries}).  This happens in the eager
          tuple-at-a-time mode")

                       (:examples)
                       (:see-also next-tuple-available-p 
                        get-current-tuple 
                        get-next-n-remaining-tuples)))
  
  ((query null) &rest args &key &allow-other-keys)

  (declare (ignorable args))
  
  :not-found)

(nrql-defmethod (get-next-tuple)

  ((query symbol) &rest args &key &allow-other-keys)

  (with-racer-timeout
    
    (apply #'execute-if-needed query :tuple-at-a-time-p t args)

    (get-next-tuple (or (find-query query *active-queries*)
                        (find-query query *processed-queries*)))))

;;;
;;; 
;;;

(nrql-defmethod (get-next-n-remaining-tuples
                 :doc ((:doc-type :short)
                       (:category :getting-answers)
                       (:description "Like \\funref{get-next-tuple},
          but now the next \\argument{n} remaining tuples are requested. Pass
          {\\tt nil} if you want all remaining tuples; see also
          \\funref{get-all-remaining-tuples}")))

  ((query null) &optional n &rest args &key &allow-other-keys)

  (declare (ignorable n args))

  :not-found)

(nrql-defmethod (get-next-n-remaining-tuples)

  ((query symbol) &optional n &rest args &key &allow-other-keys)

  (declare (ignorable n))

  (with-racer-timeout
    (apply #'execute-if-needed query :tuple-at-a-time-p t args)

    (get-next-n-remaining-tuples (or (find-query query *active-queries*)
                                     (find-query query *processed-queries*))
                                 n)))

;;;
;;;
;;;

(nrql-defmethod (get-all-remaining-tuples
                 :doc ((:doc-type :short)
                       (:category :getting-answers)
                       (:description "Like \\funref{get-next-n-remaining-tuples} 
          with \\argument{n} = {\\tt nil}")))

  ((query null) &rest args &key &allow-other-keys)
  (declare (ignorable args))
  :not-found)

(nrql-defmethod (get-all-remaining-tuples)
  ((query symbol) &rest args &key &allow-other-keys)
  (with-racer-timeout
    (apply #'execute-if-needed query :tuple-at-a-time-p t args)

    (get-all-remaining-tuples (find-query query))))

;;;
;;; Rules
;;; Incremental Rule Application
;;; Rule Consequence Set at a time
;;; 


(nrql-defmethod (get-current-set-of-rule-consequences
                 :doc ((:doc-type :short)
                       (:category :getting-answers)
                       (:rule-equivalent get-current-tuple)
                       (:description "Returns the result of the last
          call to \\funref{get-next-set-of-rule-consequences} on the rule
          \\argument{id}")
                       (:syntax (get-current-set-of-rule-consequences id))
                       (:arguments 
                        (:first id "the ID of the rule, or {\\tt
          :last}, or {\\tt :last-rule}"))
                       (:values 
                        "See
          \\funref{get-next-set-of-rule-consequences}. Moreover, {\\tt nil} is
          returned if there was no previous call to {\\tt get-next-tuple}")
                       (:remarks)
                       (:examples)
                       (:see-also get-next-set-of-rule-consequences)))
  
  ((query null))
  
  :not-found)

(nrql-defmethod (get-current-set-of-rule-consequences)
  ((query symbol))
  (get-current-set-of-rule-consequences (find-rule query)))

;;;
;;; 
;;;

(nrql-defmethod (next-set-of-rule-consequences-available-p 

                 :doc ((:doc-type :long)
                       (:category :getting-answers)
                       (:description "Checks for the availability of
          the next set of rule consequences")
                       (:rule-equivalent-of next-tuple-available-p)
                       (:syntax (next-set-of-rule-consequences-available-p id))
                       (:arguments 
                        (:first id "the ID of the rule, or {\\tt :last}, 
          or {\\tt :last-rule}"))
                       (:values "{\\tt t} or {\\tt nil} (or {\\tt :not-found})") 
                       (:remarks "If this function returns {\\tt t},
          then \\funref{get-next-set-of-rule-consequences} returns that set immediately
          (without computation delay; the API does not block)")
                       (:examples)
                       (:see-also get-next-set-of-rule-consequences)))

  ((query null))

  :not-found)

(nrql-defmethod (next-set-of-rule-consequences-available-p)
  ((query symbol))
  (next-set-of-rule-consequences-available-p (find-rule query)))

;;;
;;;
;;;

(nrql-defmethod (get-next-set-of-rule-consequences
                 :doc ((:doc-type :long)
                       (:category :getting-answers)
                       (:description "Gets the next set of rule
                   consequences of the rule \\argument{id}. The rule must be on the list
                   of \\funref{active-rules} or \\funref{processed-rules}. A rule can
                   also be started (executed); use \\argument{execute-p} argument")
                       (:syntax (get-next-tuple id &rest args))
                       (:arguments 
                        (:first id "the ID of the rule, or {\\tt :last}, 
                   or {\\tt :last-rule}")
                        (:key execute-p nil "specify {\\tt t} if the rule 
                   is ready to also fire it. The query will be
                   started / executed then. The
                   \\argument{args} are passed to
                   \\funref{execute-query} (however, the query is always executed in
                                                   tuple-at-a-time mode, this cannot be overridden)")
                        (:reference args (execute-query)))
                       (:values 
                        "The tuple, or {\\tt :exhausted} in case there
                   are no more tuples, or special tokens such as {\\tt :not-found}, {\\tt
                   :inconsistent}, etc")
                       (:remarks "If the query had been started in
                   lazy tuple-at-a-time mode, then computation of the next tuple might
                   eventually take some time and thus the function might not return
                   immediately.

                   However, if the query had been started in eager mode, then there is a
                   chance that the next tuple (and probably some more tuples not yet
                                                   requested) have already been pre-computed, and are thus already
                   available.  The function \\funref{next-tuple-available-p} can be used
                   to check for the availability of such (immediately available) tuples.

                   Note that a query might still have tuples available, even if the query
                   process (thread) has already terminated and thus the query is no
                   longer active (the query already appears on the list of
                                      \\funref{processed-queries}).  This happens in the eager
                   tuple-at-a-time mode")

                       (:examples)
                       (:see-also next-tuple-available-p 
                        get-current-tuple 
                        get-next-n-remaining-tuples)))

  ((query null) &rest args &key &allow-other-keys)

  (declare (ignorable args))
   
  :not-found)

(nrql-defmethod (get-next-set-of-rule-consequences)
  ((query symbol) &rest args &key &allow-other-keys)
  (declare (ignorable args))

  (with-racer-timeout
    (get-next-tuple (or (find-rule query *active-rules*)
                        (find-rule query *processed-rules*)))))


;;;
;;; 
;;;


(nrql-defmethod (get-next-n-remaining-sets-of-rule-consequences  

                 :doc ((:doc-type :short)
                       (:category :getting-answers)
                       (:description "Like \\funref{get-next-set-of-rule-consequences},
                   but now the next \\argument{n} remaining sets are requested. Pass 
                   {\\tt nil} if you want all remaining sets; see also 
                   \\funref{get-all-remaining-sets-of-rule-consequences}")))

  ((query null) &optional n &rest args &key &allow-other-keys)
  (declare (ignorable n args))
  :not-found)

(nrql-defmethod (get-next-n-remaining-sets-of-rule-consequences)
  ((query symbol) &optional n &rest args &key &allow-other-keys)
  (declare (ignorable n args))
  (with-racer-timeout
    (get-next-n-remaining-tuples (find-rule query) n)))

;;;
;;;
;;;

(nrql-defmethod (get-all-remaining-sets-of-rule-consequences

                 :doc ((:doc-type :short)
                       (:category :getting-answers)
                       (:description "Like \\funref{get-next-n-remaining-sets-of-rule-consequences} 
                   with \\argument{n} = {\\tt nil}")))

  ((query null) &rest args &key &allow-other-keys)
  (declare (ignorable args))
  :not-found)

(nrql-defmethod (get-all-remaining-sets-of-rule-consequences)
  ((query symbol) &rest args &key &allow-other-keys)
  (declare (ignorable args))

  (with-racer-timeout
    (get-all-remaining-tuples (find-rule query))))


;;;
;;; Functional interface 
;;; Query answering (Prepare + Execute + Get-Answer) 
;;;

(nrql-defun (racer-answer-query

             :doc ((:doc-type :long)

                   (:category :abox-queries)

                   (:description "Prepares an ABox query using
                   \\funref{racer-prepare-query} and then executes it with
                   \\funref{execute-query}") 
        
                   (:syntax (racer-answer-query head body &rest args))
        
                   (:arguments 
                    (:reference (head body) (racer-prepare-query))
                    (:rest args nil "the union of the keyword
                   arguments accepted by \\funref{racer-prepare-query} and
                   \\funref{execute-query}. If the query is executed in set-at-a-time
                   mode, also \\funref{get-answer} is called automatically by
                   \\funref{execute-query}; thus, the keyword arguments of
                   \\funref{get-answer} are accepted as well"))
        
                   (:values "Conceptually, {\\tt racer-answer-query}
                   first calls \\funref{racer-prepare-query} and then
                   \\funref{execute-query}. If query is executed in set-at-a-time mode, 
                   then the result of \\funref{execute-query} is returned (the query answer). 
                   If the query is executed in tuple-at-a-time mode, then a query status description is
                   returned")

                   (:remarks)
        
                   (:examples
                    (racer-answer-query '(?x) '(and (?x woman) (?x ?y has-child)))
                    (racer-answer-query '(?x) '(and (?x woman) (?x ?y has-child))
                                        :abox smith-family :id test
                                        :how-many 2))
         
                   (:see-also racer-prepare-query execute-query get-answer)))

  (res-args query &rest args)

  (apply #'racer-prepare-query res-args query :execute-p t args))

(nrql-defun (racer-answer-query1
             :doc ((:doc-type :short)
                   (:category :abox-queries)
                   (:description "Like \\funref{racer-answer-query}, but with
                   flipped argument positions for \\argument{head} and \\argument{body}"))  )

  (query res-args  &rest args)

  (apply #'racer-answer-query res-args query 
         :dont-show-lambdas-p t
         args))



(nrql-defun (answer-query 
             :doc ((:doc-type :short)
                   (:category :abox-queries)
                   (:synonym racer-answer-query)))
  (&rest args)

  (apply #'racer-answer-query args))



(nrql-defun (answer-query1 
             :doc ((:doc-type :short)
                   (:category :abox-queries)
                   (:synonym racer-answer-query1)))
  (&rest args)

  (apply #'racer-answer-query1 args))

;;;
;;;
;;;

(nrql-defun (racer-answer-query-under-premise

             :doc ((:doc-type :short)
                   (:category :abox-queries)
                   (:description "Like \\funref{racer-answer-query},
                   but a query \\argument{premise} is added to the ABox before the query
                   is answered (this can also be achieved with the \\argument{premise}
                                     keyword argument of \\funref{racer-answer-query})")))

  (premise res-args query &rest args)
    
  (apply #'racer-prepare-query res-args query 
         :premise premise 
         :execute-p t args))

(nrql-defun (racer-answer-query-under-premise1  

             :doc ((:doc-type :short)
                   (:category :abox-queries)
                   (:description "Like \\funref{racer-answer-query-under-premise}, but with
                   flipped argument positions for \\argument{head} and \\argument{body}")))

  (premise query res-args &rest args)
    
  (apply #'racer-answer-query-under-premise 
         premise res-args query args))


(nrql-defun (answer-query-under-premise
             :doc ((:doc-type :short)
                   (:category :abox-queries)
                   (:synonym racer-answer-query-under-premise)))
  (&rest args)

  (apply #'racer-answer-query-under-premise args))



(nrql-defun (answer-query-under-premise1
             :doc ((:doc-type :short)
                   (:category :abox-queries)
                   (:synonym racer-answer-query-under-premise1)))
  (&rest args)

  (apply #'racer-answer-query-under-premise args))

;;; 
;;; Rules 
;;; 

;;;
;;; Functional interface 
;;; Rule preparation 
;;; 

(nrql-defun (racer-prepare-rule 
             :doc ((:doc-type :long)
                   (:category :rules)
                   
                   (:rule-equivalent-of racer-prepare-query)

                   (:description "Prepares (i.e., parses and compiles) a nRQL
                   ABox query but does not execute (start) it yet")

                   (:syntax (racer-prepare-rule antecedence consequence 
                                                &rest args 
                                                &key id
                                                abox 
                                                execute-p 
                                                premise
                                                type-of-substrate 
                                                prepare-now-p))

                   (:arguments
                    (:first antecedence "the antecedence of the rule, see {\\tt
                   <rule-antecedence>}, Section 6.1.8 in the User Guide")
                    (:second consequence "the consequence of the rule, see {\\tt
                   <rule-consequence>}, Section 6.1.8 in the User Guide")
                    (:key id RULE-XXX "the ID (name) of the query")
                    (:key abox (current-abox) "the (name of the) ABox
                   to which the rule is applied")
                    (:key execute-p nil "if {\\tt t}, then the rule is automatically
                   executed; the \\argument{args}  are passed to \\funref{execute-rule}")
                    (:key premise nil "the  premise of the rule, a list of ABox assertions,
                   see {\\tt <query-premise>}, Section 6.1.8 in the User Guide")
                    (:key type-of-substrate "'racer-dummy-substrate" "a symbol
                   naming a substrate type, one of: \\argument{racer-dummy-substrate,
                   data-substrate, mirror-data-substrate, rcc-substrate,
                   racer-tbox-mirror-substrate}")
                    (:key prepare-now-p nil "if {\\tt t}, then the substrate
                   will be prepared immediately, otherwise later when the rule is
                   about to be applied")
                    (:reference args (execute-rule)))

                   (:values  "A list like {\\tt (:RULE-XXX :READY-TO-RUN)}, where {\\tt :RULE-XXX}
                   is the rule ID and {\\tt :READY-TO-RUN} indicates the current
                   status of the rule" )

                   (:remarks "To fire (start, apply) the rule, use \\funref{execute-rule}")

                   (:examples )

                   (:see-also prepare-abox-query execute-query)))

  (query res-args &rest args)                                   
  
  (if (ensure-deadlock-prevention)
      :denied-due-to-deadlock-prevention
    (with-racer-timeout
      (apply #'racer-prepare-substrate args)    
      (apply #'apply-rule-int *cur-substrate* query res-args (append args (list :execute-p nil))))))

(nrql-defun (racer-prepare-rule1  

             :doc ((:doc-type :short)
                   (:category :rules)
                   (:rule-equivalent-of racer-prepare-query1)
                   (:description "Like \\funref{racer-prepare-rule}, but with
                   flipped argument positions for \\argument{antecedence} and \\argument{consequence}"))) 

  (res-args query &rest args) 
    
  (apply #'racer-prepare-rule query res-args args))


(nrql-defun (prepare-rule
             :doc ((:doc-type :short)
                   (:category :abox-queries)
                   (:synonym racer-prepare-rule)))
  (&rest args)

  (apply #'racer-prepare-rule args))


(nrql-defun (prepare-rule1
             :doc ((:doc-type :short)
                   (:category :abox-queries)
                   (:synonym racer-prepare-rule1)))
  (&rest args)

  (apply #'racer-prepare-rule1 args))

;;;
;;; Functional interface
;;; Rule execution
;;; 

(nrql-defmethod (execute-rule
                 :doc ((:doc-type :long)
                       (:category :rules)
                       (:rule-equivalent-of :execute-query) 
                       (:description "See \\funref{execute-query}")
                       (:syntax (execute-rule id
                                              &rest args &key 
                                              add-rule-consequences-p))

                       (:arguments 
                        (:first id "the ID of the rule to be executed")
                        (:key add-rule-consequences-p "by environment" 
                         "if {\\tt t} is specified and the rule is
                   executed, the rule consequences are added to the ABox and returned,
                   otherwise they are only returned but not added. Note that in
                   set-at-a-time mode, this applies to all generated rule consequences,
                   whereas in tuple-at-a-time mode this applies to the selected set of
                   consequences (see \\funref{get-next-set-of-rule-consequences},
                                     \\funref{choose-current-set-of-rule-consequences})")
                        (:key dont-add-abox-duplicates-p nil "if {\\tt t}, prevents addition of ABox duplicates")
                        (:reference args (execute-query answer-query)))
  
                       (:values "If the rule is executed in
                   set-at-a-time mode, then the ABox assertions generated by the rule
                   consequence since \\funref{get-answer} is called automatically,
                   otherwise a list like {\\tt (:RULE-466 :RUNNING)}, where {\\tt
                   :RULE-466} is the query ID and {\\tt :RUNNING} indicates the current
                   status of the rule" )

                       (:remarks  "The rule has to be prepared (ready) before it can 
                   be executed, see \\funref{rule-ready-p}, \\funref{prepared-rules}.

                   Note that rules cannot be execute in eager tuple-at-a-time mode")
                       (:examples)

                       (:see-also racer-prepare-rule get-answer prepare-nrql-engine)))

  ((query null) &rest args)

  (declare (ignorable args))
  :not-found)

(nrql-defmethod (execute-rule) ((query t) &rest args)
  (with-racer-timeout
    (apply #'eval-nrql-settings2
           #'(lambda ()
               (apply #'execute-rule (find-rule query *ready-rules*) args))
           :allow-other-keys t
           args)))


;;; Functional interface Rule application (Prepare + Execute + Get-Answer)
;;;

(nrql-defun (racer-apply-rule
             :doc ((:doc-type :long)
                   (:category :rules)
                   
                   (:rule-equivalent-of racer-answer-query)

                   (:description "Prepares a rule using
                   \\funref{racer-prepare-rule} and then executes it with
                   \\funref{execute-rule}")
                   
                   (:syntax (racer-apply-rule antecedence consequence &rest args))
        
                   (:arguments 
                    (:reference (head body) (racer-prepare-rule))
                    (:rest args nil "the union of the keyword arguments accepted by 
                   \\funref{racer-prepare-rule} and \\funref{execute-rule} (see there)"))
        
                   (:values "Conceptually, {\\tt racer-apply-rule}
                   first calls \\funref{racer-prepare-rule}
                   and then \\funref{execute-rule}.  Thus, the result of
                   \\funref{execute-rule} is returned. However, in case the rule is 
                   not executed (for example, if it has been recognized as 
                                     inconsistent), then the result of \\funref{racer-prepare-rule} is 
                   returned")

                   (:remarks)
        
                   (:examples)

                   (:see-also racer-prepare-rule execute-rule get-answer)))

  (query res-args &rest args)

  (apply #'racer-prepare-rule query res-args :execute-p t args))


(nrql-defun (racer-apply-rule-under-premise 

             :doc ((:doc-type :short)
                   (:category :rules)
                   (:rule-equivalent-of racer-answer-query-under-premise)
                   
                   (:description "Like \\funref{racer-apply-rule}, but with
                   argument list {\\tt (premise antecedence consequence \\&rest args)}")))

  (premise query res-args &rest args) 
    
  (apply #'racer-apply-rule query res-args :premise premise args))



(nrql-defun (racer-apply-rule1  

             :doc ((:doc-type :short)
                   (:category :rules)
                   (:rule-equivalent-of racer-answer-query1)
                   
                   (:description "Like \\funref{racer-apply-rule}, but with
                   flipped argument positions for \\argument{antecedence} and \\argument{consequence}"))) 

  (res-args  query &rest args) 
    
  (apply #'racer-apply-rule query res-args args))


(nrql-defun (racer-apply-rule-under-premise1 

             :doc ((:doc-type :short)
                   (:category :rules)
                   (:rule-equivalent-of racer-answer-query-under-premise1)
                   
                   (:description "Like \\funref{racer-apply-rule-under-premise}, but with
                   argument list {\\tt (premise antecedence consequence \\&rest args)}")))

  (premise res-args query &rest args) 
    
  (apply #'racer-apply-rule-under-premise premise query res-args args))



(nrql-defun (apply-rule
             :doc ((:doc-type :short)
                   (:category :rules)
                   (:synonym racer-apply-rule)))
  (&rest args)

  (apply #'racer-apply-rule args))

(nrql-defun (apply-rule-under-premise 
             :doc ((:doc-type :short)
                   (:category :rules)
                   (:synonym racer-apply-rule-under-premise)))
  (&rest args)

  (apply #'racer-apply-rule-under-premise args))


(nrql-defun (apply-rule-under-premise1
             :doc ((:doc-type :short)
                   (:category :rules)
                   (:synonym racer-apply-rule-under-premise1)))
  (&rest args)

  (apply #'racer-apply-rule-under-premise1 args))

;;;
;;; Queries
;;; 
    
;;;
;;; Macro interface  
;;; Query preparation
;;; 

(nrql-defmacro (prepare-abox-query :nrql-function racer-prepare-query
                                   :doc ((:doc-type :short)
                                         (:category :abox-queries))))

(nrql-defmacro (prepare-abox-query1 :nrql-function racer-prepare-query1
                                    :doc ((:doc-type :short)
                                          (:category :abox-queries))))

;;;
;;; Macro interface  
;;; Query answering (Prepare + Execute + Get-Answer) 
;;; 

(nrql-defmacro (retrieve :nrql-function racer-answer-query
                         :doc ((:doc-type :short)
                               (:category :abox-queries))))

(nrql-defmacro (retrieve1 :nrql-function racer-answer-query1
                          :doc ((:doc-type :short)
                                (:category :abox-queries))))

(nrql-defmacro (retrieve-under-premise :nrql-function racer-answer-query-under-premise
                                       :doc ((:doc-type :short)
                                             (:category :abox-queries))))

(nrql-defmacro (retrieve-under-premise1 :nrql-function racer-answer-query-under-premise1
                                        :doc ((:doc-type :short)
                                              (:category :abox-queries))))

;;;
;;; Rules 
;;; 

;;;
;;; Macro interface
;;; Rule preparation
;;;  

(nrql-defmacro (prepare-abox-rule :nrql-function racer-prepare-rule
                                  :doc ((:doc-type :short)
                                        (:category :rules)
                                        (:rule-equivalent-of prepare-abox-query))))


(nrql-defmacro (prepare-abox-rule1 :nrql-function racer-prepare-rule1 
                                   :doc ((:doc-type :short)
                                         (:category :rules)
                                         (:rule-equivalent prepare-abox-query1))))

;;;
;;; Synonyme 
;;; 

(nrql-defmacro (preprule :nrql-function racer-prepare-rule
                         :doc ((:doc-type :short)
                               (:category :rules)
                               (:synonym prepare-abox-rule))))

(nrql-defmacro (preprule1 :nrql-function racer-prepare-rule1 
                          :doc ((:doc-type :short)
                                (:category :rules)
                                (:synonym prepare-abox-rule1))))


;;;
;;; Macro interface
;;; Rule application (Prepare + Execute + Get-Answer)
;;;

(nrql-defmacro (apply-abox-rule :nrql-function racer-apply-rule
                                :doc ((:doc-type :short)
                                      (:category :rules)
                                      (:rule-equivalent-of retrieve))))

(nrql-defmacro (apply-abox-rule1 :nrql-function racer-apply-rule1 
                                 :doc ((:doc-type :short)
                                       (:category :rules)
                                       (:rule-equivalent-of retrieve1))))

(nrql-defmacro (apply-abox-rule-under-premise :nrql-function racer-apply-rule-under-premise
                                              :doc ((:doc-type :short)
                                                    (:category :rules)
                                                    (:rule-equivalent-of retrieve-under-premise))))

(nrql-defmacro (apply-abox-rule-under-premise1 :nrql-function racer-apply-rule-under-premise1 
                                               :doc ((:doc-type :short)
                                                     (:category :rules)
                                                     (:rule-equivalent-of retrieve-under-premise1))))

;;;
;;; Synonyme 
;;; 

(nrql-defmacro (firerule :nrql-function racer-apply-rule
                         :doc ((:doc-type :short)
                               (:category :rules)
                               (:synonym apply-abox-rule))))


(nrql-defmacro (firerule1 :nrql-function racer-apply-rule1 
                          :doc ((:doc-type :short)
                                (:category :rules)
                                (:synonym apply-abox-rule1))))

(nrql-defmacro (firerule-under-premise :nrql-function racer-apply-rule-under-premise
                                       :doc ((:doc-type :short)
                                             (:category :rules)
                                             (:synonym apply-abox-rule-under-premise))))


(nrql-defmacro (firerule-under-premise1 :nrql-function racer-apply-rule-under-premise1 
                                        :doc ((:doc-type :short)
                                              (:category :rules)
                                              (:synonym apply-abox-rule-under-premise1))))

;;;
;;; Copy Rules
;;; 

(nrql-defun (copy-rules
             :doc ((:doc-type :long)
                   (:category :rules)
                   (:description "Copies the set of rules associated with substrate of type \\argument{type-of-substrate} for ABox \\argument{from-abox} into substrate for ABox \\argument{to-abox}. ")
                   (:syntax (copy-rules default new-abox))
                   (:arguments 
                    (:first from-abox  "the source ABox")
                    (:second to-abox  "the destination ABox")
                    (:key type-of-substrate  racer-dummy-substrate "consider substrates of 
                   specified type only, one of:
                   \\argument{racer-dummy-substrate,
                   data-substrate, mirror-data-substrate, rcc-substrate,
                   racer-tbox-mirror-substrate}"))))

  (from-abox to-abox &rest args)

  (apply #'copy-rules1 from-abox to-abox args))

;;;
;;; Move Rules
;;; 

(nrql-defun (move-rules
             :doc ((:doc-type :long)
                   (:category :rules)
                   (:description "Moves the set of rules associated with substrate of type \\argument{type-of-substrate} for ABox \\argument{from-abox} into substrate for ABox \\argument{to-abox}. ")
                   (:syntax (move-rules default new-abox))
                   (:arguments 
                    (:first from-abox  "the source ABox")
                    (:second to-abox  "the destination ABox")
                    (:key type-of-substrate  racer-dummy-substrate "consider substrates of 
                   specified type only, one of:
                   \\argument{racer-dummy-substrate,
                   data-substrate, mirror-data-substrate, rcc-substrate,
                   racer-tbox-mirror-substrate}"))))

  (from-abox to-abox &rest args)

  (apply #'move-rules1 from-abox to-abox args))
  
  
  

;;;
;;; TBox queries 
;;;

;;;
;;; Functional interface 
;;; Query preparation
;;; 

(nrql-defun (racer-prepare-tbox-query 
             :doc ((:doc-type :long)
                   (:category :tbox-queries)
                   (:description "Prepares  (i.e., parses and compiles) a nRQL
                   TBox query but does not execute (start) it yet")
                   (:syntax (racer-prepare-tbox-query head body &key
                                                      tbox id prepare-now-p))
                   (:arguments
                    (:first head  "the head of the TBox query.  Only 
                   variables, individuals and lambda operators are allowed")
                    (:second body "the body of the TBox query.
                   Only concept and role query atoms are allowed.  Only concept names can
                   be used in these concept query atoms, and only the role names
                   {\\tt has-child, has-parent, has-descendant, has-ancestor} in the role query atoms")
                    (:key id QUERY-XXX "the ID (name) of the query")
                    (:key tbox (current-tbox) "the (name of the) TBox to be queried")
                    (:key execute-p nil "if {\\tt t} is specified, then the query is 
                   automatically started with \\funref{execute-query}; the \\argument{args} are passed to 
                   \\funref{execute-query}")
                    (:key prepare-now-p nil "if {\\tt t}, then the substrate
                   will be prepared immediately, otherwise later when the query is
                   about to be executed")
                    (:reference args (execute-query)))
                   
                   (:values  "A list like {\\tt (:QUERY-466 :READY-TO-RUN)}, where {\\tt :QUERY-466}
                   is the query ID and {\\tt :READY-TO-RUN} indicates the current
                   status of the query" )
                   
                   (:remarks "To start the query, use \\funref{execute-query} 
                   (or \\argument{execute-p} = {\\tt t})")
                   
                   (:examples 
                    (racer-prepare-tbox-query '(?x) '(and (?x woman) (?x ?y has-child))))
                   
                   (:see-also execute-query get-answer)))

  (res-args query &rest args)

  (with-racer-timeout
    (let ((*use-repository-p* nil)
          (*rewrite-semantically-p* nil)
          (*report-inconsistent-queries-p* nil)
          (*report-tautological-queries-p* nil))

      (apply #'racer-prepare-tbox-substrate args)
      (apply #'answer-query-int *cur-substrate* query res-args (append args (list :execute-p nil))))))


(nrql-defun (racer-prepare-tbox-query1 
             :doc ((:doc-type :short)
                   (:category :tbox-queries)
                   (:description "Like \\funref{racer-prepare-tbox-query}, but with
                   flipped argument positions for \\argument{head} and \\argument{body}")))
  
  (query res-args &rest args)

  (apply #'racer-prepare-tbox-query res-args query args))

#|

;;; eigentlich sollten diese Funktionen so heissen, 
;;; aber wie nenne ich dann die Makros? 

(nrql-defun (prepare-tbox-query 
             :doc ((:doc-type :short)
                   (:category :tbox-queries)
                   (:synonym racer-prepare-tbox-query)))
  (&rest args)

  (apply #'racer-prepare-tbox-query args))

(nrql-defun (prepare-tbox-query1
             :doc ((:doc-type :short)
                   (:category :tbox-queries)
                   (:synonym racer-prepare-tbox-query1)))
  (&rest args)

  (apply #'racer-prepare-tbox-query1 args))

|#


;;; 
;;; Functional interface 
;;; Query answering (Prepare + Execute + Get-Answer)  
;;; 


(nrql-defun (racer-answer-tbox-query
             :doc ((:doc-type :short)
                   (:category :tbox-queries)
                   (:description "TBox query equivalent of \\funref{racer-answer-query}")))

  (res-args query &rest args)

  (apply #'racer-prepare-tbox-query res-args query :execute-p t args))

(nrql-defun (racer-answer-tbox-query1
             :doc ((:doc-type :short)
                   (:category :tbox-queries)
                   (:description "Like \\funref{racer-answer-tbox-query}, but with
                   flipped argument positions for \\argument{head} and \\argument{body}")))

  (query res-args &rest args)
 
  (apply #'racer-prepare-tbox-query res-args query 
         :execute-p t 
         :dont-show-lambdas-p t
         args))


(nrql-defun (answer-tbox-query 
             :doc ((:doc-type :short)
                   (:category :tbox-queries)
                   (:synonym racer-answer-tbox-query)))
  (&rest args)

  (apply #'racer-answer-tbox-query args))

(nrql-defun (answer-tbox-query1 
             :doc ((:doc-type :short)
                   (:category :tbox-queries)
                   (:synonym racer-answer-tbox-query)))
  (&rest args)

  (apply #'racer-answer-tbox-query1 args))


;;;
;;; Macro interface 
;;; Query preparation
;;; 

(nrql-defmacro (prepare-tbox-query :nrql-function racer-prepare-tbox-query
                                   :doc ((:doc-type :short)
                                         (:category :tbox-queries))))

(nrql-defmacro (prepare-tbox-query1 :nrql-function racer-prepare-tbox-query1
                                    :doc ((:doc-type :short)
                                          (:category :tbox-queries))))

;;;
;;; Macro interface 
;;; TBox query answering (Prepare + Execute + Get-Answer) 
;;; 

(nrql-defmacro (tbox-retrieve :nrql-function racer-answer-tbox-query
                              :doc ((:doc-type :short)
                                    (:category :tbox-queries))))


(nrql-defmacro (tbox-retrieve1 :nrql-function racer-answer-tbox-query1
                               :doc ((:doc-type :short)
                                     (:category :tbox-queries))))

;;;
;;; Query Lifecycle 
;;; Ready (Prepared), Active (Running, Waiting (Sleeping)), Processed (Terminated)
;;; 


;;;
;;; Praedikate  
;;; Ready (Prepared) Queries and Rules  
;;; 

(nrql-defmethod (query-ready-p
                 :doc ((:doc-type :long)
                       (:category :query-lifecycle)
                       (:description "Checks whether  query \\argument{id} is ready for execution")
                       (:syntax (query-ready-p id))
                       (:arguments
                        (:first id
                         "the ID of the query, or {\\tt :last} or {\\tt :last-query}"))
                       (:values "{\\tt t} or {\\tt  nil}")
                       (:remarks "Use \\funref{execute-query} to start the query")
                       (:examples)
                       (:see-also ready-queries)))
                       
  ((query null))

  :not-found)

(nrql-defmethod (query-ready-p)
  ((query symbol))
  (when (find-query query *ready-queries*)
    t))

;;;
;;; 
;;;

(nrql-defmethod (rule-ready-p 
                 :doc ((:doc-type :short)
                       (:category :rule-lifecycle)
                       (:rule-equivalent-of query-ready-p)))

  ((query null))

  :not-found)

(nrql-defmethod (rule-ready-p)
  ((query symbol))
  (when (find-rule query *ready-rules*)
    t))

;;;
;;; Prepared-p = Synonym zu Ready-p 
;;;


(nrql-defmethod (query-prepared-p
                 :doc ((:doc-type :short)
                       (:category :query-lifecycle)
                       (:synonym query-ready-p)))

  ((query t)) 
  (query-ready-p query))

(nrql-defmethod (rule-prepared-p
                 :doc ((:doc-type :short)
                       (:category :rule-lifecycle)
                       (:synonym rule-ready-p)))
  ((query t))  
  (rule-ready-p query))


;;;
;;; Active Queries and Rules 
;;; Active = Partitioniert in Running und Waiting 
;;; 

(nrql-defmethod (query-active-p
                 :doc ((:doc-type :long)
                       (:category :query-lifecycle)
                       (:description "Checks whether query \\argument{id} is active. A query is active iff a
                   corresponding query answering thread exists")
                       (:syntax (query-active-p id))
                       (:arguments
                        (:first id
                         "the ID of the query, or {\\tt :last} or {\\tt :last-query}"))
                       (:values "{\\tt t} or {\\tt  nil}")
                       (:remarks "An active query can either be
                   waiting (if it has been started in lazy mode, \\funref{query-waiting-p}) or running
                   (\\funref{query-running-p}), until its process terminates or 
                   is manually aborted (see \\funref{abort-query}), \\funref{query-processed-p}")
                       (:see-also active-queries query-waiting-p query-running-p)))
  ((query null))
  :not-found)

(nrql-defmethod (query-active-p) ((query symbol))
  (query-active-p (find-query query)))

;;;
;;;
;;;

(nrql-defmethod (rule-active-p
                 :doc ((:doc-type :short)
                       (:category :rule-lifecycle)
                       (:rule-equivalent-of query-active-p)))

  ((query null))
  :not-found)

(nrql-defmethod (rule-active-p) ((query symbol))
  (rule-active-p (find-rule query)))

;;;
;;; Running Queries and Rules
;;;

(nrql-defmethod (query-running-p 
                 :doc ((:doc-type :long)
                       (:category :query-lifecycle)
                       (:description "Checks whether query \\argument{id}
                   is active and running, i.e., its query answering process (thread) is 
                   currently consuming CPU cycles")
                       (:syntax (query-running-p id))
                       (:arguments
                        (:first id
                         "the ID of the query, or {\\tt :last} or {\\tt :last-query}"))
                       (:values "{\\tt t} or {\\tt  nil}")
                       (:remarks "Use \\funref{abort-query} to abort the query")
                       (:examples)
                       (:see-also  running-queries query-active-p 
                        query-waiting-p)))
  ((query null))

  :not-found)

(nrql-defmethod (query-running-p)
  ((query symbol))
  (query-running-p (find-query query)))

;;;
;;;
;;;

(nrql-defmethod (rule-running-p 
                 :doc ((:doc-type :short)
                       (:category :query-lifecycle)
                       (:rule-equivalent-of query-running-p)))
  ((query null))
  :not-found)

(nrql-defmethod (rule-running-p) ((query symbol))
  (rule-running-p (find-rule query)))

;;;
;;; Waiting Queries and Rules
;;;

(nrql-defmethod (query-waiting-p 
                 :doc ((:doc-type :long)
                       (:category :query-lifecycle)
                       (:description "Checks whether the query \\argument{id}
                   is active and waiting (sleeping), i.e., its query answering process (thread)
                   is currently not consuming CPU cycles")
                       (:syntax (query-waiting-p id))
                       (:arguments
                        (:first id
                         "the ID of the query, or {\\tt :last} or {\\tt :last-query}"))
                       (:values "{\\tt t} or {\\tt  nil}")
                       (:remarks "Use \\funref{abort-query} to abort the query")
                       (:examples)
                       (:see-also waiting-queries query-active-p 
                        query-running-p)))
  ((query null))

  :not-found)

(nrql-defmethod (query-waiting-p)
  ((query symbol))
  (query-waiting-p (find-query query)))

;;;
;;;
;;;

(nrql-defmethod (rule-waiting-p 
                 :doc ((:doc-type :short)
                       (:category :query-lifecycle)
                       (:rule-equivalent-of query-waiting-p)))
  ((query null))
  :not-found)

(nrql-defmethod (rule-waiting-p) ((query symbol))
  (rule-waiting-p (find-rule query)))


;;;
;;; Sleeping = Synonym zu Waiting 
;;;


(nrql-defmethod (query-sleeping-p
                 :doc ((:doc-type :short)
                       (:category :query-lifecycle)
                       (:synonym query-waiting-p)))

  ((query t)) 
  (query-waiting-p query))

(nrql-defmethod (rule-sleeping-p
                 :doc ((:doc-type :short)
                       (:category :rule-lifecycle)
                       (:synonym rule-waiting-p)))
  ((query t))  
  (rule-waiting-p query))


;;;
;;; Processed Queries 
;;;


(nrql-defmethod (query-processed-p
                 :doc ((:doc-type :long)
                       (:category :query-lifecycle)
                       (:description "Checks whether the query
                   \\argument{id} is processed and terminated, i.e., its query answering
                   process (thread) has died") 
                       (:syntax (query-processed-p id))
                       (:arguments
                        (:first id
                         "the ID of the query, or {\\tt :last} or {\\tt :last-query}"))
                       (:values "{\\tt t} or {\\tt  nil}")
                       (:remarks "Use \\funref{reprepare-query} to
                   reprepare the query, \\funref{reexecute-query} to reexecute it")
                       (:examples)
                       (:see-also processed-queries query-active-p 
                        query-prepared-p)))

  ((query null))
  :not-found)

(nrql-defmethod (query-processed-p) ((query symbol))
  (query-processed-p (find-query query)))

(nrql-defmethod (query-processed-p) ((query query))
  (when (member query *processed-queries*) 
    t))

;;;
;;;
;;; 

(nrql-defmethod (rule-processed-p
                 :doc ((:doc-type :short)
                       (:category :rule-lifecycle)
                       (:rule-equivalent-of query-processed-p)))

  ((query null))
  :not-found)

(nrql-defmethod (rule-processed-p) ((query symbol))
  (rule-processed-p (find-rule query)))

(nrql-defmethod (rule-processed-p) ((query query))
  (when (member query *processed-rules*)
    t))

;;;
;;; Terminated-p = Synonym zu Processed-p 
;;; 

(nrql-defmethod (query-terminated-p
                 :doc ((:doc-type :short)
                       (:category :query-lifecycle)
                       (:synonym query-processed-p)))
  ((query t))
  (query-processed-p query))


(nrql-defmethod (rule-terminated-p
                 :doc ((:doc-type :short)
                       (:category :rule-lifecycle)
                       (:synonym rule-processed-p)))
  ((query t))
  (rule-processed-p query))


;;;
;;; Listenmethoden fuer eben definierte Praedikate 
;;; 

(nrql-defun (ready-queries 
             :doc ((:doc-type :long)
                   (:category :query-lifecycle)
                   (:description "Returns a list of (the IDs of) all queries which satisfy
                   \\funref{query-ready-p}")
                   (:syntax (ready-queries))
                   (:arguments
                    (:key abox (current-abox) "consider queries for substrates for ABox {\\tt abox} only")
                    (:key type-of-substrate  racer-dummy-substrate "consider queries for substrates of 
                   specified type only, one of:
                   \\argument{racer-dummy-substrate,
                   data-substrate, mirror-data-substrate, rcc-substrate,
                   racer-tbox-mirror-substrate}"))
                   (:see-also active-queries processed-queries)))
  (&rest args)
  (apply #'map-over-queries-for-aboxes-of-type 
         #'iterator-id 
         (with-access-to-lifecycle-lists
           (copy-list *ready-queries*))
         args))

(nrql-defun (prepared-queries 
             :doc ((:doc-type :short)
                   (:category :query-lifecycle)
                   (:synonym ready-queries)))
  (&rest args)
  (apply #'ready-queries args))

;;;
;;;
;;;

(nrql-defun (ready-rules 
             :doc ((:doc-type :long)
                   (:category :rule-lifecycle)
                   (:rule-equivalent ready-queries)
                   (:description "Returns a list of (the IDs of) all rules which satisfy
                   \\funref{rule-ready-p}")
                   (:syntax (ready-rules))
                   (:arguments
                    (:key abox (current-abox) "consider rules for substrates for ABox {\\tt abox} only")
                    (:key type-of-substrate  racer-dummy-substrate "consider rules for substrates of 
                   specified type only, one of:
                   \\argument{racer-dummy-substrate,
                   data-substrate, mirror-data-substrate, rcc-substrate,
                   racer-tbox-mirror-substrate}"))
                   (:see-also active-rules processed-rules)))
  (&rest args)
  (apply #'map-over-queries-for-aboxes-of-type 
         #'iterator-id 
         (with-access-to-lifecycle-lists
           (copy-list *ready-rules*))
         args))


(nrql-defun (prepared-rules 
             :doc ((:doc-type :short)
                   (:category :rule-lifecycle)
                   (:synonym ready-rules)))
  (&rest args)
  (apply #'ready-rules args))

;;;
;;;
;;;

(nrql-defun (active-queries
             :doc ((:doc-type :long)
                   (:category :query-lifecycle)
                   (:description "Returns a list of (the IDs of) all queries which satisfy
                   \\funref{query-active-p}")
                   (:syntax (active-queries))
                   (:arguments
                    (:key abox (current-abox) "consider queries for substrates for ABox {\\tt abox} only")
                    (:key type-of-substrate  racer-dummy-substrate "consider queries for substrates of 
                   specified type only, one of:
                   \\argument{racer-dummy-substrate,
                   data-substrate, mirror-data-substrate, rcc-substrate,
                   racer-tbox-mirror-substrate}"))
                   (:see-also ready-queries processed-queries)))             
  (&rest args)
  (apply #'map-over-queries-for-aboxes-of-type 
         #'iterator-id 
         (with-access-to-lifecycle-lists
           (copy-list *active-queries*))
         args))


(nrql-defun (active-rules 
             :doc ((:doc-type :long)
                   (:category :rule-lifecycle)
                   (:rule-equivalent active-queries)
                   (:description "Returns a list of (the IDs of) all rules which satisfy
                   \\funref{rule-active-p}")
                   (:syntax (active-rules))
                   (:arguments
                    (:key abox (current-abox) "consider rules for substrates for ABox {\\tt abox} only")
                    (:key type-of-substrate  racer-dummy-substrate "consider rules for substrates of 
                   specified type only, one of:
                   \\argument{racer-dummy-substrate,
                   data-substrate, mirror-data-substrate, rcc-substrate,
                   racer-tbox-mirror-substrate}"))
                   (:see-also ready-rules processed-rules)))
  (&rest args)
  (apply #'map-over-queries-for-aboxes-of-type 
         #'iterator-id 
         (with-access-to-lifecycle-lists
           (copy-list *active-rules*))
         args))

;;;
;;;
;;;

(nrql-defun (running-queries 
             :doc ((:doc-type :long)
                   (:category :query-lifecycle)
                   (:description "Returns a list of (the IDs of) all queries which satisfy
                   \\funref{query-running-p}")
                   (:syntax (running-queries))
                   (:arguments
                    (:key abox (current-abox) "consider queries for substrates for ABox {\\tt abox} only")
                    (:key type-of-substrate  racer-dummy-substrate "consider queries for substrates of 
                   specified type only, one of:
                   \\argument{racer-dummy-substrate,
                   data-substrate, mirror-data-substrate, rcc-substrate,
                   racer-tbox-mirror-substrate}"))
                   (:see-also active-queries waiting-queries)))
  (&rest args)
  (apply #'map-over-queries-for-aboxes-of-type 
         #'iterator-id 
         (remove-if-not #'query-running-p 
			(with-access-to-lifecycle-lists
                          (copy-list *active-queries*)))
         args))


(nrql-defun (running-rules 
             :doc ((:doc-type :long)
                   (:category :rule-lifecycle)
                   (:syntax (running-rules))
                   (:arguments
                    (:key abox (current-abox) "consider rules for substrates for ABox {\\tt abox} only")
                    (:key type-of-substrate  racer-dummy-substrate "consider rules for substrates of 
                   specified type only, one of:
                   \\argument{racer-dummy-substrate,
                   data-substrate, mirror-data-substrate, rcc-substrate,
                   racer-tbox-mirror-substrate}"))
                   (:rule-equivalent-of running-queries)
                   (:description "Returns a list of (the IDs of) all rules which satisfy
                   \\funref{rule-running-p}")
                   (:see-also active-rules waiting-rules)))
  (&rest args)
  (apply #'map-over-queries-for-aboxes-of-type 
         #'iterator-id 
         (remove-if-not #'rule-running-p (with-access-to-lifecycle-lists
                                           (copy-list *active-rules*)))
         args))



;;;
;;;
;;;


(nrql-defun (waiting-queries 
             :doc ((:doc-type :long)
                   (:category :query-lifecycle)
                   (:description "Returns a list of (the IDs of) all queries which satisfy
                   \\funref{query-waiting-p}")
                   (:syntax (waiting-queries))
                   (:arguments
                    (:key abox (current-abox) "consider queries for substrates for ABox {\\tt abox} only")
                    (:key type-of-substrate  racer-dummy-substrate "consider queries for substrates of 
                   specified type only, one of:
                   \\argument{racer-dummy-substrate,
                   data-substrate, mirror-data-substrate, rcc-substrate,
                   racer-tbox-mirror-substrate}"))
                   (:see-also active-queries running-queries)))
  (&rest args)
  (apply #'map-over-queries-for-aboxes-of-type 
         #'iterator-id
         (remove-if-not #'query-waiting-p (with-access-to-lifecycle-lists
                                            (copy-list *active-queries*)))
         args))


(nrql-defun (sleeping-queries 
             :doc ((:doc-type :short)
                   (:category :query-lifecycle)
                   (:synonym waiting-queries)))
  (&rest args)
  (apply #'waiting-queries args))


(nrql-defun (waiting-rules 
             :doc ((:doc-type :long)
                   (:category :rule-lifecycle)
                   (:rule-equivalent-of :waiting-queries)
                   (:description "Returns a list of (the IDs of) all rules which satisfy
                   \\funref{rule-waiting-p}")
                   (:syntax (waiting-rules))
                   (:arguments
                    (:key abox (current-abox) "consider rules for substrates for ABox {\\tt abox} only")
                    (:key type-of-substrate  racer-dummy-substrate "consider rules for substrates of 
                   specified type only, one of:
                   \\argument{racer-dummy-substrate,
                   data-substrate, mirror-data-substrate, rcc-substrate,
                   racer-tbox-mirror-substrate}"))
                   (:see-also active-rules running-rules)))
  (&rest args)
  (apply #'map-over-queries-for-aboxes-of-type 
         #'iterator-id
         (remove-if-not #'rule-waiting-p 
			(with-access-to-lifecycle-lists
                          (copy-list *active-rules*)))
         args))


(nrql-defun (sleeping-rules 
             :doc ((:doc-type :short)
                   (:category :rule-lifecycle)
                   (:synonym waiting-rules)))
  (&rest args)
  (apply #'waiting-rules args))

;;;
;;; 
;;;

(nrql-defun (processed-queries 
             :doc ((:doc-type :long)
                   (:category :query-lifecycle)
                   (:description "Returns a list of (the IDs of) all queries which satisfy
                   \\funref{query-processed-p}")
                   (:syntax (processed-queries))
                   (:arguments
                    (:key abox (current-abox) "consider queries for substrates for ABox {\\tt abox} only")
                    (:key type-of-substrate  racer-dummy-substrate "consider queries for substrates of 
                   specified type only, one of:
                   \\argument{racer-dummy-substrate,
                   data-substrate, mirror-data-substrate, rcc-substrate,
                   racer-tbox-mirror-substrate}"))
                   (:see-also prepared-queries active-queries)))            
  (&rest args)
  (apply #'map-over-queries-for-aboxes-of-type
         #'iterator-id 
         (with-access-to-lifecycle-lists
           (copy-list *processed-queries*))
         args))


(nrql-defun (terminated-queries 
             :doc ((:doc-type :short)
                   (:category :query-lifecycle)
                   (:synonym processed-queries)))
  (&rest args)
  (apply #'processed-queries args))

;;;
;;;
;;;

(nrql-defun (processed-rules 
             :doc ((:doc-type :long)
                   (:category :rule-lifecycle)
                   (:description "Returns a list of (the IDs of) all rules which satisfy
                   \\funref{rule-processed-p}")
                   (:syntax (processed-rules))
                   (:arguments
                    (:key abox (current-abox) "consider rules for substrates for ABox {\\tt abox} only")
                    (:key type-of-substrate  racer-dummy-substrate "consider rules for substrates of 
                   specified type only, one of:
                   \\argument{racer-dummy-substrate,
                   data-substrate, mirror-data-substrate, rcc-substrate,
                   racer-tbox-mirror-substrate}"))
                   (:see-also prepared-rules active-rules)))
  
  (&rest args)
  (apply #'map-over-queries-for-aboxes-of-type 
         #'iterator-id 
         (with-access-to-lifecycle-lists
           (copy-list *processed-rules*))
         args))

(nrql-defun (terminated-rules 
             :doc ((:doc-type :short)
                   (:category :rule-lifecycle)
                   (:synonym processed-rules)))
  (&rest args)
  (apply #'processed-rules args))


;;; 
;;; Cheap vs. Expensive Tuples 
;;; Praedikate
;;; 

(nrql-defmethod (cheap-query-p
                 :doc ((:doc-type :long)
                       (:category :execution-control)
                       (:description "Checks whether query 
                   \\argument{id} is still in \emph{phase one} (see
                                                                User Guide). Only active queries (see \\funref{query-active-p}) can be
                   recognized as cheap")
                       (:syntax (cheap-query-p id))
                       (:arguments 
                        (:first id
                         "the ID of the query, or {\\tt :last} or {\\tt :last-query}"))
                       (:values "{\\tt t} or {\\tt  nil}")
                       (:remarks "A query will only produce cheap
                   tuples if it has been started in two-phase query processing mode, see
                   \\funref{execute-query},
                   \\funref{enable-two-phase-query-processing-mode}.  Note also that
                   query must be executed in lazy tuple-at-a-time mode")
                       (:examples)
                       (:see-also cheap-queries expensive-query-p enable-two-phase-query-processing-mode
                        set-nrql-mode execute-query)))
  ((query null))
  :not-found)

(nrql-defmethod (cheap-query-p) ((query symbol))
  (cheap-query-p (find-query query)))

;;;
;;;
;;;

(nrql-defmethod (cheap-rule-p
                 :doc ((:doc-type :short)
                       (:category :execution-control)
                       (:rule-equivalent-of cheap-query-p)))
  ((query null))
  :not-found)

(nrql-defmethod (cheap-rule-p) ((query symbol))
  (cheap-rule-p (find-rule query)))

;;;
;;;
;;;


(nrql-defmethod (expensive-query-p
                 :doc ((:doc-type :short)
                       (:category :execution-control)
                       (:description "If an active query is not cheap
                   \\funref{cheap-query-p}, then it is expensive; thus, \emph{phase 2} has
                   started")))

  ((query null))

  :not-found)

(nrql-defmethod (expensive-query-p) ((query symbol))
  (expensive-query-p (find-query query)))

;;;
;;;
;;;

(nrql-defmethod (expensive-rule-p
                 :doc ((:doc-type :short)
                       (:category :execution-control)
                       (:rule-equivalent-of expensive-query-p)))

  ((query null))

  :not-found)

(nrql-defmethod (expensive-rule-p) ((query symbol))
  (expensive-rule-p (find-rule query)))

;;;
;;; Listenmethoden fuer soeben definierte Praedikate 
;;;

(nrql-defun (cheap-queries 
             :doc ((:doc-type :long)
                   (:category :execution-control)
                   (:syntax (cheap-queries))
                   (:arguments
                    (:key abox (current-abox) "consider queries for substrates for ABox {\\tt abox} only")
                    (:key type-of-substrate  racer-dummy-substrate "consider queries for substrates of 
                   specified type only, one of:
                   \\argument{racer-dummy-substrate,
                   data-substrate, mirror-data-substrate, rcc-substrate,
                   racer-tbox-mirror-substrate}"))
                   (:description "Returns a list of (the IDs of) the
                   queries that satisfy \\funref{cheap-query-p}. Note that this is a subset of the 
                   set of active queries (see \\funref{active-queries}")))

  (&rest args)

  (apply #'map-over-queries-for-aboxes-of-type 
         #'iterator-id 
         (remove-if-not #'cheap-query-p (with-access-to-lifecycle-lists
                                          (copy-list *active-queries*)))
         args))

(nrql-defun (expensive-queries 
             :doc ((:doc-type :long)
                   (:category :execution-control)
                   (:syntax (expensive-queries))
                   (:arguments
                    (:key abox (current-abox) "consider queries for substrates for ABox {\\tt abox} only")
                    (:key type-of-substrate  racer-dummy-substrate "consider queries for substrates of 
                                              specified type only, one of:
                                              \\argument{racer-dummy-substrate,
                                              data-substrate, mirror-data-substrate, rcc-substrate,
                                              racer-tbox-mirror-substrate}"))
                   (:description "Returns a list of (the IDs of) the
                                              queries that satisfy \\funref{expensive-query-p}. Note that this is a subset of the 
                                              set of active queries (see \\funref{active-queries}")))

  (&rest args)
  
  (apply #'map-over-queries-for-aboxes-of-type
         #'iterator-id
         (remove-if-not #'expensive-query-p (with-access-to-lifecycle-lists
                                              (copy-list *active-queries*)))
         args))

;;;
;;;
;;; 

(nrql-defun (cheap-rules 
             :doc ((:doc-type :short)
                   (:category :execution-control)
                   (:rule-equivalent-of cheap-queries)))
  (&rest args)
  (apply #'map-over-queries-for-aboxes-of-type
         #'iterator-id
         (remove-if-not #'cheap-rule-p (with-access-to-lifecycle-lists
                                         (copy-list *active-rules*)))
         args))

(nrql-defun (expensive-rules 
             :doc ((:doc-type :short)
                   (:category :execution-control)
                   (:rule-equivalent-of expensive-queries)))
  (&rest args)
  (apply #'map-over-queries-for-aboxes-of-type
         #'iterator-id 
         (remove-if-not #'expensive-rule-p (with-access-to-lifecycle-lists
                                             (copy-list *active-rules*)))
         args))

;;;
;;; Listenmethoden fuer komplexe Praedikate  
;;;

(nrql-defun (running-cheap-queries 
             :doc ((:doc-type :long)
                   (:category :execution-control)
                   (:syntax (running-cheap-queries))
                   (:arguments
                    (:key abox (current-abox) "consider queries for substrates for ABox {\\tt abox} only")
                    (:key type-of-substrate  racer-dummy-substrate "consider queries for substrates of 
                                                                         specified type only, one of:
                                                                         \\argument{racer-dummy-substrate,
                                                                         data-substrate, mirror-data-substrate, rcc-substrate,
                                                                         racer-tbox-mirror-substrate}"))
                   (:description "Returns a list of (the IDs of) the
                                                                         queries that satisfy \\funref{cheap-query-p} and
                                                                         \\funref{query-running-p}")))
  (&rest args)
  (apply #'map-over-queries-for-aboxes-of-type
         #'iterator-id
         (remove-if-not #'(lambda (x) 
                            (and (query-running-p x)
                                 (cheap-query-p x)))
			(with-access-to-lifecycle-lists
                          (copy-list *active-queries*)))
         args))

(nrql-defun (running-expensive-queries
             :doc ((:doc-type :long)
                   (:category :execution-control)
                   (:syntax (running-expensive-queries))
                   (:arguments
                    (:key abox (current-abox) "consider queries for substrates for ABox {\\tt abox} only")
                    (:key type-of-substrate  racer-dummy-substrate "consider queries for substrates of 
                                                                         specified type only, one of:
                                                                         \\argument{racer-dummy-substrate,
                                                                         data-substrate, mirror-data-substrate, rcc-substrate,
                                                                         racer-tbox-mirror-substrate}"))
                   (:description "Returns a list of (the IDs of) the
                                                                         queries that satisfy \\funref{expensive-query-p} and
                                                                         \\funref{query-running-p}")))
  (&rest args)
  (apply #'map-over-queries-for-aboxes-of-type
         #'iterator-id
         (remove-if-not #'(lambda (x) 
                            (and (query-running-p x)
                                 (expensive-query-p x)))
			(with-access-to-lifecycle-lists
                          (copy-list *active-queries*)))
         args))

;;;
;;;
;;;

(nrql-defun (running-cheap-rules 
             :doc ((:doc-type :short)
                   (:rule-equivalent-of running-cheap-rules)
                   (:category :execution-control)))
  (&rest args)
  (apply #'map-over-queries-for-aboxes-of-type
         #'iterator-id
         (remove-if-not #'(lambda (x) 
                            (and (rule-running-p x)
                                 (cheap-rule-p x)))
                        (with-access-to-lifecycle-lists
                          (copy-list *active-rules*)))
         args))

(nrql-defun (running-expensive-rules 
             :doc ((:doc-type :short)
                   (:rule-equivalent-of running-expensive-rules)
                   (:category :execution-control)))
  (&rest args)
  (apply #'map-over-queries-for-aboxes-of-type
         #'iterator-id
         (remove-if-not #'(lambda (x) 
                            (and (rule-running-p x)
                                 (expensive-rule-p x)))
			(with-access-to-lifecycle-lists
                          (copy-list *active-rules*)))
         args))

;;;
;;;
;;;

(nrql-defun (waiting-cheap-queries 
             :doc ((:doc-type :long)
                   (:category :execution-control)
                   (:syntax (waiting-cheap-queries))
                   (:arguments
                    (:key abox (current-abox) "consider queries for substrates for ABox {\\tt abox} only")
                    (:key type-of-substrate  racer-dummy-substrate "consider queries for substrates of 
                                                                         specified type only, one of:
                                                                         \\argument{racer-dummy-substrate,
                                                                         data-substrate, mirror-data-substrate, rcc-substrate,
                                                                         racer-tbox-mirror-substrate}"))
                   (:description "Returns a list of (the IDs of) the
                                                                         queries that satisfy \\funref{cheap-query-p} and
                                                                         \\funref{query-waiting-p}")))

  (&rest args)
             
  (apply #'map-over-queries-for-aboxes-of-type
         #'iterator-id
         (remove-if-not #'(lambda (x) 
                            (and (query-waiting-p x)
                                 (cheap-query-p x)))
                        (with-access-to-lifecycle-lists
                          (copy-list *active-queries*)))
         args))

(nrql-defun (waiting-expensive-queries 
             :doc ((:doc-type :long)
                   (:category :execution-control)
                   (:syntax (waiting-expensive-queries))
                   (:arguments
                    (:key abox (current-abox) "consider queries for substrates for ABox {\\tt abox} only")
                    (:key type-of-substrate  racer-dummy-substrate "consider queries for substrates of 
                                                                         specified type only, one of:
                                                                         \\argument{racer-dummy-substrate,
                                                                         data-substrate, mirror-data-substrate, rcc-substrate,
                                                                         racer-tbox-mirror-substrate}"))
                   (:description "Returns a list of (the IDs of) the
                                                                         queries that satisfy \\funref{expensive-query-p} and
                                                                         \\funref{query-waiting-p}")))
  (&rest args)
  (apply #'map-over-queries-for-aboxes-of-type
         #'iterator-id
         (remove-if-not #'(lambda (x) 
                            (and (query-waiting-p x)
                                 (expensive-query-p x)))
			(with-access-to-lifecycle-lists
                          (copy-list *active-queries*)))
         args))

(nrql-defun (sleeping-cheap-queries 
             :doc ((:doc-type :short)
                   (:category :execution-control)
                   (:synonym waiting-cheap-queries)))
  (&rest args)
  (apply #'waiting-cheap-queries args))


(nrql-defun (sleeping-expensive-queries
             :doc ((:doc-type :short)
                   (:category :execution-control)
                   (:synonym waiting-expensive-queries)))
  (&rest args)
  (apply #'waiting-expensive-queries args))

;;;
;;;
;;;

(nrql-defun (waiting-cheap-rules 
             :doc ((:doc-type :long)
                   (:category :execution-control)
                   (:rule-equivalent-of waiting-cheap-queries)
                   (:syntax (waiting-cheap-rules))
                   (:arguments
                    (:key abox (current-abox) "consider rules for substrates for ABox {\\tt abox} only")
                    (:key type-of-substrate  racer-dummy-substrate "consider rules for substrates of 
                                                                         specified type only, one of:
                                                                         \\argument{racer-dummy-substrate,
                                                                         data-substrate, mirror-data-substrate, rcc-substrate,
                                                                         racer-tbox-mirror-substrate}"))                   
                   (:description "Returns a list of (the IDs of) the
                                                                         rules that satisfy \\funref{cheap-rule-p} and
                                                                         \\funref{rule-waiting-p}")))
  (&rest args)
  (apply #'map-over-queries-for-aboxes-of-type
         #'iterator-id
         (remove-if-not #'(lambda (x) 
                            (and (rule-waiting-p x)
                                 (cheap-rule-p x)))
                        (with-access-to-lifecycle-lists
                          (copy-list *active-rules*)))
         args))


(nrql-defun (waiting-expensive-rules 
             :doc ((:doc-type :long)
                   (:category :execution-control)
                   (:rule-equivalent-of waiting-expensive-queries)
                   (:syntax (waiting-expensive-rules))
                   (:arguments
                    (:key abox (current-abox) "consider rules for substrates for ABox {\\tt abox} only")
                    (:key type-of-substrate  racer-dummy-substrate "consider rules for substrates of 
                                                                         specified type only, one of:
                                                                         \\argument{racer-dummy-substrate,
                                                                         data-substrate, mirror-data-substrate, rcc-substrate,
                                                                         racer-tbox-mirror-substrate}"))
                   (:description "Returns a list of (the IDs of) the
                                                                         rules that satisfy \\funref{expensive-rule-p} and
                                                                         \\funref{rule-waiting-p}")))

  (&rest args)
  (apply #'map-over-queries-for-aboxes-of-type
         #'iterator-id
         (remove-if-not #'(lambda (x) 
                            (and (rule-waiting-p x)
                                 (expensive-rule-p x)))
                        (with-access-to-lifecycle-lists
                          (copy-list *active-rules*)))
         args))


(nrql-defun (sleeping-cheap-rules 
             :doc ((:doc-type :short)
                   (:category :execution-control)
                   (:synonym waiting-cheap-rules)))
  (&rest args)
  (apply #'waiting-cheap-rules args))


(nrql-defun (sleeping-expensive-rules
             :doc ((:doc-type :short)
                   (:category :execution-control)
                   (:synonym waiting-expensive-rules)))
  (&rest args)
  (apply #'waiting-expensive-rules args))

;;;
;;; Accurate, Inaccurate 
;;;  

;;;
;;; Praedikate 
;;; 

(nrql-defmethod (query-accurate-p
                 :doc ((:doc-type :long)
                       (:category :execution-control)
                       (:description "Determines whether the computed
                                                                         and stored answer of a processed query \\funref{query-processed-p} is 
                                                                         still accurate. The answer resp. processed query is called  
                                                                         \emph{accurate} iff the queried KB (TBox, ABox) has not changed
                                                                         since the query was executed. Thus, the answers of an accuarte query
                                                                         must not be recomputed. Inaccurate query answers can be reecomputed
                                                                         see \\funref{reexecute-query}")
                       (:syntax (query-accurate-p id))
                       (:arguments 
                        (:first id
                         "the ID of the query, or {\\tt
                                                                         :last} or {\\tt :last-query}"))
                        
                       (:values "{\\tt t} or {\\tt nil}")
                       (:remarks)
                       (:examples)
                       (:see-also accurate-queries)))
  ((query null))

  :not-found)

(nrql-defmethod (query-accurate-p)
  ((query symbol))
  (query-accurate-p (find-query query *processed-queries*)))

;;;
;;;
;;;

(nrql-defmethod (rule-accurate-p
                 :doc ((:doc-type :short)
                       (:category :execution-control)
                       (:rule-equivalent-of query-accurate-p)))
  ((query null))

  :not-found)

(nrql-defmethod (rule-accurate-p) ((query symbol))
  (rule-accurate-p (find-rule query *processed-rules*)))

(nrql-defmethod (rule-accurate-p) ((query query))
  (query-accurate-p query))

;;;
;;; Listenmethoden 
;;;

(nrql-defun (accurate-queries 
             :doc ((:doc-type :long)
                   (:category :execution-control)
                   (:syntax (accurate-queries))
                   (:arguments
                    (:key abox (current-abox) "consider queries for substrates for ABox {\\tt abox} only")
                    (:key type-of-substrate  racer-dummy-substrate "consider queries for substrates of 
                                                                         specified type only, one of:
                                                                         \\argument{racer-dummy-substrate,
                                                                         data-substrate, mirror-data-substrate, rcc-substrate,
                                                                         racer-tbox-mirror-substrate}"))
                   (:description "Returns a list of (the IDs of) the
                                                                         queries that satisfy
                                                                         \\funref{query-accurate-p}. Note that this is a subset of the set of
                                                                         processed queries (see \\funref{processed-queries}")))
  (&rest args)
  (apply #'map-over-queries-for-aboxes-of-type
         #'iterator-id
         (remove-if-not #'query-accurate-p 
			(with-access-to-lifecycle-lists
                          (copy-list *processed-queries*)))
         args))


(nrql-defun (inaccurate-queries
             :doc ((:doc-type :long)
                   (:category :execution-control)
                   (:syntax (inaccurate-queries))
                   (:arguments
                    (:key abox (current-abox) "consider queries for substrates for ABox {\\tt abox} only")
                    (:key type-of-substrate  racer-dummy-substrate "consider queries for substrates of 
                                                                                                specified type only, one of:
                                                                                                \\argument{racer-dummy-substrate,
                                                                                                data-substrate, mirror-data-substrate, rcc-substrate,
                                                                                                racer-tbox-mirror-substrate}"))
                   (:description "Returns a list of (the IDs of) the
                                                                                                queries that do not satisfy
                                                                                                \\funref{query-accurate-p}. Note that this is a subset of the set of
                                                                                                processed queries (see \\funref{processed-queries}")))
  (&rest args)
  (apply #'map-over-queries-for-aboxes-of-type
         #'iterator-id 
         (remove-if #'query-accurate-p 
		    (with-access-to-lifecycle-lists
                      (copy-list *processed-queries*)))
         args))

;;;
;;;
;;; 

(nrql-defun (accurate-rules 
             :doc ((:doc-type :short)
                   (:category :execution-control)
                   (:rule-equivalent-of accurate-queries)))
  (&rest args)
  (apply #'map-over-queries-for-aboxes-of-type
         #'iterator-id
         (remove-if-not #'rule-accurate-p 
			(with-access-to-lifecycle-lists
                          (copy-list *processed-rules*)))
         args))

(nrql-defun (inaccurate-rules 
             :doc ((:doc-type :short)
                   (:category :execution-control)
                   (:rule-equivalent-of inaccurate-queries)))
  (&rest args)
  (apply #'map-over-queries-for-aboxes-of-type
         #'iterator-id
         (remove-if #'rule-accurate-p 
		    (with-access-to-lifecycle-lists
                      (copy-list *processed-rules*)))
         args))


;;;
;;; Defined Queries
;;; 

(nrql-defun (define-query 

             :doc ((:doc-type :long)

                   (:category :defined-queries)

                   (:description "Associates a query head and body
                                                                                                                       with a name which is the name of the
                                                                                                                       definition.  This defined query can be reused by means of
                                                                                                                       \code{substitute} query atoms. The definitions are local to
                                                                                                                       \argument{tbox}")

                   (:syntax (defquery name head body &key keep-p tbox))

                   (:arguments 
                    (:first name "the name of the definition, see {\\tt
                                                                                                                       <query-name>}, Section 6.1.8 in the User Guide")
                    (:second head "the head of the query, see {\\tt
                                                                                                                       <def-query-head>}, Section 6.1.8 in the User Guide.  Projection operators are
                                                                                                                       not allowed here")
                    (:third body "the body of the query, see {\\tt
                                                                                                                       <query-body>}, Section 6.1.8 in the User Guide")
                    
                    (:key tbox (current-tbox) "the TBox to which this
                                                                                                                       definition is local")
                    
                    (:key keep-p nil "The query is prepared (and thus
                                                                 parsed) for syntax checking purposes, but the compiled query is
                                                                                                                       discarded. The definition is registered and \\argument{name} returned.
                                                                                                                       However, if {\\tt t} is specified for this argument, then the prepared
                                                                                                                       query is not discarded and returned instead of \\argument{name}. Since
                                                                                                                       \\funref{racer-prepare-query} is called and the arguments \\argument{args}
                                                                                                                       are passed through, the query can automatically be executed (see
                                                                                                                                                                                    \\argument{execute-p} in \\funref{racer-prepare-query})"))

                   (:values "The \\argument{name} of the defined
                                                                                                                       query if {\\tt keep-p} = {\\tt nil}, and the result of
                                                                                                                       \\funref{racer-prepare-query} otherwise")

                   (:remarks "The \argument{body} can reference other
                                                                                                                       defined queries as well, but cyclic definitions are not possible. Note
                                                                                                                       that defined queries can also be used in a rule antecedence")

                   (:examples
                    (define-query 'is-a-mother '(?x) '(and (?x woman) (?x ?y has-child)))
                    (define-query '(?a) '(substitute (is-a-mother ?a)))
                    (retrieve (?a) (?a is-a-mother)))
                    
                   (:see-also )))

  (name head body &rest args 
        &key keep-p (tbox (or *nrql-tbox* (current-tbox)))
        &allow-other-keys)

  (declare (ignorable keep-p))

  (apply #'define-query1 name head body :tbox tbox args))

(nrql-defun (define-and-execute-query
             :doc ((:doc-type :short)
                   (:category :defined-queries)
                   (:description "Like \\funref{define-query} 
                                                                                                                       with {\\tt keep-p} = {\\tt t} and {\\tt execute-p} = {\\tt t}")))
  
  (name head body &rest args)
  
  (apply #'define-query name head body :execute-p t :keep-p t args))

(nrql-defun (define-and-prepare-query 
             :doc ((:doc-type :short)
                   (:category :defined-queries)
                   (:description "Like \\funref{define-query} with
                                                                                                                       {\\tt keep-p} = {\\tt t} and {\\tt execute-p} = {\\tt nil}")))

  (name head body &rest args)

  (apply #'define-query name head body :keep-p t (append args (list :execute-p nil))))

(nrql-defun (undefine-query
             :doc ((:doc-type :long)
                   (:category :defined-queries)
                   (:description "Deletes a defined query")
                   (:syntax (undefine-query name &key tbox))
                   (:arguments 
                    (:first name "the name of the definition, see {\\tt
                                                                                                                       <query-name>}, Section 6.1.8 in the User Guide")
                    (:key tbox (current-tbox) "the TBox to which this
                                                                                                                       definition is local")
                    (:key arity nil "the arity of the defined query to be delete; {\tt nil} deletes all 
                                                                                                                       queries with that \\argument{name}"))
                   (:values "The names of the remaining definitions (local to \argument{tbox})")
                   (:remarks)
                   (:examples (undefine-query 'is-a-mother))
                   (:see-also define-query)))

  (name &key (tbox (or *nrql-tbox* (current-tbox))) arity)
  
  (undefine-query1 name arity :tbox tbox))

;;;
;;;
;;;

(nrql-defun (allow-overloaded-definitions 
             :doc ((:doc-type :short)
                   (:category :defined-queries)
                   (:inverse-of dont-allow-overloaded-definitions)
                   (:description "Allow multiple defined queries with same name and arity")))
  ()
  
  (setf *allow-multiple-definitions-p* t)

  :okay-allowing-overloaded-definitions)


(nrql-defun (dont-allow-overloaded-definitions 
             :doc ((:doc-type :short)
                   (:category :defined-queries)
                   (:inverse-of allow-overloaded-definitions)
                   (:description "Dont allow multiple defined queries with same name and arity")))
  ()
  
  (setf *allow-multiple-definitions-p* nil)
  
  :okay-not-allowing-overloaded-definitions)

;;;
;;;
;;;

(nrql-defun (prefer-defined-queries
             :doc ((:doc-type :short)
                   (:category :defined-queries)
                   (:inverse-of dont-prefer-defined-queries)
                   (:description "If a unary (binary) query is
                                                                                                                       referenced in a body and there are corresponding concept (role) with
                                                                                                                       the same name as the defined query, then the ambiguity is resolved in
                                                                                                                       favor of the defined query")))
  ()
  
  (setf *always-prefer-defined-queries-p* t)
  
  :okay-prefering-defined-queries)


(nrql-defun (dont-prefer-defined-queries 
             :doc ((:doc-type :short)
                   (:category :defined-queries)
                   (:inverse-of prefer-defined-queries)
                   (:description "If a unary (binary) query is
                                                                                                                       referenced in a body and there are corresponding concept (role) with
                                                                                                                       the same name as the defined query, then the ambiguity is resolved in
                                                                                                                       favor of the concept (role)")))
  ()
  
  (setf *always-prefer-defined-queries-p* nil)

  :okay-not-prefering-defined-queries)

;;;
;;;
;;;

(nrql-defun (disable-defined-queries
             :doc ((:doc-type :short)
                   (:category :defined-queries)
                   (:inverse-of enable-defined-queries)
                   (:description "Disables the defined queries, but keeps the definitions.")))
  ()
  
  (setf *disable-defined-queries-p* t)
  
  :okay-defined-queries-disabled)


(nrql-defun (enable-defined-queries 
             :doc ((:doc-type :short)
                   (:category :defined-queries)
                   (:inverse-of disable-defined-queries)
                   (:description "Enables the defined queries.")))
  ()
  
  (setf *disable-defined-queries-p* nil)

  :okay-defined-queries-enabled)

;;;
;;;
;;;


(nrql-defun (disable-lazy-unfolding-of-defined-queries
             :doc ((:doc-type :short)
                   (:category :defined-queries)
                   (:inverse-of enable-lazy-unfolding-of-defined-queries)
                   (:description "Disables the lazy unfolding defined queries.")))
  ()
  
  (setf *lazy-query-unfolding-p* nil)
  
  :okay-lazy-unfolding-of-defined-queries-disabled)


(nrql-defun (enable-lazy-unfolding-of-defined-queries
             :doc ((:doc-type :short)
                   (:category :defined-queries)
                   (:inverse-of disable-lazy-unfolding-of-defined-queries)
                   (:description "Enables the lazy unfolding defined queries.")))
  ()
  
  (setf *lazy-query-unfolding-p* t)
  
  :okay-lazy-unfolding-of-defined-queries-enabled)

;;;
;;;
;;;

(nrql-defun (keep-defined-query-atoms
             :doc ((:doc-type :short)
                   (:category :defined-queries)
                   (:inverse-of dont-keep-defined-query-atoms
                    (:description "A defined query atom is usually
simply replaced / expanded by its definition (which was defined by
\"defquery\"). Using this switch, the defined query atom is preseved
in the expanded query body"))))
  ()
  
  (setf *keep-expanded-predicates-p* t)
  
  :okay-keeping-defined-query-atoms)


(nrql-defun (dont-keep-defined-query-atoms
             :doc ((:doc-type :short)
                   (:category :defined-queries)
                   (:inverse-of keep-defined-query-atoms)))
  ()
  
  (setf *keep-expanded-predicates-p* nil)
  
  :okay-not-keeping-defined-query-atoms)

;;;
;;;
;;; 

(nrql-defun (deactivate-defined-query
             :doc ((:doc-type :short)
                   (:category :defined-queries)
                   (:inverse-of activate-defined-query
                    (:description "A defined query with head name and arity arity can disabled. 
It will not be seen by the query expander until it is enabled again."))))
  (name arity &rest args)
  
  
  (apply #'deactivate-defined-query1 name arity args)

  :okay)



(nrql-defun (activate-defined-query
             :doc ((:doc-type :short)
                   (:category :defined-queries)
                   (:inverse-of deactivate-defined-query
                    (:description "Re-enables a previously deactivated defined query. 
It will be seen again by the query expander until it is disabled again."))))
  (name arity &rest args)
  
  
  (apply #'activate-defined-query1 name arity args)

  :okay)


;;;
;;;
;;; 


(nrql-defun (delete-all-definitions
             :doc ((:doc-type :long)
                   (:category :defined-queries)
                   (:description "Deletes all defined queries local to \\argument{tbox}")
                   (:syntax (delete-all-definitions &key tbox))
                   (:arguments 
                    (:key tbox (current-tbox) "the TBox to which this
                                                                                                                       definition is local"))
                   (:values "{\\tt :okay-all-definitions-deleted}")
                   (:remarks)
                   (:examples)
                   (:see-also undefine-query)))

  (&key (tbox (or *nrql-tbox* (current-tbox))))
  
  (let ((dbox (find-dbox tbox)))
    (when dbox 
      (delete-all-definitions1 dbox)
      :okay-all-definitions-deleted)))

(nrql-defun (describe-definition 
             :doc ((:doc-type :long)
                   (:category :defined-queries)
                   (:description "Describes the definition
                                                                                                                       \\argument{name} local to \\argument{tbox}")
                   (:syntax (describe-definition name &key tbox tbox))
                   (:arguments 
                    (:first name "the name of the definition")
                    (:key arity nil "the arity of the requested definition")
                    (:key tbox (current-tbox) "the TBox to which this
                                                                                                                       definition is local"))
                   (:values "A defquery expression")
                   (:remarks)
                   (:examples)
                   (:see-also define-query)))

  (name &key (tbox (or *nrql-tbox* (current-tbox))) arity (error-p t))

  (let ((dbox (find-dbox tbox :error-p error-p)))
    (when dbox
      (describe-definition1 dbox name arity :error-p error-p))))

(nrql-defun (describe-all-definitions
             :doc ((:doc-type :long)
                   (:category :defined-queries)
                   (:description "Returns a list containing all
                                                                                                                       definitions (see \\funref{describe-definition}) local to
                                                                                                                       \\argument{tbox}")
                   (:syntax (describe-all-definitions &tbox tbox))
                   (:arguments 
                    (:key tbox (current-tbox) "the TBox whose
                                                                                                                       definitions are to be described"))
                   (:values "A list containing all definitions local to that TBox")
                   (:remarks)
                   (:examples)
                   (:see-also )))

  (&key (tbox (or *nrql-tbox* (current-tbox))) (error-p t))
 
  (let ((dbox (find-dbox tbox :error-p error-p)))
    (when dbox
      (describe-all-definitions1 dbox))))

;;;
;;;
;;;

(nrql-defmacro (defquery :nrql-function define-query
                         :doc ((:doc-type :short)
                               (:category :defined-queries))))

(nrql-defmacro (def-and-prep-query :nrql-function define-and-prepare-query
                                   :doc ((:doc-type :short)
                                         (:category :defined-queries))))

(nrql-defmacro (def-and-exec-query :nrql-function define-and-execute-query
                                   :doc ((:doc-type :short)
                                         (:category :defined-queries))))

(nrql-defmacro (undefquery :nrql-function undefine-query
                           :doc ((:doc-type :short)
                                 (:category :defined-queries))))

;;;
;;; Lifecycle: Reprepare, Reexecute etc. 
;;; 

(nrql-defmethod (reprepare-query
                 :doc ((:doc-type :long)
                       (:category :query-lifecycle)
                       (:description "Reprepares an already processed
                                                                                                                       query (see \\funref{query-processed-p}) and makes it ready for
                                                                                                                       execution (via \\funref{execute-query}) again (see 
                                                                                                                                                                      \\funref{query-ready-p})")
                       (:syntax (reprepare-query id))
                       (:arguments
                        (:first id "the ID of the query, or {\\tt :last} or {\\tt :last-query}"))
                        
                       (:values "The result of \\funref{racer-prepare-query}")
                       (:remarks "Note that the query cannot be altered")
                       (:examples)
                       (:see-also )))
  ((query null) &rest args)

  (declare (ignorable args))

  :not-found)

(nrql-defmethod (reprepare-query) ((query t) &rest args)
  (with-racer-timeout
    (multiple-value-bind (first second)
        (apply #'reprepare-query (find-query query *processed-queries*) args)
      (declare (ignorable second))
      first)))

;;;
;;;
;;;

(nrql-defmethod (reprepare-rule
                 :doc ((:doc-type :short)
                       (:category :rule-lifecycle)
                       (:rule-equivalent-of reprepare-query)))
  ((query null) &rest args)
  (declare (ignorable args))
  :not-found)

(nrql-defmethod (reprepare-rule)
  ((query t) &rest args)
  (declare (ignorable args))
  (with-racer-timeout
    (multiple-value-bind (first second)
        (apply #'reprepare-rule (find-rule query *processed-rules*) args)
      (declare (ignorable second))
      first)))

;;;
;;;
;;;

(nrql-defmethod (reexecute-query
                 :doc ((:doc-type :long)
                       (:category :query-lifecycle)
                       (:description "Reprepares and executes an
                                                                                                                       already processed query (see \\funref{query-processed-p})")
                       (:syntax (reexecute-query id &rest args))
                       (:arguments
                        (:first id "the ID of the query, or {\\tt :last} or {\\tt :last-query}")
                        (:reference args execute-query))
                        
                       (:values "The result of \\funref{execute-query}")
                       (:remarks "Note that the query cannot be altered, but can be executed
                                                                                                                       using different settings, since \\argument{args} are passed to \\funref{execute-query}")
                       (:examples)
                       (:see-also reprepare-query)))

  ((query null) &rest args)
  (declare (ignorable args))
  :not-found)

(nrql-defmethod (reexecute-query)
  ((query t) &rest args)
  (with-racer-timeout
    (apply #'eval-nrql-settings2
           #'(lambda ()
               (apply #'reexecute-query (find-query query *processed-queries*) args))
           :allow-other-keys t
           args)))

;;;
;;;
;;;


(nrql-defmethod (reexecute-rule
                 :doc ((:doc-type :short)
                       (:category :rule-lifecycle)
                       (:rule-equivalent-of reexecute-query)))
  ((query null) &rest args)
  (declare (ignorable args))
  :not-found)

(nrql-defmethod (reexecute-rule) ((query t) &rest args)
  (with-racer-timeout
    (apply #'eval-nrql-settings2
           #'(lambda ()
               (apply #'reexecute-rule (find-rule query *processed-rules*) args))
           :allow-other-keys t
           args)))

;;;
;;; 
;;;

(nrql-defmethod (execute-or-reexecute-query 
                 :doc ((:doc-type :short)
                       (:category :execution-control) 
                       (:description "Like \\funref{execute-query} in case the
                                                                                                                       query is already prepared (ready, see \\funref{query-ready-p}), and like 
                                                                                                                       \\funref{reexecute-query} in case the query is already processed (see \\funref{query-processed-p})")))

  ((query null) &rest args)
  (declare (ignorable args))
  :not-found)

(nrql-defmethod (execute-or-reexecute-query)
  ((query t) &rest args)
  (with-racer-timeout
    (apply #'eval-nrql-settings2
           #'(lambda ()
               (let ((res (apply #'execute-query query args)))
                 (if (eq res :not-found)
                     (apply #'reexecute-query 
                            (find-query query *processed-queries*)
                            args)
                   res)))
           :allow-other-keys t
           args)))

;;;
;;;
;;;

(nrql-defmethod (execute-or-reexecute-rule
                 :doc ((:doc-type :short)
                       (:category :execution-control)
                       (:rule-equivalent-of execute-or-reexecute-query)))
  ((query null) &rest args)
  (declare (ignorable args))
  :not-found)

(nrql-defmethod (execute-or-reexecute-rule) ((query t) &rest args)
  (with-racer-timeout
    (apply #'eval-nrql-settings2
           #'(lambda ()
               (let ((res (apply #'execute-rule query args)))
                 (if (eq res :not-found)
                     (apply #'reexecute-rule 
                            (find-rule query *processed-rules*)
                            args)
                   res)))
           :allow-other-keys t
           args)))


;;;
;;; Rule Functions 
;;; 

(nrql-defmethod (rule-applicable-p
                 :doc ((:doc-type :long)
                       (:category :rules)
                       (:description "Checks whether rule \\argument{id} is applicable, i.e. 
                                                                                                                       its antecedence is true.  Thus, its consequence
                                                                                                                       might produce new ABox assertions (or delete existing ABox
                                                                                                                                                             assertions)")
                       

                       (:syntax (rule-applicable-p id))
                       (:arguments 
                        (:first id "the ID of the rule, or {\\tt :last} or {\\tt :last-rule}"))
                       (:values 
                        "{\\tt t} or {\\tt nil} (or {\\tt :not-found})")
                       (:remarks "A rule can only be applicable if it is
                                                                                                                       either ready (see \\funref{rule-ready-p}) or processed (see
                                                                                                                                                                               \\funref{rule-processed-p}). Rules which are
                                                                                                                       already active (see \\funref{rule-active-p}) are not applicable. 

                                                                                                                       If an already processed rule is found to be applicable, then it is
                                                                                                                       also automatically reprepared, see \funref{reprepare-rule} so it can
                                                                                                                       immediately be fired again (see \\funref{execute-rule})")

                       (:examples)
                       (:see-also rule-unapplicable-p applicable-rules unapplicable-rules execute-rule)))
  ((query null) &rest args)
  (declare (ignorable args))
  ;;; args nicht nach aussen geben! 
  :not-found)

(nrql-defmethod (rule-applicable-p) ((query symbol) &rest args)
  (declare (ignorable args))
  ;;; args nicht nach aussen geben! 
  (with-racer-timeout
    (rule-applicable-p (find-rule query))))

;;;
;;;
;;;

(nrql-defmethod (rule-unapplicable-p
                 :doc ((:doc-type :short)
                       (:category :execution-control)
                       (:description "Negation of \\funref{rule-applicable-p}")))
  ((query t) &rest args)
  (let ((res 
         (apply #'rule-applicable-p query args)))
    (case res
      (:not-found res)
      (otherwise (not res)))))

;;;
;;;
;;;

(defun applicable-rules1 (&key abox (type-of-substrate *type-of-substrate*) &allow-other-keys)
  (nconc 
   (remove-if-not #'(lambda (x) 
                      (and (or (not abox)
                               (eq (abox (substrate x)) abox)
                               (eq (type-of (substrate x)) type-of-substrate))
                           (rule-applicable-p x)))
                  *ready-rules*)
   (remove-if-not #'(lambda (x) 
                      (and (or (not abox)
                               (eq (abox (substrate x)) abox)
                               (eq (type-of (substrate x)) type-of-substrate))
                           (rule-applicable-p x)))
                  *processed-rules*)))

(defun unapplicable-rules1 (&key abox (type-of-substrate *type-of-substrate*) &allow-other-keys)
  (nconc 
   (remove-if-not #'(lambda (x) 
                      (and (or (not abox)
                               (eq (abox (substrate x)) abox)
                               (eq (type-of (substrate x)) type-of-substrate))
                           (rule-unapplicable-p x)))
                  *ready-rules*)
   (remove-if-not #'(lambda (x) 
                      (and (or (not abox)
                               (eq (abox (substrate x)) abox)
                               (eq (type-of (substrate x)) type-of-substrate))
                           (rule-unapplicable-p x)))
                  *processed-rules*)))


(nrql-defmethod (applicable-rules
                 :doc ((:doc-type :short)
                       (:category :execution-control)
                       (:arguments
                        (:key abox (current-abox) "consider rules for substrates for ABox {\\tt abox} only")
                        (:key type-of-substrate  racer-dummy-substrate "consider rules for substrates of 
                                                                                                                       specified type only, one of:
                                                                                                                       \\argument{racer-dummy-substrate,
                                                                                                                       data-substrate, mirror-data-substrate, rcc-substrate,
                                                                                                                       racer-tbox-mirror-substrate}"))
                       (:description "Returns (a list of) all rules which satisfy \\funref{rule-applicable-p}")))

  (&rest args)
  (with-racer-timeout
    (mapcar #'iterator-id (apply #'applicable-rules1 args))))

(nrql-defmethod (unapplicable-rules 
                 :doc ((:doc-type :short)
                       (:category :execution-control)
                       (:arguments
                        (:key abox (current-abox) "consider rules for substrates for ABox {\\tt abox} only")
                        (:key type-of-substrate  racer-dummy-substrate "consider rules for substrates of 
                                                                                                                       specified type only, one of:
                                                                                                                       \\argument{racer-dummy-substrate,
                                                                                                                       data-substrate, mirror-data-substrate, rcc-substrate,
                                                                                                                       racer-tbox-mirror-substrate}"))
                       (:description "Returns (a list of) all rules which satisfy \\funref{rule-unapplicable-p}")))
  (&rest args)
  (with-racer-timeout
    (mapcar #'iterator-id (apply #'unapplicable-rules1 args))))


;;;
;;;
;;;

(nrql-defmethod (execute-applicable-rules
                 :doc ((:doc-type :long)
                       (:category :execution-control)
                       (:description "Calls \\funref{execute-rule} on all \\funref{applicable-rules}")
                       (:syntax (execute-applicable-rules &rest args))
                       (:arguments 
                        (:key abox (current-abox) "consider rules for substrates for ABox {\\tt abox} only")
                        (:key type-of-substrate  racer-dummy-substrate "consider rules for substrates of 
                                                                                                                       specified type only, one of:
                                                                                                                       \\argument{racer-dummy-substrate,
                                                                                                                       data-substrate, mirror-data-substrate, rcc-substrate,
                                                                                                                       racer-tbox-mirror-substrate}")
                        (:reference args execute-rule))
                       (:values "The list of results returned by \\funref{execute-rule} on 
                                                                                                                       the \\funref{applicable-rules}")
                       (:remarks)
                       (:examples)
                       (:see-also execute-rule applicable-rules rule-applicable-p)))
                       
  (&rest args)

  (with-racer-timeout
    (mapcar #'(lambda (x)
                (apply #'execute-rule x args))
            (apply #'applicable-rules1 args))))


;;;
;;;  
;;;

(nrql-defmethod (choose-current-set-of-rule-consequences
                 :doc ((:doc-type :long)
                       (:category :rules)
                       (:description "Consequences of a rule are not
                                                                                                                       added to the ABox as long as the rule is
                                                                                                                       still active (see \\funref{rule-active-p}). Some of the generated consequences 
                                                                                                                       are added when the rule has terminated. 
  
                                                                                                                       If a rule is executed (see \\funref{execute-rule}) in tuple-at-a time
                                                                                                                       mode, then rule consequences are requested and computed lazily via
                                                                                                                       \\funref{get-next-set-of-rule-consequences}.  The \\emph{current set
                                                                                                                       of rule consequences,} see
                                                                                                                       \funref{get-current-set-of-rule-consequences}, can thus be selected
                                                                                                                       and memoized in the rule object for addition to the ABox
                                                                                                                       (after
                                                                                                                        termination of the rule) with this function. Rule consequences are
                                                                                                                       added using the function
                                                                                                                       \\funref{add-chosen-sets-of-rule-consequences}.  Note that in
                                                                                                                       set-at-time-mode, all produced sets of consequences are chosen
                                                                                                                       automatically for addition.
  
                                                                                                                       Using either \\funref{add-rule-consequences-automatically}
                                                                                                                       (\\funref{dont-add-rule-consequences-automatically}) resp.  the
                                                                                                                       \\argument{add-rule-consequences-p} argument of
                                                                                                                       \\funref{execute-rule}, you can determine whether (the selected) rule
                                                                                                                       consequences will be added automatically with
                                                                                                                       \\funref{add-chosen-sets-of-rule-consequences} when the rule
                                                                                                                       terminates or not. In the latter case, 
                                                                                                                       \\funref{add-chosen-sets-of-rule-consequences} can be called 
                                                                                                                       manually later (see also \\funref{get-chosen-sets-of-rule-consequences})")
                       (:syntax (choose-current-set-of-rule-consequences id))
                       (:arguments 
                        (:first id "the ID of the rule, or {\\tt :last} or {\\tt :last-rule}"))
                       (:values "The current set of rule consequences (a list of ABox assertions)")
                       (:remarks)
                       (:examples)
                       (:see-also
                        add-chosen-sets-of-rule-consequences
                        get-chosen-sets-of-rule-consequences)))
  ((query null) &rest args)
  (declare (ignorable args))
  :not-found)

(nrql-defmethod (choose-current-set-of-rule-consequences)
  ((query symbol) &rest args)
  (declare (ignorable args))
  (choose-current-set-of-rule-consequences (find-rule query)))

;;;
;;;
;;;

(nrql-defmethod (get-chosen-sets-of-rule-consequences 
                 :doc ((:doc-type :long)
                       (:category :rules)
                       (:description "Returns the chosen (selected) sets
                                                                                                                       of rule consequences of the rule \\argument{id}. These assertions will be added
                                                                                                                       to the ABox if \\funref{add-chosen-sets-of-rule-consequences} is called on that rule")
                       (:syntax (get-chosen-sets-of-rule-consequences id))
                       (:arguments 
                        (:first id "the ID of the rule, or {\\tt :last} or {\\tt :last-rule}"))
                       (:values "the chosen sets of ABox assertions to be added")
                       (:remarks)
                       (:examples)
                       (:see-also choose-current-set-of-rule-consequences add-chosen-sets-of-rule-consequences)))
  ((query null) &rest args)
  (declare (ignorable args))
  :not-found)

(nrql-defmethod (get-chosen-sets-of-rule-consequences)
  ((query symbol) &rest args)
  (declare (ignorable args))
  (get-chosen-sets-of-rule-consequences (find-rule query)))

;;;
;;;
;;;

(nrql-defmethod (add-chosen-sets-of-rule-consequences 
                 :doc ((:doc-type :long)
                       (:category :rules)
                       (:description "Adds the chosen sets of rule
                                                                                                                       consequences (see \\funref{get-current-set-of-rule-consequences},
                                                                                                                                         \\funref{add-chosen-sets-of-rule-consequences}) to the ABox (this is
                                                                                                                                                                                                           the ABox on which the rule has fired) produced by the rule
                                                                                                                       \\argument{id}. See \\funref{get-chosen-sets-of-rule-consequences} to
                                                                                                                       learn which assertions will be added")
                       (:syntax (add-chosen-sets-of-rule-consequences id))
                       (:arguments 
			(:first id "the ID of the rule, or {\\tt :last} or {\\tt :last-rule}")
			(:key dont-add-abox-duplicates nil "if {\\tt t}, prevents addition of ABox duplicates"))
		       (:values "The added rule consequences resp. ABox assertions")
		       (:remarks "This function can only be called if
                                                                                                                       the rule has terminated (see \\funref{rule-processed-p}). Note that an
                                                                                                                       active rule (see \\funref{rule-active-p}) can be aborted (see
                                                                                                                                                                                 \\funref{abort-rule})")
                       (:examples)
                       (:see-also get-chosen-sets-of-rule-consequences 
                        get-current-set-of-rule-consequences
                        choose-current-set-of-rule-consequences)))
  ((query null) &rest args)
  (declare (ignorable args))
  :not-found)

(nrql-defmethod (add-chosen-sets-of-rule-consequences) ((query symbol) &rest args)
  (declare (ignorable args))
  (let ((bad-rules 
         (if *disable-deadlock-checking* 
             nil
           (remove-if #'proactive-tuple-computation-p *active-rules*))))
    (if bad-rules 
        (progn 
          (rule-deadlock-warning bad-rules)
          :denied-due-to-deadlock-prevention)
      (let ((bad-queries
             (if *disable-deadlock-checking* 
                 nil         
               (remove-if #'(lambda (x) 
                              (or (proactive-tuple-computation-p x)
                                  (not (modifies-state-p x))))
                          *active-queries*))))
        (if bad-queries
            (progn 
              (query-deadlock-warning bad-queries)
              :denied-due-to-deadlock-prevention)
          (add-chosen-sets-of-rule-consequences (find-rule query)))))))

;;;
;;; Execution Control 
;;; 

(nrql-defun (abort-all-queries 
             :doc ((:doc-type :long)
                   (:category :execution-control)
                   (:description "Applies \\funref{abort-query} to \\funref{active-queries}")
                   (:syntax (abort-all-queries))
                   (:values "{\\tt :okay-all-queries-aborted}")
                   (:arguments
                    (:key abox (current-abox) "consider queries for substrates for ABox {\\tt abox} only")
                    (:key type-of-substrate  racer-dummy-substrate "consider only queries for substrates of 
                                                                                                                       specified type, one of:
                                                                                                                       \\argument{racer-dummy-substrate,
                                                                                                                       data-substrate, mirror-data-substrate, rcc-substrate,
                                                                                                                       racer-tbox-mirror-substrate}"))
                   (:remarks)
                   (:examples)
                   (:see-also abort-query execute-query))) 
  (&rest args)
  
  (release-locks)
  (apply #'map-over-queries-for-aboxes-of-type #'abort-query 
	 (with-access-to-lifecycle-lists
           (copy-list *active-queries*))
	 args)

  :okay-all-queries-aborted)

(nrql-defun (abort-all-rules
             :doc ((:doc-type :short)
                   (:category :execution-control)
                   (:rule-equivalent-of abort-all-queries)))
  (&rest args)

  (apply #'map-over-queries-for-aboxes-of-type #'abort-rule
	 (with-access-to-lifecycle-lists
           (copy-list *active-rules*))
	 args)

  :okay-all-rules-aborted)

;;;
;;;
;;;

(nrql-defun (execute-all-queries 
             :doc ((:doc-type :long)
                   (:category :execution-control)
                   (:description "Applies \\funref{execute-query} to \\funref{ready-queries}")
                   (:syntax (execute-all-queries &rest args))
                   (:values "A list containing the results of \\funref{execute-query} applied to 
                                                                                                                       \\funref{ready-queries}")
                   (:arguments
                    (:key abox (current-abox) "consider queries for substrates for ABox {\\tt abox} only")
                    (:key type-of-substrate  racer-dummy-substrate "consider only queries for substrates of 
                                                                                                                       specified type, one of:
                                                                                                                       \\argument{racer-dummy-substrate,
                                                                                                                       data-substrate, mirror-data-substrate, rcc-substrate,
                                                                                                                       racer-tbox-mirror-substrate}")
                    (:reference args execute-query))
                   (:remarks )
                   (:examples)
                   (:see-also execute-query abort-query)))
  (&rest args)
  (with-racer-timeout
    (apply #'eval-nrql-settings2
           #'(lambda ()
               (apply #'map-over-queries-for-aboxes-of-type 
                      #'(lambda (q) 
                          (apply #'execute-query q args))
                      (with-access-to-lifecycle-lists
                        (copy-list *ready-queries*))
                      args))
           :allow-other-keys t
           args)))


(nrql-defun (run-all-queries
             :doc ((:doc-type :short)
                   (:category :execution-control)
                   (:synonym execute-all-queries)))
  (&rest args)
  (apply #'execute-all-queries args))


(nrql-defun (reexecute-all-queries 
             :doc ((:doc-type :long)
                   (:category :execution-control)
                   (:description "Applies \\funref{reexecute-query} to \\funref{processed-queries}")
                   (:syntax (reexecute-all-queries &rest arsg))
                   (:values "A list containing the results of \\funref{reexecute-query} applied to 
                                                                                                                       \\funref{ready-queries}")
                   (:arguments
                    (:key abox (current-abox) "consider queries for substrates for ABox {\\tt abox} only")
                    (:key type-of-substrate  racer-dummy-substrate "consider only queries for substrates of 
                                                                                                                       specified type, one of:
                                                                                                                       \\argument{racer-dummy-substrate,
                                                                                                                       data-substrate, mirror-data-substrate, rcc-substrate,
                                                                                                                       racer-tbox-mirror-substrate}")
                    (:reference args execute-query)
                    (:reference args reexecute-query))
                   (:remarks )
                   (:examples)
                   (:see-also reexecute-query execute-query execute-all-queries)))
  (&rest args)
  (with-racer-timeout
    (apply #'eval-nrql-settings2
           #'(lambda ()
               (apply #'map-over-queries-for-aboxes-of-type 
                      #'(lambda (q) 
                          (apply #'reexecute-query q args))
                      (with-access-to-lifecycle-lists
                        (copy-list *processed-queries*))
                      args))
           :allow-other-keys t
           args)))

(nrql-defun (execute-or-reexecute-all-queries 
             :doc ((:doc-type :long)
                   (:category :execution-control)
                   (:description "Applies \\funref{execute-or-reexecute-query} to \\funref{processed-queries}")
                   (:syntax (execute-or-reexecute-all-queries &rest arsg))
                   (:values "A list containing the results of \\funref{execute-or-reexecute-query} applied to  
                                                                                                                       \\funref{ready-queries} and \\funref{processed-queries}")
                   (:arguments
                    (:key abox (current-abox) "consider queries for substrates for ABox {\\tt abox} only")
                    (:key type-of-substrate  racer-dummy-substrate "consider only queries for substrates of 
                                                                                                                       specified type, one of:
                                                                                                                       \\argument{racer-dummy-substrate,
                                                                                                                       data-substrate, mirror-data-substrate, rcc-substrate,
                                                                                                                       racer-tbox-mirror-substrate}")
                    (:reference args (execute-query reexecute-query)))
                   (:remarks "First the ready queries are executed,
                                                                                                                       and then the processed queries reexecuted (however, the ready queries
                                                                                                                                                                         are not executed twice)") 
                   (:examples)
                   (:see-also reexecute-query execute-query execute-all-queries)))
  (&rest args &key debug-p &allow-other-keys)
  (with-racer-timeout
    (apply #'eval-nrql-settings2
           #'(lambda ()
               (let ((proc (with-access-to-lifecycle-lists
                             (copy-list *processed-queries*)))
                     (ready  (with-access-to-lifecycle-lists
                               (copy-list *ready-queries*))))
                 (nconc (apply #'map-over-queries-for-aboxes-of-type
                               #'(lambda (x) 
                                   (let* ((t1 (get-internal-real-time))
                                          (res (apply #'execute-query x args))
                                          (t2 (get-internal-real-time)))
                                     (if debug-p
                                         (list (iterator-id x) res `(:time ,(float (- t2 t1))))
                                       res)))
                               ready
                               args)
                        (apply #'map-over-queries-for-aboxes-of-type
                               #'(lambda (x) 
                                   (let* ((t1 (get-internal-real-time))
                                          (res (apply #'reexecute-query x args))
                                          (t2 (get-internal-real-time)))
                                     (if debug-p
                                         (list (iterator-id x) res `(:time ,(float (- t2 t1))))
                                       res)))
                               proc
                               args))))
           :allow-other-keys t
           args)))

;;;
;;;
;;;

(nrql-defun (execute-all-rules 
             :doc ((:doc-type :short)
                   (:category :execution-control)
                   (:rule-equivalent-of execute-all-queries)))
  (&rest args)
  (with-racer-timeout
    (apply #'eval-nrql-settings2
           #'(lambda ()
               (apply #'map-over-queries-for-aboxes-of-type
                      #'(lambda (x) 
                          (apply #'execute-rule x args))
                      (with-access-to-lifecycle-lists
                        (copy-list *ready-rules*))
                      args))
           :allow-other-keys t
           args)))

(nrql-defun (reexecute-all-rules 
             :doc ((:doc-type :short)
                   (:category :execution-control)
                   (:rule-equivalent-of reexecute-all-queries)))
  (&rest args)
  (with-racer-timeout
    (apply #'eval-nrql-settings2
           #'(lambda ()    
               (apply #'map-over-queries-for-aboxes-of-type
                      #'(lambda (x)
                          (apply #'reexecute-rule x args))
                      (with-access-to-lifecycle-lists
                        (copy-list *processed-rules*))
                      args))
           :allow-other-keys t
           args)))

(nrql-defun (run-all-rules
             :doc ((:doc-type :short)
                   (:category :execution-control)
                   (:rule-equivalent-of run-all-rules)
                   (:synonym execute-all-rules)))
  (&rest args)
  (apply #'execute-all-rules args))


(nrql-defun (execute-or-reexecute-all-rules 
             :doc ((:doc-type :short)
                   (:category :execution-control)
                   (:rule-equivalent-of execute-or-reexecute-all-queries)))
  (&rest args &key debug-p &allow-other-keys)
  (with-racer-timeout
    (apply #'eval-nrql-settings2
           #'(lambda ()    
               (let ((proc (with-access-to-lifecycle-lists
                             (copy-list *processed-rules*)))
                     (ready (with-access-to-lifecycle-lists
                              (copy-list *ready-rules*))))
                 (nconc 
                  (apply #'map-over-queries-for-aboxes-of-type 
                         #'(lambda (x)
                             (let* ((t1 (get-internal-real-time))
                                    (res (apply #'execute-rule x args))
                                    (t2 (get-internal-real-time)))
                               (if debug-p
                                   (list (iterator-id x) res `(:time ,(float (- t2 t1))))
                                 res)))
                         ready
                         args)
                  (apply #'map-over-queries-for-aboxes-of-type
                         #'(lambda (x)
                             (let* ((t1 (get-internal-real-time))
                                    (res (apply #'reexecute-rule x args))
                                    (t2 (get-internal-real-time)))
                               (if debug-p
                                   (list (iterator-id x) res `(:time ,(float (- t2 t1))))
                                 res)))
                         proc
                         args))))
           :allow-other-keys t
           args)))

;;;
;;;
;;;

(nrql-defun (get-all-answers
             :doc
             ((:doc-type :long)
              (:category :execution-control)
              (:description "Applies \\funref{get-answer} to specified lists of queries and/or rules")
              (:syntax (execute-or-reexecute-all-queries 
                        &rest args &key ready-p (active-p t) (processed-p t)  (queries-p t) (rules-p t) execute-p))
                                                         
              (:values "A list containing the results of \\funref{get-answer} applied to  the specified lists 
                                                                                                                       of queries and/or rules")
              (:arguments
               (:key abox (current-abox) "consider queries/rules for substrates for ABox {\\tt abox} only")
               (:key type-of-substrate  racer-dummy-substrate "consider only queries/rules for substrates of 
                                                                                                                       specified type, one of:
                                                                                                                       \\argument{racer-dummy-substrate,
                                                                                                                       data-substrate, mirror-data-substrate, rcc-substrate,
                                                                                                                       racer-tbox-mirror-substrate}")
               (:key rules-p nil "if {\\tt t}, applies \\funref{get-answer} to rules")
               (:key ready-p nil "if {\\tt t}, applies {\\tt
                                                                                                                       get-answer} to \\funref{ready-queries} and/or \\funref{ready-rules};
                                                                                                                       note that \\argument{args} is passed to \\funref{get-answer} and the
                                                                                                                       queries are executed automatically (see
                                                                                                                                                           \\funref{get-answer}, \\argument{execute-p} = {\\tt t}")
               (:key active-p nil "if {\\tt t}, applies {\\tt
                                                                                                                                                           get-answer} to \\funref{active-queries} and/or \\funref{active-rules}. Note that this requires
                                                                                                                                                           computational ressources, i.e., the answers have to be computed") 
               (:key processed-p nil "if {\\tt t}, applies {\\tt
                                                                                                                                                           get-answer} to \\funref{processed-queries} and/or
                                                                                                                                                           \\funref{processed-rules}. Note that this requires no computational
                                                                                                                                                           ressources, since the answers are already available and stored within
                                                                                                                                                           the query objects")) 
              (:remarks "First the ready queries are executed,
                                                                                                                                                           and then the processed queries reexecuted (however, the ready queries
                                                                                                                                                                                                             are not executed twice)")
              (:examples)
              (:see-also reexecute-query execute-query execute-all-queries)))

  (&rest args &key ready-p (active-p t) (processed-p t)  (queries-p t) (rules-p t) execute-p)
  (let ((execute-p (or execute-p ready-p))
        (rq (with-access-to-lifecycle-lists 
              (copy-list *ready-queries*)))
        (rr (with-access-to-lifecycle-lists 
              (copy-list *ready-rules*)))
        (aq (with-access-to-lifecycle-lists 
              (copy-list *active-queries*)))
        (ar (with-access-to-lifecycle-lists 
              (copy-list *active-rules*)))
        (pq (with-access-to-lifecycle-lists 
              (copy-list *processed-queries*)))
        (pr (with-access-to-lifecycle-lists 
              (copy-list *processed-rules*))))

    (with-racer-timeout
      (apply #'map-over-queries-for-aboxes-of-type
             #'(lambda (q)
                 (list (iterator-id q)
                       (apply #'get-answer q 
                              (append args
                                      (list :execute-p execute-p)))))

             (append (when (and queries-p ready-p)
                       rq)
                     (when (and queries-p active-p)
                       aq)
                     (when (and queries-p processed-p)
                       pq)
                    
                     (when (and rules-p ready-p)
                       rr)
                     (when (and rules-p active-p)
                       ar)
                     (when (and rules-p processed-p)
                       pr))

             args))))

;;;
;;;
;;;

(nrql-defun (wait-for-queries-to-terminate 
             :doc ((:doc-type :long)
                   (:category :execution-control)
                   (:description "Waits (i.e., blocks the API) until
                                                                                                                                                           all queries have terminated, i.e., \\funref{active-queries} returns
                                                                                                                                                           {\\tt nil}")
                   (:syntax (wait-for-queries-to-terminate))
                   (:arguments )
                   (:values "{\\tt :okay}")
                   (:remarks "queries which are executed in lazy tuple-at-a-time mode do not terminate automatically. Thus, in order to prevent deadlocks, this function can only be called if no such queries are active")
                   (:examples)
                   (:see-also active-queries abort-query)))
  ()
  (if (every #'proactive-tuple-computation-p 
             (with-access-to-lifecycle-lists 
               (copy-list *active-queries*)))
      (progn 
        ;;; auf automatische Terminierung warten! 
        (process-wait (not *active-queries*))
        :okay)

    (progn
      (unless *disable-deadlock-checking*
        (query-deadlock-warning
         (remove-if #'proactive-tuple-computation-p 
                    (with-access-to-lifecycle-lists 
                      (copy-list *active-queries*))))
    
        :denied-due-to-deadlock-prevention))))

(nrql-defun (wait-for-rules-to-terminate 
             :doc ((:doc-type :short)
                   (:category :execution-control)
                   (:rule-equivalent-of wait-for-queries-to-terminate)))
  ()
  (if (every #'proactive-tuple-computation-p 
             (with-access-to-lifecycle-lists 
               (copy-list *active-rules*)))
      (progn 
        (process-wait (not *active-rules*))
        :okay)

    (progn
      (unless *disable-deadlock-checking*
        (rule-deadlock-warning
         (remove-if #'proactive-tuple-computation-p
                    (with-access-to-lifecycle-lists 
                      (copy-list *active-rules*))))
        
        :denied-due-to-deadlock-prevention))))


;;;
;;; nRQL Settings 
;;;

(nrql-defun (enable-data-substrate-mirroring 
             :doc ((:doc-type :long)
                   (:category :substrate-layer)
                   (:description "Advises nRQL (globally) to create
                                                                                                                                                           substrates of type {\\tt mirror-data-substrate}. Additional retrieval
                                                                                                                                                           facilities (especially for OWL) are provided on this kind of
                                                                                                                                                           substrate.  Please refer to the User Guide. See also 
                                                                                                                                                           argument \\argument{type-of-substrate} of function
                                                                                                                                                           \\funref{racer-prepare-query}")
                   (:syntax (enable-data-substrate-mirroring))
                   (:inverse-of disable-data-substrate-mirroring)
                   (:arguments )
                   (:values )
                   (:remarks "If you want to exploit the additional 
                                                                                                                                                           retrieval facilities offered by the (mirror) data substrate, then 
                                                                                                                                                           make sure that this function is called before the first nRQL query is posed")
                   (:examples)
                   (:see-also disable-data-substrate-mirroring 
                    set-nrql-mode 
                    with-nrql-settings
                    describe-query-processing-mode)))
  ()
  (setf *type-of-substrate* 'mirror-data-substrate)
  (setf *initial-abox-mirroring-p* t) ;;; wichtig!!!
  :okay-data-substrate-mirroring-enabled)

(nrql-defun (disable-data-substrate-mirroring 
             :doc ((:doc-type :short)
                   (:category :substrate-layer)
                   (:inverse-of enable-data-substrate-mirroring)))
  ()
  (setf *type-of-substrate* 'racer-dummy-substrate)
  :okay-data-substrate-mirroring-disabled)

;;;
;;;
;;;

(nrql-defun (add-missing-top-conjuncts
             :doc ((:doc-type :short)
                   (:category :querying-modes)
                   (:inverse-of dont-add-missing-top-conjuncts-p
                    (:description "Always add top conjuncts for free variables
in nRQL queries (variables which only appear in the head of a query)"))))
  ()
  
  (setf *auto-add-top-conjuncts-p* t)
  
  :okay-adding-missing-top-conjuncts)


(nrql-defun (dont-add-missing-top-conjuncts
             :doc ((:doc-type :short)
                   (:category :querying-modes)
                   (:inverse-of add-missing-top-conjuncts)))
  ()
  
  (setf *auto-add-top-conjuncts-p* nil)
  
  :okay-not-adding-missing-top-conjuncts)


(nrql-defun (use-individual-synonym-equivalence-classes 
             :doc ((:doc-type :long)
                   (:category :querying-modes)
                   (:description "Usually, nRQL's query variables are
                                                                                                                                                           bound to ABox individuals. However, sometimes RacerPro can infer that
                                                                                                                                                           a set of differently named individuals must represent the same
                                                                                                                                                           (identical) domain object. In this case, the different individuals are
                                                                                                                                                           called \\emph{individual synonyms}.  Sometimes it is meaningful to bind
                                                                                                                                                           query variables not to single individuals, but to \\emph{synonym
                                                                                                                                                           equivalence classes}. This can be achieved by enabling this mode.
                                                                                                                                                           If this mode is enabled, then variables will not be bound to single
                                                                                                                                                           ABox individuals, but to representative individuals from the synonym 
                                                                                                                                                           equivalence classes")
                   (:syntax (use-individual-synonym-equivalence-classes))
                   (:inverse-of dont-use-individual-synonym-equivalence-classes)
                   
                   (:values "{\\tt :okay-using-individual-equivalence-classes}")
                   
                   (:remarks "see also \\argument{use-individual-synonyms-p} 
                                                                                                                                                           in \\funref{execute-query}")
                                               
                   (:examples)
                   (:see-also dont-use-individual-synonym-equivalence-classes
                    set-nrql-mode 
                    with-nrql-settings
                    describe-query-processing-mode
                    )))
  ()
  (setf *use-individual-synonyms-p* t)
  :okay-using-individual-equivalence-classes)

(nrql-defun (dont-use-individual-synonym-equivalence-classes 
             :doc ((:doc-type :short)
                   (:category :querying-modes)
                   (:inverse-of dont-use-individual-synonym-equivalence-classes)))
  ()
  (setf *use-individual-synonyms-p* nil)
  :okay-not-using-individual-equivalence-classes)

;;;
;;;
;;;

(nrql-defun (use-injective-variables-by-default
             :doc ((:doc-type :long)
                   (:category :querying-modes)
                   (:inverse-of use-injective-variables-by-default)
                   (:description "nRQL offers injective variables.
                                                                                                                                                           Usually, all variables are non-injective. That means two different
                                                                                                                                                           variables can be bound to the same ABox individual -- the mapping from
                                                                                                                                                           variable to ABox individuals must not be injective, unlike for
                                                                                                                                                           injective variables, for which the mapping must be injective. By
                                                                                                                                                           default, all variables with {\\tt ?}-prefix are non-injective, and
                                                                                                                                                           injective variables get a {\\tt \\$?}-prefix. In the older nRQL, all
                                                                                                                                                           variables were injective by default; thus, {\\tt ?}-prefix denoted an
                                                                                                                                                           injective, and {\\tt \\$?}-prefix a non-injective variable.  This
                                                                                                                                                           function allows you to switch to the old nRQL mode. Note that you can
                                                                                                                                                           also use negated {\\tt same-as} query atoms to enforce injective
                                                                                                                                                           bindings")
                   (:syntax (use-injective-variables-by-default))
                   (:arguments )
                   (:values "{\\tt :okay-using-?-prefix-for-injective-variables}")
                   (:remarks)
                   (:examples)
                   (:see-also dont-use-injective-variables-by-default
                    set-nrql-mode 
                    with-nrql-settings
                    describe-query-processing-mode)))
  ()
  (setf *toggle-una-variables-p* nil)
  :okay-using-?-prefix-for-injective-variables)

(nrql-defun (dont-use-injective-variables-by-default
             :doc ((:doc-type :short)
                   (:category :querying-modes)
                   (:inverse-of use-injective-variables-by-default)))
  ()
  (setf *toggle-una-variables-p* t)
  :okay-using-$?-prefix-for-injective-variables)


;;;
;;;
;;;


(nrql-defun (enable-abox-mirroring 
             :doc ((:doc-type :short)
                   (:category :querying-modes)
                   (:description "Instructs nRQL (globally) to mirror
                                                                                                                                                           the asserted content of an ABox (the ABox assertions) into its
                                                                                                                                                           internal data caches before querying starts. Note that the amount of
                                                                                                                                                           information in the substrate resp. caches determines the degree of
                                                                                                                                                           query answering completness in the incomplete modes (see
                                                                                                                                                                                                                \\funref{enable-told-information-querying}). See also argument
                                                                                                                                                           \\argument{ told-information-reasoning-p} of function
                                                                                                                                                           \\funref{execute-query}")
                   (:syntax (enable-abox-mirroring))
                   (:inverse-of disable-abox-mirroring)
                   (:arguments )
                   (:values "{\\tt :okay-abox-mirroring-enabled}")
                   (:remarks)
                   (:examples)
                   (:see-also disable-abox-mirroring enable-smart-abox-mirroring
                    enable-very-smart-abox-mirroring
                    enable-told-information-querying
                    set-nrql-mode 
                    with-nrql-settings
                    describe-query-processing-mode)))
  ()
  (setf *initial-abox-mirroring-p* t
        *ensure-tbox-classification-p* nil
        *classify-concepts-in-instance-assertions-p* nil)
  
  :okay-abox-mirroring-enabled)


(nrql-defun (disable-abox-mirroring 
             :doc ((:doc-type :short)
                   (:category :querying-modes)
                   (:inverse-of enable-abox-mirroring)))
  ()
  (setf *initial-abox-mirroring-p* nil)

  :okay-abox-mirroring-disabled)

;;;
;;;
;;;

(nrql-defun (enable-smart-abox-mirroring 
             :doc ((:doc-type :long)
                   (:category :querying-modes)
                   (:description "Enables ABox mirroring, see
                                                                                                                                                           \funref{enable-abox-mirroring}, but in a
                                                                                                                                                           smarter way. Not only are the ABox assertion mirrored and put
                                                                                                                                                           into the substrate caches and index structures, but also the 
                                                                                                                                                           TBox information is exploited. In case of a concept assertion 
                                                                                                                                                           such as {\\tt (instance i  
                                                                                                                                                                                   C)} with atomic concept {\\tt C}, not only {\\tt C} is added as told 
                                                                                                                                                           information for {\\tt i} to 
                                                                                                                                                           the ABox mirror resp. substrate caches and index structures, but
                                                                                                                                                           also the set of concept synonyms and concept 
                                                                                                                                                           ancestors from the TBox is computed and added as well. The same
                                                                                                                                                           applies for {\\tt related} role membership assertions in the  
                                                                                                                                                           presence of role hierarchies, etc. Please consult the User Guide for
                                                                                                                                                           more details. See also argument \\argument{
                                                                                                                                                           told-information-reasoning-p} of function \\funref{execute-query}")
                   (:syntax (enable-smart-abox-mirroring))
                   (:arguments )
                   (:values "{\\tt :okay-smart-abox-mirroring-enabled}")
                   (:remarks)
                   (:examples)
                   (:see-also enable-very-smart-abox-mirroring disable-abox-mirroring
                    enable-abox-mirroring
                    set-nrql-mode
                    with-nrql-settings
                    describe-query-processing-mode)))
  ()
  (setf *initial-abox-mirroring-p* t
        *ensure-tbox-classification-p* t
        *classify-concepts-in-instance-assertions-p* nil)
  
  :okay-smart-abox-mirroring-enabled)



(nrql-defun (enable-very-smart-abox-mirroring 
             :doc ((:doc-type :long)
                   (:category :querying-modes)
                   (:description "Like
                                                                                                                                                           \\funref{enable-smart-abox-mirroring}, but now not only atomic concept
                                                                                                                                                           assertions will be used for augmenting the information in the mirror
                                                                                                                                                           resp. substrate caches and index structures, but also non-atomic
                                                                                                                                                           concepts in ABox assertions. Thus, if {\\tt (instance i C)} is present
                                                                                                                                                           for a non-atomic concepts {\\tt C}, then also the set of concept
                                                                                                                                                           synonyms and concept ancestors is computed and added to the mirror.
                                                                                                                                                           See also argument \\argument{ told-information-reasoning-p} of
                                                                                                                                                           function \\funref{execute-query}")
                   (:syntax (enable-very-smart-abox-mirroring))
                   (:arguments )
                   (:values "{\\tt :okay-very-smart-abox-mirroring-enabled}")
                   (:remarks "Might be expensive, since concepts in 
                                                                                                                                                           ABox concept assertion must be classified in order to compute the synonyms and
                                                                                                                                                           ancestors")
                   (:examples)
                   (:see-also enable-smart-abox-mirroring
                    disable-abox-mirroring
                    enable-abox-mirroring
                    set-nrql-mode
                    with-nrql-settings
                    describe-query-processing-mode)))
  ()
  (setf *initial-abox-mirroring-p* t
        *ensure-tbox-classification-p* t
        *classify-concepts-in-instance-assertions-p* t)

  :okay-very-smart-abox-mirroring-enabled)

;;;
;;;
;;;

(nrql-defun (enable-two-phase-query-processing-mode
             :doc ((:doc-type :long)
                   (:category :querying-modes)
                   (:description "Enables (global) two phase query
                                                                                                                                                           processing.  In this mode, nRQL can distinguish between cheap (see
                                                                                                                                                                                                                          \\funref{cheap-query-p}) and expensive answer tuples (see
                                                                                                                                                                                                                                                                                \\funref{expensive-query-p}) of a query. Please consult the User Guide
                                                                                                                                                           for more information. See also argument \\argument{
                                                                                                                                                           two-phase-processing-p} of function \\funref{execute-query}")
                   (:inverse-of disable-two-phase-query-processing-mode)
                   (:syntax (enable-two-phase-query-processing-mode))
                   (:arguments )
                   (:values "{\\tt  :okay-two-phase-query-processing-mode-enabled}")
                   (:remarks "Before the first expensive tuple is
                                                                                                                                                           computed, nRQL can be advised to deliver a so-called warning token,
                                                                                                                                                           see \\funref{enable-phase-two-starts-warning-tokens}")
                   (:examples)
                   (:see-also disable-two-phase-query-processing-mode
                    enable-phase-two-starts-warning-tokens
                    set-nrql-mode
                    with-nrql-settings
                    describe-query-processing-mode)))
  ()
  (setf *two-phase-processing-p* t
        *initial-abox-mirroring-p* t
        *ensure-tbox-classification-p* t
        *told-information-reasoning-p* nil
        *tuple-at-a-time-p* t
        *proactive-tuple-computation-p* nil
        *check-abox-consistency-p* nil
        *deliver-kb-has-changed-warning-tokens-p* t
        *deliver-phase-two-warning-tokens-p* t)
  
  :okay-two-phase-query-processing-mode-enabled)

(nrql-defun (disable-two-phase-query-processing-mode 
             :doc ((:doc-type :short)
                   (:category :querying-modes)
                   (:inverse-of disable-two-phase-query-processing-mode)))
  ()
  (setf *two-phase-processing-p* nil)
  
  :okay-two-phase-query-processing-mode-disabled)

;;;
;;;
;;;

(nrql-defun (enable-phase-two-starts-warning-tokens
             :doc ((:doc-type :long)
                   (:category :querying-modes)
                   (:inverse-of disable-phase-two-starts-warning-tokens)
                   (:description "Enables (global) delivery of {\\tt
                                                                                                                                                           :warning-expensive-phase-two-starts} tokens in two-phase query
                                                                                                                                                           processing modes, denoting the transition between cheap (see
                                                                                                                                                                                                                    \\funref{cheap-query-p}) and expensive answer tuples (see
                                                                                                                                                                                                                                                                          \\funref{expensive-query-p}). See also argument
                                                                                                                                                           \\argument{deliver-phase-two-warning-tokens-p} of function
                                                                                                                                                           \\funref{execute-query}")
                   (:syntax (enable-phase-two-starts-warning-tokens))
                   (:arguments )
                   (:values "{\\tt :okay-phase-two-warning-tokens-enabled} or
                                                                                                                                                           {\\tt :ignored-not-in-two-phase-processing-mode}")
                   (:remarks "Can only be called if nRQL is in two
                                                                                                                                                           phase processing mode (see
                                                                                                                                                                                  \\funref{enable-two-phase-query-processing-mode})")
                   (:examples)
                   (:see-also  enable-two-phase-query-processing-mode 
                    disable-two-phase-query-processing-mode
                    set-nrql-mode
                    with-nrql-settings
                    describe-query-processing-mode)))

  ()
  (if *two-phase-processing-p*
      (progn 
        (setf *deliver-phase-two-warning-tokens-p* t)

        :okay-phase-two-warning-tokens-enabled)
    
    :ignored-not-in-two-phase-processing-mode))

(nrql-defun (disable-phase-two-starts-warning-tokens 
             :doc ((:doc-type :short)
                   (:category :querying-modes)
                   (:inverse-of enable-phase-two-starts-warning-tokens)))

  ()
  (if *two-phase-processing-p*
      (progn 
        (setf *deliver-phase-two-warning-tokens-p* nil)

        :okay-phase-two-warning-tokens-disabled)
    
    :ignored-not-in-two-phase-processing-mode))

;;;
;;; 
;;;

(nrql-defun (enable-kb-has-changed-warning-tokens 
             :doc ((:doc-type :long)
                   (:category :querying-modes)
                   (:inverse-of disable-kb-has-changed-warning-tokens)
                   (:description "Enables (global) delivery of {\\tt
                                                                                                                                                           :warning-kb-has-changed} tokens in tuple-at-a-time query processing
                                                                                                                                                           mode. Such a token is delivered iff the query ABox / TBox changes
                                                                                                                                                           during query answering (in the time the query is still active).
                                                                                                                                                           See also argument \\argument{deliver-kb-has-changed-warning-tokens-p}
                                                                                                                                                           of function \\funref{execute-query}")
                   (:syntax (enable-kb-has-changed-warning-tokens))
                   (:arguments )
                   (:values "{\\tt :okay-kb-has-changed-warning-tokens-enabled}
                                                                                                                                                           or {\\tt :ignored-not-in-tuple-at-a-time-mode}")
                   (:remarks "Can only be called if nRQL is in 
                                                                                                                                                           tuple-at-a-time mode")
                   (:examples)
                   (:see-also 
                    set-nrql-mode 
                    with-nrql-settings
                    describe-query-processing-mode)))
  ()
  (if *tuple-at-a-time-p*
      (progn 
        (setf *deliver-kb-has-changed-warning-tokens-p* t)

        :okay-kb-has-changed-warning-tokens-enabled)

    :ignored-not-in-tuple-at-a-time-mode))

(nrql-defun (disable-kb-has-changed-warning-tokens
             :doc ((:doc-type :short)
                   (:category :querying-modes)
                   (:inverse-of enable-kb-has-changed-warning-tokens)))
  ()
  (if *tuple-at-a-time-p*
      (progn 

        (setf *deliver-kb-has-changed-warning-tokens-p* nil)

        :okay-kb-has-changed-warning-tokens-disabled)

    :ignored-not-in-tuple-at-a-time-mode))

;;;
;;;
;;;


(nrql-defun (enable-nrql-warnings 
             :doc ((:doc-type :long)
                   (:inverse-of disable-nrql-warnings)
                   (:category :querying-modes)
                   (:description "Advises nRQL to print out warnings
                                                                                                                                                           on STDOUT in certain circumstances and enables delivery of warning
                                                                                                                                                           tokens (see \\funref{enable-kb-has-changed-warning-tokens},
                                                                                                                                                                       \\funref{enable-phase-two-starts-warning-tokens})")
                   (:syntax (enable-nrql-warnings))
                   (:arguments )
                   (:values "{\\tt :okay-warnings-enabled}")
                   (:remarks)
                   (:examples)
                   (:see-also describe-query-processing-mode
                    enable-kb-has-changed-warning-tokens
                    with-nrql-settings
                    disable-kb-has-changed-warning-tokens)))
  ()
  (setf *deliver-kb-has-changed-warning-tokens-p* t
        *deliver-phase-two-warning-tokens-p* t
        *warnings-p* t)

  :okay-warnings-enabled)


(nrql-defun (disable-nrql-warnings 
             :doc ((:doc-type :short)
                   (:category :querying-modes)
                   (:inverse-of enable-nrql-warnings)))

  ()
  (setf *deliver-kb-has-changed-warning-tokens-p* nil
        *deliver-phase-two-warning-tokens-p* nil
        *warnings-p* nil)

  :okay-warnings-disabled)


;;;
;;;
;;;

(nrql-defun (enable-told-information-querying 
             :doc ((:doc-type :long)
                   (:category :querying-modes)
                   (:inverse-of disable-told-information-querying)
                   (:description "Puts nRQL (globally) into told information
                                                                                                                                                           querying mode; see also \\argument{told-information-reasoning-p} of
                                                                                                                                                           \\funref{execute-query}. Told information querying means means that
                                                                                                                                                           calls to RacerPro's ABox retrieval functions are avoided and only the
                                                                                                                                                           information in the substrate caches is used for query answering. It is 
                                                                                                                                                           recommended to use \\funref{set-nrql-mode} instead of this function. See
                                                                                                                                                           also argument \\argument{ told-information-reasoning-p} of function 
                                                                                                                                                           \\funref{execute-query}")
                   (:syntax (enable-told-information-querying))
                   (:arguments )
                   (:values "{\\tt   :okay-told-information-querying-enabled}")
                   (:remarks)
                   (:examples)
                   (:see-also set-nrql-mode describe-query-processing-mode
                    with-nrql-settings)))
  ()
  (setf *told-information-reasoning-p* t
        *check-abox-consistency-p* nil
        *ensure-tbox-classification-p* nil
        *classify-concepts-in-instance-assertions-p* nil
        *initial-abox-mirroring-p* t)
  
  :okay-told-information-querying-enabled)

(nrql-defun (disable-told-information-querying 
             :doc ((:doc-type :short)
                   (:category :querying-modes)
                   (:inverse-of enable-told-information-querying)))
  ()
  (setf *told-information-reasoning-p* nil)

  :okay-told-information-querying-disabled)

;;;
;;;
;;;

(nrql-defun (add-role-assertions-for-datatype-properties 
             :doc ((:doc-type :long)
                   (:category :querying-modes)
                   (:inverse-of dont-add-role-assertions-for-datatype-properties)
                   (:description "Constraint query atoms referring OWL
                                                                                                                                                           datatype properties only work on OWL KBs if some additional auxiliary ABox 
                                                                                                                                                           assertions are added to the ABox created from the OWL file. Use this function 
                                                                                                                                                           to ensure that nRQL adds these additional assertions. Note that this function
                                                                                                                                                           must be called before the first nRQL query to that OWL KB is posed")
                   (:syntax (add-role-assertions-for-datatype-properties))
                   (:arguments )
                   (:values "{\\tt :okay-adding-role-assertions-for-datatype-properties}")
                   (:remarks)
                   (:examples)
                   (:see-also describe-query-processing-mode 
                    set-nrql-mode 
                    with-nrql-settings)))
  ()
  (setf *add-role-assertions-for-datatype-properties-p* t)

  :okay-adding-role-assertions-for-datatype-properties)


(nrql-defun (dont-add-role-assertions-for-datatype-properties
             :doc ((:doc-type :short)
                   (:category :querying-modes)
                   (:inverse-of add-role-assertions-for-datatype-properties)))
  ()
  (setf *add-role-assertions-for-datatype-properties-p* nil)

  :okay-not-adding-role-assertions-for-datatype-properties)

;;;
;;;
;;; 

(nrql-defun (enable-eager-tuple-computation
             :doc ((:doc-type :long)
                   (:category :querying-modes)
                   (:inverse-of enable-lazy-tuple-computation)
                   (:description "Advises nRQL to precompute answer
                                                                                                                                                           tuples in tuple-at-a-time mode, even if these tuples have not yet been
                                                                                                                                                           requested (see \\funref{get-next-tuple}); see also argument
                                                                                                                                                           \\argument{proactive-tuple-computation-p} of function
                                                                                                                                                           \\funref{execute-query}. 
                                                                                                                                                           A query started in eager mode will never appear on 
                                                                                                                                                           \\funref{waiting-queries}. The inverse tuple-at-a-time mode is
                                                                                                                                                           called lazy tuple-at-a-time mode. In this mode, the next answer tuple
                                                                                                                                                           will not be computed by the query answering process (thread) until it
                                                                                                                                                           is really requested; in the meantime, such a query appears on the list
                                                                                                                                                           of \\funref{waiting-queries}" )
                   (:syntax (enable-eager-tuple-computation))
                   (:arguments )
                   (:values "{\\tt :okay-eager-mode-enabled} or 
                                                                                                                                                           {\\tt :ignored-not-in-tuple-at-a-time-mode}") 
                   (:remarks "Is only effective if nRQL is in
                                                                                                                                                           tuple-at-a-time-mode")
                   (:examples)
                   (:see-also describe-query-processing-mode 
                    set-nrql-mode
                    with-nrql-settings)))
  ()
  (if *tuple-at-a-time-p*
      (progn 
        (setf *proactive-tuple-computation-p* t)
        
        :okay-eager-mode-enabled)

    :ignored-not-in-tuple-at-a-time-mode))

(nrql-defun (enable-lazy-tuple-computation
             :doc ((:doc-type :short)
                   (:category :querying-modes)
                   (:inverse-of enable-eager-tuple-computation)))
  ()
  (if *tuple-at-a-time-p*
      (progn 
        (setf *proactive-tuple-computation-p* nil)
        
        :okay-lazy-mode-enabled)

    :ignored-not-in-tuple-at-a-time-mode))

;;;
;;;
;;;

(nrql-defun (check-abox-consistency-before-querying
             :doc ((:doc-type :long)
                   (:category :querying-modes)
                   (:inverse dont-check-abox-consistency-before-querying)
                   (:description "Advises nRQL to check 
                                                                                                                                                           the ABox consistency before a query is executed; see also 
                                                                                                                                                           argument \\argument{check-abox-consistency-p} of function \\funref{execute-query}.
                                                                                                                                                           Queries on inconsistent ABoxes are not meaningful")
                   (:syntax (check-abox-consistency-before-querying))
                   (:arguments )
                   (:values "{\\tt
                                                                                                                                                           :okay-checking-abox-consistency-before-querying}")
                   (:remarks)
                   (:examples)
                   (:see-also describe-query-processing-mode with-nrql-settings)))
  ()
  (setf *check-abox-consistency-p* t)

  :okay-checking-abox-consistency-before-querying)

(nrql-defun (dont-check-abox-consistency-before-querying
             :doc ((:doc-type :short)
                   (:category :querying-modes)
                   (:inverse-of check-abox-consistency-before-querying)))
  ()
  (setf *check-abox-consistency-p* nil)

  :okay-not-checking-abox-consistency-before-querying)


;;;
;;;
;;;

(nrql-defun (set-max-no-of-tuples-bound
             :doc ((:doc-type :long)
                   (:category :querying-modes)
                   (:inverse-of get-max-no-of-tuples-bound)
                   (:description "Sets a (global) bound on the
                                                                                                                                                           number of answer tuples that are computed. Use {\\tt nil} to set
                                                                                                                                                           to unbounded (infinite). See also argument \\argument{how-many} 
                                                                                                                                                           of function \\funref{execute-query}")
                   (:syntax (set-max-no-of-tuples-bound n))
                   (:arguments 
                    (:first n "A natural number (the bound) or {\\tt nil} (means unbounded)"))
                   (:values "\\argument{n}")
                   (:remarks)
                   (:examples)
                   (:see-also describe-query-processing-mode
                    get-max-no-of-tuples-bound with-nrql-settings)))
  (&optional n)
  (setf *how-many* n))

(nrql-defun (get-max-no-of-tuples-bound 
             :doc ((:doc-type :short)
                   (:category :querying-modes)
                   (:inverse-of set-max-no-of-tuples-bound)))
  ()
  *how-many*)

;;;
;;;
;;;

(nrql-defun (process-tuple-at-a-time
             :doc ((:doc-type :long)
                   (:category :querying-modes)
                   (:inverse-of process-set-at-a-time)
                   (:description "Puts nRQL (globally) into 
                                                                                                                                                           tuple-at-time-mode. See also  argument \\argument{tuple-at-a-time-p}
                                                                                                                                                           of function \\funref{execute-query}")
                   (:syntax (process-tuple-at-a-time))
                   (:arguments )
                   (:values "{\\tt :okay-processing-tuple-at-a-time}")
                   (:remarks)
                   (:examples)
                   (:see-also describe-query-processing-mode)))
  ()
  #+:multiprocess-queries
  (progn
    (setf *tuple-at-a-time-p* t
          *deliver-kb-has-changed-warning-tokens-p* t)

    :okay-processing-tuple-at-a-time)
  #-:multiprocess-queries
  (nrql-error "Tuple at a time querying mode only possible if :multiprocess-queries is on *features*, please recompile"))
  

(nrql-defun (process-set-at-a-time
             :doc ((:doc-type :short)
                   (:category :querying-modes)
                   (:inverse-of process-tuple-at-a-time)))
  ()
  (setf *tuple-at-a-time-p* nil
        *proactive-tuple-computation-p* t)
  
  :okay-processing-set-at-a-time)

;;;
;;;
;;; 

(nrql-defun (add-rule-consequences-automatically
             :doc ((:doc-type :long)
                   (:category :querying-modes)
                   (:inverse-of dont-add-rule-consequences-automatically)
                   (:description "Rule consequences / ABox assertions
                                                                                                                                                           generated by a rule application can be added automatically after the
                                                                                                                                                           rule has terminated.  Use this function to put nRQL (globally) into that
                                                                                                                                                           mode.  Note that in tuple-at-a-time mode this applies to the chosen 
                                                                                                                                                           sets of rule consequences (see
                                                                                                                                                                                      \\funref{choose-current-set-of-rule-consequences}), whereas in
                                                                                                                                                           set-at-a-time mode this applies to all sets of rule consequences. In
                                                                                                                                                           case rule consequences are not automatically, the functino
                                                                                                                                                           \\funref{add-chosen-sets-of-rule-consequences} can be called manually,
                                                                                                                                                           but only once. See also argument \\argument{add-rule-consequences-p} 
                                                                                                                                                           of function \\funref{execute-rule}")
                   (:syntax (add-rule-consequences-automatically))
                   (:arguments )
                   (:values "{\\tt :okay-adding-rule-consequences-automatically}")
                   (:remarks)
                   (:examples)
                   (:see-also describe-query-processing-mode
                    with-nrql-settings
                    process-tuple-at-a-time process-set-at-a-time
                    execute-rule get-current-set-of-rule-consequences
                    choose-current-set-of-rule-consequences
                    add-chosen-sets-of-rule-consequences)))
  ()
  (setf *add-rule-consequences-p* t)
  :okay-adding-rule-consequences-automatically)

(nrql-defun (dont-add-rule-consequences-automatically
             :doc ((:doc-type :short)
                   (:category :querying-modes)
                   (:inverse-of add-rule-consequences-automatically)))
  ()
  (setf *add-rule-consequences-p* nil)
  :okay-not-adding-rule-consequences-automatically)


;;;
;;;
;;; 

(nrql-defun (exclude-permutations 
             :doc ((:doc-type :long)
                   (:category :querying-modes)
                   (:inverse-of include-permutations)
                   (:description "Advises nRQL to (globally) exclude
                                                                                                                                                           permutations of anwers tuples. See also argument
                                                                                                                                                           \\argument{exclude-permutations-p} of function
                                                                                                                                                           \\funref{execute-query}")
                   (:syntax (exclude-permutations))
                   (:arguments )
                   (:values "{\\tt   :okay-exluding-permuatation}")
                   (:remarks)
                   (:examples)
                   (:see-also describe-query-processing-mode execute-query
                    with-nrql-settings)))
  ()
  (setf *exclude-permutations-p* t)

  :okay-exluding-permuatation)

(nrql-defun (include-permutations 
             :doc ((:doc-type :short)
                   (:category :querying-modes)
                   (:inverse-of exclude-permutations)))

  ()
  (setf *exclude-permutations-p* nil)

  :okay-including-permuations)

;;;
;;; Optimizer Settings 
;;;


(nrql-defun (enable-query-optimization
             :doc ((:doc-type :long)
                   (:category :querying-modes)
                   (:inverse-of disable-query-optimization)
                   (:description "Enables the cost-based query optimizer")
                   (:syntax (enable-query-optimization))
                   (:arguments )
                   (:values "{\\tt   :okay-query-optimization-enabled}")
                   (:remarks "Note that queries must be brought into
                                                                                                                                                           DNF (Disjunctive Normal Form). Thus, query optimization might be
                                                                                                                                                           expensive")
                   (:examples)
                   (:see-also optimizer-use-cardinality-heuristics 
                    describe-query-processing-mode)))
  ()
  (setf *optimize-p* t
        *rewrite-to-dnf-p* t)
                          
  :okay-query-optimization-enabled)

(nrql-defun (disable-query-optimization 
             :doc ((:doc-type :short)
                   (:category :querying-modes)
                   (:inverse-of enable-query-optimization)))
  ()
  (setf *optimize-p* nil)
                          
  :okay-query-optimization-disabled)

;;;
;;;
;;;

(nrql-defun (optimizer-set-no-of-plans-upper-bound 
             :doc ((:doc-type :long)
                   (:category :querying-modes)
                   (:inverse-of optimizer-get-no-of-plans-upper-bound)
                   (:description "Advises the cost-based optimizer to
generate a most \\argument{n} query evaluation plans during
query optimization")
                   (:syntax (optimizer-set-no-of-plans-upper-bound 100))
                   (:arguments 
                    (:first n "a cardinal number"))
                   (:values "the value \\argument{n}")
                   (:remarks)
                   (:examples)
                   (:see-also enable-query-optimization describe-query-processing-mode optimizer-get-no-of-plans-upper-bound)))
  (n)
  (setf *optimizer-max-plans* (max n 1))
  *optimizer-max-plans*)

(nrql-defun (optimizer-get-no-of-plans-upper-bound 
             :doc ((:doc-type :long)
                   (:category :querying-modes)
                   (:inverse-of optimizer-set-no-of-plans-upper-bound)
                   (:description "Returns the current upper bound on the number 
of query evaluation plans to consider during query optimization")
                   (:syntax (optimizer-get-no-of-plans-upper-bound))
                   (:arguments )
                   (:values "the value \\argument{n}, a cardinal")
                   (:remarks)
                   (:examples)
                   (:see-also enable-query-optimization describe-query-processing-mode optimizer-set-no-of-plans-upper-bound)))
  ()
  *optimizer-max-plans*)


;;;
;;;
;;;

(nrql-defun (optimizer-set-time-bound
             :doc ((:doc-type :long)
                   (:category :querying-modes)
                   (:inverse-of optimizer-get-time-bound)
                   (:description "Advises the cost-based optimizer to
spend at most \\argument{n} seconds for query optimization before reducing the number
 of plans to the half and retrying")
                   (:syntax (optimizer-set-time-bound 3))
                   (:arguments 
                    (:first n "a cardinal number (seconds)"))
                   (:values "the value \\argument{n}")
                   (:remarks)
                   (:examples)
                   (:see-also enable-query-optimization describe-query-processing-mode optimizer-set-no-of-plans-upper-bound optimizer-get-time-bound)))
  (n)
  (setf *optimizer-spend-at-most-n-seconds* (max n 1))
  *optimizer-spend-at-most-n-seconds*)

(nrql-defun (optimizer-get-time-bound
             :doc ((:doc-type :long)
                   (:category :querying-modes)
                   (:inverse-of optimizer-set-time-bound)
                   (:description "Returns the current upper time bound 
the query optimizer spends for query optimization")
                   (:syntax (optimizer-get-time-bound))
                   (:arguments )
                   (:values "the value \\argument{n}, a cardinal (seconds)")
                   (:remarks)
                   (:examples)
                   (:see-also enable-query-optimization describe-query-processing-mode optimizer-set-time-bound)))
  ()
  *optimizer-spend-at-most-n-seconds*)


;;;
;;;
;;;


(nrql-defun (optimizer-use-cardinality-heuristics 
             :doc ((:doc-type :long)
                   (:category :querying-modes)
                   (:inverse-of optimizer-dont-use-cardinality-heuristics)
                   (:description "Advises the cost-based optimizer 
                                                                                                                                                           to use ABox statistics for enhanched query optimization") 
                   (:syntax (optimizer-use-cardinality-heuristics))
                   (:arguments )
                   (:values "Also turns on the optimizer (calls
                                                          \\funref{enable-query-optimization})")
                   (:remarks)
                   (:examples)
                   (:see-also enable-query-optimization describe-query-processing-mode)))
  ()
  (if (not *optimize-p*)

      :ignored-optimizer-is-disabled 

    (progn 
      (setf *optimize-p* t
            *optimizer-use-cardinality-heuristics-p* t
            *rewrite-to-dnf-p* t)
      
      :okay-using-cardinality-heuristics)))

(nrql-defun (optimizer-dont-use-cardinality-heuristics 
             :doc ((:doc-type :short)
                   (:category :querying-modes)
                   (:inverse-of optimizer-use-cardinality-heuristics)))
  ()
  (setf *optimizer-use-cardinality-heuristics-p* nil)
                          
  :okay-not-using-cardinality-heuristics)

;;;
;;;
;;;

(nrql-defun (optimizer-ensure-late-lambda-evaluation 
             :doc ((:doc-type :long)
                   (:category :querying-modes)
                   (:inverse-of optimizer-dont-ensure-late-lambda-evaluation)
                   (:description "Advises the cost-based optimizer 
                                                                                                                                                           not to evaluate lambda query atoms before all conditions on the referenced query variables in the lambda have been checked") 
                   (:syntax (optimizer-use-cardinality-heuristics))
                   (:arguments )
                   (:values "Also turns on the optimizer (calls
                                                          \\funref{enable-query-optimization})")
                   (:remarks)
                   (:examples)
                   (:see-also enable-query-optimization describe-query-processing-mode)))
  ()
  (if (not *optimize-p*)

      :ignored-optimizer-is-disabled 

    (progn 
      (setf *optimize-p* t
            *optimizer-ensure-late-lambda-evaluation-p* t
            *rewrite-to-dnf-p* t)
      
      :okay-ensuring-late-lambda-atom-evaluation)))

(nrql-defun (optimizer-dont-ensure-late-lambda-evaluation
             :doc ((:doc-type :short)
                   (:category :querying-modes)
                   (:inverse-of optimizer-ensure-late-lambda-evaluation)))
  ()
  (setf *optimizer-ensure-late-lambda-evaluation-p* nil)
                          
  :okay-not-ensuring-late-lambda-atom-evaluation)

;;;
;;;
;;;

(nrql-defun (set-rewrite-defined-concepts 
             :doc ((:doc-type :long)
                   (:category :internal)))
  (val)
  (setf *rewrite-defined-concepts-p* val))

;;;
;;;
;;;

#+:process-pooling
(nrql-defun (set-initial-size-of-process-pool 
             :doc ((:doc-type :long)
                   (:category :querying-modes)
                   (:inverse-of get-initial-size-of-process-pool)
                   (:description "nRQL uses a process (thread) pool
                                                                                                                                                           for the query answering processes. The initial (minimal) size of the
                                                                                                                                                           pooled processes can be specified with that function. This specifies
                                                                                                                                                           the lower bound of concurrent queries") 
                   (:syntax (set-initial-size-of-process-pool n))
                   (:arguments 
                    (:first n "A natural number, the size of the pool (the number

                                                                           of processes in the pool)"))
                   (:values "\\argument{n}")
                   (:remarks "note that setting the initial process pool 
                                                                                                                                                           causes the pool to reinitialize; all active queries (and rules) are aborted")
                   (:examples)
                   (:see-also get-initial-size-of-process-pool
                    get-process-pool-size
                    set-maximum-size-of-process-pool
                    get-maximum-size-of-process-pool
                    describe-query-processing-mode)))
  (n)

  (when (and n 
             (integerp n)
             (> n 0))
    (setf *min-pool-size* n)
    (init-process-pool))

  *min-pool-size*)

#+:process-pooling
(nrql-defun (set-maximum-size-of-process-pool 
             :doc ((:doc-type :long)
                   (:category :querying-modes)
                   (:inverse-of get-maximum-size-of-process-pool)
                   (:description "Like
                                                                                                                                                           \\funref{set-initial-size-of-process-pool}, but now the maximum number
                                                                                                                                                           of processes in the process pool is specified. In case a new query
                                                                                                                                                           answering process is needed from a pool and the pool is currently
                                                                                                                                                           empty (all processes are accquired by different queries and/or rules),
                                                                                                                                                           nRQL will create an additional process. This new process is added to
                                                                                                                                                           the pool, thus, the process pool can grow up to an upper bound which 
                                                                                                                                                           is specified here")
                   (:syntax (set-maximum-size-of-process-pool n))
                   (:arguments 
                    (:first n "A natural number (the upper bound) or
                                                                                                                                                           {\\tt nil} (which means unbounded)"))
                   (:values "\\argument{n}")
                   (:remarks)
                   (:examples)
                   (:see-also  set-initial-size-of-process-pool
                    get-initial-size-of-process-pool 
                    get-process-pool-size
                    describe-query-processing-mode)))
  (n)
  (when (or (null n)
            (and n 
                 (integerp n)
                 (>= n *min-pool-size*)))
    (setf *max-pool-size* n))
  *max-pool-size*)


#+:process-pooling
(nrql-defun (get-initial-size-of-process-pool 
             :doc ((:doc-type :short)
                   (:category :querying-modes)
                   (:inverse-of set-initial-size-of-process-pool)))
  ()
  *min-pool-size*)

#+:process-pooling
(nrql-defun (get-maximum-size-of-process-pool 
             :doc ((:doc-type :short)
                   (:category :querying-modes)
                   (:inverse-of set-maximum-size-of-process-pool)))
  ()
  *max-pool-size*)

#+:process-pooling
(nrql-defun (get-process-pool-size 
             :doc ((:doc-type :long)
                   (:category :querying-modes)
                   (:description "Returns the current number of
                                                                                                                                                           processes in the process pool (the process pool size)")
                   (:syntax (set-process-pool-size))
                   (:arguments )
                   (:values "the pool size, a natural number")
                   (:remarks)
                   (:examples)
                   (:see-also  set-initial-size-of-process-pool
                    get-initial-size-of-process-pool 
                    set-maximum-size-of-process-pool
                    get-maximum-size-of-process-pool
                    set-initial-size-of-process-pool
                    get-initial-size-of-process-pool
                    describe-query-processing-mode)))
                   
  ()
  (length *process-pool*))

;;;
;;; SQL Substrate
;;;

#+:sql-substrate
(nrql-defun (enable-sql-data-substrate-mirroring) ()
  (setf *type-of-substrate* 'mirror-sql-data-substrate)
  (setf *initial-abox-mirroring-p* t)
  :okay-sql-data-substrate-mirroring-enabled)

#+:sql-substrate
(nrql-defun (disable-sql-data-substrate-mirroring) ()
  (setf *type-of-substrate* 'racer-dummy-substrate)
  :okay-sql-data-substrate-mirroring-disabled)


;;;
;;; Describe 
;;; 


#+:racer-server
(nrql-defun (describe-current-substrate 
             :doc ((:doc-type :long)
                   (:category :substrate-layer)
                   (:description "Returns a description of the current substrate
                                                                                                                                                           used for query answering") 
                   (:syntax (describe-current-substrate))
                   (:arguments )
                   (:values "The description, a structured list")
                   (:remarks)
                   (:examples)
                   (:see-also describe-query-processing-mode describe-query
                    describe-rule)))
  ()
  (if *cur-substrate*
      (describe-substrate-int *cur-substrate*)
    :not-found))


(nrql-defun (describe-query-processing-mode 
             :doc ((:doc-type :long)
                   (:category :querying-modes)
                   (:description "Returns a description of the
                                                                                                                                                           current (global) nRQL settings resp. query processing mode")
                   (:syntax (describe-query-processing-modes))
                   (:arguments )
                   (:values "The description, a structured list")
                   (:remarks)
                   (:examples)
                   (:see-also describe-query 
                    describe-current-substrate
                    set-nrql-mode with-nrql-settings
                    execute-query)))
  ()
  (let ((mode 
         (list

          (list :creating-substrates-of-type
                (to-keyword *type-of-substrate*))

          (if (not *toggle-una-variables-p*)
              :using-?-prefix-for-injective-variables
            :using-$?-prefix-for-injective-variables)
          
          (when *auto-add-top-conjuncts-p* 
            :automatically-adding-top-conjuncts-for-free-variables)
          
          (when *check-abox-consistency-p*
            :check-abox-consistency)
          
          (when *use-individual-synonyms-p*
            :using-individual-equivalence-classes)
          
          (if *use-individual-synonyms-p*
              :same-as-is-semantic-same-as
            :same-as-is-syntactic-same-as)

          (when *ensure-tbox-classification-p*
            :classify-tbox)

          (when *exclude-permutations-p*
            :exclude-permutations)

          (when *optimize-p* 
            :query-optimization-enabled)

          (when *rewrite-defined-concepts-p* 
            :query-expansion-of-defined-concepts-enabled)

          (when (and *optimize-p* 
                     *optimizer-use-cardinality-heuristics-p*)
            :optimizer-uses-cardinality-heuristics)

          (when (and *optimize-p* 
                     *optimizer-ensure-late-lambda-evaluation-p*)
            :optimizer-ensures-late-lambda-atom-evaluation)
          
          (when *add-rule-consequences-p*
            :automatically-adding-rule-consequences)

          (when *warnings-p*
            :warnings)

          (if *told-information-reasoning-p*
              :incomplete-mode
            :complete-mode)

          (when *initial-abox-mirroring-p* 
            (if *ensure-tbox-classification-p* 
                (if *classify-concepts-in-instance-assertions-p*
                    :very-smart-abox-mirroring
                  :smart-abox-mirroring)
              :abox-mirroring))

          (when *allow-multiple-definitions-p*
            :allow-overloaded-defined-queries)
          
	  (if *disable-defined-queries-p*
	      :defined-queries-disabled
	    :defined-queries-enabled)

          (when *always-prefer-defined-queries-p*
            :always-prefering-defined-queries)

          (when *keep-expanded-predicates-p*
            :keeping-defined-query-atoms-in-expansions)

          (when *lazy-query-unfolding-p*
            :lazy-query-unfolding-and-allow-cycles)

          (let ((vector 
                 (list *ensure-tbox-classification-p*
                       *classify-concepts-in-instance-assertions-p*
                       *told-information-reasoning-p*
                       *two-phase-processing-p*)))

            (cond ((equal vector '(nil nil t nil)) :mode-0)
                  ((equal vector '(t   nil t nil)) :mode-1)
                  ((equal vector '(t   t   t nil)) :mode-2)
                  ((equal vector '(t   nil nil t)) 
                   (if *tuple-at-a-time-p*
                       :mode-4
                     :mode-6))
                  ((equal vector '(t   t   nil t)) :mode-5)
                  (t :mode-3))))))

    (remove nil
            (append mode
                    
                    (if *tuple-at-a-time-p*
                        (cons :tuple-at-a-time-mode
                              (cons
                               (if *proactive-tuple-computation-p*
                                   :eager
                                 :lazy)
                               (when *two-phase-processing-p* 
                                 `(:two-phase-query-processing-mode
                                   ,@(when *deliver-phase-two-warning-tokens-p*
                                       '(:deliver-phase-two-warning-tokens))))))
                      '(:set-at-a-time-mode))

                    (when *deliver-kb-has-changed-warning-tokens-p*
                      (list :deliver-kb-has-changed-warning-tokens))

                    (when *add-role-assertions-for-datatype-properties-p*
                      (list :adding-role-assertions-for-datatype-properties))

                    (when (or *timeout*
                              #+:racer-server *server-timeout*)
                      (list (list :timeout-after 
                                  (or *timeout*
                                      #+:racer-server *server-timeout*)
                                  :seconds)))

                    (when *how-many*
                      (list (list :max-no-of-tuples *how-many*)))
                    
                    (when *report-inconsistent-queries-p*
                      (list :reporting-inconsistent-queries))
                    
                    (when *report-tautological-queries-p*
                      (list :reporting-tautological-queries))
                    
                    (when *rewrite-semantically-p* 
                      (list :query-realization-enabled))
                     
                    (when *use-repository-p*
                      (list :maintain-query-repository))))))
          
;;;
;;; Reasoning 
;;;

(nrql-defun (report-inconsistent-queries-and-rules 
             :doc ((:doc-type :long)
                   (:category :inference)
                   (:inverse-of dont-report-inconsistent-queries-and-rules)
                   (:description "Advises nRQL (globally) to
                                                                                                                                                           automatically check freshly prepared queries and/or rules using
                                                                                                                                                           \\funref{query-consistent-p} and report inconsistent queries and/or 
                                                                                                                                                           rules. Inconsistent queries return no answers and are thus a waste of
                                                                                                                                                           CPU cycles")
                   (:syntax (report-inconsistent-queries-and-rules))
                   (:arguments )
                   (:values "{\\tt   :okay-reporting-inconsistent-queries-and-rules}")
                   (:remarks "A query / rule is check for consistency
                                                                                                                                                           when it is prepared, see \\funref{racer-prepare-query} (resp.
                                                                                                                                                                                                                   \\funref{racer-prepare-rule})")
                   (:examples)
                   (:see-also ;; report-tautological-queries-and-rules
                    enable-query-repository enable-query-realization
                    describe-query-processing-mode)))
  ()
  (setf *rewrite-to-dnf-p* t
        *warnings-p* t
        *report-inconsistent-queries-p* t)

  :okay-reporting-inconsistent-queries-and-rules)

(nrql-defun (dont-report-inconsistent-queries-and-rules 
             :doc ((:doc-type :short)
                   (:category :inference)
                   (:inverse-of report-inconsistent-queries-and-rules)))
  ()
  (setf *report-inconsistent-queries-p* nil)

  :okay-not-reporting-inconsistent-queries-and-rules)


;;;
;;;
;;;

#|

(nrql-defun (report-tautological-queries-and-rules 
             :doc ((:doc-type :long)
                   (:category :inference)
                   (:inverse-of dont-report-tautological-queries-and-rules)
                   (:description "Advises nRQL (globally) to report
                                                                                                                                                           tautological queries and/or rules. Tautological are too general and
                                                                                                                                                           thus not very meaningful")
                   (:syntax (report-tautological-queries-and-rules))
                   (:arguments )
                   (:values "{\\tt   :okay-reporting-tautological-queries-and-rules}")
                   (:remarks "A query / rule is check for consistency
                                                                                                                                                           when it is prepared, see \\funref{racer-prepare-query} (resp.
                                                                                                                                                                                                                   \\funref{racer-prepare-rule})")
                   (:examples)
                   (:see-also report-tautological-queries-and-rules
                    enable-query-repository enable-query-realization
                    describe-query-processing-mode)))
  ()
  (setf *rewrite-to-dnf-p* t
        *warnings-p* t
        *report-tautological-queries-p* t)

  :okay-reporting-tautological-queries-and-rules)

(nrql-defun (dont-report-tautological-queries-and-rules 
             :doc ((:doc-type :short)
                   (:category :inference)
                   (:inverse-of report-tautological-queries-and-rules)))
  ()
  (setf *report-tautological-queries-p* nil)

  :okay-not-reporting-tautological-queries-and-rules)

|#

;;;
;;; 
;;;

(nrql-defmethod (query-consistent-p
                 :doc ((:doc-type :long)
                       (:category :inference)
                       (:description "Checks the consistency of the query \\argument{id}")
                       (:syntax (query-consistent-p id))
                       (:arguments 
                        (:first id "the ID of the query, or {\\tt
                                                                                                                                                           :last} or {\\tt :last-query}"))
                       (:values "{\\tt t}, {\\tt nil} or {\\tt :dont-know}")
                       (:examples)
                       ;(:see-also query-tautological-p)
                       ))
  ((query null) &key &allow-other-keys)
  :not-found)

(nrql-defmethod (query-consistent-p)
  ((query symbol) &key &allow-other-keys)
  (with-racer-timeout
    (query-consistent-p (find-query query))))

;;;
;;;
;;;

(nrql-defmethod (rule-consistent-p
                 :doc ((:doc-type :short)
                       (:category :inference)
                       (:rule-equivalent-of :query-consistent-p)))
  ((query null) &key &allow-other-keys)
  :not-found)

(nrql-defmethod (rule-consistent-p)
  ((query symbol) &key &allow-other-keys)
  (with-racer-timeout
    (rule-consistent-p (find-rule query))))


;;;
;;;
;;;

#|
 
(nrql-defmethod (query-tautological-p
                 :doc ((:doc-type :long)
                       (:category :inference)
                       (:description "Checks whether the query \\argument{id} is tautological")
                       (:syntax (query-tautological-p id))
                       (:arguments 
                        (:first id "the ID of the query, or {\\tt
                                                                                                                                                           :last} or {\\tt :last-query}"))
                       (:values "{\\tt t}, {\\tt nil} or {\\tt :dont-know}")
                       (:examples)
                       (:see-also query-consistent-p)))

  ((query null) &key &allow-other-keys)
  :not-found)

(nrql-defmethod (query-tautological-p) ((query symbol) &key &allow-other-keys)
  (with-racer-timeout
    (query-tautological-p (find-query query))))


;;;
;;;
;;;


(nrql-defmethod (rule-tautological-p
                 :doc ((:doc-type :short)
                       (:category :inference)
                       (:rule-equivalent-of :query-tautological-p)))
  ((query null) &key &allow-other-keys)
  :not-found)

(nrql-defmethod (rule-tautological-p)
  ((query symbol) &key &allow-other-keys)
  (with-racer-timeout
    (rule-tautological-p (find-rule query))))

|# 

;;;
;;;
;;;

(nrql-defmethod (classify-query
                 :doc ((:doc-type :long)
                       (:category :inference)
                       (:description "Classifies the query \\argument{id}, i.e., computed 
                                                                                                                                                           its correct position in the current QBox") 
                       (:syntax (classify-query id))
                       (:arguments 
                        (:first id "the ID of the query, or {\\tt
                                                                                                                                                           :last} or {\\tt :last-query}"))
                       (:values "{\\tt :classfied} or {\\tt :dont-know}")
                       (:remarks)
                       (:examples)
                       (:see-also query-entails-p query-equivalent-p 
                        query-parents query-equivalents query-children
                        query-ancestors query-descendants)))
  ((query null))
  :not-found)

(nrql-defmethod (classify-query) ((query symbol)) 
  (classify-query (find-query query)))

(nrql-defmethod (classify-query) ((query query))
  (with-racer-timeout
    (classify query)))

;;;
;;;
;;;


(nrql-defmethod (query-entails-p
                 :doc ((:doc-type :long)
                       (:category :inference)
                       (:description "Checks whether query \\argument{id1}
                                                                                                                                                           entails (is more specific than) query \\argument{id2}")
                       (:syntax (query-entails-p id1 id2))
                       (:arguments 
                        (:first id1 "the ID of a query, or {\\tt
                                                                                                                                                           :last} or {\\tt :last-query}")
                        (:second id2 "the ID of a query, or {\\tt
                                                                                                                                                           :last} or {\\tt :last-query}"))
                       (:values "{\\tt t}, {\\tt nil} or {\\tt :dont-know}")
                       (:remarks)
                       (:examples)
                       (:see-also query-consistent-p rule-consistent-p
                        ;;; query-tautological-p rule-tautological-p
                        )))
  ((a symbol) (b symbol) &rest args &key &allow-other-keys)
  (declare (ignorable args))
  (with-racer-timeout
    (query-entails-p (find-query a) (find-query b))))

(nrql-defmethod (query-entails-p)
  ((a symbol) (b null) &rest args &key &allow-other-keys)
  (declare (ignorable args))
  :not-found)

(nrql-defmethod (query-entails-p)
  ((a null) (b symbol) &rest args &key &allow-other-keys)
  (declare (ignorable args))
  :not-found)

(nrql-defmethod (query-entails-p)
  ((a null) (b null) &rest args &key &allow-other-keys)
  (declare (ignorable args))
  :not-found)

;;;
;;;
;;;

(nrql-defmethod (query-equivalent-p
                 :doc ((:doc-type :long)
                       (:category :inference)
                       (:description "Checks whether the two queries
                                                                                                                                                           \\argument{id1} and \\argument{id2} mutually subsumes each other")
                       (:syntax (query-equivalent-p id1 ide2))
                       (:arguments
                        (:first id1 "the ID of a query, or {\\tt
                                                                                                                                                           :last} or {\\tt :last-query}")
                        (:second id2 "the ID of a query, or {\\tt
                                                                                                                                                           :last} or {\\tt :last-query}"))
                       (:values  "{\\tt t}, {\\tt nil} or {\\tt :dont-know}")
                       (:remarks)
                       (:examples)
                       (:see-also query-entails-p)))
  ((a symbol) (b symbol) &rest args &key &allow-other-keys)
  
  (declare (ignorable args))
  (with-racer-timeout
    (query-equivalent-p (find-query a) (find-query b))))

(nrql-defmethod (query-equivalent-p) 
  ((a symbol) (b null) &rest args &key &allow-other-keys)
  (declare (ignorable args))
  :not-found)

(nrql-defmethod (query-equivalent-p)
  ((a null) (b symbol) &rest args &key &allow-other-keys)
  (declare (ignorable args))
  :not-found)

(nrql-defmethod (query-equivalent-p)
  ((a null) (b null) &rest args &key &allow-other-keys)
  (declare (ignorable args))
  :not-found)

;;;
;;;
;;;

(nrql-defmethod (query-parents
                 :doc ((:doc-type :long)
                       (:category :inference)
                       (:description "Returns the IDs of the parent 
                                                                                                                                                           queries of the query \\argument{id} from the QBox. 
                                                                                                                                                           See Section 6.2.8 in the User Guide")
                       (:syntax (query-parents id))
                       (:arguments 
                        (:first id1 "the ID of a query, or {\\tt
                                                                                                                                                           :last} or {\\tt :last-query}"))
                       (:values "A list of query IDs -- the parents of the query")
                       (:remarks)
                       (:examples)
                       (:see-also query-children query-equivalents query-ancestors
                        query-descendants)))
  ((query null)) 
  :not-found)

(nrql-defmethod (query-parents) ((query symbol)) 
  (query-parents (find-query query)))

(nrql-defmethod (query-parents) ((query query)) 
  (with-racer-timeout
    (classify-query query)
    (remove nil (mapcar #'iterator-id 
                        (dag-node-parents 
                         (if (in-dag query) 
                             query
                           (first (equivalents query))))))))

;;;
;;;
;;;

(nrql-defmethod (query-ancestors
                 :doc ((:doc-type :short)
                       (:category :inference)
                       (:description "Like \\funref{query-parents}, but the ancestors 
                                                                                                                                                           (i.e., all subsuming resp. entailed queries) from the QBOx are returned")))

  ((query null)) 
  :not-found)

(nrql-defmethod (query-ancestors) ((query symbol)) 
  (query-ancestors (find-query query)))

(nrql-defmethod (query-ancestors) ((query query)) 
  (with-racer-timeout
    (classify-query query)
    (remove nil (mapcar #'iterator-id 
                        (dag-node-ancestors
                         (if (in-dag query) 
                             query
                           (first (equivalents query))))))))

;;;
;;;
;;;

(nrql-defmethod (query-children
                 :doc ((:doc-type :long)
                       (:category :inference)
                       (:description "Returns the IDs of the children  
                                                                                                                                                           queries of the query \\argument{id} from the QBox. 
                                                                                                                                                           See Section 6.2.8 in the User Guide")
                       (:syntax (query-children id))
                       (:arguments 
                        (:first id1 "the ID of a query, or {\\tt
                                                                                                                                                           :last} or {\\tt :last-query}"))
                       (:values "A list of query IDs -- the children of the query")
                       (:remarks)
                       (:examples)
                       (:see-also query-parents query-equivalents query-ancestors
                        query-descendants)))

  ((query null)) 
  :not-found)

(nrql-defmethod (query-children) ((query symbol)) 
  (query-children (find-query query)))

(nrql-defmethod (query-children) ((query query))
  (with-racer-timeout
    (classify-query query)
    (remove nil (mapcar #'iterator-id
                        (dag-node-children
                         (if (in-dag query) 
                             query
                           (first (equivalents query))))))))

;;;
;;;
;;;

(nrql-defmethod (query-descendants
                 :doc ((:doc-type :short)
                       (:category :inference)
                       (:description "Like \\funref{query-parents}, but the ancestors 
                                                                                                                                                           (i.e., all subsuming resp. entailed queries) from the QBOx are returned")))
  ((query null)) 
  :not-found)

(nrql-defmethod (query-descendants) ((query symbol)) 
  (query-descendants (find-query query)))

(nrql-defmethod (query-descendants) ((query query)) 
  (with-racer-timeout
    (classify-query query)
    (remove nil (mapcar #'iterator-id 
                        (dag-node-descendants 
                         (if (in-dag query) 
                             query
                           (first (equivalents query))))))))

;;;
;;;
;;;

(nrql-defmethod (query-equivalents
                 :doc ((:doc-type :long)
                       (:category :inference)
                       (:description "Returns the IDs of the synonym /
                                                                                                                                                           equivalent queries of the query \\argument{id} from the QBox.  See
                                                                                                                                                           Section 6.2.8 in the User Guide")
                       (:syntax (query-children id))
                       (:arguments 
                        (:first id1 "the ID of a query, or {\\tt
                                                                                                                                                           :last} or {\\tt :last-query}"))
                       (:values "A list of query IDs -- the synonym / equivalent queries")
                       (:remarks)
                       (:examples)
                       (:see-also query-parents query-equivalents query-ancestors
                        query-descendants)))

  ((query null)) 
  :not-found)

(nrql-defmethod (query-equivalents) ((query symbol)) 
  (query-equivalents (find-query query)))

(nrql-defmethod (query-equivalents) ((query query)) 
  (with-racer-timeout
    (classify-query query)
    (mapcar #'iterator-id 
            (remove nil 
                    (remove-duplicates
                     (remove query
                             (cons (first (equivalents query))
                                   (equivalents 
                                    (if (in-dag query) 
                                        query
                                      (first (equivalents query)))))))))))

;;;
;;; QBox 
;;; 
;;;

(nrql-defmethod (show-qbox-for-abox 
                 :doc ((:doc-type :long)
                       (:category :query-repository)
                       (:description "Prints the DAG of the QBox for
                                                                                                                                                           the \\argument{abox} as a tree on STDOUT")
                       (:syntax (show-qbox-for-abox &optional abox definitions-p ))
                       (:arguments
                        (:optional  abox (current-abox) "the Abox whose QBox shall be printed")
                        (:definitions-p nil "if {\\tt t}, then the query bodies will be shown"))
                       (:values "{\\tt :see-output-on-stdout}")
                       (:remarks)
                       (:examples)
                       (:see-also get-dag-of-qbox-for-abox get-nodes-in-qbox-for-abox)))

  (&optional (abox (current-abox)) definitions-p)

  (show-qbox-for-abox1 abox definitions-p))


(nrql-defmethod (get-dag-of-qbox-for-abox
                 :doc ((:doc-type :long)
                       (:category :query-repository)
                       (:description "Returns the DAG of the QBox for
                                                                                                                                                           the \\argument{abox} as a structured list")
                       (:syntax (get-dag-of-qbox-for-abox abox &optional abox))
                       (:arguments 
                        (:optional  abox (current-abox) "the Abox whose QBox shall be returned"))
                       (:values "the DAG as a structured list")
                       (:remarks)
                       (:examples)
                       (:see-also show-qbox-for-abox get-nodes-in-qbox-for-abox)))
  
  (&optional (abox (current-abox)))
  
  (get-dag-of-qbox-for-abox1 abox))

(nrql-defmethod (get-nodes-in-qbox-for-abox 
                 :doc ((:doc-type :long)
                       (:category :query-repository)
                       (:description "Returns the DAG nodes (queries) of the QBox for the \\argument{abox}")
                       (:syntax (get-nodes-in-qbox-for-abox &optional abox))
                       (:arguments 
                        (:optional  abox (current-abox) "the Abox whose nodes shall be returned"))
                       (:values  "a list of the nodes (queries) from the QBox"
                        (:remarks)
                        (:examples)
                        (:see-also get-dag-of-qbox-for-abox show-qbox-for-abox))))

  (&optional (abox (current-abox)))

  (get-nodes-in-qbox-for-abox1 abox))

;;;
;;;
;;; 

(nrql-defun (enable-query-repository 
             :doc ((:doc-type :short)
                   (:category :query-repository)
                   (:description "Enables the QBox query repository facility")
                   (:inverse-of disable-query-repository)))
  ()

  (report-inconsistent-queries-and-rules)

  (setf *rewrite-to-dnf-p* t
        *warnings-p* t
        *put-into-repository-p* t
        *use-repository-p* t)

  :okay-query-repository-enabled)

(nrql-defun (disable-query-repository 
             :doc ((:doc-type :short)
                   (:category :query-repository)
                   (:description "Disables the QBox query repository facility")
                   (:inverse-of enable-query-repository)))
  ()
  (setf *use-repository-p* nil
        *put-into-repository-p* nil)

  :okay-query-repository-disabled)



;;;
;;; Query Realization 
;;;


(nrql-defun (enable-query-realization 
             :doc ((:doc-type :short)
                   (:category :inference)
                   (:description "Enables query realization")
                   (:inverse-of disable-query-realization)))

  ()
  (setf *warnings-p* t
        *rewrite-to-dnf-p* t
        *rewrite-semantically-p* t)

  :okay-query-realization-enabled)

(nrql-defun (disable-query-realization
             :doc ((:doc-type :short)
                   (:category :inference)
                   (:description "Disables query realization")
                   (:inverse-of enable-query-realization)))
  ()
  (setf *rewrite-semantically-p* nil)

  :okay-query-realization-disabled)

;;;
;;; Settings
;;;


(nrql-defmacro (with-nrql-settings :nrql-function eval-nrql-settings 
                 :auto-quote-p t
                 :doc ((:doc-type :long)
                       (:show-corresponding-p nil)
                       (:category :querying-modes)
                       (:description "Establishes a lexical local
                                                                                                                                                           environment shaddowing the global environment in which the query 
                                                                                                                                                           answering switches resp. the corresponding variables are rebound to
                                                                                                                                                           specified values. The argument forms are not evaluated (see also 
                                                                                                                                                                                                                       \\funref{with-nrql-settings-evaluated}")
                       (:syntax (with-nrql-settings (&key 
                                                     mode 
                                                     
                                                     dont-show-variables
                                                     
                                                     dont-show-lambdas 
                                                     
                                                     dont-show-head-projection-operators 
                                                     
                                                     abox-mirroring
                                                     
                                                     query-optimization 
                                                     
                                                     optimizer-use-cardinality-heuristics 
                                                     
                                                     how-many-tuples 
                                                     
                                                     timeout
                                                     
                                                     warnings
                                                     
                                                     add-rule-consequences-automatically 
                                                     
                                                     dont-add-abox-duplicates
                                                     
                                                     two-phase-query-processing-mode 
                                                     
                                                     phase-two-starts-warning-tokens 
                                                     
                                                     kb-has-changed-warning-tokens 
                                                     
                                                     told-information-querying 
                                
                                                     tuple-computation-mode 
                                                     
                                                     exclude-permutations 
                                                     
                                                     query-repository 
                                                     
                                                     report-inconsistent-queries
                                                     
                                                     report-tautological-queries 
                                                     
                                                     query-realization
                                                     
                                                     bindings
                                
                                                     check-abox-consistency 
                                                     
                                                     use-individual-equivalence-classes

                                                     rewrite-to-dnf 
                                                     
                                                     type-of-substrate 
                                
                                                     abox
                                
                                                     tbox)))

                       (:arguments
                        (:key mode 3 "a natural number from 0 to 6, see \\funref{set-nrql-mode}")
                        (:key dont-show-variables nil "a list of variables, see also \\funref{get-answer} ")
                        (:key dont-show-lambdas nil "{\\tt t} or {\\tt nil}")
                        (:key dont-show-head-projection-operators nil "{\\tt t} or {\\tt nil}")
                        (:key abox-mirroring nil "{\\tt nil, t, :smart,} or {\\tt :very-smart}")
                        (:key query-optimization nil  "{\\tt t} or {\\tt nil}")
                        (:key optimizer-use-cardinality-heuristics t  "{\\tt t} or {\\tt nil}")
                        (:key how-many-tuples nil "a natural number, or {\\tt nil} (unbounded)")
                        (:key timeout "current server timeout" "a natural number (the timeout in milliseconds)")
                        (:key warnings t  "{\\tt t} or {\\tt nil}")
                        (:key add-rule-consequences-automatically t "{\\tt t} or {\\tt nil}")
                        (:key dont-add-abox-duplicates nil "{\\tt t} or {\\tt nil}")
                        (:key two-phase-query-processing-mode nil "{\\tt t} or {\\tt nil}")
                        (:key phase-two-starts-warning-tokens nil "{\\tt t} or {\\tt nil}")
                        (:key kb-has-changed-warning-tokens t "{\\tt t} or {\\tt nil}")
                        (:key told-information-querying nil "{\\tt t} or {\\tt nil}")
                        (:key tuple-computation-mode :set-at-a-time "{\\tt :set-at-a-time} or {\\tt :tuple-at-a-time}")
                        (:key exclude-permutations nil  "{\\tt t} or {\\tt nil}")
                        (:key query-repository nil "{\\tt t} or {\\tt nil}")
                        (:key report-inconsistent-queries nil "{\\tt t} or {\\tt nil}")
                        (:key report-tautological-queries nil "{\\tt t} or {\\tt nil}")
                        (:key query-realization nil "{\\tt t} or {\\tt nil}")
                        (:key bindings nil "a list of (variable value) bindings for variables. These variables will
                                                                                                                                                                                                                       be prebound and treated as individuals, see \\funref{with-bindings}, \\funref{with-future-bindings}")
                        (:key check-abox-consistency nil  "{\\tt t} or {\\tt nil}")
                        (:key use-individual-equivalence-classes nil  "{\\tt t} or {\\tt nil}")
                        (:key rewrite-to-dnf t  "{\\tt t} or {\\tt nil}")
                        (:key type-of-substrate racer-dummy-substrate "one of: \\argument{racer-dummy-substrate,
                                                                                                                                                                                                                       data-substrate, mirror-data-substrate, rcc-substrate,
                                                                                                                                                                                                                       racer-tbox-mirror-substrate}")
                        (:key abox (current-abox) "a symbol, the name of the Abox")
                        (:key tbox (current-tbox) "a symbol, the name of the Abox"))
                       
                       (:values "Like {\\tt progn} in Common LISP, so the value of the last embedded form is returned")
                       (:remarks "This is the recommended way to
                                                                                                                                                                                                                       temporarily / lexically change the global settings, i.e., if you want
                                                                                                                                                                                                                       to execute a query in a mode which is different from the current
                                                                                                                                                                                                                       global mode (see \\funref{describe-query-processing-mode}), but you
                                                                                                                                                                                                                       don't want to alter the global settings.  For altering the global
                                                                                                                                                                                                                       setting, note that there are corresponding setter and getter
                                                                                                                                                                                                                       functions. For example, \\funref{set-nrql-mode} corresponds to the
                                                                                                                                                                                                                       \\argument{mode} argument, etc.  Moreover, the global settings can
                                                                                                                                                                                                                       also be changed using the corresponding keyword arguments of nRQL main
                                                                                                                                                                                                                       functions\\funref{racer-prepare-query}, \\funref{execute-query},
                                                                                                                                                                                                                       \\funref{get-answer}. However, using {\\tt with-nrql-settings} for
                                                                                                                                                                                                                       local settings ensures that the settings will be made in a consistent
                                                                                                                                                                                                                       way")
                       (:examples
                        (with-nrql-settings (:mode 0 :abox test) 
                          (describe-query-processing-mode)
                          (retrieve (?x) (?x top))))
                       (:see-also describe-query-processing-mode 
                        with-nrql-settings-evaluated 
                        set-nrql-mode
                        execute-query racer-prepare-query get-answer))))

(nrql-defmacro (with-nrql-settings-evaluated :nrql-function eval-nrql-settings 
                 :auto-quote-p nil

                 :doc ((:doc-type :short)
                       (:show-corresponding-p nil)
                       (:category :querying-modes)
                       (:description "Like \\funref{with-nrql-settings}, but now the argument
                                                                                                                                                                                                                       forms are evaluated"))))


;;;
;;;
;;; 

(defun1 eval-with-bindings (body &rest bindings)
  (let ((*established-bindings* bindings))
    (funcall body)))

(defun1 eval-with-future-bindings (body &rest bindings)
  (let ((*established-bindings* (mapcar #'list bindings)))
    (funcall body)))

(nrql-defmacro (with-bindings :nrql-function eval-with-bindings
                 :auto-quote-p t
                 :doc ((:doc-type :long)
                       (:show-corresponding-p nil)
                       (:category :abox-queries)
                       (:description "If a nRQL query is executed in a
                                                                                                                                                                                                                       \\funref{with-bindings} lexical environment, then the variables in the
                                                                                                                                                                                                                       query is considered to be bound as established here")

                       (:syntax (with-bindings binding-list))
                       (:arguments 
                        (:first binding-list nil 
                         "A list of (variable individual) entries"))
                       (:values )
                       (:remarks)
                       (:examples (with-bindings ((?x ind-123) (?z ind-456)) (retrieve (?y) (?x ?y r)))
                        (:see-also with-bindings-evaluated with-future-bindings )
                        ))))


(nrql-defmacro (with-bindings-evaluated :nrql-function eval-with-bindings 
                 :auto-quote-p nil
                 :doc ((:doc-type :short)
                       (:show-corresponding-p nil)
                       (:category :abox-queries)
                       (:description "Like \\funref{with-bindings},
                                                                                                                                                                                                                       but now the individual forms (entries) in \\argument{binding-list} are
                                                                                                                                                                                                                       evaluated to produce the individual (variable value) pairs, e.g., use
                                                                                                                                                                                                                       {\\tt (with-bindings-evaluated ( (list '?x 'ind-123) ) ...)}"))))


(nrql-defmacro (with-future-bindings :nrql-function eval-with-future-bindings
                 :auto-quote-p t
                 :doc ((:doc-type :long)
                       (:show-corresponding-p nil)
                       (:category :abox-queries)
                       (:description "Sometimes, a query must shall be
                                                                                                                                                                                                                       prepared (i.e., parsed and compiled) with a promsie that at execution
                                                                                                                                                                                                                       time, binding to certain variables in that query will be eastablished,
                                                                                                                                                                                                                       this means, if \\funref{execute-query} is called in a lexical
                                                                                                                                                                                                                       environment where certain variables are bound in advance with
                                                                                                                                                                                                                       \\funref{with-bindings}.  During prepartion time, the query optimizer
                                                                                                                                                                                                                       must thus be informed that these variables are in fact treated as
                                                                                                                                                                                                                       individuals. This is what \\funref{with-future-bindings} does: It declares
                                                                                                                                                                                                                       the \\argument{variables} to be individuals, and for query execution promisses 
                                                                                                                                                                                                                       that these variables will be bound with \\funref{with-bindings} priorily")
                       (:syntax (with-future-bindings variables))
                       (:arguments 
                        (:first variables "a list of variables"))
                       (:values "Like {\\tt progn} in Common LISP, so the value of the last embedded form is returned")
                       (:remarks)
                       (:examples
                        (with-future-bindings (?x)
                          (prepare-abox-query (?x ?y) (?x ?y r))))
                       (:see-also with-future-bindings-evaluated))
                 ))

(nrql-defmacro (with-future-bindings-evaluated :nrql-function eval-with-future-bindings 
                 :auto-quote-p nil
                 :doc ((:doc-type :short)
                       (:show-corresponding-p nil)
                       (:category :querying-modes)
                       (:description "Like
                                                                                                                                                                                                                       \\funref{with-future-bindings}, but now \\argument{list-of-variables} is
                                                                                                                                                                                                                       evaluated to produce the list of variables"))))

;;:
;;;
;;;

(defun eval-nrql-settings2 (body &rest args) 
  
  ;; hier war mal angedacht, fuer die lokalen Keyword-Argumente
  ;; der Hauptfunktionen ebenfalls die Keyword-Argumente aus
  ;; with-nrql-settings zu nehmen. 
  ;; Wurde nicht weiter verfolgt, weil nicht klar war, wie 
  ;; with-nrql-settings geschachtelt bzw. Modi sich "ueberschreiben" sollen
  
  (declare (ignorable args))
  (funcall body))


(defun1 eval-nrql-settings (body &key (mode 3) ; f. die End-User, siehe Manual!

                                (dont-show-variables nil)

                                (dont-show-lambdas nil)
                                
                                (dont-show-head-projection-operators nil)

                                (abox-mirroring nil abox-mirroring-supplied-p)
                                 
                                ;; nil, t, :smart, :very-smart
                                
                                (query-optimization t) ; nil, t

                                (optimizer-use-cardinality-heuristics t) ; nil, t
                                
                                how-many-tuples ; nil, <n> 
                                
                                (timeout (or *timeout* 
                                             #+:racer-server *server-timeout*))

                                (warnings t)
                                
                                (add-rule-consequences-automatically t) ; nil 

                                (dont-add-abox-duplicates nil)
                                
                                two-phase-query-processing-mode ; nil, t
                                
                                phase-two-starts-warning-tokens ; nil, t
                                
                                (kb-has-changed-warning-tokens t) ; nil, t
                                
                                told-information-querying ; nil, t
                                
                                (tuple-computation-mode :set-at-a-time)
                                
                                exclude-permutations ; nil 
                                
                                query-repository ; nil, t

                                report-inconsistent-queries
                                
                                report-tautological-queries 
                                    
                                query-realization ; nil, t

                                bindings

                                (check-abox-consistency nil check-abox-consistency-supplied-p) 

                                (use-individual-equivalence-classes
                                 nil use-individual-equivalence-classes-supplied-p)

                                (rewrite-to-dnf t)
                                
                                (type-of-substrate
                                 #+:midelora
                                 'midelora-substrate
                                 #-:midelora
                                 ;;; 'racer-dummy-substrate
                                 *type-of-substrate*
                                 )
                                
                                abox
                                
                                tbox)

  (let* ((*tuple-at-a-time-p* 
          (when 
              (or (member mode '(4 5))
                  (case tuple-computation-mode
                    (:tuple-at-a-time-lazy t)
                    (:tuple-at-a-time-eager t)
                    (:set-at-a-time nil)
                    (otherwise 
                     (nrql-error "Bad keyword: ~A" tuple-computation-mode))))
            t))

         (*add-rule-consequences-p* 
          (when add-rule-consequences-automatically
            t))

         (*dont-add-abox-duplicates-p*
          dont-add-abox-duplicates)

         (*two-phase-processing-p* 
          (when (or (and *tuple-at-a-time-p*
                         two-phase-query-processing-mode)
                    (member mode '(6)))
            t))

         (*proactive-tuple-computation-p* 
          (when (or (not *tuple-at-a-time-p*) ; set-at-a-time is immer proactive!
                    (eq tuple-computation-mode  :tuple-at-a-time-eager))
            t))

         (*initial-abox-mirroring-p* 
          (if abox-mirroring-supplied-p
              abox-mirroring
            ;;; alle bis auf mode 3! 
            (member mode '(0 1 2 4 5 6))))
         
         (*deliver-phase-two-warning-tokens-p*
          (when (and *two-phase-processing-p*
                     phase-two-starts-warning-tokens)
            t))

         (*deliver-kb-has-changed-warning-tokens-p*
          kb-has-changed-warning-tokens)

         (*ensure-tbox-classification-p* 
          (when (or (member mode '(1 2 4 5 6))
                    (cond ((eq abox-mirroring :smart) t)
                          ((eq abox-mirroring :very-smart) t)
                          ((eq abox-mirroring nil) nil)
                          ((eq abox-mirroring t) nil)
                          (t (nrql-error "Bad keyword: ~A" abox-mirroring))))
            t))

         (*classify-concepts-in-instance-assertions-p* 
          (when (or (eq abox-mirroring :very-smart)
                    (member mode '(2 5)))
            t))
         
         (*told-information-reasoning-p* 
          (when (or told-information-querying
                    (member mode '(0 1 2)))
            t))

         (*optimize-p* 
          (when query-optimization t))

         (*optimizer-use-cardinality-heuristics-p* 
          (when (and *optimize-p* optimizer-use-cardinality-heuristics)
            t))

         (*rewrite-to-dnf-p* 
          (when (or rewrite-to-dnf
                    query-optimization
                    report-inconsistent-queries
                    report-tautological-queries
                    query-repository
                    query-realization)
            t))

         (*report-inconsistent-queries-p* 
          (when report-inconsistent-queries t))

         (*report-tautological-queries-p* 
          (when report-tautological-queries t))
         
         (*warnings-p*
          (when (or report-inconsistent-queries 
                    report-tautological-queries
                    warnings)
            t))
          
         (*use-repository-p*
          (when query-repository t))

         (*put-into-repository-p* 
          *use-repository-p*)

         (*rewrite-semantically-p*
          (when query-realization t))

         (*exclude-permutations-p* 
          (when exclude-permutations t))

         (*check-abox-consistency-p*
          (if (not check-abox-consistency-supplied-p)
              (member mode '(3 6))
            check-abox-consistency))

         (*use-individual-synonyms-p* 
          (if use-individual-equivalence-classes-supplied-p 
              use-individual-equivalence-classes
            *use-individual-synonyms-p*))

         (*how-many* 
          how-many-tuples)

         (*dont-show-variables* dont-show-variables)

         (*dont-show-lambdas-p* dont-show-lambdas)
         
         (*dont-show-head-projections-operators-p* dont-show-head-projection-operators)

         #-:racer-server (*timeout* timeout) ; *timeout* hat f. nRQL in Racer keine Relevanz
         #+:racer-server (*server-timeout* timeout) ; wichtig!

         (*nrql-abox* abox)

         (*nrql-tbox* tbox)

         (*type-of-substrate* type-of-substrate)

         (*established-bindings* (append bindings
                                         *established-bindings*)))
    
    (funcall body)))

;;;
;;;
;;;

(nrql-defun (set-nrql-mode 
             :doc ((:doc-type :long)
                   (:category :querying-modes)
                   (:description "Puts nRQL globally into mode \\argument{n}. See User Guide 
                                                                                                                                                                                                                       for a description of the different querying modes")
                   (:syntax (set-nrql-mode n))
                   (:arguments 
                    (:first n 3 "a natural number between 0 and 6"))
                   (:values "the mode")
                   (:remarks)
                   (:examples)
                   (:see-also describe-query-processing-mode with-nrql-settings)))

  (mode)

  (restore-standard-settings)

  (ecase mode
    (0 (enable-told-information-querying)
       (dont-check-abox-consistency-before-querying)
       :okay-mode-0)
    (1 (enable-told-information-querying)
       (dont-check-abox-consistency-before-querying)
       (enable-smart-abox-mirroring)
       :okay-mode-1)
    (2 (enable-told-information-querying)
       (dont-check-abox-consistency-before-querying)
       (enable-very-smart-abox-mirroring)
       :okay-mode-2)
    (3 :okay-mode-3)

    ;;; two-phase modes

    (4 (enable-two-phase-query-processing-mode)
       :okay-mode-4)

    (5 (enable-two-phase-query-processing-mode)
       (enable-very-smart-abox-mirroring)
       :okay-mode-5)
    
    ;;; two-phase, but set-at-a-time!
    ;;; verhindert evtl. internal-individuals-related-p
    ;;; Aufrufe in racer-retrieve-individual-fillers 

    (6 (enable-two-phase-query-processing-mode)
       (check-abox-consistency-before-querying)
       (enable-smart-abox-mirroring)
       (process-set-at-a-time)
       :okay-mode-6)))

;;;
;;;
;;;


(nrql-defun (restore-standard-settings
             :doc ((:doc-type :long)
                   (:category :querying-modes)
                   (:description "Restores the standard nRQL settings")
                   (:syntax (restore-standard-settings))
                   (:arguments )
                   (:values )
                   (:remarks)
                   (:examples)
                   (:see-also set-nrql-mode with-nrql-settings full-reset reset-nrql-engine describe-query-processing-mode)))
  ()
  (set-unique-name-assumption nil)

  #+:multiprocess-queries 
  (setf *multiprocess-queries* t)

  (setf *add-role-assertions-for-datatype-properties-p* nil

        *use-individual-synonyms-p* nil
        *toggle-una-variables-p* t 

        *allow-multiple-definitions-p* nil
        *always-prefer-defined-queries-p* nil
        *keep-expanded-predicates-p* nil
        *disable-defined-queries-p* nil
        *lazy-query-unfolding-p* nil        
        
        *tuple-at-a-time-p* nil
        *allow-negated-roles-p* t
        *proactive-tuple-computation-p* t
        *two-phase-processing-p* nil
        *deliver-kb-has-changed-warning-tokens-p* t
        *deliver-phase-two-warning-tokens-p* nil
        *optimize-p* t        
        *optimizer-use-cardinality-heuristics-p* t
        *optimizer-ensure-late-lambda-evaluation-p* t
        *warnings-p* t
        *told-information-reasoning-p* nil
        *report-inconsistent-queries-p* nil
        *report-tautological-queries-p* nil
        *rewrite-to-dnf-p* t

        *rewrite-defined-concepts-p* nil

        *rewrite-semantically-p* nil
        *use-repository-p* nil
        *put-into-repository-p* nil
        *use-unique-name-assumption-p* t
        *how-many* nil

        *dont-show-lambdas-p* nil
        *dont-show-variables* nil
        *dont-show-head-projections-operators-p* nil
        
        *classify-concepts-in-instance-assertions-p* nil
        *ensure-tbox-classification-p* nil
        *check-abox-consistency-p* t
        *exclude-permutations-p* nil
        *initial-abox-mirroring-p* nil
        *add-rule-consequences-p* t
        *dont-add-abox-duplicates-p* nil
        
        #-:midelora
        *type-of-substrate* 
        #-:midelora
        'racer-dummy-substrate
        
        #+:midelora
        *type-of-substrate*
        #+:midelora
        'midelora-substrate
        
        *rcc-type* :rcc8
        *timeout* nil
        *syntactic-repository-p* nil)
        
  :okay-standard-settings-restored)

;;;
;;;
;;;

(nrql-defun (check-concept-coherence) (concept &optional (tbox *current-tbox*))
  (check-concept-coherence-internal concept tbox))

(nrql-defun (check-ontology) (filename &key (verbose *tbox-verbose*) (explain-all nil) (n 1))
  (check-ontology-internal filename :verbose verbose :explain-all explain-all :n n))

;;;
;;;
;;;

(declaim (special *proxy*))

(nrql-defun (set-proxy-server) (proxy)
  (setf *proxy* proxy))

(nrql-defun (get-proxy-server) ()
  *proxy*)

;;;
;;;
;;;

(restore-standard-settings)

;;;
;;;
;;;


(nrql-defun (rmi) 
  (args)
  (declare (ignorable args))
  
  "Hallo Ralf")



