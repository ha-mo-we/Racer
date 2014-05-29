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

(defun check-if-unsafe () 
  #+:racer-server 
  (unless *unsafe-mode* 
    (nrql-error "Illegal operator. Only allowed with option -u (do not forget -- under Windows).")))

(defun make-substrate-fn (filename)
  (format nil "~A.SUB.IMG" filename))

(defun make-kb-fn (filename)
  (format nil "~A.KB.IMG" filename))


(nrql-defun (store-substrate-for-abox
             :doc ((:doc-type :long)
                   (:category :persistency-facility)
                   (:inverse-of restore-substrate)
                   (:description "Stores the substrate for ABox \\argument{abox}
of type (class) \\argument{type-of-substrate}")
                   (:syntax (store-substrate-for-abox filename &optional for-abox type-of-substrate))
                   (:arguments 
                    (:first filename "the filename")
                    (:optional for-abox (current-abox) "the ABox whose
substrate shall be stored")
                    (:optional type-of-substrate "'racer-dummy-substrate" "the type of the substrate to 
be stored. Note that  ABox and type uniquely identifies the substrate, since there is at most one substrate
of a certain type for an ABox"))
                   (:values :done)
                   (:remarks "Note that Racer must be running in
unsafe mode (i.e., file io must be allowed). Also note that the KB
(ABox, TBox) is also stored in the file; Racer creates two files: \\argument{<filename>.SUB.IMG}  
for the substrate, and \\argument{<filename>.SUB.IMG} for the KB using \\funref{store-kb-image}")
                   (:examples)
                   (:see-also store-all-substrates store-server-image)))

  (filename &optional (for-abox (current-abox)) (type-of-substrate *type-of-substrate*))

  (check-if-unsafe)

  (let ((substrate 
         (find-racer-substrate for-abox type-of-substrate)))

    (if substrate

        (progn 
          
          (persistence-manager:make-object-persistent (list substrate *all-dboxes*)
                                                      (make-substrate-fn filename))
          
          (store-kb-image (make-kb-fn filename) for-abox)

          :done)
        
      :not-found)))

(nrql-defun (store-all-substrates
             :doc ((:doc-type :long)
                   (:category :persistency-facility)
                   (:inverse-of restore-all-substrates)
                   (:description "Stores all substrates in a file \\argument{filename}")
                   (:syntax (store-all-substrates filename))
                   (:arguments 
                    (:first filename "the filename"))
                   (:values :done)
                   (:remarks "Note that Racer must be running in
                    unsafe mode (i.e., file io must be allowed). Racer also stores all KBs (ABoxes, TBoxes)
                    referenced by the substrates. Thus, Racer creates two files: \\argument{<filename>.SUB.IMG}  
                    contains the substrates, and \\argument{<filename>.SUB.IMG} contains the KBs (this image is store 
                                                                                                       with \\funref{store-kbs-image})")

                   (:examples)
                   (:see-also store-substrate-for-abox store-server-image)))             
	    
	    (filename)

	    (check-if-unsafe)

	    (persistence-manager:make-object-persistent (list *all-substrates*
							      *all-dboxes*
							      'default
							      'default)
							(make-substrate-fn filename))

	    (store-kbs-image (make-kb-fn filename) 
			     (cons 'default
                                   ;;; Store-kbs-image benoetigt mindestens eine KB,
                                   ;;; sonst gibt es einen Fehler beim Einlesen :-( 
                                   ;;; update: inzwischen behoben in Racer
				   (mapcar #'abox *all-substrates*)))

	    :done)

;;;
;;;
;;;

(nrql-defun (restore-substrate
             :doc ((:doc-type :long)
                   (:category :persistency-facility)
                   (:inverse-of store-substrate-for-abox)
                   (:description "Restores the substrate from the files \\argument{<filename>.SUB.IMG} and
                    \\argument{<filename>.KBS.IMG}")
                   (:syntax (restore-substrate filename))
                   (:arguments 
                    (:first filename "the filename"))
                   (:values "The name of the restored substrate")
                   (:remarks "Note that the referenced KB (ABox, TBox)
                    is also restored from the file \\argument{<filename>.KB.IMG} using
                    \\funref{restore-kb-image}")
                   (:examples)
                   (:see-also restore-all-substrates restore-server-image)))

	    (filename)
	    
	    (restore-kb-image (make-kb-fn filename))
	    
	    (let* ((obj (persistence-manager:load-persistent-object (make-substrate-fn filename)))
		   (substrate (first obj)))
	      
	      (dolist (dbox (second obj))
		(push dbox *all-dboxes*))
	      
	      (push substrate *all-substrates*)
	      
	      (when substrate
		(setf *cur-substrate* substrate 
		      *type-of-substrate* (type-of substrate)))
	      
	      (name substrate)))


(nrql-defun (restore-all-substrates
             :doc ((:doc-type :long)
                   (:category :persistency-facility)
                   (:inverse-of store-all-substrates)
                   (:description "Restores the substrates from the files \\argument{<filename>.SUB.IMG} as
                    well as they KBs they reference from \\argument{<filename>.KBS.IMG}")
                   (:syntax (restore-substrate filename))
                   (:arguments 
                    (:first filename "the filename"))
                   (:values "The names of the restored substrates")
                   (:remarks "Note that the referenced KBs (ABox, TBox)
                    are restored from the file \\argument{<filename>.KB.IMG} using
                    \\funref{restore-kbs-image}")
                   (:examples)
                   (:see-also restore-substrate restore-server-image)))
	    (filename)
	    
	    (restore-kbs-image (make-kb-fn filename))

	    (let* ((obj (persistence-manager:load-persistent-object (make-substrate-fn filename)))
		   (substrates (first obj)))
	      
	      (setf *all-dboxes* (second obj))
	      
	      (dolist (substrate substrates) 
		(push substrate *all-substrates*))
	      
	      (when substrates
		(setf *cur-substrate* (first substrates)
		      *type-of-substrate* (type-of (first substrates))))
	      
	      (set-current-tbox (third obj))
	      (set-current-abox (fourth obj))
	      
	      (mapcar #'name substrates)))


;;;
;;;
;;;

(declaim (special     
          owlapi:*reasoners* 
          owlapi:*cur-reasoner* 
          owlapi:*default-reasoner* 
          owlapi:*ano-concept* 
          owlapi:*default-reasoner-name*))


(nrql-defun (store-server-image
             :doc ((:doc-type :long)
                   (:category :persistency-facility)
                   (:inverse-of restore-server-image)
                   (:description "Stores the server state in the files 
\\argument{<filename>.SUB.IMG} and \\argument{<filename>.KBS.IMG}")
                   (:syntax (store-server-image filename))
                   (:arguments 
                    (:first filename "the filename"))
                   (:values :done)
                   (:remarks "This is like \\funref{store-all-substrate}, but all KBs (not only the KBs reference by the 
                                                                                           substrates) are stored in the image") 
                   (:examples)
                   (:see-also store-all-substrate store-substrate-for-abox)))
	    
	    (filename)

	    (check-if-unsafe)
         
	    (persistence-manager:make-object-persistent (list *all-substrates* 
							      *all-dboxes*
                                                              
							      (current-tbox)
							      (current-abox)
							      
							      owlapi:*reasoners* 
							      owlapi:*cur-reasoner* 
							      owlapi:*default-reasoner* 
							      ;;;owlapi:*ano-concept* 
							      owlapi:*default-reasoner-name*

                                                              *prefix-mappings* 
                                                              *cached-prefixes*
                                                              *uri-mirror-table*

                                                              *registered-functions*
                                                              *registered-values*
                                                              *registered-server-functions*)
                                                        
							(make-substrate-fn filename))

            (store-kbs-image (make-kb-fn filename) 
			     (all-aboxes))
	  
	    :done)


(nrql-defun (restore-server-image
             :doc ((:doc-type :long)
                   (:category :persistency-facility)
                   (:inverse-of store-server-image)
                   (:description "Restores a server image from the files \\argument{<filename>.SUB.IMG} and \\argument{<filename>.KBS.IMG}")
                   (:syntax (restore-server-image filename))
                   (:arguments 
                    (:first filename "the filename"))
                   (:values :done)
                   (:examples)
                   (:see-also restore-all-substrates restore-substrates)))

	    (filename)
	    
	    (restore-kbs-image (make-kb-fn filename))
	    
	    (let* ((obj (persistence-manager:load-persistent-object (make-substrate-fn filename)))
		   (substrates (pop obj)))
	      
	      (setf *all-dboxes* (pop obj))

	      (dolist (substrate substrates) 
		(push substrate *all-substrates*))
	      
	      (when substrates
		(setf *cur-substrate* (first substrates)
		      *type-of-substrate* (type-of (first substrates))))
	      
	      (set-current-tbox (pop obj))
	      (set-current-abox (pop obj))
	      
	      (setf owlapi:*reasoners* (pop obj)
		    owlapi:*cur-reasoner* (pop obj)
		    owlapi:*default-reasoner* (pop obj)
		    ;;owlapi:*ano-concept* (pop obj)
		    owlapi:*default-reasoner-name* (pop obj)

                    *prefix-mappings* (pop obj)
                    *cached-prefixes* (pop obj)
                    *uri-mirror-table* (pop obj)

                    *registered-functions* (pop obj)
                    *registered-values* (pop obj)
                    *registered-server-functions*  (pop obj))
	      
	      :done))


