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

(defun find-racer-substrate (name-of-substrate type-of-substrate
                                               &optional abox (tbox (when abox (associated-tbox abox))))
  (find-if #'(lambda (x) 
               (and (equal (type-of x) type-of-substrate)
                    (equal name-of-substrate (name x))
                    (=> abox (eq abox (abox x)))
                    (=> tbox (eq tbox (tbox x)))))
           *all-substrates*))

(defun find-racer-substrate1 (name-of-substrate type-of-substrate
                                                &optional abox (tbox (when abox (associated-tbox abox))))
  (find-if #'(lambda (x) 
               (and (typep x type-of-substrate)
                    (equal name-of-substrate (name x))
                    (=> abox (eq abox (abox x)))
                    (=> tbox (eq tbox (tbox x)))))
           *all-substrates*))

#+:midelora
(defun find-midelora-substrate (name-of-substrate type-of-substrate &optional abox (tbox (when abox (prover::associated-tbox abox))))
  (find-if #'(lambda (x) 
               (and (equal (type-of x) type-of-substrate)
                    (equal name-of-substrate (name x))
                    (=> abox (eq abox x))
                    (=> tbox (eq tbox (prover::tbox x)))))
           *all-substrates*))

#+:midelora
(defun find-midelora-substrate (name-of-substrate type-of-substrate &optional abox (tbox (when abox (prover::associated-tbox abox))))
  (find-if #'(lambda (x) 
               (and (typep x type-of-substrate)
                    (equal name-of-substrate (name x))
                    (=> abox (eq abox x))
                    (=> tbox (eq tbox (prover::tbox x)))))
           *all-substrates*))

;;;
;;;
;;;

(defun racer-prepare-substrate (&rest
                                initargs
                                &key
                                substrate
                                abox 
                                create-abox-if-not-found-p
                                (package *package*)
                                (type-of-substrate *type-of-substrate*)
                                prepare-now-p 
                                &allow-other-keys)

  (declare (ignorable initargs))

  (with-critical-section
  
   (let* ((abox (or abox 
                    *nrql-abox* 
                    (current-abox)))
          
          ;;; neu: Substrate-Namen sind 
          ;;; nicht mehr unabhaengig
          ;;; immer wie die assoziierte
          ;;; ABox benannt!
          
          (name-of-substrate abox))
     
     (if (not (find-abox abox nil))
         (if create-abox-if-not-found-p
             (init-abox abox)
           (nrql-runtime-error "Can't find ABox ~A" abox)))
     
     (let* ((tbox (associated-tbox abox))
            (substrate
             (or substrate
                 (find-racer-substrate name-of-substrate type-of-substrate abox tbox))))
       
       (setf *cur-substrate*
             (or substrate
                 
                 (make-instance type-of-substrate
                                :racer-package package
                                :name name-of-substrate
                                :tbox tbox
                                :abox abox)))
       
       ;;; der Kontext wird nuer fuers Parsen etc. 
       ;;; benoetigt, und dafuer wird ja kein neuer
       ;;; Prozess gestartet -> OKAY!
       
       (when prepare-now-p 
         ;;;  wird nur in besonderen Faellen verwendet, 
         ;;; z.B. in racer-critical-functions
         (prepare-substrate1 *cur-substrate*))
       
       (set-context-for *cur-substrate*)))))



#+:midelora
(defun midelora-prepare-substrate (&rest
                                   initargs
                                   &key
                                   abox 
                                   create-abox-if-not-found-p
                                   (type-of-substrate *type-of-substrate*)
                                   prepare-now-p 
                                   &allow-other-keys)

  (declare (ignorable initargs))

  (with-critical-section

   (let* ((name abox)
          (abox
           (prover::find-abox (or abox 
                                  *nrql-abox* 
                                  (prover::current-abox)))))
     
     (unless abox
       (unless create-abox-if-not-found-p
         (nrql-runtime-error "Can't find ABox ~A" name)))
     
     (let* ((tbox (if abox
                      (prover::associated-tbox abox)
                    prover::*cur-tbox*))
            
            (substrate 
             (or abox 
                 (find-midelora-substrate abox type-of-substrate abox tbox))))
       
       (setf *cur-substrate*
             (if substrate
               
                 (if (is-midelora-substrate-p substrate)
                     substrate
                   (etypecase substrate
                     ;;; most specific first!
                     (prover::abox1 
                      (change-class substrate 'midelora-substrate1))
                     (prover::abox
                      (change-class substrate 'midelora-substrate))))

               (prover::with-tbox* (tbox)
                 (prover::in-abox* abox 
                                   :delete-if-exists-p t
                                   :type type-of-substrate))))

       (when prepare-now-p 
         (prepare-substrate1 *cur-substrate*))
       
       (set-context-for *cur-substrate*)))))

;;;
;;;
;;;

(defun find-racer-tbox-substrate (tbox) 
  (find-if #'(lambda (x) 
               (and (and (typep x 'racer-tbox-mirror-substrate))
                    (eq tbox (mirror-of-tbox x))))
           *all-substrates*))


#+:midelora
(defun find-midelora-tbox-substrate (tbox) 
  (find-if #'(lambda (x) 
               (and (and (typep x 'midelora-tbox-mirror-substrate))
                    (eq tbox (mirror-of-tbox x))))
           *all-substrates*))

;;;
;;;
;;;

(defun racer-prepare-tbox-substrate (&key (tbox (or *nrql-tbox* (current-tbox)))
                                          (package *package*)
                                          create-tbox-if-not-found-p
                                          substrate
                                          &allow-other-keys)


  (with-critical-section
   (find-tbox tbox)
  
   (let ((substrate
          (or substrate 
              (find-racer-tbox-substrate tbox))))
  
     (if (not (find-tbox tbox nil))
         (if create-tbox-if-not-found-p
             (init-tbox tbox)
           (nrql-runtime-error "Can't find TBox ~A" tbox)))

     (setf *cur-substrate*
           (or substrate
                  
               (make-instance 'racer-tbox-mirror-substrate
                              :mirror-abox-p nil
                              :racer-package package
                              :error-p nil
                              :tbox tbox
                              :name tbox)))
      
     (set-context-for *cur-substrate*))))



#+:midelora
(defun midelora-prepare-tbox-substrate (&key (tbox (or *nrql-tbox* (prover::current-tbox)))
                                             create-tbox-if-not-found-p
                                             substrate
                                             &allow-other-keys)


  (with-critical-section
   (prover::find-tbox tbox)
  
   (let ((substrate
          (or (find-midelora-tbox-substrate tbox)
              substrate)))
  
     (if (not (prover::find-tbox tbox))
         (if create-tbox-if-not-found-p
             (prover::init-tbox tbox)
           (nrql-runtime-error "Can't find TBox ~A" tbox)))

     (setf *cur-substrate*
           (or substrate
                  
               (make-instance 'midelora-tbox-mirror-substrate
                              :tbox tbox
                              :name tbox)))
      
     (set-context-for *cur-substrate*))))

;;;
;;;
;;;

(defmethod get-state-vector ((substrate racer-substrate))
  (with-critical-section
   (list *initial-abox-mirroring-p*
         *initial-role-assertion-mirroring-p* 
         *classify-concepts-in-instance-assertions-p*
         *told-information-reasoning-p* 
         *ensure-tbox-classification-p*
         
         *add-role-assertions-for-datatype-properties-p*
         #-:lracer *use-unique-name-assumption*
         (when (find-tbox (tbox substrate) nil)
           (get-tbox-version (tbox substrate)))
         (when (find-abox (abox substrate) nil)
           (get-abox-version (abox substrate))))))


#+:midelora
(defmethod get-state-vector ((substrate midelora-substrate))
  (with-critical-section
    (list *initial-abox-mirroring-p*
          *initial-role-assertion-mirroring-p* 
          *classify-concepts-in-instance-assertions-p*
          *told-information-reasoning-p* 
          *ensure-tbox-classification-p*
          
          (prover::get-tbox-version (prover::tbox substrate))
          (prover::get-abox-version substrate))))

(defmethod get-state-vector ((substrate racer-tbox-mirror-substrate))
  (with-critical-section
   (when (slot-boundp substrate 'mirror-of-tbox)
     (list (get-tbox-version (mirror-of-tbox substrate))
           nil))))

#+:midelora
(defmethod get-state-vector ((substrate midelora-tbox-mirror-substrate))
  (with-critical-section
   (when (slot-boundp substrate 'mirror-of-tbox)
     (list (prover::get-tbox-version (mirror-of-tbox substrate))
           nil))))

;;;
;;;
;;;

(defmethod substrate-needs-reset-p ((substrate dl-prover-substrate))
  (with-critical-section
   (not (equal (get-state-vector substrate)
               (saved-state-vector substrate)))))
     
;;;
;;;
;;;

(defmethod substrate-needs-reset ((substrate dl-prover-substrate))
  (setf (saved-state-vector substrate) nil))

(defmethod substrate-needs-reset ((substrate null))
  t)




