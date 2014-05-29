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

;;;===========================================================================
;;; Structure for describing DL dialects
;;;===========================================================================

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +dl-and+ 1)
  (defconstant +dl-or+ (ash +dl-and+ 1))
  (defconstant +dl-atomic-not+ (ash +dl-or+ 1))
  (defconstant +dl-not+ (ash +dl-atomic-not+ 1))
  (defconstant +dl-bottom+ (ash +dl-not+ 1))
  (defconstant +dl-simple-some+ (ash +dl-bottom+ 1))
  (defconstant +dl-some+ (ash +dl-simple-some+ 1))
  (defconstant +dl-all+ (ash +dl-some+ 1))
  (defconstant +dl-simple-at-least+ (ash +dl-all+ 1))
  (defconstant +dl-simple-at-most+ (ash +dl-simple-at-least+ 1))
  (defconstant +dl-at-least+ (ash +dl-simple-at-most+ 1))
  (defconstant +dl-at-most+ (ash +dl-at-least+ 1))
  (defconstant +dl-nominals+ (ash +dl-at-most+ 1))
  (defconstant +dl-self+ (ash +dl-nominals+ 1))
  (defconstant +dl-cd+ (ash +dl-self+ 1))                             ; full CD reasoning
  (defconstant +dl-simple-cd+ (ash +dl-cd+ 1))                        ; OWL Datatypes + universal retrictions + min/max
  (defconstant +dl-full-datatype-cd+ (ash +dl-simple-cd+ 1))          ; OWL Datatypes + universal retrictions
  (defconstant +dl-simple-datatype-cd+ (ash +dl-full-datatype-cd+ 1)) ; OWL Datatypes

  (defconstant +dl-simple-role-inclusions+ (ash +dl-simple-datatype-cd+ 1))
  (defconstant +dl-complex-role-inclusions+ (ash +dl-simple-role-inclusions+ 1))
  (defconstant +dl-feature-roles+ (ash +dl-complex-role-inclusions+ 1))
  (defconstant +dl-transitive-roles+ (ash +dl-feature-roles+ 1))
  (defconstant +dl-inverse-roles+ (ash +dl-transitive-roles+ 1))
  (defconstant +dl-symmetric-roles+ (ash +dl-inverse-roles+ 1))
  (defconstant +dl-asymmetric-roles+ (ash +dl-symmetric-roles+ 1))
  (defconstant +dl-reflexive-roles+ (ash +dl-asymmetric-roles+ 1))
  (defconstant +dl-locally-reflexive-roles+ (ash +dl-reflexive-roles+ 1))
  (defconstant +dl-irreflexive-roles+ (ash +dl-locally-reflexive-roles+ 1))
  (defconstant +dl-disjoint-roles+ (ash +dl-irreflexive-roles+ 1))
  (defconstant +dl-universal-role+ (ash +dl-disjoint-roles+ 1))
  
  (defconstant +dl-simple-cis+ (ash +dl-universal-role+ 1))
  (defconstant +dl-equiv-cis+ (ash +dl-simple-cis+ 1))
  (defconstant +dl-gcis+ (ash +dl-equiv-cis+ 1))
  (defconstant +dl-cyclic-concepts+ (ash +dl-gcis+ 1))
  
  (defconstant +dl-atomic-concept-assertions+ (ash +dl-cyclic-concepts+ 1))
  (defconstant +dl-concept-assertions+ (ash +dl-atomic-concept-assertions+ 1))
  (defconstant +dl-role-assertions+ (ash +dl-concept-assertions+ 1))
  (defconstant +dl-negated-role-assertions+ (ash +dl-role-assertions+ 1))
  (defconstant +dl-disjoint-assertions+ (ash +dl-negated-role-assertions+ 1))
  (defconstant +dl-same-as-assertions+ (ash +dl-disjoint-assertions+ 1))

  (defconstant +dl-first-concept-value+ +dl-and+)
  (defconstant +dl-last-concept-value+ +dl-simple-datatype-cd+)
  (defconstant +dl-all-concept-values+ (1- +dl-simple-role-inclusions+))
  (defconstant +dl-first-role-value+ +dl-simple-role-inclusions+)
  (defconstant +dl-last-role-value+ +dl-universal-role+)
  (defconstant +dl-first-axiom-value+ +dl-simple-cis+)
  (defconstant +dl-last-axiom-value+ +dl-cyclic-concepts+)
  (defconstant +dl-first-assertion-value+ +dl-atomic-concept-assertions+)
  (defconstant +dl-last-assertion-value+ +dl-same-as-assertions+)
  
  (defconstant +dl-last-value+ +dl-last-assertion-value+)
  
  (defconstant +dl-l-minus+ (logior +dl-and+ +dl-simple-cis+))
  (defconstant +dl-l+ (logior +dl-l-minus+ +dl-equiv-cis+))
  (defconstant +dl-lc-minus+ (logior +dl-l+ +dl-atomic-not+))
  (defconstant +dl-lcu+ (logior +dl-lc-minus+ +dl-not+ +dl-or+))
  (defconstant +dl-el-minus-minus+ (logior +dl-l-minus+ +dl-simple-some+))
  (defconstant +dl-el-minus+ (logior +dl-el-minus-minus+ +dl-some+))
  (defconstant +dl-el+ (logior +dl-el-minus+ +dl-equiv-cis+))
  (defconstant +dl-elh+ (logior +dl-el+ +dl-simple-role-inclusions+))
  (defconstant +dl-elr+ (logior +dl-elh+ +dl-complex-role-inclusions+))
  (defconstant +dl-el++ (logior +dl-elr+ +dl-transitive-roles+))
  (defconstant +dl-el+++ (logior +dl-el++ +dl-bottom+ +dl-atomic-not+))
  (defconstant +dl-el+-owl+ (logior +dl-el++ +dl-full-datatype-cd+ +dl-simple-datatype-cd+))
  (defconstant +dl-fl-zero+ (logior +dl-l-minus+ +dl-all+))
  (defconstant +dl-fl-minus+ (logior +dl-fl-zero+ +dl-simple-some+))
  (defconstant +dl-al+ (logior +dl-fl-minus+ +dl-atomic-not+))
  (defconstant +dl-alc+ (logior +dl-al+ +dl-or+ +dl-not+ +dl-bottom+ +dl-some+))
  (defconstant +dl-alcn+ (logior +dl-alc+ +dl-simple-at-least+ +dl-simple-at-most+))
  (defconstant +dl-alcq+ (logior +dl-alcn+ +dl-at-least+ +dl-at-most+))
  (defconstant +dl-alchir++ (logior +dl-alc+
                                    +dl-simple-role-inclusions+
                                    +dl-inverse-roles+
                                    +dl-transitive-roles+))
  (defconstant +dl-s+ (logior +dl-alc+
                              +dl-simple-role-inclusions+
                              +dl-transitive-roles+))
  (defconstant +dl-r+ (logior +dl-self+ 
                              +dl-complex-role-inclusions+
                              +dl-locally-reflexive-roles+
                              +dl-irreflexive-roles+
                              +dl-disjoint-roles+
                              +dl-universal-role+))
  
  (defstruct (dl-descriptor
              (:conc-name dl-)
              (:constructor make-dl-descriptor (&optional (language-set 0)))
              ;(:pconstructor find-or-make-dl-descriptor (&optional (language-set 0)))
	      )
    (language-set 0))
  )

(defparameter *all-dl-tags*
  '(and
    or
    atomic-not
    not
    bottom
    simple-some
    some
    all
    simple-at-least
    simple-at-most
    at-least
    at-most
    nominals
    self
    cd
    simple-cd
    full-datatype-cd
    simple-datatype-cd
    simple-role-inclusions
    complex-role-inclusions
    feature-roles
    transitive-roles
    inverse-roles
    symmetric-roles
    asymmetric-roles
    reflexive-roles
    locally-reflexive-roles
    irreflexive-roles
    disjoint-roles
    universal-role
    simple-cis
    equiv-cis
    gcis
    cyclic-concepts
    atomic-concept-assertions
    concept-assertions
    role-assertions
    negated-role-assertions
    disjoint-assertions
    same-as-assertions
    ))

(defparameter *all-dl-tag-values*
  (list +dl-and+
        +dl-or+
        +dl-atomic-not+
        +dl-not+
        +dl-bottom+
        +dl-simple-some+
        +dl-some+
        +dl-all+
        +dl-simple-at-least+
        +dl-simple-at-most+
        +dl-at-least+
        +dl-at-most+
        +dl-nominals+
        +dl-self+
        +dl-cd+
        +dl-simple-cd+
        +dl-full-datatype-cd+
        +dl-simple-datatype-cd+
        +dl-simple-role-inclusions+
        +dl-complex-role-inclusions+
        +dl-feature-roles+
        +dl-transitive-roles+
        +dl-inverse-roles+
        +dl-symmetric-roles+
        +dl-asymmetric-roles+
        +dl-reflexive-roles+
        +dl-locally-reflexive-roles+
        +dl-irreflexive-roles+
        +dl-disjoint-roles+
        +dl-universal-role+
        +dl-simple-cis+
        +dl-equiv-cis+
        +dl-gcis+
        +dl-cyclic-concepts+
        +dl-atomic-concept-assertions+
        +dl-concept-assertions+
        +dl-role-assertions+
        +dl-negated-role-assertions+
        +dl-disjoint-assertions+
        +dl-same-as-assertions+
        ))

(defparameter *dl-tags-assoc-list*
  (sort (pairlis *all-dl-tag-values* *all-dl-tags*) #'< :key #'car))

(defun decode-bit-mask (mask)
  (let ((found-dl-tags
         (loop for (tag-mask . dl-tag) in *dl-tags-assoc-list*
               when (logtest tag-mask mask)
               collect dl-tag)))
    found-dl-tags))

(defun print-bit-mask (mask &optional (stream t))
  (format stream "#<~B ~A>" mask (decode-bit-mask mask)))

(defun print-dl-descriptor (dl-descriptor)
  (when dl-descriptor
    (format t "(~B ~A)"
            (dl-language-set dl-descriptor)
            (decode-bit-mask (dl-language-set dl-descriptor)))))

#+:debug
(defun dl-equal-p (dl-descriptor-1 dl-descriptor-2)
  (or (null (or dl-descriptor-1 dl-descriptor-2))
      (when (and dl-descriptor-1 dl-descriptor-2)
        (eql (dl-language-set dl-descriptor-1)
             (dl-language-set dl-descriptor-2)))))

#-:debug
(defmacro dl-equal-p (dl-descriptor-1 dl-descriptor-2)
  `(or (null (or ,dl-descriptor-1 ,dl-descriptor-2))
      (when (and ,dl-descriptor-1 ,dl-descriptor-2)
        (eql (dl-language-set ,dl-descriptor-1)
             (dl-language-set ,dl-descriptor-2)))))

#+:debug
(defun dl-subset-p (dl-descriptor-1 dl-descriptor-2)
  (or (null (or dl-descriptor-1 dl-descriptor-2))
      (when (and dl-descriptor-1 dl-descriptor-2)
        (dl-bits-subset-p (dl-language-set dl-descriptor-1)
                          (dl-language-set dl-descriptor-2)))))

#-:debug
(defmacro dl-subset-p (dl-descriptor-1 dl-descriptor-2)
  `(or (null (or ,dl-descriptor-1 ,dl-descriptor-2))
       (when (and ,dl-descriptor-1 ,dl-descriptor-2)
         (dl-bits-subset-p (dl-language-set ,dl-descriptor-1)
                           (dl-language-set ,dl-descriptor-2)))))

#+:debug
(defun dl-definition-subset-p (definition-descriptor atomic-descriptor)
  (or (null (or atomic-descriptor definition-descriptor))
      (when (and atomic-descriptor definition-descriptor)
        (dl-bits-subset-p (logand (dl-language-set definition-descriptor) +dl-all-concept-values+)
                          (logand (dl-language-set atomic-descriptor) +dl-all-concept-values+)))))

#-:debug
(defmacro dl-definition-subset-p (definition-descriptor atomic-descriptor)
  `(or (null (or ,atomic-descriptor ,definition-descriptor))
       (when (and ,atomic-descriptor ,definition-descriptor)
         (dl-bits-subset-p (logand (dl-language-set ,definition-descriptor) +dl-all-concept-values+)
                           (logand (dl-language-set ,atomic-descriptor) +dl-all-concept-values+)))))

#+:debug
(defun any-bit-set-p (dl-descriptor mask)
  (and dl-descriptor (logtest mask (dl-language-set dl-descriptor))))

#-:debug
(defmacro any-bit-set-p (dl-descriptor mask)
  `(and ,dl-descriptor (logtest ,mask (dl-language-set ,dl-descriptor))))

#+:debug
(defun all-bits-set-p (dl-mask mask)
  (eql (logand dl-mask mask) mask))

#-:debug
(defmacro all-bits-set-p (dl-mask mask)
  (if (numberp mask)
    `(eql (logand ,dl-mask ,mask) ,mask)
    (let ((sym (gensym)))
      `(let ((,sym ,mask))
         (eql (logand ,dl-mask ,sym) ,sym)))))

#+:debug
(defun only-bits-set-p (dl-descriptor mask only-bits-mask)
  (and dl-descriptor (eql (logand (dl-language-set dl-descriptor) mask) only-bits-mask)))

#-:debug
(defmacro only-bits-set-p (dl-descriptor mask only-bits-mask)
  `(and ,dl-descriptor (eql (logand (dl-language-set ,dl-descriptor) ,mask) ,only-bits-mask)))

#+:debug
(defun dl-bits-subset-p (mask-1 mask-2)
  (eql (logand mask-1 mask-2) mask-1))

#-:debug
(defmacro dl-bits-subset-p (mask-1 mask-2)
  `(eql (logand ,mask-1 ,mask-2) ,mask-1))

#+:debug
(defun add-dl-bits (dl-language-set mask)
  (logior dl-language-set mask))

#-:debug
(defmacro add-dl-bits (dl-language-set mask)
  `(logior ,dl-language-set ,mask))

#+:debug
(defun remove-dl-bits (dl-language-set mask)
  (logandc2 dl-language-set mask))

#-:debug
(defmacro remove-dl-bits (dl-language-set mask)
  `(logandc2 ,dl-language-set ,mask))

#+:debug
(defun contains-s-p (dl-mask)
  (all-bits-set-p dl-mask +dl-s+))

#-:debug
(defmacro contains-s-p (dl-mask)
  `(all-bits-set-p ,dl-mask +dl-s+))

#+:debug
(defun contains-r-p (dl-descriptor)
  (any-bit-set-p dl-descriptor +dl-r+))

#-:debug
(defmacro contains-r-p (dl-descriptor)
  `(any-bit-set-p ,dl-descriptor +dl-r+))

#+:debug
(defun contains-alc-p (dl-mask)
  (all-bits-set-p dl-mask +dl-alc+))

#-:debug
(defmacro contains-alc-p (dl-mask)
  `(all-bits-set-p ,dl-mask +dl-alc+))

#+:debug
(defun contains-al-p (dl-mask)
  (all-bits-set-p dl-mask +dl-al+))

#-:debug
(defmacro contains-al-p (dl-mask)
  `(all-bits-set-p ,dl-mask +dl-al+))

#+:debug
(defun contains-fl-minus-p (dl-mask)
  (all-bits-set-p dl-mask +dl-fl-minus+))

#-:debug
(defmacro contains-fl-minus-p (dl-mask)
  `(all-bits-set-p ,dl-mask +dl-fl-minus+))

#+:debug
(defun contains-fl-zero-p (dl-mask)
  (all-bits-set-p dl-mask +dl-fl-zero+))

#-:debug
(defmacro contains-fl-zero-p (dl-mask)
  `(all-bits-set-p ,dl-mask +dl-fl-zero+))

#+:debug
(defun contains-el-minus-minus-p (dl-mask)
  (all-bits-set-p dl-mask +dl-el-minus-minus+))

#-:debug
(defmacro contains-el-minus-minus-p (dl-mask)
  `(all-bits-set-p ,dl-mask +dl-el-minus-minus+))

#+:debug
(defun contains-el-minus-p (dl-mask)
  (all-bits-set-p dl-mask +dl-el-minus+))

#-:debug
(defmacro contains-el-minus-p (dl-mask)
  `(all-bits-set-p ,dl-mask +dl-el-minus+))

#+:debug
(defun contains-el-p (dl-mask)
  (all-bits-set-p dl-mask +dl-el+))

#-:debug
(defmacro contains-el-p (dl-mask)
  `(all-bits-set-p ,dl-mask +dl-el+))

#+:debug
(defun contains-l-p (dl-mask)
  (all-bits-set-p dl-mask +dl-l+))

#-:debug
(defmacro contains-l-p (dl-mask)
  `(all-bits-set-p ,dl-mask +dl-l+))

#+:debug
(defun contains-l-minus-p (dl-mask)
  (all-bits-set-p dl-mask +dl-l-minus+))

#-:debug
(defmacro contains-l-minus-p (dl-mask)
  `(all-bits-set-p ,dl-mask +dl-l-minus+))

#+:debug
(defun contains-lc-minus-p (dl-mask)
  (all-bits-set-p dl-mask +dl-lc-minus+))

#-:debug
(defmacro contains-lc-minus-p (dl-mask)
  `(all-bits-set-p ,dl-mask +dl-lc-minus+))

#+:debug
(defun contains-some-p (dl-descriptor)
  (and dl-descriptor (any-bit-set-p dl-descriptor (logior +dl-some+ +dl-simple-some+))))

#-:debug
(defmacro contains-some-p (dl-descriptor)
  `(and ,dl-descriptor (any-bit-set-p ,dl-descriptor (logior +dl-some+ +dl-simple-some+))))

#+:debug
(defun superset-some-all-p (dl-descriptor)
  (and dl-descriptor (any-bit-set-p dl-descriptor (logior +dl-some+ +dl-all+))))

#-:debug
(defmacro superset-some-all-p (dl-descriptor)
  `(and ,dl-descriptor (any-bit-set-p ,dl-descriptor (logior +dl-some+ +dl-all+))))

#+:debug
(defun superset-all-p (dl-descriptor)
  (and dl-descriptor (any-bit-set-p dl-descriptor +dl-all+)))

#-:debug
(defmacro superset-all-p (dl-descriptor)
  `(and ,dl-descriptor (any-bit-set-p ,dl-descriptor +dl-all+)))

#+:debug
(defun subset-alchir+-p (dl-descriptor)
  (and dl-descriptor
       (dl-bits-subset-p (dl-language-set dl-descriptor) +dl-alchir++)))

#-:debug
(defmacro subset-alchir+-p (dl-descriptor)
  `(and ,dl-descriptor
        (dl-bits-subset-p (dl-language-set ,dl-descriptor) +dl-alchir++)))

#+:debug
(defun subset-l-p (dl-descriptor)
  (and dl-descriptor
       (dl-bits-subset-p (dl-language-set dl-descriptor) +dl-l+)))

#-:debug
(defmacro subset-l-p (dl-descriptor)
  `(and ,dl-descriptor
        (dl-bits-subset-p (dl-language-set ,dl-descriptor) +dl-l+)))

#+:debug
(defun subset-l-minus-p (dl-descriptor)
  (and dl-descriptor
       (dl-bits-subset-p (dl-language-set dl-descriptor) +dl-l-minus+)))

#-:debug
(defmacro subset-l-minus-p (dl-descriptor)
  `(and ,dl-descriptor
        (dl-bits-subset-p (dl-language-set ,dl-descriptor) +dl-l-minus+)))

#+:debug
(defun subset-el-p (dl-descriptor)
  (and dl-descriptor
       (dl-bits-subset-p (dl-language-set dl-descriptor) +dl-el+)))

#-:debug
(defmacro subset-el-p (dl-descriptor)
  `(and ,dl-descriptor
        (dl-bits-subset-p (dl-language-set ,dl-descriptor) +dl-el+)))

#+:debug
(defun subset-elh-p (dl-descriptor)
  (and dl-descriptor
       (dl-bits-subset-p (dl-language-set dl-descriptor) +dl-elh+)))

#-:debug
(defmacro subset-elh-p (dl-descriptor)
  `(and ,dl-descriptor
        (dl-bits-subset-p (dl-language-set ,dl-descriptor) +dl-elh+)))

#+:debug
(defun subset-elr-p (dl-descriptor)
  (and dl-descriptor
       (dl-bits-subset-p (dl-language-set dl-descriptor) +dl-elr+)))

#-:debug
(defmacro subset-elr-p (dl-descriptor)
  `(and ,dl-descriptor
        (dl-bits-subset-p (dl-language-set ,dl-descriptor) +dl-elr+)))

#+:debug
(defun subset-el+-p (dl-descriptor)
  (and dl-descriptor
       (dl-bits-subset-p (dl-language-set dl-descriptor) +dl-el++)))

#-:debug
(defmacro subset-el+-p (dl-descriptor)
  `(and ,dl-descriptor
        (dl-bits-subset-p (dl-language-set ,dl-descriptor) +dl-el++)))

#+:debug
(defun subset-el++-p (dl-descriptor)
  (and dl-descriptor
       (dl-bits-subset-p (dl-language-set dl-descriptor) +dl-el+++)))

#-:debug
(defmacro subset-el++-p (dl-descriptor)
  `(and ,dl-descriptor
        (dl-bits-subset-p (dl-language-set ,dl-descriptor) +dl-el+++)))

#+:debug
(defun subset-el+-owl-p (dl-descriptor)
  (and dl-descriptor
       (dl-bits-subset-p (dl-language-set dl-descriptor) +dl-el+-owl+)))

#-:debug
(defmacro subset-el+-owl-p (dl-descriptor)
  `(and ,dl-descriptor
        (dl-bits-subset-p (dl-language-set ,dl-descriptor) +dl-el+-owl+)))

(defmethod print-object ((object dl-descriptor) stream)
  (let ((dl-mask (add-dl-bits (dl-language-set object) +dl-simple-cis+)))
    (cond
     ((contains-s-p dl-mask)
      (pprint-s-dl object stream))
     ((contains-alc-p dl-mask)
      (pprint-alc-dl object stream))
     ((contains-al-p dl-mask)
      (pprint-al-dl object stream))
     ((contains-fl-minus-p dl-mask)
      (pprint-fl-minus-dl object stream))
     ((contains-fl-zero-p dl-mask)
      (pprint-fl-zero-dl object stream))
     ((contains-el-p dl-mask)
      (pprint-el-dl object stream))
     ((contains-el-minus-p dl-mask)
      (pprint-el-minus-dl object stream))
     ((contains-el-minus-minus-p dl-mask)
      (pprint-el-minus-minus-dl object stream))
     ((contains-l-minus-p dl-mask)
      (pprint-l-minus-dl object stream))
     (t 
      (let ((string
             (with-output-to-string ( stream)
               (pprint-any-dl object stream))))
        (if (equal string "")
          (print-bit-mask (dl-language-set object) stream)
          (format stream "~A" string)))))))

(defun print-dl-description (object stream)
  (print-object object stream))

(defun pprint-dl-common (dl-descriptor stream)
  (if (contains-r-p dl-descriptor)
    (format stream "R")
    (when (any-bit-set-p dl-descriptor +dl-simple-role-inclusions+)
      (format stream "H")))
  (when (any-bit-set-p dl-descriptor +dl-nominals+)
    (format stream "O"))
  (when (any-bit-set-p dl-descriptor +dl-inverse-roles+)
    (format stream "I"))
  (if (any-bit-set-p dl-descriptor (logior +dl-at-least+ +dl-at-most+))
    (format stream "Q")
    (if (any-bit-set-p dl-descriptor
                         (logior +dl-simple-at-least+ +dl-simple-at-most+))
	(format stream "N")
      (when (any-bit-set-p dl-descriptor +dl-feature-roles+)
	(format stream "f")))))

(defun pprint-dl-cd (dl-descriptor stream)
  (cond ((any-bit-set-p dl-descriptor +dl-cd+)
         (format stream "(D)"))
        ((any-bit-set-p dl-descriptor +dl-simple-cd+)
         (format stream "(D)-"))
        ((any-bit-set-p dl-descriptor +dl-full-datatype-cd+)
         (format stream "(D-)"))
        ((any-bit-set-p dl-descriptor +dl-simple-datatype-cd+)
         (format stream "(D--)"))))

(defun pprint-dl-common-r+d- (dl-descriptor stream)
  (when (and (any-bit-set-p dl-descriptor +dl-transitive-roles+)
	     (not (contains-s-p (dl-language-set dl-descriptor))))
    (format stream "r+"))
  (pprint-dl-cd dl-descriptor stream))

(defun pprint-s-dl (dl-descriptor stream)
  (format stream "S")
  (pprint-dl-common dl-descriptor stream)
  (pprint-dl-common-r+d- dl-descriptor stream))

(defun pprint-alc-dl (dl-descriptor stream)
  (format stream "ALC")
  (pprint-dl-common dl-descriptor stream)
  (pprint-dl-common-r+d- dl-descriptor stream))

(defun pprint-al-common (dl-descriptor stream)
  (when (any-bit-set-p dl-descriptor +dl-or+)
    (format stream "U"))
  (when (and (any-bit-set-p dl-descriptor +dl-some+)
             (not (any-bit-set-p dl-descriptor +dl-at-least+)))
    (format stream "E")))

(defun pprint-al-dl (dl-descriptor stream)
  (format stream "AL")
  (pprint-al-common dl-descriptor stream)
  (pprint-dl-common dl-descriptor stream)
  (pprint-dl-common-r+d- dl-descriptor stream))

(defun pprint-fl-minus-dl (dl-descriptor stream)
  (format stream "FL-")
  (pprint-al-common dl-descriptor stream)
  (pprint-dl-common dl-descriptor stream)
  (pprint-dl-common-r+d- dl-descriptor stream))

(defun pprint-fl-zero-dl (dl-descriptor stream)
  (format stream "FL0")
  (pprint-al-common dl-descriptor stream)
  (pprint-dl-common dl-descriptor stream)
  (pprint-dl-common-r+d- dl-descriptor stream))

(defun pprint-el-minus-minus-dl (dl-descriptor stream)
  (format stream "EL--")
  (when (any-bit-set-p dl-descriptor +dl-atomic-not+)
    (format stream "C-"))
  (when (any-bit-set-p dl-descriptor +dl-or+)
    (format stream "U"))
  (pprint-dl-common dl-descriptor stream)
  (pprint-dl-common-r+d- dl-descriptor stream))

(defun pprint-el-minus-dl (dl-descriptor stream)
  (format stream "EL-")
  (when (any-bit-set-p dl-descriptor +dl-atomic-not+)
    (format stream "C-"))
  (when (any-bit-set-p dl-descriptor +dl-or+)
    (format stream "U"))
  (pprint-dl-common dl-descriptor stream)
  (pprint-dl-common-r+d- dl-descriptor stream))

(defun pprint-el-dl (dl-descriptor stream)
  (format stream "EL")
  (when (any-bit-set-p dl-descriptor +dl-atomic-not+)
    (format stream "C-"))
  (when (any-bit-set-p dl-descriptor +dl-or+)
    (format stream "U"))
  (pprint-dl-common dl-descriptor stream)
  (pprint-dl-common-r+d- dl-descriptor stream))

(defun pprint-l-minus-dl (dl-descriptor stream)
  (format stream "L")
  (unless (any-bit-set-p dl-descriptor +dl-equiv-cis+)
    (format stream "-"))
  (if (any-bit-set-p dl-descriptor +dl-not+)
      (format stream "C")
    (when (any-bit-set-p dl-descriptor +dl-atomic-not+)
      (format stream "C-")))
  (when (any-bit-set-p dl-descriptor (logior +dl-or+ +dl-not+))
    (format stream "U"))
  (pprint-dl-common dl-descriptor stream)
  (pprint-dl-common-r+d- dl-descriptor stream))

(defun pprint-any-dl (dl-descriptor stream)
  (when (any-bit-set-p dl-descriptor (logior +dl-simple-some+
                                             +dl-some+
                                             +dl-simple-at-least+
                                             +dl-at-least+))
    (format stream "E"))
  (when (any-bit-set-p dl-descriptor +dl-or+)
    (format stream "U"))
  (pprint-dl-common dl-descriptor stream)
  (pprint-dl-common-r+d- dl-descriptor stream))

(defun complete-dl-bits (dl-language-set)
  (when (logtest +dl-simple-cis+ dl-language-set)
    (setf dl-language-set (logior dl-language-set +dl-and+)))
  (when (or (all-bits-set-p dl-language-set (logior +dl-atomic-not+ +dl-and+))
            (all-bits-set-p dl-language-set (logior +dl-not+ +dl-or+)))
    (setf dl-language-set (logior dl-language-set +dl-bottom+)))
  (when (logtest (logior +dl-some+ +dl-simple-at-least+ +dl-at-least+) dl-language-set)
    (if (logtest +dl-at-least+ dl-language-set)
        (setf dl-language-set (logior dl-language-set (logior +dl-simple-some+ +dl-some+)))
      (setf dl-language-set (logior dl-language-set +dl-simple-some+))))
  (when (logtest +dl-symmetric-roles+ dl-language-set)
    (setf dl-language-set (logior dl-language-set +dl-inverse-roles+)))
  (when (logtest +dl-asymmetric-roles+ dl-language-set)
    (setf dl-language-set (logior dl-language-set +dl-irreflexive-roles+)))
  (when (logtest +dl-complex-role-inclusions+ dl-language-set)
    (setf dl-language-set (logior dl-language-set +dl-simple-role-inclusions+)))
  (if (or (logtest +dl-not+ dl-language-set)
          (eql (logand dl-language-set (logior +dl-or+ +dl-some+))
               (logior +dl-or+ +dl-some+)))
      (let ((alcq-subset (logand +dl-alcq+ dl-language-set)))
        (if (logtest (logior +dl-some+ +dl-all+) alcq-subset)
            (progn
              (when (and (logtest (logior +dl-and+ +dl-or+) alcq-subset)
                         (logtest (logior +dl-some+ +dl-all+) alcq-subset))
                (setf alcq-subset (logior alcq-subset +dl-alc+)))
              (if (logtest (logior +dl-at-least+ +dl-at-most+) alcq-subset)
                  (setf alcq-subset +dl-alcq+)
                (when (logtest (logior +dl-simple-at-least+ +dl-simple-at-most+) alcq-subset)
                  (setf alcq-subset +dl-alcn+)))
              (logior dl-language-set alcq-subset))
          dl-language-set))
    dl-language-set))

(race-inline (find-dl-descriptor add-dl-descriptor))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *descriptor-table* (make-hash-table))
  
  (defun find-dl-descriptor (language-set)
    (gethash language-set *descriptor-table*))
  
  (defun add-dl-descriptor (language-set dl-descriptor)
    (setf (gethash language-set *descriptor-table*) dl-descriptor))
  
  (defun find-or-make-dl-descriptor (&optional (language-set 0))
    (or (find-dl-descriptor language-set)
        (let* ((completed-language-set (complete-dl-bits language-set))
               (changed (not (eql language-set completed-language-set))))
          (or (when changed
                (find-dl-descriptor completed-language-set))
              (let ((new-dl-descriptor (make-dl-descriptor completed-language-set)))
                (add-dl-descriptor completed-language-set new-dl-descriptor)
                (when changed
                  (add-dl-descriptor language-set new-dl-descriptor))
                new-dl-descriptor)))))
  )

(defparameter *dl-empty* (find-or-make-dl-descriptor +dl-l-minus+))
(defparameter *dl-elh* (find-or-make-dl-descriptor +dl-elh+))
(defparameter *dl-elr* (find-or-make-dl-descriptor +dl-elr+))
  
(race-inline (dl-features
              dl-merging
              dl-qualified-number-restrictions
              dl-inverse
              dl-inverse-roles
              dl-simple-role-inclusions
              dl-any-concrete-domain
              dl-full-concrete-domains
              dl-simple-concrete-domains
              dl-datatype-concrete-domains
              dl-true-datatype-concrete-domains
              dl-simple-concept-inclusions))

(defun dl-features (dl-descriptor)
  (any-bit-set-p dl-descriptor +dl-feature-roles+))

(defun dl-merging (dl-descriptor)
  (any-bit-set-p dl-descriptor (logior +dl-simple-at-least+
                                       +dl-simple-at-most+
                                       +dl-at-least+
                                       +dl-at-most+)))

(defun dl-simple-number-restrictions (dl-descriptor)
  (any-bit-set-p dl-descriptor (logior +dl-simple-at-least+ +dl-simple-at-most+)))

(defun dl-qualified-number-restrictions (dl-descriptor)
  (any-bit-set-p dl-descriptor (logior +dl-at-least+ +dl-at-most+)))

(defun dl-features-only (dl-descriptor)
  (and (any-bit-set-p dl-descriptor +dl-feature-roles+)
       (not (dl-merging dl-descriptor))))

(defun dl-inverse (dl-descriptor)
  (any-bit-set-p dl-descriptor +dl-inverse-roles+))

(defun dl-inverse-roles (dl-descriptor)
  (any-bit-set-p dl-descriptor +dl-inverse-roles+))

(defun dl-simple-role-inclusions (dl-descriptor)
  (any-bit-set-p dl-descriptor +dl-simple-role-inclusions+))

(defun dl-complex-role-inclusions (dl-descriptor)
  (any-bit-set-p dl-descriptor +dl-complex-role-inclusions+))

(defun dl-any-concrete-domain (dl-descriptor)
  (any-bit-set-p dl-descriptor (logior +dl-cd+ +dl-simple-cd+ +dl-full-datatype-cd+ +dl-simple-datatype-cd+)))

(defun dl-full-concrete-domains (dl-descriptor)
  (any-bit-set-p dl-descriptor +dl-cd+))

(defun dl-simple-concrete-domains (dl-descriptor)
  (any-bit-set-p dl-descriptor +dl-simple-cd+))

(defun dl-full-datatype-concrete-domains (dl-descriptor)
  (any-bit-set-p dl-descriptor +dl-full-datatype-cd+))

(defun dl-simple-datatype-concrete-domains (dl-descriptor)
  (any-bit-set-p dl-descriptor +dl-simple-datatype-cd+))

(defun dl-true-simple-datatype-concrete-domains (dl-descriptor)
  (only-bits-set-p dl-descriptor
                   (logior +dl-cd+ +dl-simple-cd+ +dl-full-datatype-cd+ +dl-simple-datatype-cd+)
                   +dl-simple-datatype-cd+))

(defun dl-simple-concept-inclusions (dl-descriptor)
  (any-bit-set-p dl-descriptor +dl-simple-cis+))

(defun dl-clash-possible-p (dl-descriptor)
  (or (any-bit-set-p dl-descriptor
                     (logior +dl-atomic-not+ +dl-not+ +dl-bottom+
                             +dl-simple-at-most+ +dl-at-most+
                             +dl-cd+ +dl-simple-cd+
                             +dl-disjoint-roles+))
      (all-bits-set-p (dl-language-set dl-descriptor)
                      (logior +dl-simple-datatype-cd+ +dl-some+ +dl-all+))
      (all-bits-set-p (dl-language-set dl-descriptor)
                      (logior +dl-reflexive-roles+ +dl-irreflexive-roles+))
      (all-bits-set-p (dl-language-set dl-descriptor)
                      (logior +dl-reflexive-roles+ +dl-asymmetric-roles+))
      (all-bits-set-p (dl-language-set dl-descriptor)
                      (logior +dl-asymmetric-roles+ +dl-symmetric-roles+))))

(defun dl-transitive-roles (dl-descriptor)
  (any-bit-set-p dl-descriptor +dl-transitive-roles+))

(defun dl-disjoint-roles (dl-descriptor)
  (any-bit-set-p dl-descriptor +dl-disjoint-roles+))

(defun dl-reflexive-roles (dl-descriptor)
  (any-bit-set-p dl-descriptor +dl-reflexive-roles+))

(defun dl-locally-reflexive-roles (dl-descriptor)
  (any-bit-set-p dl-descriptor +dl-locally-reflexive-roles+))

(defun dl-irreflexive-roles (dl-descriptor)
  (any-bit-set-p dl-descriptor +dl-irreflexive-roles+))

(defun dl-symmetric-roles (dl-descriptor)
  (any-bit-set-p dl-descriptor +dl-symmetric-roles+))

(defun dl-asymmetric-roles (dl-descriptor)
  (any-bit-set-p dl-descriptor +dl-asymmetric-roles+))

(defun dl-bottom (dl-descriptor)
  (any-bit-set-p dl-descriptor +dl-bottom+))

(defun dl-atomic-not (dl-descriptor)
  (any-bit-set-p dl-descriptor +dl-atomic-not+))

(defun dl-all (dl-descriptor)
  (any-bit-set-p dl-descriptor +dl-all+))

(defun dl-some (dl-descriptor)
  (any-bit-set-p dl-descriptor +dl-some+))

#|
(defun dl-cyclic (descriptor)
  (when (dl-description-p descriptor)
    (dl-cyclic-concepts descriptor)))

(defun dl-blocking (descriptor)
  (when (dl-description-p descriptor)
    (or (dl-cyclic-concepts descriptor)
        (dl-true-gcis descriptor)
        (dl-transitive-roles descriptor)
        (dl-inverse-roles descriptor))))
|#

(defun add-dl-and (dl-descriptor)
  (if (any-bit-set-p dl-descriptor +dl-and+)
    dl-descriptor
    (find-or-make-dl-descriptor (add-dl-bits (dl-language-set dl-descriptor)
                                             +dl-and+))))

(defun add-dl-or (dl-descriptor)
  (if (any-bit-set-p dl-descriptor +dl-or+)
    dl-descriptor
    (find-or-make-dl-descriptor (add-dl-bits (dl-language-set dl-descriptor)
                                             +dl-or+))))

(defun add-dl-atomic-not (dl-descriptor)
  (if (any-bit-set-p dl-descriptor +dl-atomic-not+)
    dl-descriptor
    (find-or-make-dl-descriptor (add-dl-bits (dl-language-set dl-descriptor)
                                             +dl-atomic-not+))))

(defun add-dl-not (dl-descriptor)
  (if (any-bit-set-p dl-descriptor +dl-not+)
    dl-descriptor
    (find-or-make-dl-descriptor (add-dl-bits (dl-language-set dl-descriptor)
                                             +dl-not+))))

(defun add-dl-bottom (dl-descriptor)
  (if (any-bit-set-p dl-descriptor +dl-bottom+)
    dl-descriptor
    (find-or-make-dl-descriptor (add-dl-bits (dl-language-set dl-descriptor)
                                             +dl-bottom+))))

(defun remove-dl-bottom (dl-descriptor)
  (if (any-bit-set-p dl-descriptor +dl-bottom+)
      (find-or-make-dl-descriptor (remove-dl-bits (dl-language-set dl-descriptor)
                                                  +dl-bottom+))
    dl-descriptor))

(defun add-dl-simple-some (dl-descriptor)
  (if (any-bit-set-p dl-descriptor +dl-simple-some+)
    dl-descriptor
    (find-or-make-dl-descriptor (add-dl-bits (dl-language-set dl-descriptor)
                                             +dl-simple-some+))))

(defun add-dl-some (dl-descriptor)
  (if (any-bit-set-p dl-descriptor +dl-some+)
    dl-descriptor
    (find-or-make-dl-descriptor (add-dl-bits (dl-language-set dl-descriptor)
                                             +dl-some+))))

(defun add-dl-all (dl-descriptor)
  (if (any-bit-set-p dl-descriptor +dl-all+)
    dl-descriptor
    (find-or-make-dl-descriptor (add-dl-bits (dl-language-set dl-descriptor)
                                             +dl-all+))))

(defun add-dl-simple-at-least (dl-descriptor)
  (if (any-bit-set-p dl-descriptor +dl-simple-at-least+)
    dl-descriptor
    (find-or-make-dl-descriptor (add-dl-bits (dl-language-set dl-descriptor)
                                             +dl-simple-at-least+))))

(defun add-dl-at-least (dl-descriptor)
  (if (any-bit-set-p dl-descriptor +dl-at-least+)
    dl-descriptor
    (find-or-make-dl-descriptor (add-dl-bits (dl-language-set dl-descriptor)
                                             +dl-at-least+))))

(defun add-dl-simple-at-most (dl-descriptor)
  (if (any-bit-set-p dl-descriptor +dl-simple-at-most+)
    dl-descriptor
    (find-or-make-dl-descriptor (add-dl-bits (dl-language-set dl-descriptor)
                                             +dl-simple-at-most+))))

(defun add-dl-at-most (dl-descriptor)
  (if (any-bit-set-p dl-descriptor +dl-at-most+)
    dl-descriptor
    (find-or-make-dl-descriptor (add-dl-bits (dl-language-set dl-descriptor)
                                             +dl-at-most+))))

(defun add-dl-simple-number-restrictions (dl-descriptor)
  (if (all-bits-set-p (dl-language-set dl-descriptor) (logior +dl-simple-at-least+ +dl-simple-at-most+))
    dl-descriptor
    (find-or-make-dl-descriptor (add-dl-bits (dl-language-set dl-descriptor)
                                             (logior +dl-simple-at-least+ +dl-simple-at-most+)))))

(defun add-dl-qualified-number-restrictions (dl-descriptor)
  (if (all-bits-set-p (dl-language-set dl-descriptor) (logior +dl-at-least+ +dl-at-most+))
    dl-descriptor
    (find-or-make-dl-descriptor (add-dl-bits (dl-language-set dl-descriptor)
                                             (logior +dl-at-least+ +dl-at-most+)))))

(defun add-dl-simple-role-inclusions (dl-descriptor)
  (if (any-bit-set-p dl-descriptor +dl-simple-role-inclusions+)
    dl-descriptor
    (find-or-make-dl-descriptor (add-dl-bits (dl-language-set dl-descriptor)
                                             +dl-simple-role-inclusions+))))

(defun add-dl-complex-role-inclusions (dl-descriptor)
  (if (any-bit-set-p dl-descriptor +dl-complex-role-inclusions+)
    dl-descriptor
    (find-or-make-dl-descriptor (add-dl-bits (dl-language-set dl-descriptor)
                                             +dl-complex-role-inclusions+))))

(defun add-dl-features (dl-descriptor)
  (if (any-bit-set-p dl-descriptor +dl-feature-roles+)
    dl-descriptor
    (find-or-make-dl-descriptor (add-dl-bits (dl-language-set dl-descriptor)
                                             +dl-feature-roles+))))

(defun add-dl-transitive (dl-descriptor)
  (if (any-bit-set-p dl-descriptor +dl-transitive-roles+)
    dl-descriptor
    (find-or-make-dl-descriptor (add-dl-bits (dl-language-set dl-descriptor)
                                             +dl-transitive-roles+))))

(defun add-dl-inverse (dl-descriptor)
  (if (any-bit-set-p dl-descriptor +dl-inverse-roles+)
    dl-descriptor
    (find-or-make-dl-descriptor (add-dl-bits (dl-language-set dl-descriptor)
                                             +dl-inverse-roles+))))

(defun add-dl-symmetric (dl-descriptor)
  (if (any-bit-set-p dl-descriptor +dl-symmetric-roles+)
    dl-descriptor
    (find-or-make-dl-descriptor (add-dl-bits (dl-language-set dl-descriptor)
                                             +dl-symmetric-roles+))))

(defun add-dl-asymmetric (dl-descriptor)
  (if (any-bit-set-p dl-descriptor +dl-asymmetric-roles+)
    dl-descriptor
    (find-or-make-dl-descriptor (add-dl-bits (dl-language-set dl-descriptor)
                                             +dl-asymmetric-roles+))))

(defun add-dl-reflexive (dl-descriptor)
  (if (any-bit-set-p dl-descriptor +dl-reflexive-roles+)
    dl-descriptor
    (find-or-make-dl-descriptor (add-dl-bits (dl-language-set dl-descriptor)
                                             +dl-reflexive-roles+))))

(defun add-dl-locally-reflexive (dl-descriptor)
  (if (any-bit-set-p dl-descriptor +dl-locally-reflexive-roles+)
    dl-descriptor
    (find-or-make-dl-descriptor (add-dl-bits (dl-language-set dl-descriptor)
                                             +dl-locally-reflexive-roles+))))

(defun add-dl-irreflexive (dl-descriptor)
  (if (any-bit-set-p dl-descriptor +dl-irreflexive-roles+)
    dl-descriptor
    (find-or-make-dl-descriptor (add-dl-bits (dl-language-set dl-descriptor)
                                             +dl-irreflexive-roles+))))

(defun add-dl-disjoint-roles (dl-descriptor)
  (if (any-bit-set-p dl-descriptor +dl-disjoint-roles+)
    dl-descriptor
    (find-or-make-dl-descriptor (add-dl-bits (dl-language-set dl-descriptor)
                                             +dl-disjoint-roles+))))

(defun add-dl-universal-role (dl-descriptor)
  (if (any-bit-set-p dl-descriptor +dl-universal-role+)
    dl-descriptor
    (find-or-make-dl-descriptor (add-dl-bits (dl-language-set dl-descriptor)
                                             +dl-universal-role+))))

(defun add-dl-full-concrete-domains (dl-descriptor)
  (if (any-bit-set-p dl-descriptor +dl-cd+)
    dl-descriptor
    (find-or-make-dl-descriptor (add-dl-bits (dl-language-set dl-descriptor) +dl-cd+))))

(defun add-dl-simple-concrete-domains (dl-descriptor)
  (if (any-bit-set-p dl-descriptor +dl-simple-cd+)
    dl-descriptor
    (find-or-make-dl-descriptor (add-dl-bits (dl-language-set dl-descriptor) +dl-simple-cd+))))

(defun add-dl-full-datatype-concrete-domains (dl-descriptor)
  (if (any-bit-set-p dl-descriptor +dl-full-datatype-cd+)
    dl-descriptor
    (find-or-make-dl-descriptor (add-dl-bits (dl-language-set dl-descriptor) +dl-full-datatype-cd+))))

(defun add-dl-simple-datatype-concrete-domains (dl-descriptor)
  (if (any-bit-set-p dl-descriptor +dl-simple-datatype-cd+)
    dl-descriptor
    (find-or-make-dl-descriptor (add-dl-bits (dl-language-set dl-descriptor) +dl-simple-datatype-cd+))))

(defun add-dl-equivalent-concept-inclusions (dl-descriptor)
  (if (any-bit-set-p dl-descriptor +dl-equiv-cis+)
    dl-descriptor
    (find-or-make-dl-descriptor (add-dl-bits (dl-language-set dl-descriptor) +dl-equiv-cis+))))

(defun union-dl-descriptors (dl-descriptor-1 dl-descriptor-2)
  (if dl-descriptor-1
    (if dl-descriptor-2
      (if (eq dl-descriptor-1 dl-descriptor-2)
        dl-descriptor-1
        (let ((language-set-1 (dl-language-set dl-descriptor-1))
              (language-set-2 (dl-language-set dl-descriptor-2)))
          (if (dl-bits-subset-p language-set-2 language-set-1)
            dl-descriptor-1
            (if (dl-bits-subset-p language-set-1 language-set-2)
              dl-descriptor-2
              (find-or-make-dl-descriptor (logior language-set-1 language-set-2))))))
      dl-descriptor-1)
    (when dl-descriptor-2
      dl-descriptor-2)))

(defmacro with-dl-language-set ((language-set dl-descriptor) &body body)
  `(let ((,language-set (dl-language-set ,dl-descriptor)))
     . ,body))

(defmacro subsumes-dl-language-set-p (subsumer subsumee)
  `(dl-bits-subset-p ,subsumee ,subsumer))

(defmacro set-language-set (language-set dl-descriptor)
  `(setf ,language-set (dl-language-set ,dl-descriptor)))
