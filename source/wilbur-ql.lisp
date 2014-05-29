;;; -*- package: WILBUR; Syntax: Common-lisp; Base: 10 -*-

;;;
;;;;  wilbur-ql.lisp
;;;
;;;
;;; --------------------------------------------------------------------------------------
;;;
;;;   The contents of this file are subject to the NOKOS License Version 1.0a (the
;;;   "License"); you may not use this file except in compliance with the License. 
;;;
;;;   Software distributed under the License is distributed on an "AS IS" basis, WITHOUT
;;;   WARRANTY OF ANY KIND, either express or implied. See the License for the specific
;;;   language governing rights and limitations under the License. 
;;;
;;;   The Original Software is 
;;;     WILBUR2: Nokia Semantic Web Toolkit for CLOS
;;;
;;;   Copyright (c) 2001-2005 Nokia and others. All Rights Reserved.
;;;   Portions Copyright (c) 1989-1992 Ora Lassila. All Rights Reserved.
;;;
;;;   Contributor(s): Ora Lassila (mailto:ora.lassila@nokia.com)
;;;
;;; --------------------------------------------------------------------------------------
;;;
;;;
;;;   Version: $Id: ivanhoe.lisp,v 1.12 2004/11/28 23:13:00 ora Exp $
;;;
;;;   Purpose: This file implements the Wilbur Query Language (WilburQL) which essentially
;;;   is a simple API on top of the RDF data manager (in "core-data.lisp"). Much of the
;;;   functionality is modeled after the BEEF frame system:
;;;
;;;      Ora Lassila: "BEEF Reference Manual - A Programmer's Guide to the BEEF Frame
;;;        System", Second Version, Report HTKK-TKO-C46, Otaniemi (Finland), Department of
;;;        Computer Science, Helsinki University of Technology, 1991
;;;
;;;      Juha Hynynen and Ora Lassila: "On the Use of Object-Oriented Paradigm in a
;;;        Distributed Problem Solver", AI Communications 2(3): 142-151 (1989)
;;;
;;;   A description of the WilburQL itself can be found in the following paper:
;;;
;;;      Ora Lassila: "Taking the RDF Model Theory Out for a Spin", in: Ian Horrocks &
;;;        James Hendler (eds.): "The Semantic Web - ISWC 2002", Lecture Notes in Computer
;;;        Science 2342, pp.307-317, Springer Verlag, 2002
;;;


(in-package "WILBUR")


(eval-when (:compile-toplevel :load-toplevel :execute)
   (enable-node-shorthand)
)

;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS PATH
;;;
;;;   The path grammar implementation is derived from BEEF frame system.
;;;

(defclass path ()
  ((expression
    :initarg :expression
    :reader path-expression)
   (fsa
    :accessor path-fsa)))

(defmethod print-object ((path path) stream)
  (print-unreadable-object (path stream :type t)
    (prin1 (path-expression path) stream)))

(defmethod initialize-instance :after ((self path) &rest args)
  (declare (ignore args))
  (setf (path-fsa self) (make-path-fsa (path-expression self))))

(defmethod invert ((path path))
  (make-instance 'path :expression (invert-path (path-expression path))))

(defstruct (path-node
            (:conc-name pn-)
            (:copier nil)
            (:constructor new-pn (link)))
  (link nil :read-only t)                    ; slot name i.e. link in the path
  (follows nil))                             ; possible followers of this node

(defstruct (path-fsa-state
            (:conc-name ps-)
            (:copier nil)
            (:constructor new-ps (positions)))
  (positions nil :read-only t)               ; positions defining this state
  (transitions nil))                         ; transitions from this state

(defstruct (path-fsa-transition
            (:conc-name pt-)
            (:constructor new-pt (input index))
            (:copier nil))
  (input nil :read-only t)                   ; input symbol (= predicate name)
  (index nil :read-only t))                  ; index of the target state

(defmethod print-object ((self path-fsa-transition) stream)
  (print-unreadable-object (self stream :type t)
    (let ((input (pt-input self)))
      (typecase input
        (inverse-slot
         (format stream ":inv ~S->~D"
                 (inverse-slot-node input) (pt-index self)))
        (node
         (format stream "~S->~D" input (pt-index self)))
        (default-value
         (format stream ":value ~S ->~D"
                 (default-value-value input) (pt-index self)))
        ((eql :members)
         (format stream "~S->~D" input (pt-index self)))))))

(defparameter *path-fsas* (make-hash-table :test #'equal))

(define-modify-macro unionf (items) union)

(defun canonical-path (expr)
  (etypecase expr
    (cons
     (destructuring-bind (op arg &rest args) expr
       (if arg
         (ecase op
           ((:rep* :rep+ :inv :value :filter)
            (assert (null args) nil "Extra operands for ~S in ~S" op expr)
            (case op
              (:rep+  (canonical-path `(:seq ,arg (:rep* ,arg))))
              (:rep*  `(,op ,(canonical-path arg)))
              (:inv   (invert-path arg))
              (:value (make-instance 'default-value :value arg))
	      (:filter (make-instance 'node-filter :pattern arg))))
           ((:seq :seq+ :or)
            (if args
              (let ((arg (canonical-path arg))
                    (args (mapcar #'canonical-path args)))
                (unless (or (atom arg) (not (eq op (first arg))))
                  (psetq arg (second arg)
                         args (append (cddr arg) args)))
                (cond ((rest args)
                       (canonical-path `(,op ,arg (,op ,@args))))
                      ((eq op :or)
                       `(,op ,(canonical-path (first args)) ,(canonical-path arg)))
                      (t
                       `(,op ,(canonical-path arg) ,(canonical-path (first args))))))
              (canonical-path arg))))
         (error 'query-syntax-error :thing op))))
    (string
     (node expr))
    (keyword
     (assert (member expr '(:members :any :object-of-predicate :subject-of-predicate)))
     expr)
    ((or node inverse-slot default-value)
     expr)))

(defun invert-path (expr)
  (labels ((i (p)
             (etypecase p
               (cons
                (ecase (first p)
                  (:or
                   `(,(first p)
                     ,@(mapcar #'i (reverse (remove 'default-value (rest p)
						    :key #'type-of)))))
                  ((:seq :seq+)
                   `(,(first p) ,@(mapcar #'i (reverse (rest p)))))
                  (:rep*
                   `(:rep* ,(i (second p))))))
               (node
                (make-instance 'inverse-slot :node p))
               (inverse-slot
                (inverse-slot-node p))
               (default-value
                (error 'cannot-invert-default-value :thing (default-value-value p))))))
    (i (canonical-path expr))))

(defun make-path-fsa (expr)
  (and expr
       (or (gethash expr *path-fsas*)
           (setf (gethash expr *path-fsas*)
                 (construct-new-path-fsa (canonical-path expr))))))

(defvar *fsa-states/temporary* (make-array 8 :adjustable t :fill-pointer 0))

(defun construct-new-path-fsa (expr &aux (inputs nil))
  (labels ((decorate (x)
             (if (atom x)
               (let ((node (list (new-pn x))))
                 (pushnew x inputs)
                 (values node node nil))
               (case (pop x)
                 (:seq  (multiple-value-bind (first1 last1 null1)
                                             (decorate (first x))
                          (multiple-value-bind (first2 last2 null2)
                                               (decorate (second x))
                            (add-followers last1 first2)
                            (values (if null1 (union first1 first2) first1)
                                    (if null2 (union last1 last2) last2)
                                    (and null1 null2)))))
                 (:seq+ (multiple-value-bind (first1 last1 null1)
                                             (decorate (first x))
                          (multiple-value-bind (first2 last2)
                                               (decorate (second x))
                            (add-followers last1 first2)
                            (values (if null1 (union first1 first2) first1)
                                    (union last1 last2)
                                    null1))))
                 (:or   (multiple-value-bind (first1 last1 null1)
                                             (decorate (first x))
                          (multiple-value-bind (first2 last2 null2)
                                               (decorate (second x))
                            (values (union first1 first2)
                                    (union last1 last2)
                                    (or null1 null2)))))
                 (:rep* (multiple-value-bind (first last)
                                             (decorate (first x))
                          (add-followers last first)
                          (values first last t))))))
           (add-followers (from to)
             (dolist (i from) (unionf (pn-follows i) to)))
           (add-state (positions)
             (or (position positions *fsa-states/temporary*
                           :key #'ps-positions
                           :test #'(lambda (x y)
                                     (and (subsetp x y) (subsetp y x))))
                 (vector-push-extend (new-ps positions) *fsa-states/temporary*))))
    (setf (fill-pointer *fsa-states/temporary*) 0)
    (add-state (decorate `(:seq ,expr nil)))
    (do ((i 0 (1+ i)))
        ((= i (length *fsa-states/temporary*)))
      (let ((state (elt *fsa-states/temporary* i)))
        (dolist (input inputs)
          (let ((positions nil))
            (dolist (p (ps-positions state))
              (when (eq (pn-link p) input)
                (unionf positions (pn-follows p))))
            (when positions
              (let ((index (add-state positions)))
                (when input
                  (push (new-pt input index) (ps-transitions state)))))))))
    (map 'simple-vector
         #'(lambda (s)
             (cons (and (member nil (ps-positions s) :key #'pn-link) t)
                   (reverse (ps-transitions s))))
         *fsa-states/temporary*)))

(defvar *walk-states/temporary* (list (make-hash-table :test #'eq)
                                      (make-hash-table :test #'eq)
                                      (make-hash-table :test #'eq)))

#+:junk ; not used
(defun db-get-slot-values (db frame slot)
  (etypecase slot
    (node
     (mapcar #'triple-object (db-query db frame slot nil)))
    (inverse-slot
     (let ((s (inverse-slot-node slot)))
       (mapcar #'triple-subject (db-query db nil (if (eq s :any) nil s) frame))))
    (default-value
     (list (default-value-value slot)))
    (symbol
     (get-some-values frame slot db nil))))

#+:junk ; not used?
(defun db-get-path-values (db frame path)
  (collect-using-fsa frame (make-path-fsa path) db))

(defun walk-using-fsa (root fsa action db)
  (let* ((*walk-states/temporary* *walk-states/temporary*)
         (states (clrhash (or (pop *walk-states/temporary*)
                              (make-hash-table :test #'eq)))))
    (labels ((walk (f i)
               (unless (member i (gethash f states) :test #'=)
                 (push i (gethash f states))
                 (let ((transitions (svref fsa i)))
                   (or (when (first transitions)
                         (funcall action f))
                       (when (typep f 'node)
                         (dolist (link (rest transitions))
                           (dolist (v (get-some-values f (pt-input link) db nil))
                             (let ((values (walk v (pt-index link))))
                               (when values
                                 (return-from walk-using-fsa values)))))))))))
      (declare (dynamic-extent #'walk))
      (when fsa
        (walk root 0)))))

(defun collect-using-fsa (root fsa db)
  (let ((nodes nil))
    (flet ((c (n) (pushnew n nodes) nil))
      (declare (dynamic-extent #'c))
      (walk-using-fsa root fsa #'c db)
      (nreverse nodes))))


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS INVERSE-SLOT
;;;   CLASS DEFAULT-VALUE
;;;   CLASS NODE-FILTER
;;;

(defclass inverse-slot ()
  ((node
    :initarg :node
    :reader inverse-slot-node)))

(defmethod print-object ((self inverse-slot) stream)
  (print-unreadable-object (self stream :type t)
    (prin1 (node-uri (inverse-slot-node self)) stream)))

(defclass default-value ()
  ((value
    :initarg :value
    :reader default-value-value)))

(defmethod print-object ((self default-value) stream)
  (print-unreadable-object (self stream :type t)
    (prin1 (default-value-value self) stream)))

(defclass node-filter ()
  ((pattern
    :initarg :pattern
    :reader node-filter-pattern)))

(defmethod print-object ((self node-filter) stream)
  (print-unreadable-object (self stream :type t)
    (prin1 (node-filter-pattern self) stream)))

(defmethod node-filter-match ((filter node-filter) (node node))
  (and (string-filter-match-p (node-filter-pattern filter) (node-uri node))
       node))

(defmethod node-filter-match ((filter node-filter) (literal literal))
  (and (string-filter-match-p (node-filter-pattern filter) (literal-string literal))
       literal))

(defun string-filter-match-p (pattern string)
  (name-contains-pattern-p string pattern))


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS NODE (FRAME SYSTEM API ADDITIONS)
;;;

(defmethod get-value-when-failed ((frame node) (path node) (db db))
  (when (eq path (node -rdf-type-uri-))
    (node -rdfs-resource-uri-)))

(defmethod get-value-when-failed ((frame node) (path inverse-slot) (db db))
  nil)

(declaim (inline one-or-many))

(defun one-or-many (value index)
  (if index value (list value)))

(defmethod get-some-values ((frame node) (path node) (db db) index)
  (let ((triples (db-query db frame path nil)))
    (if triples
      (if index
	(triple-object (elt triples index))
	(mapcar #'triple-object triples))
      (one-or-many (get-value-when-failed frame path db) index))))

(defmethod get-some-values ((frame node) (path inverse-slot) (db db) index)
  (let* ((slot (inverse-slot-node path))
	 (triples (db-query db nil (if (eq slot :any) nil slot) frame)))
    (if triples
      (if index
	(triple-subject (elt triples index))
	(mapcar #'triple-subject triples))
      (one-or-many (get-value-when-failed frame path db) index))))

(defmethod get-some-values ((frame node) (path cons) (db db) index)
  (let ((fsa (make-path-fsa path)))
    (if index
      (walk-using-fsa frame fsa #'identity db)
      (collect-using-fsa frame fsa db))))

(defmethod get-some-values ((frame node) (path path) (db db) index)
  (if index
    (walk-using-fsa frame (path-fsa path) #'identity db)
    (collect-using-fsa frame (path-fsa path) db)))

(defmethod get-some-values ((frame node) (path default-value) (db db) index)
  (one-or-many (default-value-value path) index))

(defmethod get-some-values ((frame node) (path node-filter) (db db) index)
  (one-or-many (node-filter-match path frame) index))

(defmethod get-some-values ((frame node) (path #-:ccl symbol 
					       #+:ccl keyword)
			    (db db) index)
  (ecase path
    (:members
     (if index
       (get-some-values frame (node (index-uri (1+ index))) db 0)
       (loop for i from 1
	     for v = (get-some-values frame (node (index-uri i)) db 0)
	     while v collect v)))
    (:any
     (let ((triples (db-query db frame nil nil)))
       (if index
	 (triple-object (elt triples index))
	 (mapcar #'triple-object triples))))))

(defmethod get-all-values ((frame node) path (db db))
  (get-some-values frame path db nil))

(defmethod get-value ((frame node) path (db db))
  (get-some-values frame path db 0))

(defmethod frames-related-p ((source node)
                             (path node)
                             (sink node)
                             (db db)
                             action)
  (declare (ignore action)) 
  (or (not (null (db-query db source path sink)))
      (eq sink (get-value-when-failed source path db))))

(defmethod frames-related-p ((source node)
                             (path inverse-slot)
                             (sink node)
                             (db db)
                             action)
  (frames-related-p sink (inverse-slot-node path) source db action))

(defmethod frames-related-p ((source node)
                             (path cons)
                             (sink node)
                             (db db)
                             action)
  (frames-related-p source (make-instance 'path :expression path) sink db action))

(defmethod frames-related-p ((source node)
                             (path path)
                             (sink node)
                             (db db)
                             action)
  (flet ((is-sink-p (node)
           (when action
             (funcall action node))
           (eq node sink)))
    (declare (dynamic-extent #'is-sink-p))
    (walk-using-fsa source (path-fsa path) #'is-sink-p db)))


;;; --------------------------------------------------------------------------------------
;;;
;;;   ADDITIONAL FUNCTIONALITY
;;;

(defun triple-reified-p (triple db)
  (let ((s-statements (db-query db nil !rdf:subject (triple-subject triple))))
    (when s-statements
      (let ((o-statements (db-query db nil !rdf:object (triple-object triple))))
	(when o-statements
	  (let ((predicate (triple-predicate triple)))
	    (remove-if-not #'(lambda (node)
			       (db-query db node !rdf:predicate predicate))
			   (intersection (mapcar #'triple-subject s-statements)
					 (mapcar #'triple-subject o-statements)))))))))
