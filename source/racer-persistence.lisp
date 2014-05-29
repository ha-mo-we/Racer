;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: PERSISTENCE-MANAGER; Base: 10 -*-

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

(in-package :persistence-manager)

#|
(defun print-persistence-manager-info (&optional (stream t))
  (format stream ";;; The store/restore facility is based on software developed~%~
                  ;;; by Michael Wessel.~2%"))
|#

(cl:defstruct (big-vector (:constructor make-big-vector-internal))
  n-elements 
  n-subarrays
  (arrays nil))

(defconstant +max-n-of-array-elements+ (- array-total-size-limit 1))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun string-transform (string)
    (ecase (readtable-case *readtable*)
      (:upcase (if (char= (aref string 0) #\|)
                 string
                 (string-upcase string)))
      (:preserve string)
      (:downcase (string-downcase string)))))

(defun make-big-vector (dimension &key (initial-element nil))
  (multiple-value-bind (n-subarrays-minus-1 size-of-rest-array)
      (floor dimension +max-n-of-array-elements+)
    (let ((big-vector (make-big-vector-internal)))
      (setf (big-vector-n-subarrays big-vector) (+ n-subarrays-minus-1 1))
      (setf (big-vector-n-elements big-vector) dimension)
      (setf (big-vector-arrays big-vector)
            (coerce (append (loop repeat n-subarrays-minus-1 
                                  collect (make-array +max-n-of-array-elements+
                                                      :initial-element initial-element))
                            (list (make-array size-of-rest-array)))
                    'vector))
      big-vector)))

(declaim (inline bvref))

(defun bvref (big-array index)
  (multiple-value-bind (subarray-index index-of-rest-array)
      (floor index +max-n-of-array-elements+)
    (svref (svref (big-vector-arrays big-array) subarray-index)
           index-of-rest-array)))

(declaim (inline (setf bvref)))

(defun (setf bvref) (new-value big-array index)
  (multiple-value-bind (subarray-index index-of-rest-array)
      (floor index +max-n-of-array-elements+)
    (setf (svref (svref (big-vector-arrays big-array) subarray-index)
                 index-of-rest-array)
          new-value)))

;;;
;;;
;;;

(declaim (inline length1 is-dotted-list-p))

(defun length1 (list)
  (let* ((x list)
         (length 0))

    (loop 
     (pop x)
       
     (unless (listp x)
       (return))
     
     (unless x
       (return))
       
     (incf length))
    
    length))
  
(defun is-dotted-list-p (list)
  (loop 
   (pop list)
   
   (unless (listp list)
     (return-from is-dotted-list-p t))

   (unless list
     (return-from is-dotted-list-p nil))))

;;;
;;;
;;;

(defconstant +persistence-version+ 3)

#+:debug (defconstant +file-type-marker+ 66647955)
#-:debug (defconstant +file-type-marker+ 66647952)

(defconstant +n-bytes-for-written-objects+ 16)

(defconstant +maximum-written-objects+ 
  (- (expt 10 +n-bytes-for-written-objects+) 1))

(defconstant +hash-table-size+ 100000)

(defconstant +rehash-size+ 2.0)

(defvar *read-objects* nil)

(defvar *written-objects* 
  (make-hash-table :test #'eql
                   :size +hash-table-size+
                   :rehash-size +rehash-size+))

;;;
;;;
;;;

(defvar *io-id-counter* 0)

(defvar *aux-counter* 0)

(defvar *ref-counter* 0)

(declaim (inline get-io-id-for get-io-id store retrieve 
                 write-coded-string
                 write-coded-string-1
                 read-coded-string
                 write-coded-symbol
                 read-coded-symbol
                 write-coded-integer
                 read-coded-integer
                 write-coded-number
                 read-coded-numer
                 write-coded-marker
                 read-coded-marker
                 write-coded-list
                 read-coded-list
                 read-byte*
                 unread-byte*))
;;;
;;;
;;;

(defvar *last-byte-read* nil)

(defvar *unread-occured-p* nil)

(defun read-byte* (stream)
  (if (and *unread-occured-p* *last-byte-read*)
      (progn 
        (setf *unread-occured-p* nil)
        *last-byte-read*)
    (let ((byte (read-byte stream nil 'eof)))
      (if (eq byte 'eof)
          (setf *last-byte-read* nil)
        (setf *last-byte-read* byte))
      byte)))

(defun unread-byte* ()
  (setf *unread-occured-p* t))


(defun write-coded-integer (stream num)
  (let ((orig-num num)
        (num (abs num)))
    (loop 
     (multiple-value-bind (div mod)
         (floor num 254)
       (write-byte mod stream)
       (setf num div)
       (when (= 0 div) 
         (write-byte (if (minusp orig-num)
                         254 255)
                     stream)
         (return-from write-coded-integer))))))

(defun write-coded-string (stream string)
  (write-coded-integer stream (length string))
  (loop as char across string do
        (write-byte (char-code char) stream)))

(defun write-coded-string-1 (stream string)
  (loop as char across string do
        (write-byte (char-code char) stream)))

(defun write-coded-symbol (stream symbol)
  (let* ((package (symbol-package symbol))
         (name (symbol-name symbol))
         (length (+ 2
                    (if (null package)
                        (+ 2 (length name))
                      (+ (length (package-name package)) 2 (length name))))))
    (write-coded-integer stream length)
    (if (null package)
        (write-coded-string-1 stream "#:")
      (progn 
        (write-coded-string-1 stream (package-name package))
        (write-coded-string-1 stream "::")))
    (write-coded-string-1 stream "|")
    (write-coded-string-1 stream name)
    (write-coded-string-1 stream "|")))


(defun read-coded-integer (stream)
  (let ((num 0)
        (index 1))
    (loop 
     (let ((byte (read-byte* stream)))
       (cond ((eq byte 'eof)
              (return-from read-coded-integer 'eof))
             ((= byte 255)
              (return-from read-coded-integer num))
             ((= byte 254)
              (return-from read-coded-integer (- num)))
             (t (incf num (* index byte))
                (setf index (* index 254))))))))

(defun read-coded-string (stream)
  (let* ((n (read-coded-integer stream))
         (string (make-string n ;;:initial-element #\space
                              )))
    (loop as i from 1 to n do
          (setf (aref string (1- i))
                (code-char (read-byte stream))))
    string))


(defun read-coded-symbol (stream)
  (read-from-string (read-coded-string stream)))



(defun write-coded-number (stream num)
  (write-coded-string stream (format nil "~S" num)))

(defun read-coded-number (stream)
  (read-from-string (read-coded-string stream)))

(defun write-coded-marker (stream marker)
  (write-byte marker stream))

(defun read-coded-marker (stream)
  (read-byte* stream))


(defun write-coded-list (stream list)
  (write-coded-string stream (format nil "~S" list)))

(defun read-coded-list (stream)
  (read-from-string (read-coded-string stream)))

;;;
;;;
;;;

(defun get-io-id-for (object)
  (or (gethash object *written-objects*)
      (error "Can't get IO id for object ~A!" object)))

(defun get-io-id ()
  (incf *io-id-counter*))

(defun store (io-id obj)
  (setf *io-id-counter* (max io-id *io-id-counter*)) 
  (setf (bvref *read-objects* io-id) obj)
  obj)

(defun retrieve (io-id)
  (or (bvref *read-objects* io-id)
      (error "Object ~A not found! Dangling pointer!" io-id)))

(defun reset ()
  (setf *io-id-counter* 0
        *aux-counter* 0
        *ref-counter* 0
        *unread-occured-p* nil)
  ;;; (clrhash *structures*)
  ;;; Ralf fragen - warum geht das nicht???
  (clrhash *written-objects*))

(defun read-reset (n-of-objects-to-read)
  (setf *io-id-counter* 0
        *aux-counter* 0
        *ref-counter* 0
        *unread-occured-p* nil)
  (setf *read-objects* (make-big-vector (+ n-of-objects-to-read 3)
                                        :initial-element nil)))

;;;
;;;
;;;

(cl:defclass persistent-object ()
  ())

(defmethod write-object-constructor ((obj persistent-object) stream)
  (declare (ignore stream))
  nil)

(defmethod write-object-initializer ((obj persistent-object) stream)
  (declare (ignore stream))
  nil)

(defmethod fill-persistent-object ((obj persistent-object) stream)
  (declare (ignore stream))
  nil)

;;;
;;;
;;;

(defmethod read-slot-value ((obj persistent-object) (slot symbol) (stream stream))
  (read-value stream))

(defmethod write-slot-value ((obj persistent-object) (slot symbol) (stream stream))
  (write-referencer (slot-value obj slot) stream))

;;; verhindere, da"s initialize-instance fuer Subklassen aufgerufen wird!

(defmethod initialize-instance :around ((obj persistent-object) 
					&rest initargs
					&key (dont-initialize nil))
  (if (not dont-initialize)
      (progn
	(call-next-method))		; normale Initialisierung
    (apply #'shared-initialize obj t initargs)))

(defmethod initialize-loaded-persistent-object ((obj persistent-object))
  nil)

;;;
;;;
;;;

(cl:defstruct persistent-structure)

(defmethod write-object-constructor ((obj persistent-structure) stream)
  (declare (ignore stream))
  nil)

(defmethod write-object-initializer ((obj persistent-structure) stream)
  (declare (ignore stream))
  nil)

(defmethod fill-persistent-object ((obj persistent-structure) stream)
  (declare (ignore stream))
  nil)

(defmethod initialize-loaded-persistent-object ((obj persistent-structure))
  nil)

;;;
;;;
;;;

(defconstant +already-present-marker+ 0)
(defconstant +string-marker+ 1)
(defconstant +array-marker+ 2)
(defconstant +hashtable-marker+ 3)
(defconstant +nil-marker+ 5)
(defconstant +integer-marker+ 6)
(defconstant +number-marker+ 7)
(defconstant +symbol-marker+ 8)
(defconstant +object-marker+ 9)
(defconstant +structure-marker+ 10)
(defconstant +unbound-marker+ 11)
(defconstant +section-marker+ 12)
(defconstant +package-marker+ 13)
(defconstant +char-marker+ 14)

(defconstant +list-marker+ 15)
(defconstant +dotted-list-marker+ 16)
(defconstant +vector-marker+ 17)


(defvar *hashtable-contents* (intern "*#?@secret-xxsax123123cdscds908"))

;;;
;;;
;;;

(defmacro with-only-once-constructed-object ((object marker stream) &body body)
  `(unless (gethash ,object *written-objects*)
     (let ((io-id (get-io-id)))
       (setf (gethash ,object *written-objects*) io-id)
       (write-coded-marker ,stream ,marker)
       (write-coded-integer ,stream io-id)
       ,@body)))

(defmethod write-constructor :after ((object t) stream)
  (declare (ignore stream))
  (incf *ref-counter*))

;;;
;;;
;;;

(defmethod write-constructor ((object t) stream)
  (declare (ignore stream))
  nil)

(defmethod write-constructor ((object null) stream)
  (declare (ignore stream))
  nil)

(defmethod write-constructor ((object list) stream)
  (if (is-dotted-list-p object)
      (let ((length (length1 object)))
        (with-only-once-constructed-object (object +dotted-list-marker+ stream)
          (write-coded-integer stream length)
          (let ((x object))
            (dotimes (i length) 
              (write-constructor (pop x) stream))
            ;; letzte CONS-Zelle schreiben
            (write-constructor (car x) stream)
            (write-constructor (cdr x) stream))))
    (let ((length (length object)))
      (with-only-once-constructed-object (object +list-marker+ stream)
        (write-coded-integer stream length)
        (let ((x object))
          (dotimes (i length) 
            (write-constructor (pop x) stream))))))
  
  (values))

(defmethod write-constructor ((object string) stream)
  (with-only-once-constructed-object (object +string-marker+ stream)
    (write-coded-string stream object)))

(defmethod write-constructor ((object symbol) stream)
  (with-only-once-constructed-object (object +symbol-marker+ stream)
    (write-coded-symbol stream object)))

(defmethod write-constructor ((object array) stream)
  (with-only-once-constructed-object (object +array-marker+ stream)
    (write-coded-list stream (array-dimensions object))
    (dotimes (i (array-total-size object))
      (write-constructor (row-major-aref object i) stream))))

(defmethod write-constructor ((object vector) stream)
  (if (array-has-fill-pointer-p object)
    (with-only-once-constructed-object (object +vector-marker+ stream)
      (write-coded-list stream (list (array-dimensions object)
                                     (fill-pointer object)))
      (dotimes (i (fill-pointer object))
        (write-constructor (row-major-aref object i) stream)))
    (call-next-method)))


(defmethod write-constructor ((object hash-table) stream)
  (with-only-once-constructed-object (object +hashtable-marker+ stream)
    (write-coded-integer stream (hash-table-size object))
    (write-coded-number stream (hash-table-rehash-size object))
    (write-coded-number stream (hash-table-rehash-threshold object))
    (write-coded-symbol stream (hash-table-test object))

    #+:allegro
    (write-coded-symbol stream (excl:hash-table-hash-function object))
    #-:allegro
    (write-coded-symbol stream 'dummy)

    
    (let ((res nil))
      (maphash #'(lambda (key value)
                   (push (list key value) res))
               object)
      (setf (gethash *hashtable-contents* object) res)
      (write-constructor res stream))))


(defmethod write-constructor ((object persistent-object) stream)
  (write-constructor (type-of object) stream)
  (with-only-once-constructed-object (object +object-marker+ stream)
    (write-referencer (type-of object) stream)
    (write-object-constructor object stream)))


(defmethod write-constructor ((object persistent-structure) stream)
  (write-constructor (type-of object) stream)
  (with-only-once-constructed-object (object +structure-marker+ stream)
    (write-referencer (type-of object) stream)
    (write-coded-list stream nil) ; Initialisierungsargumente 
    (write-object-constructor object stream)))

;;;
;;;
;;;

(defmacro with-complex-object-header ((id stream) &body body)
  `(progn 
     (write-coded-integer ,stream ,id)
     ,@body))

(defun write-referencer (object stream)
  (typecase object
    (null 
     (write-coded-marker stream +nil-marker+))
    ((or cons array hash-table string symbol persistent-object)
     (write-coded-marker stream +object-marker+)
     (write-coded-integer stream (get-io-id-for object)))
    (persistent-structure
     (write-coded-marker stream +structure-marker+)
     (write-coded-integer stream (get-io-id-for object)))
    (integer (write-coded-marker stream +integer-marker+)
             (write-coded-integer stream object))
    (number 
     (write-coded-marker stream +number-marker+)
     (write-coded-number stream object))
    (character
     (write-coded-marker stream +char-marker+)
     (write-coded-integer stream (char-code object)))
    (package 
     (write-coded-marker stream +package-marker+)
     (write-coded-string stream (package-name object)))
    (otherwise (error "Can't export objects of type ~A!"
                      (type-of object)))))


(defmethod write-initializer ((object string) id stream)
  (declare (ignorable object id stream))
  t)

(defmethod write-initializer ((object symbol) id stream)
  (declare (ignorable object id stream))
  t)

(defmethod write-initializer ((object package) id stream)
  (declare (ignorable object id stream))
  t)

(defmethod write-initializer ((object character) id stream)
  (declare (ignorable object id stream))
  t)


(defmethod write-initializer ((object list) id stream)
  (with-complex-object-header (id stream)    
    (if (is-dotted-list-p object)
        (let ((length (length1 object))
              (x object))
          (dotimes (i length) 
            (write-referencer (pop x) stream))
          (write-referencer (car x) stream)
          (write-referencer (cdr x) stream))
      (let ((length (length object))
            (x object))
        (dotimes (i length) 
          (write-referencer (pop x) stream)))))
  (values))

(defmethod write-initializer ((object array) id stream)
  (with-complex-object-header (id stream)
    (dotimes (i (array-total-size object))
      (write-referencer (row-major-aref object i)
			stream))))

(defmethod write-initializer ((object vector) id stream)
  (if (array-has-fill-pointer-p object)
    (with-complex-object-header (id stream)
      (dotimes (i (fill-pointer object))
        (write-referencer (row-major-aref object i)
                          stream)))
    (call-next-method)))


(defmethod write-initializer ((object hash-table) id stream)
   (with-complex-object-header (id stream)
     (write-referencer (gethash *hashtable-contents* object)
                      stream)))

(defmethod write-initializer ((object persistent-object) id stream)
  (with-complex-object-header (id stream)
    (write-object-initializer object stream)))

(defmethod write-initializer ((object persistent-structure) id stream)
  (with-complex-object-header (id stream)
    (write-object-initializer object stream)))

;;;
;;;
;;;

(defmethod fill-object ((object symbol) stream id)  
  (declare (ignore stream id))
  object)

(defmethod fill-object ((object string) stream id)  
  (declare (ignore stream id))
  object)

(defmethod fill-object ((object package) stream id)  
  (declare (ignore stream id))
  object)

(defmethod fill-object ((object character) stream id)  
  (declare (ignore stream id))
  object)

(defmethod fill-object ((object list) stream id)
  (declare (ignore id))
  (let ((x object))
    (loop
     (setf (car x) (read-value stream))
     (unless (cdr x) 
       (return))
     (unless (listp (cdr x))
       (setf (cdr x)
             (read-value stream))
       (return))
     (setf x (cdr x)))
    object))

(defmethod fill-object ((object array) stream id)
  (declare (ignore id))
  (dotimes (i (array-total-size object))
    (setf (row-major-aref object i)
          (read-value stream)))
  object)

(defmethod fill-object ((object vector) stream id)
  (declare (ignore id))
  (if (array-has-fill-pointer-p object)
    (dotimes (i (fill-pointer object))
      (setf (row-major-aref object i)
            (read-value stream)))
    (call-next-method)))

(defmethod fill-object ((object hash-table) stream id)  
  (declare (ignore id))

  (setf (gethash *hashtable-contents* object)
        (read-value stream))
  
  object)

(defmethod fill-object ((object persistent-object) stream id)
  (declare (ignore id))
  (fill-persistent-object object stream)
  object)

(defmethod fill-object ((object persistent-structure) stream id)
  (declare (ignore id))
  (fill-persistent-object object stream)
  object)


;;;
;;;
;;;

(defun read-value (stream)
  (let ((marker (read-coded-marker stream)))
    (cond ((or (= marker +object-marker+)
               (= marker +structure-marker+)
               (= marker +array-marker+)
               (= marker +vector-marker+)
               (= marker +hashtable-marker+))
	   (values 
	    (retrieve (read-coded-integer stream))))
          ((= marker +nil-marker+)
	   (values 
	    nil))
          ((= marker +integer-marker+)
	   (values 
	    (read-coded-integer stream)))	  
	  ((= marker +number-marker+)
	   (values 
	    (read-coded-number stream)))
          ((= marker +char-marker+)
	   (values 
            (code-char
             (read-coded-integer stream))))
	  ((= marker +package-marker+)
           (values
            (find-package 
             (read-coded-string stream))))
	  ((= marker +unbound-marker+)
           (values 
            nil))
	  (t (error "Bad marker ~A in read-value!" marker)))))



(defun probe-bound-p (stream)
  (let ((marker (read-coded-marker stream)))
    (prog1 
        (not (= marker +unbound-marker+))
      (unread-byte*))))


(defun construct-object (stream)
  (loop 
   (let ((marker (read-coded-marker stream)))

     (if (or (= marker +section-marker+)
             (eq marker 'eof))
         (return)
       (let ((id (read-coded-integer stream)))
         (store id
                (cond ((= marker +string-marker+)
                       (read-coded-string stream))
                      ((= marker +symbol-marker+)
                       (read-coded-symbol stream))
                      ((= marker +list-marker+)
                       (loop as i from 1 to (read-coded-integer stream)
                             collect (incf *aux-counter*)))
                      ((= marker +dotted-list-marker+)
                       (let ((list 
                              (loop as i from 1 to (read-coded-integer stream)
                                    collect (incf *aux-counter*))))
                         (if list 
                             (setf (cdr (last list))
                                   (cons (incf *aux-counter*)
                                         ;;; damit als DOTTED-LIST erkennbar!
                                         (incf *aux-counter*)))
                           (cons (incf *aux-counter*)
                                 (incf *aux-counter*)))))
                      ((= marker +array-marker+)
                       (make-array (read-coded-list stream)))
                      ((= marker +vector-marker+)
                       (destructuring-bind (dimensions fill-pointer)
                                           (read-coded-list stream)
                         (make-array dimensions
                                     :adjustable t
                                     :fill-pointer fill-pointer)))
                      ((= marker +hashtable-marker+)
                       (let* ((size (read-coded-integer stream))
                              (rehash-size (read-coded-number stream))
                              (rehash-threshold (read-coded-number stream))
                              (test (read-coded-symbol stream))
                              (hash-function (read-coded-symbol stream))
                              (table (if (eq hash-function 'dummy)
                                       (make-hash-table 
                                        :size size
                                        :rehash-size rehash-size
                                        :rehash-threshold rehash-threshold
                                        :test test)
                                       (make-hash-table 
                                        :size size
                                        :rehash-size rehash-size
                                        :rehash-threshold rehash-threshold
                                        :test test
				        #+:allegro :hash-function 
                                        #+:allegro hash-function))))
                         table))

                      ((= marker +object-marker+)
                       (make-instance (read-value stream)
                                      :allow-other-keys t 
                                      :dont-initialize t))
                      ((= marker +structure-marker+)
		       (let ((type (read-value stream))
			     (args (read-coded-list stream)))
			 ;; type and init args
			 (apply (structure-initializer type) args)))
                      (t (error "Unknown marker: ~A." marker)))))))))

(defun initialize-object (stream)
  (loop 
   (let ((io-id (read-coded-integer stream)))
     (if (eq io-id 'eof)
         (return)
       (fill-object (retrieve io-id) stream io-id)))))

;;;
;;;
;;;

(defmacro defpersistentclass (&rest rest)
  `(progn ,@(let* ((name (first rest))
		   (superclasses (append (second rest) '(persistent-object)))
		   (body (third rest))
                   (slotnames (mapcar #'first
                                      (remove-if #'(lambda (slot) (member :not-persistent slot))
                                                 body)))
                   (options (fourth rest)))
	      (list 
	       `(cl:defclass ,name ,superclasses 
		  ,(loop for slotspec in body collect
			 (remove :not-persistent slotspec))
                  .,(if (null options) nil (list options)))

	       `(defmethod write-object-constructor ((obj ,name) stream)
                  #+:allegro (declare (ignorable stream))
                  #+:lispworks (declare (ignore stream))
                  #+:ccl (declare (ignorable stream))
                  (with-slots ,slotnames obj
                    ,@(mapcan #'(lambda (slot)
                                  (let ((name (first slot)))
                                    (unless (member :not-persistent slot)
                                      `((when (slot-boundp obj ',name)
                                          (write-constructor ,name stream))))))
                              (reverse body)))
                  (call-next-method))

	       `(defmethod write-object-initializer ((obj ,name) stream)
                  #+:allegro (declare (ignorable stream))
                  #+:lispworks (declare (ignore stream))
                  #+:ccl (declare (ignorable stream))
                  (progn ;;with-slots ,slotname obj
                    ,@(mapcan #'(lambda (slot)
                                  (let ((name (first slot)))
                                    (unless (member :not-persistent slot)
                                      `((if (slot-boundp obj ',name)
                                            (write-slot-value obj ',name stream)
                                          (write-coded-marker stream +unbound-marker+))))))
                              (reverse body)))
                  (call-next-method))
	       
	       `(defmethod fill-persistent-object ((obj ,name) stream)
                  #+:allegro (declare (ignorable stream))
                  #+:lispworks (declare (ignore stream))
                  #+:ccl (declare (ignorable stream))
		  (let (val unbound)
		    #+:allegro (declare (ignorable val unbound))
                    #+:lispworks (declare (ignore val unbound))
		    #+:ccl (declare (ignorable val unbound))
		    (with-slots ,slotnames obj
		      ,@(mapcan #'(lambda (slot)
				    (let ((name (first slot)))
				      (unless (member :not-persistent slot)
					`((if (probe-bound-p stream)
                                              (setf ,name (read-slot-value obj ',name stream))
                                            (read-value stream))))))
				(reverse body)))
		    (call-next-method)))
               (list 'quote name)))))

(defmacro defclass (&rest forms)
  `(defpersistentclass .,forms))

;;;
;;;
;;;


(cl:defstruct structure-info
  name 
  options
  slot-specifications
  slotnames
  initializer)

(defvar *structures* (make-hash-table
                      :size +hash-table-size+
                      :rehash-size +rehash-size+))

(defun find-structure (name &key (error-p t))
  (let ((result (gethash name *structures*)))
    (if (null result)
        (if error-p
            (error "Cannot find structure with name ~A." name)
          nil)
      result)))

(defun (setf find-structure) (new-value name)
  (if (null new-value) 
      (remhash name *structures*)
    (setf (gethash name *structures*) new-value)))


(defmacro with-structure-slots (structure-name slotnames obj package &body forms)
  `(symbol-macrolet ,(mapcar (lambda (slotname)
                               (list slotname 
                                     (structure-slot-accessor structure-name 
                                                              slotname
                                                              obj
                                                              package)))
                             slotnames)
     .,forms))

(defun structure-slot-accessor (structure-name slotname obj &optional package)
  
  (let ((conc-name (let ((conc
                          (find ':conc-name
                                (structure-info-options (find-structure structure-name))
                                :key #'first)))
                     (if conc 
                         (or (second conc)
                             "")
                       (concatenate 'string (symbol-name structure-name) "-")))))
    (if (symbolp conc-name)
        (setf conc-name (symbol-name conc-name)))
    (list 
     (intern (string-transform 
	      (concatenate 'string 
                          conc-name
                          (symbol-name slotname)))
             (or (find-package package)
                 (symbol-package structure-name)))
     obj)))

(defun set-structure-initializer (structure-entry &optional package)
  (let* ((user-defined-constructor 
          (or (find ':pconstructor
                    (structure-info-options structure-entry)
                    :key #'first)
              (find ':constructor
                    (structure-info-options structure-entry)
                    :key #'first)))
         (constructor
          (if user-defined-constructor
              (let ((arguments (required-arguments (third user-defined-constructor))))
                (if arguments
                    #'(lambda ()
                        (apply (second user-defined-constructor)
                               (loop repeat (length arguments) collect nil)))
                  (second user-defined-constructor)))
            (intern 
             (concatenate 'string 
                          (string-transform
                           "make-")
                          (if (char= (elt (format nil "~S" (structure-info-name structure-entry)) 0) #\|)
                              (symbol-name (structure-info-name structure-entry))
                            (string-transform (symbol-name (structure-info-name structure-entry)))))
             
	     (or (find-package package)
		 (symbol-package (structure-info-name structure-entry)))))))
             
    (setf (structure-info-initializer structure-entry) constructor)))

(defun required-arguments (arguments)
  (if (null arguments)
      nil
    (if (member (first arguments) '(&rest &key &optional &allow-other-keys))
        nil
      (cons (first arguments)
            (required-arguments (rest arguments))))))

(declaim (inline structure-initializer))

(defun structure-initializer (structure-name)
  (structure-info-initializer (find-structure structure-name)))

(defun all-structure-slots (structure-entry)
  (let ((included-structure-name 
         (structure-info-included-structure structure-entry)))
    (if (null included-structure-name)
        (structure-info-slotnames structure-entry)
      (append (structure-info-slotnames structure-entry)
              (all-structure-slots (find-structure included-structure-name))))))

(defun structure-info-included-structure (structure-entry)
  (second (find ':include 
		(structure-info-options structure-entry)
                :key #'first)))

(defun real-structure-p (options)
  (not (find ':type options :key #'first)))

(defmacro defpersistentstruct (name-or-name-and-specifications &rest doc-and-slots)
  (let* ((name (if (consp name-or-name-and-specifications)
                   (first name-or-name-and-specifications)
                 name-or-name-and-specifications))
         (options (if (consp name-or-name-and-specifications)
                      (rest name-or-name-and-specifications)
                    nil))
         (include (find ':include options :key #'first))         
         (package (second (assoc ':package options)))
         (options (if package
                      (remove ':package options :key #'first)
                    options))
         (documentation (if (stringp (first doc-and-slots)) 
                            (list (first doc-and-slots))))
         (slots (if documentation 
                    (rest doc-and-slots)
                  doc-and-slots))
         (slot-specifications (mapcar (lambda (slot-spec)
                                        (cond ((symbolp slot-spec) (list slot-spec nil))
                                              ((consp slot-spec) slot-spec)
                                              (t (error "Illegal slot specification: ~A."
                                                        slot-spec))))
                                      slots))
         (slotnames (mapcar #'first slot-specifications))
         all-slotnames 
         (structure-entry-var (gensym))
         (structure-entry (find-structure name :error-p nil)))
    (setf include (second include))
    (if structure-entry 
        (setf (structure-info-options structure-entry) options
              (structure-info-slot-specifications structure-entry) slots
              (structure-info-slotnames structure-entry) slotnames)
      (setf structure-entry
            (make-structure-info :name name
                                 :options options
                                 :slot-specifications slot-specifications
                                 :slotnames slotnames)))
    (set-structure-initializer structure-entry package)
    (setf (find-structure name) structure-entry)
    (setf all-slotnames (all-structure-slots structure-entry))
    (if (real-structure-p options)
        `(progn 
           (let ((,structure-entry-var (find-structure ',name :error-p nil)))
             (if ,structure-entry-var 
                 (setf (structure-info-options ,structure-entry-var) ',options
                       (structure-info-slot-specifications ,structure-entry-var) ',slots
                       (structure-info-slotnames ,structure-entry-var) ',slotnames)
               (setf ,structure-entry-var
                     (make-structure-info :name ',name
                                          :options ',options
                                          :slot-specifications ',slot-specifications
                                          :slotnames ',slotnames)))
             (set-structure-initializer ,structure-entry-var ,package)
             (setf (find-structure ',name) ,structure-entry-var))
           ;; With :pconstructor one can declare a constructor that is used 
           ;; during reading a persistent object description only.
           ;; :pconstructor can, e.g., be used to enforce singletons for structure instances
           ;; We must eliminate it again in order to pass the options to cl:defstruct
           (cl:defstruct (,name .,(if (null include)
                                      (cons (list :include
                                                  'persistent-structure)
                                            (remove ':pconstructor options :key #'first))
                                    (remove ':pconstructor options :key #'first)))
             ,@documentation
             .,slot-specifications)

           (defmethod write-object-constructor ((obj ,name) stream)
             #+:allegro (declare (ignorable stream))
             #+:lispworks (declare (ignore stream))
             #+:ccl (declare (ignorable stream))
             (with-structure-slots ,name ,all-slotnames obj ,package
               ,@(mapcan #'(lambda (slotname)
                             `((write-constructor ,slotname stream)))
                         all-slotnames)))

           (defmethod write-object-initializer ((obj ,name) stream)
             #+:allegro (declare (ignorable stream))
             #+:lispworks (declare (ignore stream))
             #+:ccl (declare (ignorable stream))
             (with-structure-slots ,name ,all-slotnames obj ,package
               ,@(mapcan #'(lambda (slotname)
                             `((write-referencer ,slotname stream)))
                         all-slotnames)))

           (defmethod fill-persistent-object ((obj ,name) stream)
             #+:allegro (declare (ignorable stream))
             #+:lispworks (declare (ignore stream))
             #+:ccl (declare (ignorable stream))
             (with-structure-slots ,name ,all-slotnames obj ,package
               ,@(mapcan #'(lambda (slotname)
                             `((setf ,slotname 
                                     (read-value stream))))
                         all-slotnames)))
           ',name)
      `(cl:defstruct (,name .,(remove ':pconstructor options :key #'first))
         ,@documentation
         .,slot-specifications))))

(defmacro defstruct (&rest forms)
  `(defpersistentstruct .,forms))

;;;
;;;
;;;

(defun write-section-separator (stream)
  (write-coded-marker stream +section-marker+))

(defun generate-temp-filename (filename)
  (let* ((pathname (pathname filename))
         (name (pathname-name pathname))
         new-name)
    (loop for i from 1 to 100 do
          (when (not (probe-file (setf new-name 
                                       (merge-pathnames (concatenate 'string
                                                                     name 
                                                                     "_"
                                                                     (format nil "~D" i))
                                                        pathname))))
            (return-from generate-temp-filename new-name)))
    (error "Cannot generate name for temp file.")))


(defun make-object-persistent (obj fn &optional (package *package*))
  (let ((filename (generate-temp-filename fn)))
    (with-open-file (stream filename
                            :element-type 'unsigned-byte
                            :direction :output :if-exists :supersede
                            :if-does-not-exist :create)
      (let ((*package* package))
	(reset)
	(write-coded-integer stream +file-type-marker+)
	(write-coded-string stream 
			    (make-string +n-bytes-for-written-objects+ 
					 :initial-element #\space))
	(write-coded-string stream (package-name package))
	(write-coded-integer stream +persistence-version+)
	(write-constructor (list obj) stream)
	(write-section-separator stream)
	(maphash #'(lambda (key value)
		     (write-initializer key value stream))
		 *written-objects*)))
    
    (maphash #'(lambda (key value)
		 (declare (ignorable value))
		 (when (hash-table-p key)
		   (remhash *hashtable-contents* key)))
	     *written-objects*)
    
    (when (> *io-id-counter* +maximum-written-objects+)
      (error "Maximum number of objects per file (~D) exceeded."
	     +maximum-written-objects+))
    
    (with-open-file (stream filename 
                            :element-type 'unsigned-byte                          
                            :direction :output :if-exists :overwrite
                            :if-does-not-exist :create)

      (write-coded-integer stream +file-type-marker+)
      (write-coded-string stream 
			  (format nil
				  (concatenate 'string
                                               "~"
                                               (format nil "~D" +n-bytes-for-written-objects+)
                                               ",d")
				  *io-id-counter*)))
    (when (probe-file fn)
      (delete-file fn))
    
    (rename-file filename fn)
    
    (format nil "~A objects and ~A references have been written to ~A." *io-id-counter* *ref-counter* fn)

    'done))


(defun load-persistent-object (fn &key (initialization-protocol t))
  (with-open-file (stream fn :direction :input :element-type 'unsigned-byte)
    (let ((file-type-marker (read-coded-integer stream)))
      (unless (= file-type-marker +file-type-marker+)
        (error "File ~S is not a dump file in the correct format for this version of Racer." fn))
      (let* ((n-of-objects-to-read (read-from-string (read-coded-string stream)))
             (package-name (read-coded-string stream))
             (package (find-package package-name))
             (*package* (or package
                            (error "Cannot restore object: Package ~S not found." package-name)))
             (version (read-coded-integer stream)))
        (unless (= version +persistence-version+)
          (error "Dump file format of ~A not compatible with current version of Racer." fn))
        (read-reset n-of-objects-to-read)
        (construct-object stream)
        (initialize-object stream)
        
        ;;; Hashtable-Initialization: Erst am Ende! Sonst stimmen
        ;;; die Key-Referenzen beim Aufbauen der Tabelle noch nicht!
        
        (dotimes  (i (+ 2 *io-id-counter*))
          (let ((obj (bvref *read-objects* i)))
            (when (typep obj 'hash-table)
              (let ((contents (gethash *hashtable-contents* obj)))
                (loop as (key value) in contents do
                      (setf (gethash key obj) value))
                (remhash *hashtable-contents* obj)))))

        (when initialization-protocol
          (dotimes  (i (+ 2 *io-id-counter*))
            (let ((obj (bvref *read-objects* i)))
              (when (or (typep obj 'persistent-object)
                        (typep obj 'persistent-structure))
                (initialize-loaded-persistent-object obj)))))

        (let ((result (first (retrieve 1))))
          (setf *read-objects* nil)
          (values result
                  (format nil "Loaded ~A objects from ~A." (+ 2 *io-id-counter*) fn)))))))


;;(pushnew :persistence-management *features*)

;;;
;;; Tests
;;;

#|

(progn 

  (defvar *orig*)

  (defvar *copy*)
 
  (defpersistentclass test ()
    ((a :accessor a :initarg :a)
     (b :accessor b :initarg :b)))
  
  (defpersistentclass test2 (test)
    ((c :accessor c :initarg :c)))

  (defun test1 ()
   
    (let* ((table (let ((table (make-hash-table :test #'equal
                                                :size 100
                                                :rehash-size 100)))
                    (loop as i from 1 to 100 do
                          (setf (gethash (list i i i) table)
                                (loop as j from 1 to (+ i 10) collect 
                                      (list (* j j )
                                            (- (* j j ))))))
                    table))

           (x (make-instance 'test :a table))
           (y (make-instance 'test :a x :b (make-array '(10))))
           (z (make-instance 'test2 :c (list x y 
                                             (make-array '(3)
                                                         :initial-contents (list x y x))))))
  
      (setf *orig* (vector x y z (list x table (vector x z y) x z)))
    
      (make-object-persistent *orig* "test")

      (setf *copy* (load-persistent-object "test"))))


  (defpersistentstruct stest 
                       (a)
                       (b))

  (defpersistentstruct (stest2 (:include stest))
                       (c))

  (defun test2 ()

    (let* ((table (let ((table (make-hash-table :test #'equal
                                                :size 100
                                                :rehash-size 100)))
                    (loop as i from 1 to 100 do
                          (setf (gethash (list i i i) table)
                                (loop as j from 1 to (+ i 10) collect (* j j ))))
                    table))

           (x (make-stest :a table))

           (y (make-stest :a x :b (make-array '(10))))

           (z (make-stest2 :c (list x y 
                                    (make-array '(3)
                                                :initial-contents (list x y x))))))
  
      (setf *orig* (vector x y z (list x table (vector x z y) x z)))
  
      (make-object-persistent *orig* "test")
  
      (setf *copy* (load-persistent-object "test"))))



  (defun test3 ()
    (with-open-file (stream "pertest" 
                            :element-type 'unsigned-byte
                            :direction :output :if-exists :supersede
                            :if-does-not-exist :create)

      (write-coded-string stream "das ist ein test!")
      (write-coded-integer stream 123)
      (write-coded-symbol stream 'test)
      (write-coded-number stream 123.345)
      (write-coded-number stream 4/3)
      (write-coded-list stream (list 1 2 3 4)))

    (with-open-file (stream "pertest" 
                            :element-type 'unsigned-byte
                            :direction :input)
      (list      
       (read-coded-string stream)
       (read-coded-integer stream)
       (read-coded-symbol stream)
       (read-coded-number stream)
       (read-coded-number stream)
       (read-coded-list stream))))



  (defun test4 ()
    (make-object-persistent (list 'symbol 123 'test "abcdef" 123.3 4/3 (list 'a 1 2 'b "xyz"))
                            "pertest")
    (load-persistent-object "pertest"))


  (defun run-tests ()
    (test1)
    (test2) 
    (test3)
    (test4)))

|#
