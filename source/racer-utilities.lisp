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

;;; ======================================================================

#|
(defun subseqp (list1 list2)
  "Test whether list1 is a subsequence of list2 
provided both list elements are totally ordered"
  (loop with new-list2 = list2
        for elem in list1
        for found = (member elem new-list2) do
        (if (null found)
          (return nil)
          (setf new-list2 (rest found)))
        finally (return t)))
|#

(race-inline (safe-percentage))

(defun safe-percentage (nominator denominator)
  (if (zerop denominator)
    0
    (* 100 (/ nominator denominator))))

;;; ======================================================================

#+allegro
(eval-when (compile)
  (deftype hashcode ()
    `(integer 0 #-64bit #xffffff #+64bit #xffffffff)))


#+allegro
(defun internal-concept-hash (s-expr recurse)
  (declare (optimize (speed 3) (safety 0)))
  (macrolet ((chop (x) `(logand ,x #-64bit #xffffff #+64bit #xffffffff))
             (%1- (i) `(the fixnum (1- (the fixnum ,i))))
             (<^ (x a y) `(chop (logxor (ash ,x ,a) ,y)))
             (<>^ (x a b y)
               (let ((xx (gensym)))
                 `(let ((,xx ,x))
                    (chop (the hashcode
                            (logxor (the hashcode
                                      (logior (the hashcode (ash ,xx ,a))
                                              (the hashcode (ash ,xx ,b))))
                                    ,y))))))
             (sxhash-cons (seq lim)
               `(if* (eql 0 ,lim)
                     then 13
                     else (setq ,lim (%1- ,lim))
                     (let ((index 13)
                           (hash 2))
                       (declare (fixnum index)
                                (type hashcode hash))
                       (loop
                         (setq hash
                               #-64bit
                               (<>^ hash 5 -19 ;; [rfe6200]
				    (internal-concept-hash (car ,seq) ,lim))
                               #+64bit
                               (<>^ hash 5 -27
                                    (internal-concept-hash (car ,seq) ,lim)))
                         (setq ,seq (cdr ,seq))
                         (setq index (%1- index))
                         (if* (atom ,seq)
                              then (return
                                    #-64bit
                                    (<>^ hash 5 -19 ;; [rfe6200]
                                         (internal-concept-hash ,seq ,lim))
                                    #+64bit
                                    (<>^ hash 5 -27
                                         (internal-concept-hash ,seq ,lim)))
                              elseif (eql index 0)
                              then (return hash)))))))
    (cond ((consp s-expr) (sxhash-cons s-expr recurse))
          ((racer-structure-id-p s-expr)
	   (sxhash (racer-structure-id-hash-id s-expr))
	   )
          (t (sxhash s-expr)))))




#+:allegro
(defun traverse-key (s-expr)
  (when s-expr
    (if (consp s-expr)
	(cons (if (racer-structure-id-p (car s-expr))
		  (racer-structure-id-hash-id (car s-expr))
		(traverse-key (car s-expr)))
	      (traverse-key (cdr s-expr)))
      (if (racer-structure-id-p s-expr)
	  (racer-structure-id-hash-id s-expr)
	s-expr))))

#+:allegro
(defun save-transformed-table-keys (table)
  (with-open-file  (stream "table-keys.lisp" :direction :output :if-exists :supersede)
    (print (loop for key being the hash-key of table
	       collect (cons (traverse-key key) (internal-concept-hash key 6)))
	   stream))
  (values))

#+:allegro
(defun load-transformed-table-keys ()
  (with-open-file  (stream "table-keys.lisp" :direction :input)
    (read stream)))

#+:allegro
(defun concept-hash (thing)
  (internal-concept-hash thing 6))

#+:sbcl
(defun concept-hash (thing)
  (declare (optimize (speed 3) (safety 0)))
  (cond ((racer-structure-id-p thing)
	 (sxhash (racer-structure-id-hash-id thing)))
	((consp thing) (list-hash thing 11))
	(t (sxhash thing))))

#+:sbcl
(defun list-hash (thing depthoid)
  (typecase thing
    (cons
     (if (plusp depthoid)
	 (sb-impl::mix (list-hash (car thing) (1- depthoid))
		       (list-hash (cdr thing) (1- depthoid)))
	 (sxhash nil)))
    (t (concept-hash thing))))

(defun print-obsolete-tables-info (&optional (stream t))
  (let ((*print-pretty* nil))
    (print-obsolete-tables-info-1 stream *obsolete-eql-tables* ':eql)
    (print-obsolete-tables-info-1 stream *obsolete-equal-tables* ':equal)
    #+(or :allegro :sbcl)
    (print-obsolete-tables-info-1 stream *obsolete-equal-concept-tables* ':equal-concept)))

(defun print-obsolete-tables-info-1 (stream tables-index type)
  (loop for size being the hash-key in tables-index using (hash-value table-list)
        for length = (length table-list)
        for no-of-entries = (* size length)
        collect (list size length no-of-entries) into size-infos
        sum no-of-entries into total-size
        finally (format stream "~&~A: (total-size=~:D) (size length #entries)=~S"
                        type total-size size-infos)))

(race-inline (get-obsolete-tables
              delete-from-obsolete-tables
              add-to-obsolete-tables))

(defun get-matching-size (size index-table)
  (loop with size = (or size *initial-hash-table-size*)
        with best-size = size
        with best-difference = nil
        for available-size being the hash-key of index-table do
        (if (eql size available-size)
          (return size)
          (when (or (null best-difference)
                    (< (abs (- size available-size)) best-difference))
            (setf best-size available-size)
            (setf best-difference (abs (- size available-size)))))
        finally (return best-size)))

(defun clear-all-obsolete-tables ()
  (clrhash *obsolete-eql-tables*)
  (clrhash *obsolete-equal-tables*)
  #+(or :allegro :sbcl)
  (clrhash *obsolete-equal-concept-tables*)
  nil)

(defun get-obsolete-tables (type)
  (ecase type
    (:eql *obsolete-eql-tables*)
    (:equal *obsolete-equal-tables*)
    #+(or :allegro :sbcl)
    (:equal-concept *obsolete-equal-concept-tables*)))

(defun delete-from-obsolete-tables (table type)
  (let* ((index-table (get-obsolete-tables type))
	 (size (hash-table-size table))
	 (result (pop (gethash size index-table))))
    (unless (gethash size index-table)
      (remhash size index-table))
    result))

(defun add-to-obsolete-tables (table type)
  (let ((tables-index (get-obsolete-tables type)))
    (racer-without-interrupts
     (let* ((size (hash-table-size table))
            (tables-list (gethash size tables-index)))
       (when (< (length tables-list) 1000)
         #+:debug (assert (null (member table tables-list)))
         (push table (gethash size tables-index))))))
  nil)

(defun find-table (size type)
  (let ((index (get-obsolete-tables type)))
    (let ((result nil)
	  (table nil))
      (racer-without-interrupts
	(let ((obsolete-tables (gethash (get-matching-size size index) index)))
	  (when obsolete-tables
	    (setf table (first obsolete-tables))
	    (setf result (delete-from-obsolete-tables table type)))))
      (when result
	#+:debug (assert (eq table result))
	(return-from find-table result)))
    #+:debug (assert (eql (hash-table-count index) 0))
    #+(or :ccl :allegro :sbcl)
    (let ((threshold *obsolete-table-misses-threshold*))
      (incf *no-of-obsolete-table-misses*)
      (when (> *no-of-obsolete-table-misses* threshold)
        (format t "-T(~D)" threshold)
        #+:ccl (ccl:gc)
        #+:allegro (excl:gc nil)
	#+:sbcl (sb-ext:gc)
        (reset-recycle-counts)
        (let ((result nil)
	      (table nil))
	  (racer-without-interrupts
	    (let* ((obsolete-tables (gethash (get-matching-size size index) index))
                   (no-of-states (length obsolete-tables))
                   (new-threshold (round (* 1.5 threshold))))
              (if (and (< no-of-states threshold) (< new-threshold 1000))
                (setf *obsolete-table-misses-threshold* new-threshold)
                (when (and (> no-of-states new-threshold) (> new-threshold 500))
                  (setf *obsolete-table-misses-threshold* (round (/ threshold 1.5)))))
	      (when obsolete-tables
                (format t "+T(~D)" no-of-states)
	        (setf table (first obsolete-tables))
	        (setf result (delete-from-obsolete-tables table type)))))
	  #+:debug (assert (eq table result))
	  result)))))

(defun dispose-hash-table (table)
  (clrhash table)
  (let ((test-fcn (hash-table-test table)))
    (if (or (eq test-fcn 'equal) (eq test-fcn #'equal))
      #+:allegro
      (if (eq (hash-table-hash-function table) 'concept-hash)
        (add-to-obsolete-tables table ':equal-concept)
	(add-to-obsolete-tables table ':equal))
      #+:sbcl
      (if (eq (sb-impl::hash-table-hash-fun table) 'concept-hash)
        (add-to-obsolete-tables table ':equal-concept)
	(add-to-obsolete-tables table ':equal))
      #-(or :allegro :sbcl)
      (add-to-obsolete-tables table ':equal)
      (progn
	#+:debug (assert (or (eq test-fcn 'eql) (eq test-fcn #'eql)))
	(add-to-obsolete-tables table ':eql)))))

(defun recycle-hash-table (test size #+(or :allegro :sbcl) concept-hash-p)
  ;(princ "-")
  (if (or (eq test 'equal) (eq test #'equal))
    #+(or :allegro :sbcl)
    (if concept-hash-p
      (find-table size ':equal-concept)
      (find-table size ':equal))
    #-(or :allegro :sbcl)
    (find-table size ':equal)
    (find-table size ':eql)))

(defun racer-make-hash-table-internal (&key 
				       (test 'eql)
				       (size nil)
				       (rehash-size nil)
				       (rehash-threshold nil)
				       (weak nil)
				       #+(or :allegro :sbcl)
				       (hash-function nil))
  (declare (ignore rehash-size rehash-threshold))
  (apply #'make-hash-table
         (nconc (and test (list :test test))
                (and size (list :size size))
                ;(and rehash-size (list :rehash-size rehash-size))
                ;(and rehash-threshold (list :rehash-threshold rehash-threshold))
                (and weak
                     #+:ccl (and (eq test 'eq) '(:weak t))
                     #+:allegro '(:values :weak)
                     #+:lispworks '(:weak-kind t)
		     #+:sbcl '(:weakness :key-or-value)
                     #-(or :ccl :allegro :lispworks :sbcl) nil)
                #+(or :allegro :sbcl)
                (and hash-function (list :hash-function hash-function))
                )))

(defun racer-make-hash-table (&key (test 'eql)
                                       (size nil)
                                       (rehash-size nil)
                                       (rehash-threshold nil)
                                       (structure-p nil)
                                       (weak nil))
  #-(or :allegro :sbcl) (declare (ignore structure-p))
  (let* ((recycle-hash-tables *recycle-hash-tables*)
         #+(or :allegro :sbcl)
         (concept-hash-p (and structure-p (or (eq test 'equal) (eq test #'equal))))
         (table 
          (or (and recycle-hash-tables
                   (recycle-hash-table test size #+(or :allegro :sbcl) concept-hash-p))
              #+(or :allegro :sbcl)
              (if concept-hash-p
                (racer-make-hash-table-internal :test test
                                                :size size
                                                :rehash-size rehash-size
                                                :rehash-threshold rehash-threshold
                                                :weak weak
                                                :hash-function 'concept-hash)
                (racer-make-hash-table-internal :test test
                                                :size size
                                                :rehash-size rehash-size
                                                :rehash-threshold rehash-threshold
                                                :weak weak))
              #-(or :allegro :sbcl)
              (racer-make-hash-table-internal :test test
                                              :size size
                                              :rehash-size rehash-size
                                              :rehash-threshold rehash-threshold
                                              :weak weak))))
    (when (and recycle-hash-tables (not weak))
      (register-object-for-termination table #'dispose-hash-table))
    table))

;;;===========================================================================

(defun smart-clrhash (hash-table list variable-symbol &optional (length nil))
  (if (eql (hash-table-count hash-table) 0)
      hash-table
  (let* ((length (or length (length list)))
         (table-size (hash-table-size hash-table))
         (new-p (if (and (> length 2000) (> table-size 2000))
                  (> (- table-size (* length (/ 3 2))) 500 )
                  (> (- (hash-table-size hash-table) length) 500))))
    (if new-p
      (let ((result
             (if #+:allegro (eq (hash-table-hash-function hash-table) 'concept-hash)
		 #+:sbcl (eq (sb-impl::hash-table-hash-fun hash-table) 'concept-hash)
                 #-(or :allegro :sbcl) nil
                 (racer-make-hash-table :test (hash-table-test hash-table)
                                        :size (max length 200)
                                        :structure-p t)
                 (racer-make-hash-table :test (hash-table-test hash-table)
                                        :size (max length 200)))))
        (when variable-symbol
          (set variable-symbol result))
        ;(print (list length variable-symbol (hash-table-size hash-table) hash-table (hash-table-size result)))
        result)
      (clrhash hash-table)))))

;;;===========================================================================

(defun stable-set-difference (list1 list2 &optional (use-table-p nil))
  "Order preserving set-difference"
  (if (or (null list2) (null list1))
    list1
    (if (rest list2)
      (let* ((count (length list2))
             (table (when (or use-table-p (> (* (length list1) count) 1000))
                      *stable-set-difference-table*)))
        (if table
          (progn
            (unless (eq *stable-set-difference-last-list2* list2)
              (setf *stable-set-difference-last-list2* list2)
              (setf table (smart-clrhash table list2 '*stable-set-difference-table* count))
              (loop for elem in list2 do
                    (setf (gethash elem table) t)))
            (loop with rest = count
                  with modified-p = nil
                  for rest-list on list1
                  for prev-list = nil then rest-list
                  for elem = (first rest-list)
                  if (gethash elem table)
                  do
                  (setf modified-p t)
                  (decf rest)
                  (when (zerop rest)
                    (if prev-list
                      (return (values (nconc result (rest rest-list)) t))
                      (return (values (rest list1) t))))
                  else collect elem into result
		finally
		  ;(unless modified-p (princ "+"))
		  (return (values result modified-p))))
          (let* ((modified-p nil)
                 (result (loop for elem in list1
                               if (member elem list2)
                               do (setf modified-p t)
                               else collect elem)))
            (if modified-p
              (values result t)
              list1))))
      (racer-remove (first list2) list1))))

(defun stable-set-table-difference (list table)
  "Order preserving set-difference"
  (loop for elem in list
      when (gethash elem table)
      collect elem into removed-elems
      finally
	(if removed-elems
	    (return (constraint-set-difference list removed-elems))
	  (return list))))

(defun stable-union (list1 list2)
  (if (or (null list1) (eq list1 list2))
      list2
    (if (null list2)
	list1
      (let* ((size (length list1))
             (count (length list2))
             (table (when (> (* size count) 1000)
                      *stable-set-difference-table*)))
	(if (eql size 1)
          (let ((elem (first list1)))
            (cond ((eql elem (first list2)) list2)
                  ((member elem (rest list2)) (cons elem (remove elem list2)))
                  (t (cons elem list2))))
	  (if (eql count 1)
            (let* ((elem (first list2))
                   (tail (member elem list1)))
              (if tail
                (if (null (rest tail))
                  list1
                  (nconc (remove elem list1) list2))
		(append list1 list2)))
	    (if table
		(progn
		  (setf table (smart-clrhash table list1 '*stable-set-difference-table* size))
		  (loop for elem in list1 do
			(setf (gethash elem table) t))
		  (append list1 
			  (loop for elem in list2
			      unless (gethash elem table)
			      collect elem)))
	      (append list1
		      (loop for elem in list2
                          unless (member elem list1)
                          collect elem)))))))))

(defun union-equal (list1 list2-or-table)
  (if (or (null list1) (eq list1 list2-or-table))
    list2-or-table
    (if (null list2-or-table)
      list1
      (if (listp list2-or-table)
        (union-equal-list list1 list2-or-table)
        (union-equal-table list1 list2-or-table)))))

(defun union-equal-list (list1 list2)
  (let* ((size (length list1))
         (count (length list2))
         (table (when (> (* size count) 100)
                  *racer-remove-duplicates-table*)))
    (if (eql size 1)
      (let ((elem (first list1)))
        (cond ((equal elem (first list2)) list2)
              ((member elem (rest list2) :test #'equal)
               (cons elem (remove elem list2 :test #'equal)))
              (t (cons elem list2))))
      (if (eql count 1)
        (let* ((elem (first list2))
               (tail (member elem list1 :test #'equal)))
          (if tail
            (if (null (rest tail))
              list1
              (nconc (remove elem list1 :test #'equal) list2))
            (append list1 list2)))
        (if table
          (progn
            (setf table (smart-clrhash table list1 '*racer-remove-duplicates-table* size))
            (loop for elem in list1 do
                  (setf (gethash elem table) t))
            (append list1 
                    (loop for elem in list2
                          unless (gethash elem table)
                          collect elem)))
          (append list1
                  (loop for elem in list2
                        unless (member elem list1 :test #'equal)
                        collect elem)))))))

(defun union-equal-table (list table)
  (loop for elem in list
        unless (gethash elem table)
        do (setf (gethash elem table) t))
  table)

(defun stable-assoc-union (list1 list2)
  (if (or (null list1) (eq list1 list2))
    list2
    (if (null list2)
      list1
      (if (null (rest list1))
        (let* ((new (first list1))
               (old (assoc (car new) list2 :test 'equal)))
          (if (and old (not (eq (cdr new) (cdr old))))
            (cons (cons (car old) (stable-union (cdr new) (cdr old)))
                  (remove old list2 :test 'equal))
            (append list1 list2)))
        (loop for elem in list2
              for old = (assoc (car elem) list1 :test 'equal)
              if (and old (not (eq (cdr elem) (cdr old))))
              collect (cons (car old) (stable-union (cdr elem) (cdr old))) into replaced-elems
              else collect elem into new-elems
              finally
              (if replaced-elems
                (return (nconc (set-difference list1 replaced-elems :test 'equal :key #'car)
                               replaced-elems
                               new-elems))
                (return (append list1 new-elems))))))))

;;; ======================================================================

#+:debug
(defun racer-remove-duplicates (list &key (test 'eql))
  (when list
    #+:debug (assert (member test (list 'eql 'equal #'equal)))
    (if (rest list)
        (if (or (eq test 'equal) (eq test #'equal))
            (racer-remove-duplicates-equal list)
          (racer-remove-duplicates-eql list))
      list)))

#-:debug
(defmacro racer-remove-duplicates (list &key (test ''eql))
  (let ((sym (gensym)))    
    (if (or (eq (second test) 'equal) (eq (second test) #'equal))
        `(let ((,sym ,list))
           (when ,sym
             (if (rest ,sym)
                 (racer-remove-duplicates-equal ,sym)
               ,sym)))
      `(let ((,sym ,list))
         (when ,sym
           (if (rest ,sym)
               (racer-remove-duplicates-eql ,sym)
             ,sym))))))

(defun racer-remove-duplicates-eql (list)
  (let ((list-length (length list)))
    (if (> list-length 50)
      (loop with table = (smart-clrhash *stable-set-difference-table*
                                        list
                                        '*stable-set-difference-table*
                                        list-length)
            for elem in list
            unless (gethash elem table)
            do (setf (gethash elem table) t)
            and collect elem)
      (remove-duplicates list))))

(defun racer-remove-duplicates-equal (list)
  (let ((list-length (length list)))
    (if (> list-length 50)
      (loop with table = (smart-clrhash *racer-remove-duplicates-table*
                                        list
                                        '*racer-remove-duplicates-table*
                                        list-length)
            for elem in list
            unless (gethash elem table)
            do (setf (gethash elem table) t)
            and collect elem)
      (remove-duplicates list :test #'equal))))

(defun racer-remove-concept-constraint-duplicates (list)
  (when list
    (let ((list-length (length list)))
      (if (> list-length 50)
          (loop with table = (smart-clrhash *racer-remove-constraint-duplicates-table*
                                            list
                                            '*racer-remove-constraint-duplicates-table*
                                            list-length)
                for elem in list
                for key = (list (constraint-ind elem)
                                (concept-hash-id
                                 (if (constraint-negated-p elem)
                                     (concept-negated-concept (constraint-term elem))
                                   (constraint-term elem))))
	      if (gethash key table)
		 collect elem into duplicates
                else
                do (setf (gethash key table) t)
	      finally
		(if duplicates
		    (return (constraint-set-difference list duplicates))
		  (return list)))
        (remove-duplicates list :test #'constraint-equal-test)))))

(defun racer-merge-remove-concept-constraint-duplicates (list)
  (when list
    (loop with table = (smart-clrhash *racer-remove-constraint-duplicates-table*
                                      list
                                      '*racer-remove-constraint-duplicates-table*)
          for elem in list
          for key = (list (constraint-ind elem)
                          (concept-hash-id
                           (if (constraint-negated-p elem)
                             (concept-negated-concept (constraint-term elem))
                             (constraint-term elem))))
          for old-elem = (gethash key table)
          if old-elem
          do
          (let ((synonyms (constraint-ind-synonyms elem)))
            (when synonyms
              (setf (constraint-ind-synonyms old-elem)
                    (stable-union (constraint-ind-synonyms old-elem) synonyms))))
          else
          do (setf (gethash key table) elem)
          and collect elem)))

(defun racer-remove-rel-constraint-duplicates (list)
  (when list
    (if (rest list)
        (let ((list-length (length list)))
          (if (> list-length 50)
              (loop with table = (smart-clrhash *racer-remove-constraint-duplicates-table*
                                                list
                                                '*racer-remove-constraint-duplicates-table*
                                                list-length)
                    for elem in list
                    for key = (list (constraint-ind-1 elem)
                                    (constraint-ind-2 elem)
                                    (role-name (constraint-term elem)))
                    unless (gethash key table)
                    do (setf (gethash key table) t)
                    and collect elem)
            (remove-duplicates list
                               :test (lambda (elem-1 elem-2)
                                       (and (eql (constraint-ind-1 elem-1)
                                                 (constraint-ind-1 elem-2))
                                            (eql (constraint-ind-2 elem-1)
                                                 (constraint-ind-2 elem-2))
                                            (eq (constraint-term elem-1)
                                                (constraint-term elem-2)))))))
      list)))

(defun racer-merge-remove-rel-constraint-duplicates (list)
  (when list
    ;(princ "*")
    (loop with table = (smart-clrhash *racer-remove-constraint-duplicates-table*
                                      list
                                      '*racer-remove-constraint-duplicates-table*)
          for elem in list
          for key = (list (constraint-ind-1 elem)
                          (constraint-ind-2 elem)
                          (role-name (constraint-term elem)))
          for old-elem = (gethash key table)
          if old-elem
          do
          (let ((synonyms (constraint-ind-1-synonyms elem)))
            (when synonyms
              (setf (constraint-ind-1-synonyms old-elem)
                    (stable-union (constraint-ind-1-synonyms old-elem) synonyms))))
          (let ((synonyms (constraint-ind-2-synonyms elem)))
            (when synonyms
              (setf (constraint-ind-2-synonyms old-elem)
                    (stable-union (constraint-ind-2-synonyms old-elem) synonyms))))
          (let ((dependencies (constraint-dependencies elem)))
            (when dependencies
              (setf (constraint-dependencies old-elem)
                    (stable-union (constraint-dependencies old-elem) dependencies))))
          else
          do (setf (gethash key table) elem)
          and collect elem)))

(defun reduced-stable-rel-constraint-set-difference (abox list1 list2)
  (cond 
   ((or (null list1)
        (null list2)
        (let ((language (abox-language abox)))
          (not (or (dl-features language)
                   (dl-merging language)
                   (dl-inverse-roles language)))))
    nil)
   ((or *use-unique-name-assumption*
        (loop for elem in list1
              always (null (or (constraint-ind-1-synonyms elem)
                               (constraint-ind-2-synonyms elem)))))
    (stable-constraint-set-difference list1 list2))
   (t
    (let ((table *racer-remove-constraint-duplicates-table*))
      (setf table (smart-clrhash table
                                 list2
                                 '*racer-remove-constraint-duplicates-table*))
      (loop for elem in list2
            do (setf (gethash (list (constraint-ind-1 elem)
                                    (constraint-ind-2 elem)
                                    (role-name (constraint-term elem)))
                              table)
                     elem))
      (loop for elem-1 in list1
            for elem-2 = (gethash (list (constraint-ind-1 elem-1)
                                        (constraint-ind-2 elem-1)
                                        (role-name (constraint-term elem-1)))
                                  table)
            for elem-3 = (unless elem-2
                           (gethash (list (constraint-ind-2 elem-1)
                                          (constraint-ind-1 elem-1)
                                          (role-name (role-inverse-internal
                                                      (constraint-term elem-1))))
                                    table))
            when (and (or (null elem-2)
                          (not (set-equal-p (constraint-ind-1-synonyms elem-1)
                                            (constraint-ind-1-synonyms elem-2)))
                          (not (set-equal-p (constraint-ind-2-synonyms elem-1)
                                            (constraint-ind-2-synonyms elem-2))))
                      (or (null elem-3)
                          (not (set-equal-p (constraint-ind-1-synonyms elem-1)
                                            (constraint-ind-2-synonyms elem-3)))
                          (not (set-equal-p (constraint-ind-2-synonyms elem-1)
                                            (constraint-ind-1-synonyms elem-3)))))
            collect elem-1)))))

(race-inline (set-equal-p))

(defun set-equal-p (list-1 list-2 &optional list-1-length)
  (and (eql (or list-1-length (length list-1)) (length list-2))
       (subsetp list-1 list-2)))

;;; ======================================================================

(race-inline (make-constraints-table-key add-to-constraints-table constraint-found-p))

(defun make-constraints-table-key (constraint &optional (add-synonyms nil))
  (cond ((concept-constraint-p constraint)
         (let* ((hash-id (concept-hash-id (if (constraint-negated-p constraint)
                                              (concept-negated-concept (constraint-term constraint))
                                            (constraint-term constraint))))
                (key (list (constraint-ind constraint) hash-id)))
           (if add-synonyms
               (loop for ind in (constraint-ind-synonyms constraint)
                     collect (list ind hash-id) into result
                     finally (return (cons key result)))
             key)))
        ((relation-constraint-p constraint)
         (list (constraint-ind-1 constraint)
               (constraint-ind-2 constraint)
               (role-name (constraint-term constraint))))
        ((qualified-role-signature-p constraint)
         (list (signature-ind constraint)
               (signature-successor-ind-set constraint)
               (role-name (signature-role constraint))))))

(defun make-constraints-table (constraints)
  (loop with table = (smart-clrhash *racer-remove-constraint-duplicates-table*
                                    constraints
                                    '*racer-remove-constraint-duplicates-table*
                                    (if constraints
                                      (length constraints)
                                      300))
        for constraint in constraints do
        (setf (gethash (make-constraints-table-key constraint) table) constraint)
        finally (return table)))

(defun add-to-constraints-table (constraint table)
  (loop for key in (make-constraints-table-key constraint t) do
        (setf (gethash key table) constraint)))

(defun constraint-found-p (constraint table)
  (gethash (make-constraints-table-key constraint) table))

(defun constraint-or-synonym-found-p (constraint table)
  (loop for key in (make-constraints-table-key constraint t)
        thereis (gethash key table)))

(defun stable-constraint-set-difference (list1 list2)
  (if (or (null list2) (null list1))
      list1
    (let ((table (make-constraints-table list2)))
      (loop with modified-p = nil
            for elem-1 in list1
            for elem-2 = (constraint-found-p elem-1 table)
            if elem-2
            do 
            (unless modified-p
              (setf modified-p t))
            else
            collect elem-1 into result
            finally
            (if modified-p
                (return (values result t))
              (return list1))))))

;;; ======================================================================

(defparameter *show-time* nil)

(defmacro race-time (form &optional (string nil))
  #+(or :lispworks :allegro)
  `(if *show-time*
     (multiple-value-prog1
       (time ,form)
       (if ,string
         (format t "~&for activity ~A~%" ',string)
         (format t "~&for form ~A~%" ',form)))
     ,form)
  #+:ccl
  `(if *show-time*
     (if ,string
       (ccl::report-time ',(format nil "Activity ~A" string)
                         #'(lambda () ,form))
       (time ,form))
     ,form)
  #-(or :lispworks :allegro :ccl)
  `(if *show-time*
     (time ,form)
     ,form)
  )

;;; ======================================================================

(defun safe-append (list-of-lists)
  (safe-append-1 nil list-of-lists nil))

(defun safe-append-1 (list list-of-lists acc)
  (if (null list)
      (if (null list-of-lists)
          (nreverse acc)
        (safe-append-1 (first list-of-lists) (rest list-of-lists) acc))
    (safe-append-1 (rest list) list-of-lists (cons (first list) acc))))

;;; ======================================================================

#|
(defun racer-substitute (new old list &key (test nil))
  (if test
    (loop for rest-list on list
          for prev-list = nil then rest-list
          for item = (first rest-list)
          if (funcall test item old)
          do
          (if prev-list
            (return (nconc result (cons new (rest rest-list))))
            (return (cons new (rest list))))
          else
          collect item into result
          finally
          (return list))
    (loop for rest-list on list
          for prev-list = nil then rest-list
          for item = (first rest-list)
          if (eql item old)
          do
          (if prev-list
            (return (nconc result (cons new (rest rest-list))))
            (return (cons new (rest list))))
          else
          collect item into result
          finally
          (return list))))
|#


;;; ===============================================================================

(defun possibly-count-only (result count)
  (if count
    (length result)
    result))

;;; used for testing to introduce randomness
(defun shuffle-list (list)
  (let* ((vector (coerce list 'vector))
         (shuffle (loop with size = (array-total-size vector)
                        with table = (make-array size)
                        for i from 0 to (1- size)
                        do (setf (aref table i) i)
                        finally
                        (when (> size 1)
                          (loop for i from 0 to (1- size)
                                for j = (random (1- size))
                                do
                                (rotatef (svref table i) (svref table j))))
                        (return table))))
    (loop for rel-index across shuffle
          collect (svref vector rel-index))))

;;; ===============================================================================

(defun svn-revision ()
  (let (#+:debug (succeeded nil)
        (shell-command nil))
    (ignore-errors
     (setf shell-command
           (format nil "svn info ~A | grep Revision" (translate-logical-pathname "racer:")))
     #+:lispworks
     (multiple-value-bind (out err)
                          (sys:run-shell-command shell-command :wait nil :output :stream)
       (declare (ignore err))
       (with-open-stream (out out)
	 (setf *svn-revision* (read-from-string (subseq (read-line out) 9)))))
     #+:allegro
     (excl.osi:with-command-output (result shell-command)
       (setf *svn-revision* (read-from-string (subseq result 9))))
     #+:ccl
     (let ((result (with-output-to-string (stream)
                     (ccl:run-program "sh" (list "-c" shell-command) :output stream))))
       (setf *svn-revision* (read-from-string (subseq result 9))))
     #-(or :lispworks :allegro :ccl)
     (racer-warn "Retrieving SVN revision is not implemented in function svn-revision")
     #+:debug (setf succeeded t))
    #+:debug
    (unless succeeded
      (racer-warn "Could not determine actual SVN revision using Shell command \"~A\""
                  shell-command)))
  *svn-revision*)
