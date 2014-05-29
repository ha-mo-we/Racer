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
;;; CLSQL initilization
;;;

(setf *default-database-type* :mysql)

(defconstant +db-user+ "root")

(defconstant +db-passwd+ "root")

(initialize-database-type)

;; (locally-enable-sql-reader-syntax)

(defparameter +string-marker+ 'string-marker-123)

(defparameter +symbol-marker+ 'symbol-marker-123)

;;;
;;;
;;;

(defpersistentclass sql-data-substrate (data-substrate)
  ((db-name :initform nil)
   (db-spec :initform nil)
   (db-type :initform nil)))


;;;
;;; Aux Functions
;;;

(defun decode-value (val)
  ;; (setf *x* val)
  (labels ((do-it (val)
             (if (stringp val)
                 val
               (if (consp val)
                   (if (eq (first val) +symbol-marker+)
                       (if (second val)
                           (intern (format nil "~A" (third val))
                                   (find-package (second val)))
                         (gensym (format nil "~A" (third val))))
                     (mapcar #'do-it val))
                 val))))
    (let ((*package* (find-package :thematic-substrate)))
      (do-it (read-from-string val)))))

(defun encode-value (val)
  ;; (setf *y* val)
  (if (stringp val)
      (format nil "~S" val)
    (if (consp val)
        (format nil "~A" 
                (mapcar #'encode-value val))
      (if (symbolp val)
          (format nil "(~A ~A |~A|)"
                  +symbol-marker+
                  (if (symbol-package val)
                      (package-name (symbol-package val))
                    nil)
                  (symbol-name val))
        (format nil "~A" val)))))

;;;
;;;
;;;

(defun convert-db-tuples (tuples)
  (if (consp (car tuples))
      (mapcar #'(lambda (tuple) (mapcar #'(lambda (x) 
                                            (decode-value x))
                                        tuple))
              tuples)
    (mapcar #'decode-value tuples)))

(defun combine-labels (a b)
  (encode-value
   (remove-duplicates
    (if (eq a +no-descr-marker+)
        (if (eq b +no-descr-marker+)
            +no-descr-marker+
          b)
      (if (eq b +no-descr-marker+)
          a
        (append (ensure-list a) 
                (ensure-list b))))
    :test #'set-equal)))

;;;
;;;
;;;

(defmacro with-db-connection ((db substrate) &body body)
  `(when (is-sql-data-substrate-p ,substrate)
     (with-slots (db-spec db-type) ,substrate
       (with-database (,db db-spec
                           :database-type db-type
                           :pool t
                           :make-default t)
         (set-autocommit t :database ,db)
         ,@body))))
  
(defun all-dbs ()
  (list-databases (list "localhost" "" +db-user+ +db-passwd+) :database-type :mysql))

(defun destroy-all-dbs ()
  (full-reset)
  (dolist (db (list-databases (list "localhost" "" +db-user+ +db-passwd+) :database-type :mysql))
    (when (search "SDS" db)
      (destroy-db db))))

(defun destroy-db (db)
  (destroy-database (list "localhost" db +db-user+ +db-passwd+) :database-type :mysql))

(defun all-db-nodes ()
  (with-db-connection (db *cur-substrate*)
    (convert-db-tuples 
     (clsql:query "SELECT * FROM NODES"
                   :database db))))

(defun all-db-edges ()
  (with-db-connection (db *cur-substrate*)
    (convert-db-tuples 
     (clsql:query "SELECT * FROM EDGES"
                   :database db))))


;;;
;;;
;;;

(defmethod find-db-node ((substrate sql-data-substrate) name)
  (with-db-connection (db substrate)
    (let ((node
           (first 
            (convert-db-tuples
             (first 
              (clsql:query 
               (format nil "SELECT label FROM NODES WHERE NAME = '~A'" 
                       (encode-value name))
               :database db))))))
      (when node
        (list node nil)))))
          
(defmethod find-db-edge ((substrate sql-data-substrate) from to)
  (with-db-connection (db substrate)
    (let ((edge 
           (first
            (convert-db-tuples
             (first 
              (clsql:query 
               (format nil "SELECT label FROM EDGES WHERE FROMNODE = '~A' AND TONODE = '~A'" 
                       (encode-value from)
                       (encode-value to))
               :database db))))))
      (when edge
        (list edge nil)))))

;;;
;;;
;;;

(defmethod initialize-instance :after ((substrate sql-data-substrate) &rest args)
  (declare (ignorable args))
  ;;;(princ "***************")
  (unless (is-mirror-data-substrate-p substrate)
    (initialize-db substrate))
  )


(defmethod initialize-db ((substrate sql-data-substrate))
  (with-slots (db-spec db-type db-name name) substrate

    (setf db-type :mysql)

    (setf db-spec 
          (list "localhost" (format nil "SDS~A" 
                                    name
                                    ;'test
                                    )
                +db-user+
                +db-passwd+))

    (setf db-name (database-name-from-spec db-spec db-type))

    (cond ((member (second db-spec) 
                   (all-dbs)
                   :test #'string-equal)
           
           (nrql-warning "Database ~A already exists, using existing DB" name)
           
           )

          (t 
           
           (create-database db-spec :database-type db-type)

           (with-db-connection (db substrate)
             
             (execute-command "CREATE TABLE NODES(NAME VARCHAR(255), LABEL BLOB,  RACERLABEL BLOB, PRIMARY KEY(NAME))"
                              :database db)
             (execute-command "CREATE TABLE EDGES(FROMNODE VARCHAR(255), TONODE VARCHAR(255), LABEL BLOB,  RACERLABEL BLOB)"
                              :database db)

             #|
             (create-table 'edges
                           '((fromnode varchar :not-null)
                             (tonode   varchar :not-null)
                             (label varchar)
                             (racerlabel varchar)))
|# )))))
           

(defmethod delete-substrate :after ((substrate sql-data-substrate) &key &allow-other-keys)

  ;;; (destroy-db (slot-value substrate 'db-name))

  )

;;;
;;; Node construction
;;; 
    

(defmethod node-instance ((substrate sql-data-substrate) name 
                          &key
                          (description nil des-supplied-p)
                          (racer-description nil racer-des-supplied-p) &allow-other-keys)

  (with-db-connection (db substrate)
    (let* ((info (find-db-node substrate name))
           (description (mapcar #'ensure-list (ensure-list description))))

      (if info

          (let ((av (list (list 'label 
                                (combine-labels (first info)
                                                (if des-supplied-p
                                                    description
                                                  +no-descr-marker+)))
                          (list 'racerlabel 
                                (combine-labels (second info)
                                                (if racer-des-supplied-p
                                                    racer-description
                                                  +no-descr-marker+))))))
            
            (update-records 'nodes
                            :av-pairs av
                            :where (sql-operation '= (sql-expression :attribute 'name) (encode-value name))
                            :database db))

        (let ((av (list (list 'name (encode-value name))
                        (list 'label 
                              (encode-value
                               (if des-supplied-p 
                                   description
                                 +no-descr-marker+)))
                        (list 'racerlabel 
                              (encode-value
                               (if racer-des-supplied-p 
                                   racer-description
                                 +no-descr-marker+))))))

          (insert-records :into 'nodes
                          :av-pairs av
                          :database db))))))

#|

(defmethod loop-over-nodes1 ((substrate sql-data-substrate) fn)
  (with-db-connection (db substrate)
    (do-query (tuple "select * from NODES")
      ;; geht nicht! obwohl eigentlich schoener, 
      ;; aber mySQL kann nicht mehr als eine 
      ;; Query "aktiv" halten ? 
      (let* ((tuple (convert-db-tuples tuple))
             (node (first tuple))
             (info (list (second tuple) nil)))
        (funcall fn node info)))))
|#

(defmethod loop-over-nodes1 ((substrate sql-data-substrate) fn)
  (with-db-connection (db substrate)
    (dolist (tuple 
             (convert-db-tuples 
              (clsql:query "SELECT * FROM NODES"
                            :database db)))
      (let ((node (first tuple))
            (info (list (second tuple) nil)))
        (funcall fn node info)))))


(defmethod get-node-info ((substrate sql-data-substrate) node)
  (let ((node (find-db-node substrate node)))
    (or node +no-descr-marker+)))

(defmethod node-p ((substrate sql-data-substrate) node)
  (when (find-db-node substrate node)
    t))

;;;
;;; Edge construction
;;; 

(defmethod nodes-related ((substrate sql-data-substrate) from to description 
                          &key told-info-p
                          (racer-description nil racer-des-supplied-p))

  (with-db-connection (db substrate)
    
    (let* ((description (mapcar #'ensure-list (ensure-list description))))

      (unless (node-p substrate from)
        (node-instance substrate from :told-info-p told-info-p))
    
      (unless (node-p substrate to)
        (node-instance substrate to :told-info-p told-info-p))
    
      (let ((info (find-db-edge substrate from to)))
    
        (if info

            (let ((av (list (list 'label 
                                  (combine-labels (first info)
                                                  description))
                            (list 'racerlabel 
                                  (combine-labels (second info)
                                                  (if racer-des-supplied-p
                                                      racer-description
                                                    +no-descr-marker+))))))
            
              (update-records 'edges
                              :av-pairs av
                              :where (sql-operation 'and 
                                                    (sql-operation '= (sql-expression :attribute 'fromnode) (encode-value from))
                                                    (sql-operation '= (sql-expression :attribute 'tonode) (encode-value to)))
                              :database db))
                                                  
        
          (let ((av (list (list 'fromnode (encode-value from))
                          (list 'tonode (encode-value to))
                          (list 'label 
                                (encode-value
                                 description))
                                
                          (list 'racerlabel 
                                (encode-value
                                 (if racer-des-supplied-p 
                                     racer-description
                                   +no-descr-marker+))))))
          
            (insert-records :into 'edges
                            :av-pairs av
                            :database db))))))
    
  (values))


(defmethod get-node-successors ((substrate sql-data-substrate) from)
  (with-db-connection (db substrate)
    (convert-db-tuples
     (mapcar #'first
             (clsql:query 
              (format nil "select TONODE from EDGES where FROMNODE = '~A'" 
                      (encode-value from))
              :database db)))))

(defmethod get-node-predecessors ((substrate sql-data-substrate) to)
  (with-db-connection (db substrate)
    (convert-db-tuples
     (mapcar #'first
             (clsql:query 
              (format nil "select FROMNODE from EDGES where TONODE = '~A'" 
                      (encode-value to))
              :database db)))))


#|
(defmethod loop-over-edges1 ((substrate sql-data-substrate) fn)
  (with-db-connection (db substrate)
    (do-query (tuple "select * from EDGES")
      (let* ((tuple (convert-db-tuples tuple))
             (edge (list (first tuple)
                         (second tuple)))
             (info (list (third tuple))))
        (funcall fn edge info)))))
|#


(defmethod loop-over-edges1 ((substrate sql-data-substrate) fn)
  (with-db-connection (db substrate)
    (dolist (tuple (convert-db-tuples 
                    (clsql:query "select * from EDGES"
                                  :database db)))
      (let* ((edge (list (first tuple)
                         (second tuple)))
             (info (list (third tuple))))
        (funcall fn edge info)))))


(defmethod get-edge-info ((substrate sql-data-substrate) from to)
  (let ((edge (find-db-edge substrate from  to)))
    (or edge +no-descr-marker+)))

(defmethod edge-p ((substrate sql-data-substrate) from to)
  (when (find-db-edge substrate from to)
    t))

  
;;;
;;;
;;;                    

(defmethod delete-node ((substrate sql-data-substrate) name &key &allow-other-keys)
  (with-db-connection (db substrate)
    (delete-records :from 'nodes
                    :where (sql-operation '= (sql-expression :attribute 'name) (encode-value name)))))

(defmethod delete-edge ((substrate sql-data-substrate) from to &key &allow-other-keys)
  (with-db-connection (db substrate)
    (delete-records :from 'edges
                    :where (sql-operation 'and 
                                          (sql-operation '= (sql-expression :attribute 'fromnode) (encode-value from))
                                          (sql-operation '= (sql-expression :attribute 'tonode) (encode-value to))))))

;;;
;;;
;;;

(defmethod reset-substrate :after ((substrate sql-data-substrate) &key &allow-other-keys)
  )

(defmethod reset-caches :after ((substrate sql-data-substrate))
  nil)

;;;
;;; 
;;;

(nrql-defun (set-sql-data-box) (name)
  (unless (eq *type-of-substrate* 'mirror-sql-data-substrate)
    (setf *type-of-substrate* 'sql-data-substrate))

  (set-data-box name))

(nrql-defmacro (in-sql-data-box :nrql-function set-sql-data-box))


;;;
;;;

(defun test ()
  (full-reset)
  (in-sql-data-box test)
  
  (data-node a a)
  (data-node b b)
  (data-edge a b r)
  
  (pprint (retrieve (?*x ?*y) (and (?*x a) (?*y b) (?*x ?*y r)))))

