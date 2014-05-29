;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: DAG; Base: 10 -*-

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

(in-package :dag)

;;;
;;;
;;;

(defvar *node* nil)

(defvar *type* :directed-acyclic-graph)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun string-transform (string)
    (ecase (readtable-case *readtable*)
      (:upcase (if (char= (aref string 0) #\|)
                 string
                 (string-upcase string)))
      (:preserve string)
      (:downcase (string-downcase string)))))

#+:racer
(defmacro defpersistentclass (&rest rest)
  `(progn ,@(let ((name (first rest)))
	      (list 
	       `(persistence-manager:defclass ,@rest)
               `(defun ,(intern (string-transform (format nil "is-~A-p" name))) (obj)
                  (typep obj ',name))))))

#-:racer
(defmacro defpersistentclass (&rest rest)
  `(progn ,@(let ((name (first rest)))
	      (list 
	       `(defclass ,@rest)
               `(defun ,(intern (string-transform (format nil "is-~A-p" name))) (obj)
                  (typep obj ',name))))))


(defpersistentclass dag ()
  (#+:clim 
   (gprinter :accessor gprinter :initarg :gprinter :initform 'draw-dag-node)
   (tprinter :accessor tprinter :initarg :tprinter :initform 'dag-node-name)
   (dag-name :accessor dag-name :initarg :name)
   (dag-nodes :accessor dag-nodes :initform nil :initarg :nodes)))

;;;
;;;
;;;

(defmethod dag-roots ((dag dag))
  (remove-if #'dag-node-parents (dag-nodes dag)))

(defmethod dag-top ((dag dag))
  (let ((roots  
         (dag-roots dag)))
    (when (and roots (not (cdr roots)))
      (first roots))))

;;;
;;;
;;;

(defmethod dag-leaves ((dag dag))
  (remove-if #'dag-node-children (dag-nodes dag)))

(defmethod dag-bottom ((dag dag))
  (let ((leaves 
         (dag-leaves dag)))
    (when (and leaves (not (cdr leaves)))
      (first leaves))))

;;;
;;;
;;;

(defpersistentclass dag-node ()
  ((dag-node-name :accessor dag-node-name :initarg :name :initform nil)
   
   (in-dag :accessor in-dag :initarg :in-dag :initform nil)
   
   (dag-node-ancestors)
   (dag-node-descendants)

   (dag-node-children :accessor dag-node-children :initarg :children :initform nil)
   (dag-node-parents :accessor dag-node-parents :initarg :parents :initform nil)

   (dag-node-marked-p :accessor dag-node-marked-p :initarg :dag-node-marked-p :initform nil)))

(defpersistentclass node-proxy (dag-node)
  ((for-node :accessor for-node :initform nil :initarg :for-node)))

;;;
;;;
;;;

(defmethod copy-dag-node ((node dag-node))
  (make-instance 'node-proxy :for-node node))

(defmethod copy-dag-node ((node node-proxy))
  (make-instance 'node-proxy :for-node (for-node node)))

;;;
;;;
;;;

(defmethod dag-node-ancestors ((node dag-node))
  (unless (slot-boundp node 'dag-node-ancestors)
    (compute-transitive-closure (in-dag node)))
  (slot-value node 'dag-node-ancestors))


(defmethod dag-node-descendants ((node dag-node))
  (unless (slot-boundp node 'dag-node-descendants)
    (compute-transitive-closure (in-dag node)))
  (slot-value node 'dag-node-descendants))

;;;
;;;
;;;

(defun make-dag (&rest args &key (type 'dag) &allow-other-keys)
  (apply #'make-instance type :allow-other-keys t args))

(defmethod make-dag-node (&rest args &key (type 'dag-node) &allow-other-keys)
  (apply #'make-instance type :allow-other-keys t args))

;;;
;;;
;;;

(defmethod insert-dag-node ((dag dag) (node dag-node))
  (let ((parents (dag-node-parents node))
        (children (dag-node-children node)))

    (setf (in-dag node) dag)
    
    ;;; (break "Node: ~A Parents: ~A Children: ~A" node parents children)
    
    (dolist (parent parents)
      (setf (dag-node-children parent)
            (remove-duplicates
             (cons node (set-difference (dag-node-children parent) children)))))
    
    (dolist (child children)
      (setf (dag-node-parents child)
            (remove-duplicates
             (cons node (set-difference (dag-node-parents child) parents)))))

    (push node (dag-nodes dag))

    (dolist (node (dag-nodes dag))
      (slot-makunbound node 'dag-node-descendants)
      (slot-makunbound node 'dag-node-ancestors))

    dag))


(defmethod delete-dag-node ((dag dag) (node dag-node))
  (let ((parents (dag-node-parents node))
        (children (dag-node-children node)))
 
    (dolist (child children)
      (setf (dag-node-parents child)
            (append (delete node (dag-node-parents child))
                    (dag-node-parents node))))
               
    (dolist (parent parents)
      (setf (dag-node-children parent)
            (append (delete node (dag-node-children parent))
                    (dag-node-children node))))

    (setf (dag-nodes dag) 
          (delete node (dag-nodes dag)))

    (dolist (node (dag-nodes dag))
      (setf (slot-value node 'dag-node-descendants) nil
            (slot-value node 'dag-node-ancestors) nil))
    
    dag))


;;;
;;;
;;;

(defmethod unmark-all-dag-nodes ((dag dag))
  (dolist (node (dag-nodes dag))
    (unmark-dag-node node)))

(defmethod mark-all-dag-nodes ((dag dag) &optional (val t))
  (dolist (node (dag-nodes dag))
    (mark-dag-node node val)))
  

;;;
;;;
;;;

(defmethod mark-dag-node ((node dag-node) &optional (val t))
  (setf (dag-node-marked-p node) val))

(defmethod unmark-dag-node ((node dag-node))
  (setf (dag-node-marked-p node) nil))

;;;
;;;
;;;

(defmethod print-object ((node dag-node) stream)
  (format stream "#<~A ~A>"
          (type-of node)
          (dag-node-name node)))

(defmethod print-object ((dag dag) stream)
  (format stream "#<~A ~A>" 
          (type-of dag) 
          (dag-name dag)))
  
;;;
;;;
;;;

(defmethod show-dag ((dag dag) stream &key (printer (tprinter dag)))
  (labels ((do-it (node &optional (level 0) next)	     
	     (format stream ";;;")
	     (if (= level 0)
		 (format stream "  ~A~%" (funcall printer node))
	       (progn 
		 (dolist (item (butlast next))
		   (if item 
		       (format stream "   |")
		     (format   stream "    ")))
		 (let ((last (first (last next))))
		   (if last
                       (format stream "   |---~A~%" (funcall printer node))
                     (format stream   "   L___~A~%" (funcall printer node))))))
	     (let ((last (first (last (dag-node-children node)))))
	       (dolist (child (dag-node-children node))
		 (do-it child (1+ level) (append next (list (not (eq child last)))))))))
    ;; (format stream "~%~%")
    (dolist (root (dag-roots dag))
      (do-it root)
      (terpri))))

;;;
;;;
;;;

(defmethod node-equivalent-p ((a dag-node) (b dag-node))
  (equal (dag-node-name a) 
         (dag-node-name b)))

;;;
;;;
;;;

(defmethod dag-isomorphic-p ((dag1 dag) (dag2 dag))
  (labels ((matches-p (a b)
             
             (and (node-equivalent-p a b)
                  
                  (let ((as (dag-node-children a))
                        (bs (dag-node-children b)))
                    (when (= (length as) (length bs))
                      (setf as (sort as #'string-lessp :key #'dag-node-name))
                      (setf bs (sort bs #'string-lessp :key #'dag-node-name))
                      
                      (every #'matches-p
                             (dag-node-children a)
                             (dag-node-children b)))))))
    (or (eq dag1 dag2)
        (let ((as (dag-roots dag1))
              (bs (dag-roots dag2)))
          (when (= (length as) (length bs))
            (setf as (sort as #'string-lessp :key #'dag-node-name))
            (setf bs (sort bs #'string-lessp :key #'dag-node-name))

            (every #'matches-p
                   as
                   bs))))))

;;;
;;;
;;;

#+:clim
(defmethod create-clos-classes ((dag dag))
  (labels ((do-it (node)
             (eval `(cl:defclass ,(dag-node-name node)
                              ,(mapcar #'(lambda (x) 
                                           (dag-node-name x))
                                       (dag-node-parents node))))))
    (mapc #'do-it (dag-nodes dag)))
  dag)


(defmethod compute-hasse-diagram ((dag dag))
  (labels ((do-it (node)
             (let* ((succs (slot-value node 'dag-node-descendants))
                    (dag-node-children (copy-list succs)))
               (dolist (succ succs)
                 (dolist (succ-succ (slot-value succ 'dag-node-descendants))
                   (when (member succ-succ succs)
                     (setf dag-node-children
                           (delete succ-succ dag-node-children)))))
               (setf (dag-node-children node) dag-node-children))))
    (dolist (node (dag-nodes dag))
      (do-it node))
    (dolist (node (dag-nodes dag))
      (dolist (child (dag-node-children node))
        (pushnew node (dag-node-parents child))))
    dag))


(defmethod compute-transitive-closure ((dag dag))
  (labels ((compute-descendants (node)
             (append (dag-node-children node)
                     (apply #'append
                            (mapcar #'compute-descendants (dag-node-children node)))))
           (compute-ancestors (node)
             (append (dag-node-parents node)
                     (apply #'append
                            (mapcar #'compute-ancestors (dag-node-parents node))))))
    
    (dolist (node (dag-nodes dag))
      (setf (slot-value node 'dag-node-descendants)
            (remove-duplicates (compute-descendants node)))
      (setf (slot-value node 'dag-node-ancestors)
            (remove-duplicates (compute-ancestors node))))
    
    dag))

;;;
;;;
;;;


#+(and :clim :lispworks)
(progn 

  (defmethod make-postscript-file ((dag dag) filename)
    (with-open-file (stream filename
                            :direction :output
                            :if-exists :supersede)
      #| (let ((*standard-output* stream))
           (when (dag-top dag)
             (cl-user::psgraph (dag-top dag) 
                               #'dag-node-children
                               #'(lambda (x) (list (dag-node-name x)))))) |#
      (clim:with-output-to-postscript-stream (ps-stream stream
                                                        :scale-to-fit t)
        (draw-dag-display *application-frame* ps-stream :printing-p t))))


  (define-application-frame dag-browser ()
    ((dag :initform nil
          :initarg :dag
          :accessor dag)
     (orientation :accessor orientation :initform :right)
     (graph-type :accessor graph-type :initform *type*)
     (show-top-p :initform t)   
     (show-bottom-p :initform nil)      
     (app-stream :initform nil :accessor app-stream))
    (:panes
     (dag-display :application
                  :display-function 'draw-dag-display
                  :end-of-line-action :allow
                  :end-of-page-action :allow             
                  :display-after-commands t)
     (dag-info :application
               :scroll-bars :both
               :end-of-line-action :allow
               :end-of-page-action :allow
               :display-after-commands t)
     (dag-options :accept-values
                  :scroll-bars nil
                  :display-function
                  `(accept-values-pane-displayer
                    :displayer ,#'(lambda (frame stream)
                                    (accept-options frame stream)))))

    (:layouts
     (:defaults
      (vertically () 
        (1/10 dag-options)           
        (5/10 dag-display)
        (4/10 dag-info)))))
 
  (defmethod accept-options ((frame dag-browser) stream)  
    (with-slots (show-top-p show-bottom-p orientation graph-type) frame
    
      (formatting-table (stream :multiple-columns t)
        (formatting-row (stream)

          (formatting-cell (stream)
            (multiple-value-bind (object)
                (accept 'clim:boolean
                        :prompt "Show Top"
                        :prompt-mode :raw
                        :stream stream 
                        :default show-top-p)
              (setf show-top-p object)))

          (formatting-cell (stream)
            (multiple-value-bind (object)
                (accept 'clim:boolean
                        :prompt "Show Bottom"
                        :prompt-mode :raw
                        :stream stream 
                        :default show-bottom-p)
              (setf show-bottom-p object)))

          (formatting-cell (stream)
            (multiple-value-bind (object)
                (accept 'completion
                        :prompt "Orientation"
                        :stream stream 
                        :default orientation
                        :view `(option-pane-view :items (:down :right)))
              (setf orientation object)))

          
          (formatting-cell (stream)
            (multiple-value-bind (object)
                (accept 'completion
                        :prompt "Graph Type"
                        :stream stream 
                        :default graph-type
                        :view `(option-pane-view :items (:tree :directed-acyclic-graph)))
              (setf graph-type object)))))))

  ;;;
  ;;;
  ;;;
                
  (defmethod draw-dag-display ((frame dag-browser) stream &key printing-p)
    (with-slots (show-bottom-p show-top-p dag graph-type orientation) frame
      (with-slots (gprinter) dag

        (labels ((do-it ()

                   (format-graph-from-roots 
     
                    (if show-top-p
                        (dag-roots dag)
       
                      (let ((roots (dag-roots dag)))
                        (remove-if-not #'(lambda (x)
                                           (some #'(lambda (parent)
                                                     (member parent roots))
                                                 (dag-node-parents x)))
                                       (dag-nodes dag))))
     
                    (symbol-function gprinter)
     
                    #'(lambda (x)
                        (remove-duplicates
                         (if show-bottom-p
                             (dag-node-children x)
                           (remove-if #'(lambda (node)
                                          (eq node (dag-bottom dag)))
                                      (dag-node-children x)))))
     
                    :center-nodes t
                    :orientation orientation 
                    :stream stream
                    :graph-type graph-type
                    :duplicate-test #'equalp
                    :merge-duplicates t
                    :arc-drawer 
                    #'(lambda (stream from-object
                                      to-object x1 y1
                                      x2 y2 
                                      &rest
                                      drawing-options)
                        (declare (dynamic-extent
                                  drawing-options))
                        (declare (ignore from-object
                                         to-object))
                        (apply #'draw-arrow* stream
                               x1 y1 x2 y2 drawing-options))
                    :merge-duplicates t)
                   (setf (app-stream frame) stream)))


        
          (if printing-p 
              (indenting-output (stream '(8 :character))
                (declare (ignorable stream))
                (do-it))
            (do-it))))))

  ;;;
  ;;;
  ;;;  

  (defmethod draw-dag-node ((object node-proxy) stream)  
    (draw-dag-node (for-node object) stream))

  (defmethod draw-dag-node ((object dag-node) stream)
    (with-output-as-presentation (stream object (type-of object))
      (if (dag-node-marked-p object)
          (surrounding-output-with-border
              (stream :shape :rectangle)
            (surrounding-output-with-border
                (stream :shape :rectangle)
              (format stream "~A" (funcall (symbol-function (tprinter (in-dag object))) object))))
        (surrounding-output-with-border
            (stream :shape :rectangle)
          (format stream "~A" (funcall (symbol-function (tprinter (in-dag object))) object))))))

  ;;;
  ;;;
  ;;;

  (defmethod make-subdag-from ((node dag-node))
    (let* ((dag (in-dag node))
           (subdag (make-dag :type (type-of dag) 
                             :name (format nil "Sub-DAG of DAG ~A from Node ~A" 
                                           (dag-name dag) (dag-node-name node))))
           (nodes (cons node (dag-node-descendants node))))

      (unmark-all-dag-nodes dag)
      (dolist (node nodes) 
        (mark-dag-node node))

      (let ((new-nodes (make-hash-table :size (length nodes))))
        (dolist (node nodes)
          (setf (gethash node new-nodes) 
                (copy-dag-node node)))
        (dolist (node nodes)
          (let* ((new-node (gethash node new-nodes))
                 (parents (mapcar #'(lambda (x) 
                                      (gethash x new-nodes))
                                  (remove-if-not #'dag-node-marked-p (dag-node-parents node))))
                 (children (mapcar #'(lambda (x) 
                                       (gethash x new-nodes))
                                   (remove-if-not #'dag-node-marked-p (dag-node-children node)))))
            (setf (dag-node-parents new-node) parents
                  (dag-node-children new-node) children)
            (setf (in-dag new-node) subdag)
            (push new-node (dag-nodes subdag)))))
      (unmark-all-dag-nodes subdag)
      (unmark-all-dag-nodes dag)
      (visualize-dag subdag)))
                           
  (defun swap (first second list)
    (let* ((pos1 (position first list))
           (pos2 (position second list))
           (min (min pos1 pos2))
           (max (max pos1 pos2)))
      (append (subseq list 0 min)
              (if (= min pos1)
                  (list second)
                (list first))
              (subseq list (1+ min) max)
              (if (= min pos1)
                  (list first)
                (list second))
              (subseq list (1+ max)))))
    
  ;;;
  ;;; Commands
  ;;;
  
  (define-dag-browser-command (com-exit :menu "Exit") ()
    (frame-exit *application-frame*))

  (define-dag-browser-command (com-ps-file :menu "Make Postscript File") ()
    (make-postscript-file (dag *application-frame*)
                          "dag.ps" ))

  (define-dag-browser-command (com-inspect-node
                               :menu nil)
      ((node 'dag-node :gesture :select))
    (let ((*standard-output* (get-frame-pane *application-frame* 'dag-info))
          (*print-pretty* t))
      (setf *node* node)
      (window-clear *standard-output*)
      (describe-object node *standard-output*)))


  (define-dag-browser-command (com-make-subdag-from-node :menu nil)
      ((node 'dag-node :gesture :select))
    (make-subdag-from node))


  (define-dag-browser-command (com-exchange-child-node-positions :menu nil)
      ((first 'dag-node :gesture :select))

    (let ((dag (in-dag first)))
      (unmark-all-dag-nodes dag)
      (dolist (parent (dag-node-parents first))
        (dolist (child (dag-node-children parent))
          (unless (eq child first)
            (mark-dag-node child))))
    
      (redisplay-frame-panes *application-frame*)
      (terpri) (terpri)
    
      (let* ((second (accept 
                      `(and dag-node
                            (satisfies ,#'(lambda (object)
                                            (dag-node-marked-p object))))
                      :prompt "Enter exchange node on same level")))


        (exchange-child-nodes 
         (dag *application-frame*)  ;; wichtig! evtl. ungleich dag! wegen proxies etc. ! 
         first second))

      (unmark-all-dag-nodes dag))
  
    (draw-dag-display *application-frame*
                      (get-frame-pane *application-frame* 'dag-display)))


  (defmethod exchange-child-nodes ((dag dag) (first dag-node) (second dag-node))
    (let ((first (find-if #'(lambda (x) (if (typep x 'node-proxy)
                                            (eq (for-node x) first)
                                          (eq x first)))
                          (dag-nodes dag)))
          (second (find-if #'(lambda (x) (if (typep x 'node-proxy)
                                             (eq (for-node x) second)
                                           (eq x second)))
                           (dag-nodes dag))))

      (let ((common-parents (intersection (dag-node-parents first)
                                          (dag-node-parents second))))
        (dolist (parent common-parents)
          (let* ((children (dag-node-children parent))
                 (pos1 (position first children))
                 (pos2 (position second children)))
            (when (and pos1 pos2)
              (setf (dag-node-children parent)
                    (swap first second children))))))))

  ;;;
  ;;;
  ;;;

  (define-presentation-method highlight-presentation ((node dag-node) record stream state) 
    (let* ((node (presentation-object record))
           (parents (dag-node-parents node))
           (children (dag-node-children node))
           (objects (cons node (append parents children)))
         
           (history (stream-output-history stream))
           (roots (graph-root-nodes 
                   (first (output-record-children history))))

           (nodes nil))

      (labels ((do-it (current)
                 (let ((object 
                        (slot-value current 'clim-internals::object)))
                   (let ((object (if (typep object 'node-proxy)
                                     (for-node object)
                                   object)))
                     (when (member object objects)
                       (unless (member object nodes)
                         (push object nodes)                 
                         (multiple-value-bind (x y)
                             (bounding-rectangle-position current)
                           (multiple-value-bind (w h)
                               (bounding-rectangle-size current)
                             (draw-rectangle* stream x y (+ x w) (+ y h) :ink +flipping-ink+)))))))
                     
                 (dolist (child (graph-node-children current))
                   (do-it child))))

        (dolist (root roots)
          (do-it root)))))

  )

;;;
;;;
;;;

(defmethod visualize-dag ((dag dag) &rest args &key
                          printer 
                          #+:clim (view :graphically)
                          #-:clim (view :textually)
                          (type 'dag-browser) &allow-other-keys)
  (declare (ignorable type args))
  (ecase view 
    #+(and :clim :lispworks)
    (:graphically
     (let ((dag-browser
            (apply #'make-application-frame type
                   :pretty-name (dag-name dag)
                   :dag dag
                   :frame-manager
                   (find-frame-manager :port (find-port))
                   :width 800
                   :height 600
                   :allow-other-keys t 
                   args)))
       
       (mp:process-run-function
        "DAG Browser"
        '(:size 1400000)
        #'(lambda ()
            (run-frame-top-level dag-browser)))))
    (:textually 
     (show-dag dag t :printer (or printer (tprinter dag))))))


