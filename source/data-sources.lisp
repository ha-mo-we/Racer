;;; -*- package: WILBUR; Syntax: Common-lisp; Base: 10 -*-

;;;
;;;;  data-sources.lisp
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
;;;   Version: $Id: wilbur2-file-header.lisp,v 1.1 2004/08/10 16:24:46 ora Exp $
;;;
;;;   Purpose: 
;;;


(in-package :wilbur)


;;; --------------------------------------------------------------------------------------
;;;
;;;   DB SOURCE LOADING PROTOCOL
;;;

(defgeneric source-open-stream (source))
(defgeneric source-close-stream (source stream &optional abortp))
(defgeneric source-locator (source))
(defgeneric source-make-temporary-db (source))
(defgeneric source-fill-db (source db))
(defgeneric db-load-using-source (db source &rest options))

(defmethod db-load ((db db) source
		    &rest options
		    &key (error-handling :signal)
		         (merge-results-p (eq error-handling :signal))
		    &allow-other-keys)
  (declare (dynamic-extent options))
  (let ((errors nil))
    (remf options :error-handling)
    (remf options :merge-results-p)
    (multiple-value-bind (temporary-db source-node)
			 (handler-case (apply #'db-load-using-source db source options)
			   (nox::wilbur-error (e)
			     (ecase error-handling
			       (:signal  (cerror "Keep going" e))
			       (:collect (push e errors)
					 (continue e)))))
      (when (and source-node temporary-db merge-results-p)
	(db-del-source db source-node)
	(db-merge db temporary-db))
      (values temporary-db source-node errors))))

(defmethod db-load-using-source ((db db) source
				 &rest options
				 &key (locator (source-locator source))
				 &allow-other-keys)
  (declare (dynamic-extent options))
  ;; This mimics the possible expansion of WITH-OPEN-FILE
  (let ((abortp t)
	(stream (source-open-stream source)))
    (unwind-protect (multiple-value-prog1 (apply #'parse-db-from-stream
						 stream locator options)
		      (setf abortp nil))
      (source-close-stream source stream abortp))))

(defmethod source-close-stream (source stream &optional abortp)
  (declare (ignore source))
  (close stream :abort abortp))

(defmethod db-load-using-source ((db db) (source string) &rest options)
  (declare (dynamic-extent options))
  (apply #'db-load-using-source db (make-url source) options))

(defmethod source-locator ((source string)) ; assuming it is a URL
  source)

(defmethod source-locator ((source url))
  (url-string source))

(defmethod source-open-stream ((source file-url))
  (open (url-path source)))

(defmethod source-open-stream ((source http-url))
  (http-body (http-request source :get)))


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS SOURCE-WITH-MODIFICATION
;;;

(defclass source-with-modification ()
  ((original-source
    :accessor source-original-source)
   (original-stream
    :initform nil
    :accessor source-original-stream)))

(defmethod initialize-instance :after ((self source-with-modification)
				       &rest options
				       &key original-source
				       &allow-other-keys)
  (declare (ignore options))
  (setf (source-original-source self)
	(if (stringp original-source)
	  (make-url original-source)
	  original-source)))

(defgeneric source-modification (source original-stream))

(defmethod source-locator ((source source-with-modification))
  (source-locator (source-original-source source)))

(defmethod source-open-stream ((source source-with-modification))
  (source-modification source
		       (setf (source-original-stream source)
			     (source-open-stream (source-original-source source)))))

(defmethod source-close-stream :after ((source source-with-modification) stream
				       &optional abortp)
  (declare (ignore stream))
  (source-close-stream (source-original-source source)
		       (shiftf (source-original-stream source) nil)
		       abortp))


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS COMPUTED-SOURCE
;;;

(defclass computed-source ()
  ())

(defmethod db-load-using-source ((db db) (source computed-source)
				 &rest options
				 &key (locator (source-locator source))
				 &allow-other-keys)
  (declare (ignore options))
  (let ((temporary-db (source-make-temporary-db source)))
    (source-fill-db source temporary-db)
    (values temporary-db (node locator))))
