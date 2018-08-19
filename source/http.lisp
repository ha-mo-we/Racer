;;; -*- package: WILBUR-RACER; Syntax: Common-lisp; Base: 10 -*-

;;;
;;;;  http-client.lisp
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
;;;                   Louis Theran <theran@pobox.com>
;;;
;;; --------------------------------------------------------------------------------------
;;;
;;;
;;;   Purpose: This is a simple implementation of an HTTP client, conditionalized for
;;;   several platforms and environments. OK, so it is somewhat braindead, but at least
;;;   it works.
;;;


(in-package :wilbur-racer)


;;; --------------------------------------------------------------------------------------
;;;
;;;   EXTERNAL MODULES
;;;

#+(and :mcl (not :openmcl))
(eval-when (:compile-toplevel :load-toplevel)
  (require :opentransport))

#+:excl
(eval-when (:compile-toplevel :load-toplevel)
  (require :iodefs)
  (require :sock))


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS URL
;;;   CLASS HTTP-URL
;;;   CLASS FILE-URL
;;;

(defclass url ()
  ((string
    :initarg :string
    :reader url-string)
   (path
    :initarg :path
    :reader url-path)))

(defmethod print-object ((url url) stream)
  (print-unreadable-object (url stream :type t)
    (prin1 (url-string url) stream)))

(defclass http-url (url)
  ((host
    :initarg :host
    :reader url-host)
   (port
    :initarg :port
    :reader url-port)))

(defclass file-url (url)
  ())

(defun make-url (string)
  (multiple-value-bind (scheme args)
                       (nox-racer:parse-url string)
    (apply #'make-instance (ecase scheme (:http 'http-url) (:file 'file-url))
           :string string args)))


;;; --------------------------------------------------------------------------------------
;;;
;;;   HTTP CONDITIONS
;;;

(define-condition http-error (nox-racer::wilbur-error)
  ((thing
    :initarg :thing
    :reader http-error-thing))
  (:default-initargs 
   :format-control "HTTP --- ~A")
  (:report (lambda (c s)
             (funcall #'report-http-error c s))))

(define-condition http-bad-response (http-error)
  ((got
     :initform nil
     :initarg :got
    :reader http-error-got))
  (:default-initargs 
   :format-control "HTTP --- Expected ~A, got ~A"))

(define-condition http-bad-redirect (http-bad-response)
  ()
  (:default-initargs 
   :format-control "HTTP --- ~A header not found"))

(define-condition http-not-found (http-error)
  ()
  (:default-initargs 
   :format-control "HTTP --- Entity ~A not found"))

(define-condition http-too-many-redirects (http-error)
  ()
  (:default-initargs 
   :format-control "HTTP --- Too many redirects: last was ~A"))

(define-condition http-incomplete-entity (http-error)
  ()
  (:default-initargs 
   :format-control "HTTP --- Incomplete entity: expected ~A more bytes"))

(define-condition http-bad-request (http-error)
  ()
  (:default-initargs 
   :format-control "HTTP --- Bad request"))

(define-condition http-server-error (http-error)
  ()
  (:default-initargs 
   :format-control "HTTP --- Server error"))

(defmethod report-http-error ((condition http-error) stream)
  (apply #'format stream (simple-condition-format-control condition)
         (http-error-thing condition) (simple-condition-format-arguments condition)))

(defmethod report-http-error ((condition http-bad-response) stream)
  (format stream (simple-condition-format-control condition)
          (http-error-thing condition) (http-error-got condition)))


;;; --------------------------------------------------------------------------------------
;;;
;;;   HTTP STREAM GENERIC FUNCTIONS
;;;

(defgeneric open-http-stream (url proxy))

(defgeneric make-http-body-stream (socket-stream))

(defgeneric http-stream-character-count (stream))

(defgeneric (setf http-stream-character-count) (new-value stream))

(defgeneric http-stream-enable-input-chunking (stream))

(defgeneric http-stream-disable-input-chunking (stream))

(defgeneric http-stream-chunked-p (http-stream))


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS HTTP-MESSAGE
;;;

(defclass http-message ()
  ((status
    :initarg :status
    :reader http-status)
   (version
    :initarg :version
    :reader http-version)
   (headers
    :initarg :headers
    :initform nil
    :accessor http-headers)
   (request-time
    :initarg :request-time
    :initform (get-universal-time)
    :reader http-request-time)
   (response-time
    :initform (get-universal-time)
    :reader http-response-time)
   (body
    :initarg :body
    :initform nil
    :reader http-body)))

(defmethod print-object ((self http-message) stream)
  (print-unreadable-object (self stream :identity t :type t)
    (prin1 (http-status self) stream)))

(defmethod get-header ((message http-message) (header string))
  (get-header (http-headers message) header))

(defmethod get-header ((headers list) (header string))
  (nox-racer:string-dict-get headers header))

(defmethod add-header ((headers list) (header string) value)
  (nox-racer:string-dict-add headers header value))

(defun infer-character-count (headers)
  (parse-integer (get-header headers "Content-Length") :junk-allowed t))


;;; --------------------------------------------------------------------------------------
;;;
;;;   HTTP CLIENT API
;;;

#-:openmcl
(defmethod http-request ((url http-url) method
			 &key (proxy (find-http-proxy))
			      (accept "application/rdf+xml"))
  (let ((time (get-universal-time)))
    (with-open-stream (input (open-http-stream url proxy))
      (multiple-value-bind (status version headers)
	                   (http-get-headers input url
					     (ecase method
					       (:get "GET")
					       (:head "HEAD"))
					     accept)
	(make-instance 'http-message
	  :status status
	  :version version
	  :headers headers
	  :request-time time
	  :body (and (eq method :get)
		     (let ((chunkedp (string= (get-header headers "Transfer-Encoding")
					      "chunked"))
			   (stream (make-http-body-stream input)))
		       (if chunkedp
			 (http-stream-enable-input-chunking stream)
			 (setf (http-stream-character-count stream) 
			       (infer-character-count headers)))
		       stream)))))))

#+:openmcl
(defmethod http-request ((url http-url) method
			 &key (proxy (find-http-proxy))
			      (accept "application/rdf+xml"))
  (let ((time (get-universal-time)))
    (multiple-value-bind (status version headers body)
                         (make-external-http-request method url proxy accept)
      (when (eq method :head)
	(close body))
      (make-instance 'http-message
	:status status
	:version version
	:headers headers
	:request-time time
	:body (and (eq method :get) body)))))

#+:openmcl
(defun make-external-http-request (method url proxy accept)
  (declare (special *http-parse-buffers*))
  (let* ((process (run-program "curl"
			       `("-s"
				 ,@(and proxy (list (format nil "-x ~A" proxy)))
				 "--header" ,(format nil "Accept: ~A" accept)
				 ,(ecase method
				    (:get "-i")
				    (:head "--head"))
				 ,(url-string url))
			       :output :stream :wait nil))
	 (input (external-process-output-stream process)))
    (multiple-value-bind (status version headers)
	                 (nox-racer::with-resource-from-pool (parse-buffer *http-parse-buffers*)
			   (read-headers-into-pb parse-buffer input)
			   (compute-response parse-buffer))
      (values status version headers input))))

#-:openmcl
(nox-racer:define-constant -new-line-string- (concatenate 'string (list #\Return #\Linefeed)))

#-:openmcl
(defun make-http-request (method url-path url-host accept)
  (format nil "~@:(~A~) ~A HTTP/1.1~A~
               Host: ~A~A~
               Accept: ~A~A~
               Connection: close~A~A"
	  method url-path -new-line-string- url-host -new-line-string-
	  accept -new-line-string- -new-line-string- -new-line-string-))

#-:openmcl
(defun http-get-headers (input url operation accept)
  (declare (special *http-parse-buffers*))
  (write-sequence (make-http-request operation (url-path url) (url-host url) accept)
		  input)
  (force-output input)
  (nox-racer:with-resource-from-pool (parse-buffer *http-parse-buffers*)
    (read-headers-into-pb parse-buffer input)
    (compute-response parse-buffer)))

(defun http-connection-reusable-p (headers)
  (not (string= "close" (get-header headers "Connection"))))

(defmacro http-response-case (response &body cases)
  (labels ((process-case-head (head)
             (cond ((consp head)
                    (mapcar #'process-case-head head))
                   ((symbolp head)
                    (ecase head
                      ((:found :redirect-temporary) 301)
                      ((:moved-permanently :redirect-permanent) 302)
                      ((:bad-request :request-syntax-error) 400)
                      ((:not-found :entity-not-found) 404)
                      ((:server-error :internal-server-error) 500)
                      ((:ok :success) 200)
                      ((t) t)))
                   (t head))))
    `(case (http-status ,response)
       ,@(mapcar #'(lambda (case) 
                     (cons (process-case-head (car case)) (cdr case)))
                 cases))))

(defparameter *http-max-redirects* 5) ; see RFC 2616

(defun strip-trailing-hash (string)
  (let ((i (1- (length string))))
    (if (char= (char string i) #\#)
      (subseq string 0 i)
      string)))

(defmethod http-request :around ((url http-url) method &rest args)
  (declare (dynamic-extent args))
  (dotimes (i *http-max-redirects*)
    (let ((response (apply #'call-next-method url method args)))
      (http-response-case response
	((:redirect-temporary :redirect-permanent)
	 (let ((location (get-header response "Location")))
	   (if location
	     (let ((stream (http-body response)))
	       (when stream
		 (close stream))
	       (setf url (make-url (strip-trailing-hash location))))
	     (error 'http-bad-redirect :thing "Location"))))
	(:not-found
	 (error 'http-not-found :thing (url-string url)))
	(:bad-request
	 (error 'http-bad-request))
	(:server-error
	 (error 'http-server-error))
	(t
	 (return-from http-request response)))))
  (error 'http-too-many-redirects :thing url))


;;; --------------------------------------------------------------------------------------
;;;
;;;   FAST HTTP HEADER PARSING
;;;

(defparameter *http-parse-buffer-size* 4096)

(defparameter *http-expected-headers* 20)

(defstruct (http-response-parse-buffer (:conc-name pb-))
  (buf (make-string *http-parse-buffer-size*))
  (left-bounds (make-array *http-expected-headers*))
  (right-bounds (make-array *http-expected-headers*))
  seen-headers)

(defun clear-pb (pb)
  (setf (pb-seen-headers pb) 0))

(nox-racer:define-resource-pool *http-parse-buffers* 
  #'make-http-response-parse-buffer #'clear-pb)

(defun stretch-pb-buf (pb delta)
  (let* ((old (pb-buf pb))
         (new (make-string (+ (length old) delta))))
    (setf (substring new 0) old
          (pb-buf pb) new)))

(defun stretch-pb-headers (pb delta)
  (let* ((old-left (pb-left-bounds pb))
         (old-right (pb-right-bounds pb))
         (len (+ (length old-left) delta))
         (new-left (make-array len))
         (new-right (make-array len)))
    (setf (subseq new-left 0) old-left
          (subseq new-right 0) old-right
          (pb-left-bounds pb) new-left
          (pb-right-bounds pb) new-right)))

(defparameter *http-parsepuf-buffer-default-delta* 2048) 

(defparameter *http-parsebuf-headers-default-delta* 20)

(defun read-headers-into-pb (pb stream)
  (let ((len (length (pb-buf pb)))
        (max-lines (length (pb-left-bounds pb))))
    (flet ((push-char (char index)
             (when (>= index len)
               (stretch-pb-buf pb *http-parsepuf-buffer-default-delta*)
               (incf len *http-parsepuf-buffer-default-delta*))
             (setf (char (pb-buf pb) index) char))
           (push-left-bound (pos index)
             (when (>= index max-lines)
               (stretch-pb-headers pb *http-parsebuf-headers-default-delta*)
               (incf max-lines *http-parsebuf-headers-default-delta*))
             (setf (svref (pb-left-bounds pb) index) pos))
           (push-right-bound (pos index)
             (setf (svref (pb-right-bounds pb) index) pos)))
      (declare (inline push-char push-left-bound push-right-bound))
      (loop with state = :want-cr
            with bounds-index = 0
            initially (push-left-bound 0 0)
            for index from 0
            do (let ((ch (read-char stream t)))
		 (push-char ch index)
		 (ecase state
		   (:want-cr
		    (when (eql #\Return ch)
		      (setf state :need-lf)
		      (push-right-bound index bounds-index)
		      (incf bounds-index)))
		   (:need-lf
		    (unless (eql #\Linefeed ch)
		      (error 'http-bad-response
			     :stream stream :thing #\Linefeed :got ch))
		    (setf state :maybe-end))
		   (:maybe-end
		    (push-left-bound index bounds-index)
		    (setf state (if (eql #\Return ch) :end :want-cr)))
		   (:end
		    (unless (eql #\Linefeed ch)
		      (error 'http-bad-response
			     :stream stream :expected #\Linefeed :got ch))
		    (setf (pb-seen-headers pb) bounds-index)
		    (return))))))))

(defun compute-response (pb) 
  (let ((buf (pb-buf pb)))
    (declare (type string buf))
    (flet ((parse-response-line (start end)
             (declare (ignore end))
             (let (version)
               (cond ((string= buf "HTTP/1.0" :start1 start :end1 (+ start 8))
                      (setf version :http/1.0))
                     ((string= buf "HTTP/1.1" :start1 start :end1 (+ start 8))
                      (setf version :http/1.1))
                     (t (error 'http-bad-response :thing "HTTP/1.0 or HTTP/1.1"
                               :got (substring buf start (+ start 8)))))
               (values (parse-integer buf :start 9 :end 12 :radix 10) version)))
           (parse-header-line (start end)
             (multiple-value-bind (header index)
		                  (collect-to-char #\: buf :start start :end end)
               (if (not index)
                 (error 'http-bad-response :thing #\:)
                 (let ((value-start (position #\Space buf
					      :start (1+ index) :end end
					      :test-not #'char=)))
                   (values header (substring buf value-start end)))))))
      (declare (inline parse-response-line parse-header-line))
      (let ((left (pb-left-bounds pb))
            (right (pb-right-bounds pb)))
        (multiple-value-bind (response-code protocol-version)
                             (parse-response-line (svref left 0) (svref right 0))
          (do ((index 1 (1+ index))
               (headers nil)
               (line-count (pb-seen-headers pb)))
              ((>= index line-count) (values response-code protocol-version headers))
            (multiple-value-bind (header value)
                                 (parse-header-line (svref left index) (svref right index))
              (setf headers (add-header headers header value)))))))))

(defun substring (string start &optional end downcasep)
  (declare (type string string) (type fixnum start) (optimize (speed 3) (safety 0)))
  (let* ((end (or end (length string)))
         (rv (make-string (- end start))))
    (declare (type fixnum end))
    (do ((r-index start (1+ r-index))
         (w-index 0 (1+ w-index)))
        ((>= r-index end) rv)
      (declare (type fixnum r-index w-index))
      (setf (char rv w-index) 
            (let ((c (char string r-index)))
              (if downcasep
                (char-downcase c)
                c))))))

(defun (setf substring) (new-value string &optional (start 0) end downcasep)
  (declare (type string string new-value))
  (let* ((end (or end (length string)))
         (end (min (+ start (length new-value)) end)))
    (declare (type fixnum end))
    (do ((w-index start (1+ w-index))
         (r-index 0 (1+ r-index)))
        ((>= w-index end) new-value)
      (declare (type fixnum r-index w-index))
      (setf (char string w-index) 
            (let ((c (char new-value r-index)))
              (if downcasep
                (char-downcase c)
                c))))))

(defun collect-to-char (char string &key (start 0) end downcasep)
  (declare (type string string)
           (type fixnum start)
           (optimize (speed 3) (safety 0)))
  (let ((end-index (position char string :start start :end end :test #'char=)))
    (when end-index 
      (values (substring string start end-index downcasep) end-index))))


;;; --------------------------------------------------------------------------------------
;;;
;;;   HTTP DATE PARSING
;;;
;;;   RFC 2616 says the following:
;;;
;;;       HTTP-date    = rfc1123-date | rfc850-date | asctime-date
;;;       rfc1123-date = wkday "," SP date1 SP time SP "GMT"
;;;       rfc850-date  = weekday "," SP date2 SP time SP "GMT"
;;;       asctime-date = wkday SP date3 SP time SP 4DIGIT
;;;       date1        = 2DIGIT SP month SP 4DIGIT
;;;                      ; day month year (e.g., 02 Jun 1982)
;;;       date2        = 2DIGIT "-" month "-" 2DIGIT
;;;                      ; day-month-year (e.g., 02-Jun-82)
;;;       date3        = month SP ( 2DIGIT | ( SP 1DIGIT ))
;;;                      ; month day (e.g., Jun  2)
;;;       time         = 2DIGIT ":" 2DIGIT ":" 2DIGIT
;;;                      ; 00:00:00 - 23:59:59
;;;       wkday        = "Mon" | "Tue" | "Wed"
;;;                    | "Thu" | "Fri" | "Sat" | "Sun"
;;;       weekday      = "Monday" | "Tuesday" | "Wednesday"
;;;                    | "Thursday" | "Friday" | "Saturday" | "Sunday"
;;;       month        = "Jan" | "Feb" | "Mar" | "Apr"
;;;                    | "May" | "Jun" | "Jul" | "Aug"
;;;                    | "Sep" | "Oct" | "Nov" | "Dec
;;;

(defun parse-http-date (string)
  (labels ((parse-month (s i)
             (do ((j 1 (1+ j))
                  (m '("Jan" "Feb" "Mar" "Apr" "May" "Jun"
                       "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
                     (rest m)))
                 ((or (> j 12) (string= (first m) s :start2 i :end2 (+ i 3)))
                  (and (< j 13) j))))
           (parse-int (s start end)
             (parse-integer s :start start :end end :junk-allowed t))
           (encode-date (year month date hour minute second)
             (when (and year month date hour minute second)
               (encode-universal-time second minute hour date month year 0)))
           (parse-time (s i)
             (values (parse-int s i       (+ i 2))
                     (parse-int s (+ i 3) (+ i 5))
                     (parse-int s (+ i 6) (+ i 8)))))
    (cond
     ;; -- RFC 1123
     ((char= (char string 3) #\,)
      (let ((date            (parse-int   string 5 7))
            (month           (parse-month string 8))
            (year            (parse-int   string 12 16)))
        (multiple-value-bind (hour minute second)
                             (parse-time  string 17)
          (encode-date year month date hour minute second))))
     ;; -- ASCTIME
     ((char= (char string 3) #\Space)
      (let ((month           (parse-month string 4))
            (date            (parse-int   string 8 10)))
        (multiple-value-bind (hour minute second)
                             (parse-time  string 11)
          (multiple-value-bind (a b c d e year)
                               (decode-universal-time (get-universal-time))
            (declare (ignore a b c d e))
            (encode-date year month date hour minute second)))))
     ;; -- RFC 850, with the assumption of 21st century
     (t
      (let ((p (position #\, string :test #'char=)))
        (when p
          (let ((date            (parse-int   string (+ p 2) (+ p 4)))
                (month           (parse-month string (+ p 5)))
                (year            (parse-int   string (+ p 9) (+ p 11))))
            (multiple-value-bind (hour minute second)
                                 (parse-time  string (+ p 12))
              (encode-date (and year (+ year 2000))
                           month date hour minute second)))))))))

(defun parse-iso8601-date (string)
  (labels ((fail-char-p (s c p)
             (not (and (> (length s) p)
                       (char= (char s p) c))))
           (time-zone (s p)
             (ecase (char s p)
               (#\Z 0)
               (#\- (parse-integer string :start (1+ p) :end (+ p 3)))
               (#\+ (- (parse-integer string :start (1+ p) :end (+ p 3)))))))
    (let ((year (parse-integer string :start 0 :end 4)))
      (if (fail-char-p string #\- 4)
        (encode-universal-time 0 0 0 1 1 year)
        (let ((month (parse-integer string :start 5 :end 7)))
          (if (fail-char-p string #\- 7)
            (encode-universal-time 0 0 0 1 month year)
            (let ((day (parse-integer string :start 8 :end 10)))
              (if (fail-char-p string #\T 10)
                (encode-universal-time 0 0 0 day month year)
                (let ((hour (parse-integer string :start 11 :end 13))
                      (min (parse-integer string :start 14 :end 16)))
                  (if (fail-char-p string #\: 16)
                    (encode-universal-time 0 min hour day month year
                                           (time-zone string 16))
                    (let ((sec (parse-integer string :start 17 :end 19)))
                      (encode-universal-time sec min hour day month year
                                             (time-zone string 19)))))))))))))

(defun iso8601-date-string (universal-time &optional omit-time-p)
  (multiple-value-bind (sec min hour day month year weekday dst-p time-zone)
                       (decode-universal-time universal-time)
    (declare (ignore weekday dst-p))
    (if omit-time-p
      (format nil "~4,'0D-~2,'0D-~2,'0D"
              year month day ;hour min sec Too many format args RM Apr. 14
              )
      (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D~A~@[~2,'0D:00~]"
              year month day hour min sec
              (cond ((zerop time-zone) "Z")
                    ((> time-zone 0)   "-")
                    (t                 "+"))
              (and (not (zerop time-zone)) (abs time-zone))))))


;;; --------------------------------------------------------------------------------------
;;;
;;;   PROXIES
;;;

#-(and :openmcl :darwin :uffi)
(defvar *http-proxy* nil)

#-(and :openmcl :darwin :uffi)
(defmethod find-http-proxy ()
  *http-proxy*)

#+(and :openmcl :darwin :uffi)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (uffi:load-foreign-library
   (translate-logical-pathname "wilbur:libs;FindProxies;build;FindProxies.dylib")))

#+(and :openmcl :darwin :uffi)
(uffi:def-function ("FindHTTPProxy" %find-http-proxy)
		   ((host (* :char))
		    (host-size :unsigned-int))
  :returning :unsigned-byte)

#+(and :openmcl :darwin :uffi)
(defun find-http-proxy ()
  (let ((buffer (uffi:allocate-foreign-string 256)))
    (unwind-protect (progn
		      (%find-http-proxy buffer 256)
		      (let ((proxy-string (unless (%null-ptr-p buffer)
					    ;; UFFI:CONVERT-FROM-FOREIGN-STRING broken
					    (%get-cstring buffer))))
			(unless (zerop (length proxy-string))
			  proxy-string)))
      (uffi:free-foreign-object buffer))))


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS SIMPLE-UNTYI-MIXIN
;;;

(defclass simple-untyi-mixin ()
  ((last-char
    :initform nil
    :accessor stream-last-char)))

(defmethod stream-tyi :around ((stream simple-untyi-mixin))
  (or (shiftf (stream-last-char stream) nil)
      (call-next-method)))

(defmethod stream-untyi ((stream simple-untyi-mixin) char)
  (if (stream-last-char stream)
    (error "Two UNTYIs in a row on ~S" stream)
    (setf (stream-last-char stream) char)))


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS HTTP-NETWORK-STREAM (FOR MCL)
;;;

#+(and :mcl (not :openmcl))
(defclass http-network-stream (ccl::opentransport-tcp-stream)
  ((via-proxy-p
    :initform nil
    :initarg :via-proxy-p
    :reader stream-via-proxy-p)
   (url-path
    :initarg :url-path
    :initform nil
    :accessor stream-url-path))
  (:default-initargs
    :reuse-local-port-p t
    :writebufsize ccl::*ot-conn-outbuf-size*))


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS HTTP-BODY-STREAM (FOR MCL)
;;;

#+(and :mcl (not :openmcl))
(defclass http-body-stream (simple-untyi-mixin input-stream)
  ((chunkedp
    :initform nil
    :initarg :chunkedp
    :accessor http-stream-chunked-p)
   (count
    :initform 0
    :initarg :character-count
    :accessor http-stream-character-count)
   (eofp
    :initform nil
    :accessor stream-eofp)
   (network
    :initarg :network-stream
    ;; XXX: Why did I change this? It's totally gratutious.
    ;; Make a decision before the final release.
    :reader http-stream-network-stream)))

;;; Now we eagerly read the chunk length.  Note that there is still a serious problem with 
;;; this stream, in that it doesn't know if it's really eofp and our buffering seems to get
;;; in the way of the correct OT errors being raised.  I thought that reimplementing would
;;; fix that problem, but further reflection showed it just catches one case.
;;;
;;; Ultimately, it looks like the only solution is to implement this directly over OT, 
;;; which is what CL-HTTP does.  Or just tell people that we don't support read-sequence.

#+(and :mcl (not :openmcl))
(defmethod stream-tyi ((stream http-body-stream))
  (with-slots (network count chunkedp eofp) stream
    (cond (chunkedp 
           (flet ((read-chunk-length ()
                    (let* ((line (loop for char = (stream-tyi network)
                                       while (not (char= char #\Linefeed))
                                       collecting char))
                           (new-count (parse-integer (concatenate 'string line)
                                                     :radix 16 :junk-allowed t)))
                      (setf count new-count)
                      (when (zerop new-count) (setf eofp t)))))
             (declare (dynamic-extent read-chunk-length))
             (when (null count) (read-chunk-length))
             (prog1 (stream-tyi network)
               (when (zerop (decf count))
                 (stream-tyi network)
                 (stream-tyi network)
                 (read-chunk-length)))))
          ((null count)
	   (stream-tyi network))
          (t
           (prog1 (stream-tyi network)
             (when (zerop (decf count))
               (setf eofp t)))))))
  
#+(and :mcl (not :openmcl))
(defmethod stream-close :after ((stream http-body-stream))
  (stream-close (http-stream-network-stream stream)))
  
#+(and :mcl (not :openmcl))
(defmethod stream-abort :after ((stream http-body-stream))
  (stream-abort (http-stream-network-stream stream)))


;;; --------------------------------------------------------------------------------------
;;;
;;;   HTTP STREAM METHODS (FOR MCL)
;;;

#+(and :mcl (not :openmcl))
(defmethod open-http-stream ((url http-url) (proxy http-url))
  (make-instance 'http-network-stream
    :host (url-host proxy) :port (url-port proxy) :via-proxy-p t))

#+(and :mcl (not :openmcl))
(defmethod open-http-stream ((url http-url) (proxy null))
  (make-instance 'http-network-stream
    :host (url-host url) :port (url-port url)))

#+(and :mcl (not :openmcl))
(defmethod make-http-body-stream ((stream http-network-stream))
  (make-instance 'http-body-stream :network-stream stream))

#+(and :mcl (not :openmcl))
(defmethod http-stream-enable-input-chunking ((stream http-body-stream))
  (setf (http-stream-chunked-p stream) t)
  (setf (http-stream-character-count stream) nil))

#+(and :mcl (not :openmcl))
(defmethod http-stream-disable-input-chunking ((stream http-body-stream))
  (setf (http-stream-chunked-p stream) nil))


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS HTTP-BODY-STREAM (FOR EXCL)
;;;

#+:excl
(def-stream-class http-body-stream (socket-stream-internet-active)
  ((count
    :accessor http-stream-character-count)
   (state
    :initarg :stream-state
    :accessor http-body-stream-state))
  (:default-initargs
   :state :read-headers))

#+:excl
(defmethod device-read ((stream http-body-stream) buffer start end blockingp)
  (declare (ignore buffer start end blockingp))
  (with-slots (state count) stream
    (let ((c (call-next-method)))
      (when (and (eq state :read-body) (not (eq count :chunked)))
        (case c
          ((-1 -2) (unless (= count 0) 
                     (error 'http-incomplete-entity :stream stream :bytes-left count)))
          ;; XXX: What's the right thing to do here, since we're really losing if this 
          ;;      happens in blocking mode?
          (0)
          ;; XXX: This is a non-pipelining implementation.  We can reuse connections, but to
          ;;      pipeline we'd have to latch in a soft EOF once we've seen the end.  The
          ;;      docs just aren't good enough for me to figure the simple-stream/socket
          ;;      interaction.  ---Louis
          (t (decf count c)
             (when (<= c 0)             ; XXX: Sloppy
               (setf state :read-headers)))))
      c)))

#+:excl
(defmethod (setf http-stream-character-count) (new-count (stream http-body-stream))
  ;; Some of the body may already be buffered in the socket.
  (declare (fixnum new-count))
  (with-slots (excl::buffer excl::buffer-ptr excl::buffpos count) stream
    (declare (fixnum excl::buffpos excl::buffer-ptr))
    (setf count (- new-count (- excl::buffer-ptr excl::buffpos)))))

#+:excl
(defmethod (setf http-stream-character-count) ((new-count (eql :chunked))
					       (stream http-body-stream))
  (setf (slot-value stream 'count) new-count))


;;; --------------------------------------------------------------------------------------
;;;
;;;   HTTP STREAM METHODS (FOR EXCL)
;;;

#+:excl
(defmethod open-http-stream ((url http-url) (proxy http-url))
  (make-socket :remote-host (url-host proxy)
	       :remote-port (url-port proxy)
	       :format :bivalent))

#+:excl
(defmethod open-http-stream ((url http-url) (proxy null))
  (make-socket :remote-host (url-host url)
	       :remote-port (url-port url)
	       :format :bivalent))

#+:excl
(defmethod make-http-body-stream ((stream socket))
  (unless (typep stream 'http-body-stream)
    (change-class stream 'http-body-stream))
  (setf (http-stream-character-count stream) -1
        (http-body-stream-state stream) :read-body)
  stream)

#+:excl
(defmethod http-stream-enable-input-chunking ((stream socket))
  (socket-control stream :input-chunking t))

#+:excl
(defmethod http-stream-enable-input-chunking ((stream http-body-stream))
  (setf (http-stream-character-count stream) :chunked)
  (call-next-method))

#+:excl
(defmethod http-stream-disable-input-chunking ((stream socket))
  (socket-control stream :input-chunking nil))

#+:excl
(defmethod http-stream-disable-input-chunking ((stream http-body-stream))
  (setf (http-stream-character-count stream) -1)
  (call-next-method))
