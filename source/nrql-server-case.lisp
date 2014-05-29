(in-package :racer)

;;;
;;;----------------------------------------------
;;;   Automatically Generated nRQL Server Case   
;;;          Version: 2.0, Build: 2013-03-07 
;;;          Date: March 07 2013, 13:05  
;;;----------------------------------------------
;;;

(defvar *null-stream* (make-broadcast-stream))

(defun process-nrql-request (expr stream n state output-string-stream)
  (case (first expr)
    ((xml-output xml-native-output)
     (process-racer-expr (second expr)
                         nil
                         n
                         state
                         output-string-stream)
     (let* ((native (eq (first expr) 'xml-native-output))
            (value
             (with-output-to-string (sstream)
               (apply #'lisp-to-xml
                      (if *cur-reasoner*
                          (if (last-error *cur-reasoner*)
                              (last-error *cur-reasoner*)
                            (last-answer *cur-reasoner*))
                        (if *last-error* *last-error* *last-answer*))
                      sstream
                      :include-answer-string-p
                      native
                      :top-level-attributes
                      (format nil
                              "id=\"~d\" type=\"~A\""
                              n
                              (if (and *cur-reasoner*
                                       (last-error *cur-reasoner*))
                                  'error
                                'answer))
                      (cddr expr)))))
       (format stream
               ":answer ~D \"~A\" \"~A\""
               n
               (transform-value value)
               (convert-output-to-string output-string-stream))
       (racer-eol stream)))
    ((xml-input)
     (let* ((string (second expr)) (expr (xml-to-lisp string)))
       (process-racer-expr expr stream n state output-string-stream)))
    ((|OWLAPI-getLastOutputStreamString|)
     (with-reasoner ((if (cdr expr) (second expr) *cur-reasoner*))
       (let ((old-policy (return-policy *cur-reasoner*)))
         (unwind-protect
             (progn
               (setf (return-policy *cur-reasoner*) :answer-direct)
               (answer expr
                       state
                       stream
                       n
                       (last-output-stream-string *cur-reasoner*)
                       output-string-stream))
           (setf (return-policy *cur-reasoner*) old-policy)))))
    ((|OWLAPI-getLastAnswer|)
     (with-reasoner ((if (cdr expr) (second expr) *cur-reasoner*))
       (let ((old-policy (return-policy *cur-reasoner*)))
         (unwind-protect
             (progn
               (setf (return-policy *cur-reasoner*) :answer-direct)
               (answer expr
                       state
                       stream
                       n
                       (last-answer *cur-reasoner*)
                       output-string-stream))
           (setf (return-policy *cur-reasoner*) old-policy)))))
    ((|OWLAPI-getIDsOfLastAnswer|)
     (labels ((do-it (obj)
                (or (|OWLAPI-findIDFromObject| obj)
                    (if (listp obj) (mapcar #'do-it obj) obj))))
       (with-reasoner ((if (cdr expr) (second expr) *cur-reasoner*))
         (let ((old-policy (return-policy *cur-reasoner*)))
           (unwind-protect
               (progn
                 (setf (return-policy *cur-reasoner*) :answer-direct)
                 (answer expr
                         state
                         stream
                         n
                         (do-it (last-answer *cur-reasoner*))
                         output-string-stream))
             (setf (return-policy *cur-reasoner*) old-policy))))))
    ((add-doc-phrase)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'add-doc-phrase1)
                        (rest expr)))
               output-string-stream)))
    ((add-doc-image-data)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'add-doc-image-data1)
                        (rest expr)))
               output-string-stream)))
    ((add-doc-image-data-from-file)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'add-doc-image-data-from-file1)
                        (rest expr)))
               output-string-stream)))
    ((add-doc-image-file)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'add-doc-image-file1)
                        (rest expr)))
               output-string-stream)))
    ((del-doc-entry)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'del-doc-entry1) (rest expr)))
               output-string-stream)))
    ((add-doc-entry)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'add-doc-entry1) (rest expr)))
               output-string-stream)))
    ((compute-abox-difference-alternative)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'compute-abox-difference2)
                        (rest expr)))
               output-string-stream)))
    ((compute-abox-difference)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'compute-abox-difference1)
                        (rest expr)))
               output-string-stream)))
    ((retrieve-with-explanation)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'racer-answer-query-with-explanation)
                        (rest expr)))
               output-string-stream)))
    ((del-rcc-edge)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'del-rcc-edge1) (rest expr)))
               output-string-stream)))
    ((del-rcc-node)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'del-rcc-node1) (rest expr)))
               output-string-stream)))
    ((rcc-edge-description)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'rcc-edge-description1)
                        (rest expr)))
               output-string-stream)))
    ((rcc-node-description)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'rcc-node-description1)
                        (rest expr)))
               output-string-stream)))
    ((rcc-edge-label)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'rcc-edge-label1)
                        (rest expr)))
               output-string-stream)))
    ((rcc-node-label)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'rcc-node-label1)
                        (rest expr)))
               output-string-stream)))
    ((rcc-edge)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'rcc-edge1) (rest expr)))
               output-string-stream)))
    ((rcc-node)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'rcc-node1) (rest expr)))
               output-string-stream)))
    ((rcc-related)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'rcc-related1) (rest expr)))
               output-string-stream)))
    ((rcc-instance)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'rcc-instance1) (rest expr)))
               output-string-stream)))
    ((rcc-synonym)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'register-rcc-synonym)
                        (rest expr)))
               output-string-stream)))
    ((rcc-consistent?)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'rcc-consistent-p)
                        (rest expr)))
               output-string-stream)))
    ((in-rcc-box)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'set-rcc-box) (rest expr)))
               output-string-stream)))
    ((in-mirror-data-box)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'set-mirror-data-box)
                        (rest expr)))
               output-string-stream)))
    ((description-implies?)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'description-implies-p)
                        (rest expr)))
               output-string-stream)))
    ((edge-description)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'edge-description1)
                        (rest expr)))
               output-string-stream)))
    ((node-description)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'node-description1)
                        (rest expr)))
               output-string-stream)))
    ((edge-label)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'edge-label1) (rest expr)))
               output-string-stream)))
    ((node-label)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'node-label1) (rest expr)))
               output-string-stream)))
    ((del-data-edge)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'del-data-edge1) (rest expr)))
               output-string-stream)))
    ((del-data-node)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'del-data-node1) (rest expr)))
               output-string-stream)))
    ((data-edge)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'data-edge1) (rest expr)))
               output-string-stream)))
    ((data-node)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'data-node1) (rest expr)))
               output-string-stream)))
    ((in-data-box)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'set-data-box) (rest expr)))
               output-string-stream)))
    ((undefquery)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'undefine-query) (rest expr)))
               output-string-stream)))
    ((def-and-exec-query)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'define-and-execute-query)
                        (rest expr)))
               output-string-stream)))
    ((def-and-prep-query)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'define-and-prepare-query)
                        (rest expr)))
               output-string-stream)))
    ((defquery)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'define-query) (rest expr)))
               output-string-stream)))
    ((tbox-retrieve1)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'racer-answer-tbox-query1)
                        (rest expr)))
               output-string-stream)))
    ((tbox-retrieve)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'racer-answer-tbox-query)
                        (rest expr)))
               output-string-stream)))
    ((prepare-tbox-query1)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'racer-prepare-tbox-query1)
                        (rest expr)))
               output-string-stream)))
    ((prepare-tbox-query)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'racer-prepare-tbox-query)
                        (rest expr)))
               output-string-stream)))
    ((firerule-under-premise1)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'racer-apply-rule-under-premise1)
                        (rest expr)))
               output-string-stream)))
    ((firerule-under-premise)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'racer-apply-rule-under-premise)
                        (rest expr)))
               output-string-stream)))
    ((firerule1)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'racer-apply-rule1)
                        (rest expr)))
               output-string-stream)))
    ((firerule)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'racer-apply-rule)
                        (rest expr)))
               output-string-stream)))
    ((apply-abox-rule-under-premise1)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'racer-apply-rule-under-premise1)
                        (rest expr)))
               output-string-stream)))
    ((apply-abox-rule-under-premise)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'racer-apply-rule-under-premise)
                        (rest expr)))
               output-string-stream)))
    ((apply-abox-rule1)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'racer-apply-rule1)
                        (rest expr)))
               output-string-stream)))
    ((apply-abox-rule)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'racer-apply-rule)
                        (rest expr)))
               output-string-stream)))
    ((preprule1)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'racer-prepare-rule1)
                        (rest expr)))
               output-string-stream)))
    ((preprule)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'racer-prepare-rule)
                        (rest expr)))
               output-string-stream)))
    ((prepare-abox-rule1)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'racer-prepare-rule1)
                        (rest expr)))
               output-string-stream)))
    ((prepare-abox-rule)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'racer-prepare-rule)
                        (rest expr)))
               output-string-stream)))
    ((retrieve-under-premise1)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'racer-answer-query-under-premise1)
                        (rest expr)))
               output-string-stream)))
    ((retrieve-under-premise)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'racer-answer-query-under-premise)
                        (rest expr)))
               output-string-stream)))
    ((retrieve1)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'racer-answer-query1)
                        (rest expr)))
               output-string-stream)))
    ((retrieve)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'racer-answer-query)
                        (rest expr)))
               output-string-stream)))
    ((prepare-abox-query1)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'racer-prepare-query1)
                        (rest expr)))
               output-string-stream)))
    ((prepare-abox-query)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'racer-prepare-query)
                        (rest expr)))
               output-string-stream)))
    ((unbind)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'unbind1) (rest expr)))
               output-string-stream)))
    ((defpar)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'defpar1) (rest expr)))
               output-string-stream)))
    ((defcon)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'defcon1) (rest expr)))
               output-string-stream)))
    ((undefine)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'undefine1) (rest expr)))
               output-string-stream)))
    ((define)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'define1) (rest expr)))
               output-string-stream)))
    ((evaluate)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'evaluate1) (rest expr)))
               output-string-stream)))
    ((sequence)
     (let ((*server-timeout* nil)
           (reasoner *cur-reasoner*)
           (*check-subscriptions-inhibited* t)
           (thematic-substrate::*changes-for-aboxes-pending* nil))
       (with-output-to-string (string-stream)
         (prog1
             (mapl #'(lambda (expressions)
                       (let ((expr1 (first expressions)))
                         (prog1
                             (process-racer-expr expr1
                                                 (if (cdr expressions)
                                                     *null-stream*
                                                   stream)
                                                 n
                                                 state
                                                 (if (cdr expressions)
                                                     string-stream
                                                   output-string-stream))
                           (when (and (last-error reasoner)
                                      (cdr expressions))
                             (error (last-error reasoner))))))
                   (rest expr))
           (let ((*check-subscriptions-inhibited* nil))
             (dolist (abox
                      thematic-substrate::*changes-for-aboxes-pending*)
               (check-nrql-subscriptions abox)))))))
    ((|OWLAPI-sequence|
      owlapi-sequence
      |OWLAPI-testSequence|
      owlapi-test-sequence)
     (let ((*server-timeout* nil)
           (reasoner *cur-reasoner*)
           (*check-subscriptions-inhibited* t)
           (thematic-substrate::*changes-for-aboxes-pending* nil)
           (count 0)
           (test-sequence-p
            (member (first expr)
                    '(|OWLAPI-testSequence| owlapi-test-sequence))))
       (with-reasoner ((second expr))
         (let ((*progress-value* 0))
           (with-progress-range ((length (cddr expr)) (0 100))
             (with-output-to-string (string-stream)
               (prog1
                   (mapl #'(lambda (expressions)
                             (let ((expr1 (first expressions)))
                               (when test-sequence-p (sleep 0.1))
                               (set-progress-value (incf count))
                               (prog1
                                   (process-racer-expr expr1
                                                       (if (cdr expressions)
                                                           *null-stream*
                                                         stream)
                                                       n
                                                       state
                                                       (if (cdr expressions)
                                                           string-stream
                                                         output-string-stream))
                                 (when (and (last-error reasoner)
                                            (cdr expressions))
                                   (error (last-error reasoner))))))
                         (cddr expr))
                 (let ((*check-subscriptions-inhibited* nil))
                   (dolist (abox
                            thematic-substrate::*changes-for-aboxes-pending*)
                     (check-nrql-subscriptions abox))))))))))
    ((answer-sequence)
     (let ((*server-timeout* nil)
           (reasoner *cur-reasoner*)
           (*check-subscriptions-inhibited* t)
           (thematic-substrate::*changes-for-aboxes-pending* nil))
       (prog1
           (answer expr
                   state
                   stream
                   n
                   (let ((x nil))
                     (with-output-to-string (string-stream)
                       (setf x
                             (mapcar #'(lambda (expr1)
                                         (process-racer-expr expr1
                                                             *null-stream*
                                                             n
                                                             state
                                                             output-string-stream)
                                         (if (last-error reasoner)
                                             (error (last-error reasoner))
                                           (last-answer reasoner)))
                                     (rest expr))))
                     x)
                   output-string-stream)
         (let ((*check-subscriptions-inhibited* nil))
           (dolist (abox
                    thematic-substrate::*changes-for-aboxes-pending*)
             (check-nrql-subscriptions abox))))))
    ((|OWLAPI-answerSequence| owlapi-answer-sequence)
     (let ((*server-timeout* nil)
           (reasoner *cur-reasoner*)
           (*check-subscriptions-inhibited* t)
           (thematic-substrate::*changes-for-aboxes-pending* nil)
           (count 0))
       (prog1
           (with-reasoner ((second expr))
             (let ((*progress-value* 0))
               (with-progress-range ((length (cddr expr)) (0 100))
                 (answer expr
                         state
                         stream
                         n
                         (let ((x nil))
                           (with-output-to-string (string-stream)
                             (setf x
                                   (mapcar #'(lambda (expr1)
                                               (set-progress-value (incf count))
                                               (process-racer-expr expr1
                                                                   *null-stream*
                                                                   n
                                                                   state
                                                                   output-string-stream)
                                               (if (last-error reasoner)
                                                   (error (last-error reasoner))
                                                 (last-answer reasoner)))
                                           (cddr expr))))
                           x)
                         output-string-stream)
                 (let ((*check-subscriptions-inhibited* nil))
                   (dolist (abox
                            thematic-substrate::*changes-for-aboxes-pending*)
                     (check-nrql-subscriptions abox)))))))))
    ((quiet-sequence)
     (let ((*server-timeout* nil)
           (reasoner *cur-reasoner*)
           (*check-subscriptions-inhibited* t)
           (thematic-substrate::*changes-for-aboxes-pending* nil))
       (with-output-to-string (string-stream)
         (mapc #'(lambda (expr1)
                   (prog1
                       (process-racer-expr expr1
                                           *null-stream*
                                           n
                                           state
                                           output-string-stream)
                     (when (last-error reasoner)
                       (error (last-error reasoner)))))
               (rest expr)))
       (prog1
           (ok expr stream n state nil output-string-stream)
         (let ((*check-subscriptions-inhibited* nil))
           (dolist (abox
                    thematic-substrate::*changes-for-aboxes-pending*)
             (check-nrql-subscriptions abox))))))
    ((|OWLAPI-quietSequence| owlapi-quiet-sequence)
     (let ((*server-timeout* nil)
           (reasoner *cur-reasoner*)
           (*check-subscriptions-inhibited* t)
           (thematic-substrate::*changes-for-aboxes-pending* nil)
           (count 0))
       (with-reasoner ((second expr))
         (let ((*progress-value* 0))
           (with-progress-range ((length (cddr expr)) (0 100))
             (with-output-to-string (string-stream)
               (mapc #'(lambda (expr1)
                         (set-progress-value (incf count))
                         (prog1
                             (process-racer-expr expr1
                                                 *null-stream*
                                                 n
                                                 state
                                                 output-string-stream)
                           (when (last-error reasoner)
                             (error (last-error reasoner)))))
                     (cddr expr)))
             (prog1
                 (ok expr stream n state nil output-string-stream)
               (let ((*check-subscriptions-inhibited* nil))
                 (dolist (abox
                          thematic-substrate::*changes-for-aboxes-pending*)
                   (check-nrql-subscriptions abox)))))))))
    ((with-future-bindings-evaluated)
     (let ((*server-timeout* nil))
       (apply (symbol-function 'thematic-substrate::eval-with-future-bindings)
              (lambda ()
                (loop for expr1 in (cddr expr)
                      do (process-racer-expr expr1
                                             stream
                                             n
                                             state
                                             output-string-stream)))
              (second expr))))
    ((with-future-bindings)
     (let ((*server-timeout* nil))
       (apply (symbol-function 'thematic-substrate::eval-with-future-bindings)
              (lambda ()
                (loop for expr1 in (cddr expr)
                      do (process-racer-expr expr1
                                             stream
                                             n
                                             state
                                             output-string-stream)))
              (second expr))))
    ((with-bindings-evaluated)
     (let ((*server-timeout* nil))
       (apply (symbol-function 'thematic-substrate::eval-with-bindings)
              (lambda ()
                (loop for expr1 in (cddr expr)
                      do (process-racer-expr expr1
                                             stream
                                             n
                                             state
                                             output-string-stream)))
              (second expr))))
    ((with-bindings)
     (let ((*server-timeout* nil))
       (apply (symbol-function 'thematic-substrate::eval-with-bindings)
              (lambda ()
                (loop for expr1 in (cddr expr)
                      do (process-racer-expr expr1
                                             stream
                                             n
                                             state
                                             output-string-stream)))
              (second expr))))
    ((with-nrql-settings-evaluated)
     (let ((*server-timeout* nil))
       (apply (symbol-function 'thematic-substrate::eval-nrql-settings)
              (lambda ()
                (loop for expr1 in (cddr expr)
                      do (process-racer-expr expr1
                                             stream
                                             n
                                             state
                                             output-string-stream)))
              (second expr))))
    ((with-nrql-settings)
     (let ((*server-timeout* nil))
       (apply (symbol-function 'thematic-substrate::eval-nrql-settings)
              (lambda ()
                (loop for expr1 in (cddr expr)
                      do (process-racer-expr expr1
                                             stream
                                             n
                                             state
                                             output-string-stream)))
              (second expr))))
    ((add-explanation-assertions get-explanations
                                 get-number-of-explanations
                                 query-subscribers
                                 unsubscribe-from
                                 subscribe-to
                                 get-nodes-in-qbox-for-abox
                                 get-dag-of-qbox-for-abox
                                 show-qbox-for-abox
                                 query-equivalents
                                 query-descendants
                                 query-children
                                 query-ancestors
                                 query-parents
                                 query-equivalent-p
                                 query-entails-p
                                 classify-query
                                 rule-consistent-p
                                 query-consistent-p
                                 add-chosen-sets-of-rule-consequences
                                 get-chosen-sets-of-rule-consequences
                                 choose-current-set-of-rule-consequences
                                 execute-applicable-rules
                                 unapplicable-rules
                                 applicable-rules
                                 rule-unapplicable-p
                                 rule-applicable-p
                                 execute-or-reexecute-rule
                                 execute-or-reexecute-query
                                 reexecute-rule
                                 reexecute-query
                                 reprepare-rule
                                 reprepare-query
                                 rule-accurate-p
                                 query-accurate-p
                                 expensive-rule-p
                                 expensive-query-p
                                 cheap-rule-p
                                 cheap-query-p
                                 rule-terminated-p
                                 query-terminated-p
                                 rule-processed-p
                                 query-processed-p
                                 rule-sleeping-p
                                 query-sleeping-p
                                 rule-waiting-p
                                 query-waiting-p
                                 rule-running-p
                                 query-running-p
                                 rule-active-p
                                 query-active-p
                                 rule-prepared-p
                                 query-prepared-p
                                 rule-ready-p
                                 query-ready-p
                                 execute-rule
                                 get-all-remaining-sets-of-rule-consequences
                                 get-next-n-remaining-sets-of-rule-consequences
                                 get-next-set-of-rule-consequences
                                 next-set-of-rule-consequences-available-p
                                 get-current-set-of-rule-consequences
                                 get-all-remaining-tuples
                                 get-next-n-remaining-tuples
                                 get-next-tuple
                                 next-tuple-available-p
                                 get-current-tuple
                                 get-answer
                                 abort-rule
                                 abort-query
                                 execute-query
                                 delete-rule
                                 delete-query
                                 original-rule-antecedence
                                 original-query-body
                                 rule-antecedence
                                 query-body
                                 original-rule-consequence
                                 original-query-head
                                 rule-consequence
                                 query-head
                                 describe-rule
                                 describe-query
                                 describe-rule-status
                                 describe-query-status
                                 add-doc-phrase1
                                 add-doc-image-data1
                                 add-doc-image-data-from-file1
                                 add-doc-image-file1
                                 del-doc-entry1
                                 add-doc-entry1
                                 clear-all-documentation
                                 compute-abox-difference2
                                 compute-abox-difference1
                                 remove-implied-concept-assertions
                                 make-abduction-rule-from-aboxes
                                 make-backward-rule-from-aboxes
                                 make-forward-rule-from-aboxes
                                 make-query-from-abox
                                 create-subgraph-aboxes
                                 compute-subgraph-aboxes
                                 get-new-ind-prefix
                                 set-new-ind-prefix
                                 get-new-ind-counter
                                 set-new-ind-counter
                                 get-minimum
                                 get-maximum
                                 abox-entails-abox-p
                                 disable-abduction
                                 enable-abduction
                                 |OWLAPI-readOntology|
                                 |OWLAPI-saveOntology|
                                 |OWLAPI-writeXMLOntologyFile|
                                 |OWLAPI-writeFunctionalOntologyFile|
                                 |OWLAPI-writeOntologyFile|
                                 |OWLAPI-readXMLOntologyDocument|
                                 |OWLAPI-readXMLOntologyFile|
                                 |OWLAPI-readFunctionalOntologyDocument|
                                 |OWLAPI-readFunctionalOntologyFile|
                                 |OWLAPI-parseNative|
                                 |OWLAPI-parse|
                                 update-racer
                                 check-for-updates
                                 load-racer-plugins
                                 load-racer-patches
                                 installed-plugins
                                 installed-patches
                                 load-racer-plugin
                                 load-racer-patch
                                 make-plugin-from-fasl-file
                                 swrl-create-forward-chainging-rules
                                 swrl-create-abduction-rules-if-possible
                                 get-prefixes
                                 delete-prefix-mappings
                                 |OWLAPI-restoreImage|
                                 |OWLAPI-storeImage|
                                 owlapi-add-axiom
                                 owlapi-add-axioms
                                 owlapi-axiom-loaded?
                                 owlapi-axiom-to-id
                                 owlapi-id-to-axiom
                                 owlapi-remove-axiom
                                 owlapi-remove-axioms
                                 owlapi-set-ontology-uri
                                 owlapi-abort
                                 owlapi-add-prefix
                                 owlapi-advance-progress
                                 owlapi-apply-changes
                                 owlapi-auto-add-axioms-to
                                 owlapi-auto-apply-changes
                                 owlapi-auto-batch-add-axioms-to
                                 owlapi-auto-batch-remove-axioms-from
                                 owlapi-auto-remove-axioms-from
                                 owlapi-batch-synchronize
                                 owlapi-classify
                                 owlapi-clear-changes
                                 owlapi-clear-ontologies
                                 owlapi-clear-registry
                                 owlapi-consider-declarations
                                 owlapi-contains
                                 owlapi-describe-ontologies
                                 owlapi-describe-ontology
                                 owlapi-describe-reasoner
                                 owlapi-describe-reasoners
                                 owlapi-disable-auto-mode
                                 owlapi-disable-incremental-updates
                                 owlapi-disable-lookup-mode
                                 owlapi-disable-memory-saving-mode
                                 owlapi-disable-simplified-protocol
                                 owlapi-disable-transient-axiom-mode
                                 owlapi-dispose
                                 owlapi-dispose-axiom
                                 owlapi-dispose-axioms
                                 owlapi-dispose-ontologies
                                 owlapi-dispose-ontology
                                 owlapi-dispose-reasoner
                                 owlapi-dont-register-declared-entities
                                 owlapi-dont-register-referenced-entities
                                 owlapi-enable-incremental-updates
                                 owlapi-enable-lookup-mode
                                 owlapi-enable-memory-saving-mode
                                 owlapi-enable-simplified-protocol
                                 owlapi-enable-transient-axiom-mode
                                 owlapi-export-ontology
                                 owlapi-export-reasoner
                                 owlapi-find-id-from-object
                                 owlapi-find-object-from-id
                                 owlapi-get-all-ontologies
                                 owlapi-get-ancestor-classes
                                 owlapi-get-ancestor-properties
                                 owlapi-get-annotation-axioms-for-axiom
                                 owlapi-get-auto-declare-data-properties
                                 owlapi-get-auto-ontology
                                 owlapi-get-axiom-counter
                                 owlapi-get-axioms
                                 owlapi-get-axioms-in
                                 owlapi-get-axioms-of-type
                                 owlapi-get-axioms-of-type-in
                                 owlapi-get-axioms-per-ontology
                                 owlapi-get-changes
                                 owlapi-get-current-reasoner
                                 owlapi-get-data-property-relationships
                                 owlapi-get-data-property-values
                                 owlapi-get-descendant-classes
                                 owlapi-get-descendant-properties
                                 owlapi-get-different-individuals
                                 owlapi-get-disjoint-classes
                                 owlapi-get-disjoint-data-properties
                                 owlapi-get-disjoint-object-properties
                                 owlapi-get-domains
                                 owlapi-get-equivalent-classes
                                 owlapi-get-equivalent-properties
                                 owlapi-get-inconsistent-classes
                                 owlapi-get-individuals
                                 owlapi-get-instances
                                 owlapi-get-inverse-properties
                                 owlapi-get-loaded-ontologies
                                 owlapi-get-owl-annotation-assertion-axiom
                                 owlapi-get-owl-annotation-property-domain-axiom
                                 owlapi-get-owl-annotation-property-range-axiom
                                 owlapi-get-owl-asymmetric-object-property-axiom
                                 owlapi-get-owl-axiom-annotation-axiom
                                 owlapi-get-owl-class-assertion-axiom
                                 owlapi-get-owl-data-property-assertion-axiom
                                 owlapi-get-owl-data-property-domain-axiom
                                 owlapi-get-owl-data-property-range-axiom
                                 owlapi-get-owl-data-sub-property-axiom
                                 owlapi-get-owl-datatype-definition-axiom
                                 owlapi-get-owl-declaration-axiom
                                 owlapi-get-owl-different-individuals-axiom
                                 owlapi-get-owl-disjoint-classes-axiom
                                 owlapi-get-owl-disjoint-data-properties-axiom
                                 owlapi-get-owl-disjoint-object-properties-axiom
                                 owlapi-get-owl-disjoint-union-axiom
                                 owlapi-get-owl-entity-annotation-axiom
                                 owlapi-get-owl-equivalent-classes-axiom
                                 owlapi-get-owl-equivalent-data-properties-axiom
                                 owlapi-get-owl-equivalent-object-properties-axiom
                                 owlapi-get-owl-functional-data-property-axiom
                                 owlapi-get-owl-functional-object-property-axiom
                                 owlapi-get-owl-has-key-axiom
                                 owlapi-get-owl-implicit-declaration-axiom
                                 owlapi-get-owl-imports-declaration-axiom
                                 owlapi-get-owl-inverse-functional-object-property-axiom
                                 owlapi-get-owl-inverse-object-properties-axiom
                                 owlapi-get-owl-irreflexive-object-property-axiom
                                 owlapi-get-owl-negative-data-property-assertion-axiom
                                 owlapi-get-owl-negative-object-property-assertion-axiom
                                 owlapi-get-owl-object-property-assertion-axiom
                                 owlapi-get-owl-object-property-chain-sub-property-axiom
                                 owlapi-get-owl-object-property-domain-axiom
                                 owlapi-get-owl-object-property-range-axiom
                                 owlapi-get-owl-object-sub-property-axiom
                                 owlapi-get-owl-ontology-annotation-axiom
                                 owlapi-get-owl-ontology-version-declaration-axiom
                                 owlapi-get-owl-prefix-declaration-axiom
                                 owlapi-get-owl-really-implicit-declaration-axiom
                                 owlapi-get-owl-reflexive-object-property-axiom
                                 owlapi-get-owl-same-individuals-axiom
                                 owlapi-get-owl-sub-annotation-property-axiom
                                 owlapi-get-owl-sub-annotation-property-of-axiom
                                 owlapi-get-owl-sub-class-axiom
                                 owlapi-get-owl-symmetric-object-property-axiom
                                 owlapi-get-owl-transitive-object-property-axiom
                                 owlapi-get-object-property-relationships
                                 owlapi-get-object-property-values
                                 owlapi-get-ontologies
                                 owlapi-get-prefixes
                                 owlapi-get-ranges
                                 owlapi-get-reasoners
                                 owlapi-get-related-individuals
                                 owlapi-get-related-values
                                 owlapi-get-same-individuals
                                 owlapi-get-sub-classes
                                 owlapi-get-sub-properties
                                 owlapi-get-super-classes
                                 owlapi-get-super-properties
                                 owlapi-get-types
                                 owlapi-has-data-property-relationship
                                 owlapi-has-object-property-relationship
                                 owlapi-has-type
                                 owlapi-ignore-annotations
                                 owlapi-ignore-declarations
                                 owlapi-init
                                 owlapi-is-asymmetric
                                 owlapi-is-class
                                 owlapi-is-classified
                                 owlapi-is-consistent
                                 owlapi-is-defined-class
                                 owlapi-is-defined-data-property
                                 owlapi-is-defined-individual
                                 owlapi-is-defined-object-property
                                 owlapi-is-different-individual
                                 owlapi-is-entailed
                                 owlapi-is-equivalent-class
                                 owlapi-is-functional
                                 owlapi-is-inverse-functional
                                 owlapi-is-irreflexive
                                 owlapi-is-realised
                                 owlapi-is-reflexive
                                 owlapi-is-same-individual
                                 owlapi-is-satisfiable
                                 owlapi-is-sub-class-of
                                 owlapi-is-symmetric
                                 owlapi-is-transitive
                                 owlapi-keep-annotations
                                 owlapi-load-axiom
                                 owlapi-load-axioms
                                 owlapi-load-ontologies
                                 owlapi-load-ontology
                                 owlapi-manually-apply-changes
                                 owlapi-merge-ontologies
                                 owlapi-new-ontology
                                 owlapi-new-reasoner
                                 owlapi-new-reasoner1
                                 owlapi-next-axiom-use-id
                                 owlapi-parse
                                 owlapi-parse-native
                                 owlapi-read-functional-ontology-document
                                 owlapi-read-functional-ontology-file
                                 owlapi-read-ontology
                                 owlapi-read-xml-ontology-document
                                 owlapi-read-xml-ontology-file
                                 owlapi-realize
                                 owlapi-register-declared-entities
                                 owlapi-register-last-answer
                                 owlapi-register-object
                                 owlapi-register-referenced-entities
                                 owlapi-reload-loaded-ontologies
                                 owlapi-remove-prefix
                                 owlapi-reset-axiom-counter
                                 owlapi-restore-image
                                 owlapi-save-ontology
                                 owlapi-set-auto-declare-data-properties
                                 owlapi-set-axiom-counter
                                 owlapi-set-current-reasoner
                                 owlapi-set-progress
                                 owlapi-set-progress-range
                                 owlapi-set-return-policy
                                 owlapi-sleep
                                 owlapi-store-image
                                 owlapi-unload-axiom
                                 owlapi-unload-axioms
                                 owlapi-unload-ontologies
                                 owlapi-unload-ontology
                                 owlapi-uses-incremental-updates
                                 owlapi-uses-simplified-protocol
                                 owlapi-write-functional-ontology-file
                                 owlapi-write-ontology-file
                                 owlapi-write-xml-ontology-file
                                 |OWLAPI-exportReasoner|
                                 |OWLAPI-exportOntology|
                                 |OWLAPI-SetOntologyURI|
                                 |OWLAPI-RemoveAxioms|
                                 |OWLAPI-removeAxioms|
                                 |OWLAPI-RemoveAxiom|
                                 |OWLAPI-removeAxiom|
                                 |OWLAPI-AddAxioms|
                                 |OWLAPI-addAxioms|
                                 |OWLAPI-AddAxiom|
                                 |OWLAPI-addAxiom|
                                 |OWLAPI-applyChanges|
                                 |OWLAPI-manuallyApplyChanges|
                                 |OWLAPI-autoApplyChanges|
                                 |OWLAPI-getChanges|
                                 |OWLAPI-clearChanges|
                                 |OWLAPI-getOWLObjectPropertyRangeAxiom|
                                 |OWLAPI-getOWLDataPropertyRangeAxiom|
                                 |OWLAPI-getOWLObjectPropertyDomainAxiom|
                                 |OWLAPI-getOWLDataPropertyDomainAxiom|
                                 |OWLAPI-getOWLDataSubPropertyAxiom|
                                 |OWLAPI-getOWLObjectSubPropertyAxiom|
                                 |OWLAPI-getOWLTransitiveObjectPropertyAxiom|
                                 |OWLAPI-getOWLSymmetricObjectPropertyAxiom|
                                 |OWLAPI-getOWLReflexiveObjectPropertyAxiom|
                                 |OWLAPI-getOWLIrreflexiveObjectPropertyAxiom|
                                 |OWLAPI-getOWLInverseFunctionalObjectPropertyAxiom|
                                 |OWLAPI-getOWLAsymmetricObjectPropertyAxiom|
                                 |OWLAPI-getOWLObjectPropertyChainSubPropertyAxiom|
                                 |OWLAPI-getOWLInverseObjectPropertiesAxiom|
                                 |OWLAPI-getOWLEquivalentObjectPropertiesAxiom|
                                 |OWLAPI-getOWLEquivalentDataPropertiesAxiom|
                                 |OWLAPI-getOWLDisjointObjectPropertiesAxiom|
                                 |OWLAPI-getOWLDisjointDataPropertiesAxiom|
                                 |OWLAPI-getOWLFunctionalObjectPropertyAxiom|
                                 |OWLAPI-getOWLFunctionalDataPropertyAxiom|
                                 |OWLAPI-getOWLNegativeObjectPropertyAssertionAxiom|
                                 |OWLAPI-getOWLObjectPropertyAssertionAxiom|
                                 |OWLAPI-getOWLNegativeDataPropertyAssertionAxiom|
                                 |OWLAPI-getOWLDataPropertyAssertionAxiom|
                                 |OWLAPI-getOWLSameIndividualsAxiom|
                                 |OWLAPI-getOWLDifferentIndividualsAxiom|
                                 |OWLAPI-getOWLClassAssertionAxiom|
                                 |OWLAPI-getOWLSubClassAxiom|
                                 |OWLAPI-getOWLEquivalentClassesAxiom|
                                 |OWLAPI-getOWLDisjointUnionAxiom|
                                 |OWLAPI-getOWLDisjointClassesAxiom|
                                 |OWLAPI-getOWLPrefixDeclarationAxiom|
                                 |OWLAPI-getOWLOntologyVersionDeclarationAxiom|
                                 |OWLAPI-getOWLImportsDeclarationAxiom|
                                 |OWLAPI-getOWLReallyImplicitDeclarationAxiom|
                                 |OWLAPI-getOWLImplicitDeclarationAxiom|
                                 |OWLAPI-getOWLDeclarationAxiom|
                                 |OWLAPI-getOWLAnnotationPropertyRangeAxiom|
                                 |OWLAPI-getOWLAnnotationPropertyDomainAxiom|
                                 |OWLAPI-getOWLSubAnnotationPropertyAxiom|
                                 |OWLAPI-getOWLSubAnnotationPropertyOfAxiom|
                                 |OWLAPI-getOWLAnnotationAssertionAxiom|
                                 |OWLAPI-getOWLOntologyAnnotationAxiom|
                                 |OWLAPI-getOWLEntityAnnotationAxiom|
                                 |OWLAPI-getOWLAxiomAnnotationAxiom|
                                 |OWLAPI-getOWLHasKeyAxiom|
                                 |OWLAPI-getOWLDatatypeDefinitionAxiom|
                                 |OWLAPI-getPrefixes|
                                 |OWLAPI-removePrefix|
                                 |OWLAPI-addPrefix|
                                 |OWLAPI-getAnnotationAxiomsForAxiom|
                                 |OWLAPI-getAxiomsOfTypeIn|
                                 |OWLAPI-getAxiomsOfType|
                                 |OWLAPI-getAxiomsIn|
                                 |OWLAPI-getAxiomsPerOntology|
                                 |OWLAPI-getAxioms|
                                 |OWLAPI-AxiomLoaded?|
                                 |OWLAPI-AxiomToID|
                                 |OWLAPI-IDToAxiom|
                                 |OWLAPI-disposeAxioms|
                                 |OWLAPI-disposeAxiom|
                                 |OWLAPI-unloadAxioms|
                                 |OWLAPI-unloadAxiom|
                                 |OWLAPI-loadAxioms|
                                 |OWLAPI-loadAxiom|
                                 |OWLAPI-resetAxiomCounter|
                                 |OWLAPI-setAxiomCounter|
                                 |OWLAPI-getAxiomCounter|
                                 |OWLAPI-isSatisfiable|
                                 |OWLAPI-dispose|
                                 |OWLAPI-clearOntologies|
                                 |OWLAPI-unloadOntology|
                                 |OWLAPI-unloadOntologies|
                                 |OWLAPI-isDefinedIndividual|
                                 |OWLAPI-isDefinedDataProperty|
                                 |OWLAPI-isDefinedObjectProperty|
                                 |OWLAPI-isDefinedClass|
                                 |OWLAPI-realize|
                                 |OWLAPI-isRealised|
                                 |OWLAPI-classify|
                                 |OWLAPI-isClassified|
                                 |OWLAPI-considerDeclarations|
                                 |OWLAPI-ignoreDeclarations|
                                 |OWLAPI-keepAnnotations|
                                 |OWLAPI-ignoreAnnotations|
                                 |OWLAPI-disableMemorySavingMode|
                                 |OWLAPI-enableMemorySavingMode|
                                 |OWLAPI-batchSynchronize|
                                 |OWLAPI-isEntailed|
                                 |OWLAPI-disableTransientAxiomMode|
                                 |OWLAPI-enableTransientAxiomMode|
                                 |OWLAPI-disableLookupMode|
                                 |OWLAPI-enableLookupMode|
                                 |OWLAPI-disableAutoMode|
                                 |OWLAPI-autoBatchRemoveAxiomsFrom|
                                 |OWLAPI-autoRemoveAxiomsFrom|
                                 |OWLAPI-autoBatchAddAxiomsTo|
                                 |OWLAPI-autoAddAxiomsTo|
                                 |OWLAPI-getAutoOntology|
                                 |OWLAPI-getAutoDeclareDataProperties|
                                 |OWLAPI-setAutoDeclareDataProperties|
                                 |OWLAPI-reloadLoadedOntologies|
                                 |OWLAPI-disposeOntologies|
                                 |OWLAPI-loadOntologies|
                                 |OWLAPI-getAllOntologies|
                                 |OWLAPI-contains|
                                 |OWLAPI-getLoadedOntologies|
                                 |OWLAPI-getOntologies|
                                 |OWLAPI-loadOntology|
                                 |OWLAPI-disposeOntology|
                                 |OWLAPI-mergeOntologies|
                                 |OWLAPI-newOntology|
                                 |OWLAPI-describeOntologies|
                                 |OWLAPI-describeOntology|
                                 |OWLAPI-describeReasoners|
                                 |OWLAPI-describeReasoner|
                                 |OWLAPI-isAsymmetric|
                                 |OWLAPI-isIrreflexive|
                                 |OWLAPI-isReflexive|
                                 |OWLAPI-isTransitive|
                                 |OWLAPI-isSymmetric|
                                 |OWLAPI-isInverseFunctional|
                                 |OWLAPI-getInverseProperties|
                                 |OWLAPI-getDisjointDataProperties|
                                 |OWLAPI-getDisjointObjectProperties|
                                 |OWLAPI-getDisjointClasses|
                                 |OWLAPI-isFunctional|
                                 |OWLAPI-getRanges|
                                 |OWLAPI-getDomains|
                                 |OWLAPI-getEquivalentProperties|
                                 |OWLAPI-getDescendantProperties|
                                 |OWLAPI-getAncestorProperties|
                                 |OWLAPI-getSubProperties|
                                 |OWLAPI-getSuperProperties|
                                 |OWLAPI-getDifferentIndividuals|
                                 |OWLAPI-getDataPropertyValues|
                                 |OWLAPI-getObjectPropertyValues|
                                 |OWLAPI-getInstances|
                                 |OWLAPI-isDifferentIndividual|
                                 |OWLAPI-isSameIndividual|
                                 |OWLAPI-getSameIndividuals|
                                 |OWLAPI-getRelatedValues|
                                 |OWLAPI-getRelatedIndividuals|
                                 |OWLAPI-hasDataPropertyRelationship|
                                 |OWLAPI-hasObjectPropertyRelationship|
                                 |OWLAPI-hasType|
                                 |OWLAPI-getDataPropertyRelationships|
                                 |OWLAPI-getObjectPropertyRelationships|
                                 |OWLAPI-getIndividuals|
                                 |OWLAPI-getTypes|
                                 |OWLAPI-isConsistent|
                                 |OWLAPI-getInconsistentClasses|
                                 |OWLAPI-getEquivalentClasses|
                                 |OWLAPI-getDescendantClasses|
                                 |OWLAPI-getSubClasses|
                                 |OWLAPI-getAncestorClasses|
                                 |OWLAPI-getSuperClasses|
                                 |OWLAPI-isEquivalentClass|
                                 |OWLAPI-isSubClassOf|
                                 |OWLAPI-isClass|
                                 |OWLAPI-findIDFromObject|
                                 |OWLAPI-findObjectFromID|
                                 |OWLAPI-registerObject|
                                 |OWLAPI-registerLastAnswer|
                                 |OWLAPI-clearRegistry|
                                 |OWLAPI-setReturnPolicy|
                                 |OWLAPI-nextAxiomUseID|
                                 |OWLAPI-dontRegisterDeclaredEntities|
                                 |OWLAPI-registerDeclaredEntities|
                                 |OWLAPI-dontRegisterReferencedEntities|
                                 |OWLAPI-registerReferencedEntities|
                                 |OWLAPI-usesIncrementalUpdates|
                                 |OWLAPI-disableIncrementalUpdates|
                                 |OWLAPI-enableIncrementalUpdates|
                                 |OWLAPI-usesSimplifiedProtocol|
                                 |OWLAPI-disableSimplifiedProtocol|
                                 |OWLAPI-enableSimplifiedProtocol|
                                 |OWLAPI-advanceProgress|
                                 |OWLAPI-setProgress|
                                 |OWLAPI-setProgressRange|
                                 |OWLAPI-init|
                                 |OWLAPI-abort|
                                 |OWLAPI-sleep|
                                 |OWLAPI-getReasoners|
                                 |OWLAPI-getCurrentReasoner|
                                 |OWLAPI-setCurrentReasoner|
                                 |OWLAPI-disposeReasoner|
                                 |OWLAPI-newReasoner|
                                 |OWLAPI-newReasoner1|
                                 get-agraph-version
                                 get-nrql-version
                                 restore-server-image
                                 store-server-image
                                 restore-all-substrates
                                 restore-substrate
                                 store-all-substrates
                                 store-substrate-for-abox
                                 check-nrql-subscriptions
                                 disable-rcc-substrate-mirroring
                                 enable-rcc-substrate-mirroring
                                 del-rcc-edge1
                                 del-rcc-node1
                                 rcc-edge-description1
                                 rcc-node-description1
                                 rcc-edge-label1
                                 rcc-node-label1
                                 rcc-edge1
                                 rcc-node1
                                 rcc-related1
                                 rcc-instance1
                                 delete-rcc-synonyms
                                 register-rcc-synonym
                                 rcc-consistent-p
                                 create-rcc-edge
                                 create-rcc-node
                                 set-rcc-box
                                 set-mirror-data-box
                                 get-edge-label-for-non-existent-edges
                                 set-edge-label-for-non-existent-edges
                                 description-implies-p
                                 edge-description1
                                 node-description1
                                 edge-label1
                                 node-label1
                                 del-data-edge1
                                 del-data-node1
                                 data-edge1
                                 data-node1
                                 set-data-box
                                 get-data-edge-description
                                 get-data-edge-label
                                 get-data-node-description
                                 get-data-node-label
                                 delete-data-edge
                                 delete-data-node
                                 create-data-edge
                                 create-data-node
                                 describe-all-edges
                                 get-substrate-edges
                                 describe-all-nodes
                                 get-substrate-nodes
                                 describe-all-substrates
                                 describe-substrate
                                 rmi
                                 get-proxy-server
                                 set-proxy-server
                                 check-ontology
                                 check-concept-coherence
                                 restore-standard-settings
                                 set-nrql-mode
                                 disable-query-realization
                                 enable-query-realization
                                 disable-query-repository
                                 enable-query-repository
                                 dont-report-inconsistent-queries-and-rules
                                 report-inconsistent-queries-and-rules
                                 describe-query-processing-mode
                                 describe-current-substrate
                                 get-process-pool-size
                                 get-maximum-size-of-process-pool
                                 get-initial-size-of-process-pool
                                 set-maximum-size-of-process-pool
                                 set-initial-size-of-process-pool
                                 set-rewrite-defined-concepts
                                 optimizer-dont-ensure-late-lambda-evaluation
                                 optimizer-ensure-late-lambda-evaluation
                                 optimizer-dont-use-cardinality-heuristics
                                 optimizer-use-cardinality-heuristics
                                 optimizer-get-time-bound
                                 optimizer-set-time-bound
                                 optimizer-get-no-of-plans-upper-bound
                                 optimizer-set-no-of-plans-upper-bound
                                 disable-query-optimization
                                 enable-query-optimization
                                 include-permutations
                                 exclude-permutations
                                 dont-add-rule-consequences-automatically
                                 add-rule-consequences-automatically
                                 process-set-at-a-time
                                 process-tuple-at-a-time
                                 get-max-no-of-tuples-bound
                                 set-max-no-of-tuples-bound
                                 dont-check-abox-consistency-before-querying
                                 check-abox-consistency-before-querying
                                 enable-lazy-tuple-computation
                                 enable-eager-tuple-computation
                                 dont-add-role-assertions-for-datatype-properties
                                 add-role-assertions-for-datatype-properties
                                 disable-told-information-querying
                                 enable-told-information-querying
                                 disable-nrql-warnings
                                 enable-nrql-warnings
                                 disable-kb-has-changed-warning-tokens
                                 enable-kb-has-changed-warning-tokens
                                 disable-phase-two-starts-warning-tokens
                                 enable-phase-two-starts-warning-tokens
                                 disable-two-phase-query-processing-mode
                                 enable-two-phase-query-processing-mode
                                 enable-very-smart-abox-mirroring
                                 enable-smart-abox-mirroring
                                 disable-abox-mirroring
                                 enable-abox-mirroring
                                 dont-use-injective-variables-by-default
                                 use-injective-variables-by-default
                                 dont-use-individual-synonym-equivalence-classes
                                 use-individual-synonym-equivalence-classes
                                 dont-add-missing-top-conjuncts
                                 add-missing-top-conjuncts
                                 disable-data-substrate-mirroring
                                 enable-data-substrate-mirroring
                                 wait-for-rules-to-terminate
                                 wait-for-queries-to-terminate
                                 get-all-answers
                                 execute-or-reexecute-all-rules
                                 run-all-rules
                                 reexecute-all-rules
                                 execute-all-rules
                                 execute-or-reexecute-all-queries
                                 reexecute-all-queries
                                 run-all-queries
                                 execute-all-queries
                                 abort-all-rules
                                 abort-all-queries
                                 describe-all-definitions
                                 describe-definition
                                 delete-all-definitions
                                 activate-defined-query
                                 deactivate-defined-query
                                 dont-keep-defined-query-atoms
                                 keep-defined-query-atoms
                                 enable-lazy-unfolding-of-defined-queries
                                 disable-lazy-unfolding-of-defined-queries
                                 enable-defined-queries
                                 disable-defined-queries
                                 dont-prefer-defined-queries
                                 prefer-defined-queries
                                 dont-allow-overloaded-definitions
                                 allow-overloaded-definitions
                                 undefine-query
                                 define-and-prepare-query
                                 define-and-execute-query
                                 define-query
                                 inaccurate-rules
                                 accurate-rules
                                 inaccurate-queries
                                 accurate-queries
                                 sleeping-expensive-rules
                                 sleeping-cheap-rules
                                 waiting-expensive-rules
                                 waiting-cheap-rules
                                 sleeping-expensive-queries
                                 sleeping-cheap-queries
                                 waiting-expensive-queries
                                 waiting-cheap-queries
                                 running-expensive-rules
                                 running-cheap-rules
                                 running-expensive-queries
                                 running-cheap-queries
                                 expensive-rules
                                 cheap-rules
                                 expensive-queries
                                 cheap-queries
                                 terminated-rules
                                 processed-rules
                                 terminated-queries
                                 processed-queries
                                 sleeping-rules
                                 waiting-rules
                                 sleeping-queries
                                 waiting-queries
                                 running-rules
                                 running-queries
                                 active-rules
                                 active-queries
                                 prepared-rules
                                 ready-rules
                                 prepared-queries
                                 ready-queries
                                 answer-tbox-query1
                                 answer-tbox-query
                                 racer-answer-tbox-query1
                                 racer-answer-tbox-query
                                 racer-prepare-tbox-query1
                                 racer-prepare-tbox-query
                                 move-rules
                                 copy-rules
                                 apply-rule-under-premise1
                                 apply-rule-under-premise
                                 apply-rule
                                 racer-apply-rule-under-premise1
                                 racer-apply-rule1
                                 racer-apply-rule-under-premise
                                 racer-apply-rule
                                 prepare-rule1
                                 prepare-rule
                                 racer-prepare-rule1
                                 racer-prepare-rule
                                 answer-query-under-premise1
                                 answer-query-under-premise
                                 racer-answer-query-under-premise1
                                 racer-answer-query-under-premise
                                 answer-query1
                                 answer-query
                                 racer-answer-query1
                                 racer-answer-query
                                 get-answer-size
                                 prepare-query1
                                 prepare-query
                                 racer-prepare-query1
                                 racer-prepare-query
                                 delete-all-rules
                                 delete-all-queries
                                 describe-all-rules
                                 describe-all-queries
                                 all-rules
                                 all-queries
                                 all-different-from-assertions
                                 all-same-as-assertions
                                 get-abox-graph
                                 get-role-hierarchy
                                 get-concept-properties
                                 get-individual-annotation-fillers
                                 get-individual-annotation-datatype-fillers
                                 get-individual-datatype-fillers
                                 get-individual-successors
                                 prepare-nrql-engine
                                 in-unsafe-mode?
                                 exit-server
                                 full-reset
                                 reset-nrql-engine
                                 all-substrates
                                 delete-all-substrates
                                 get-substrate-type
                                 set-substrate-type
                                 reset-all-substrates
                                 server-value
                                 get-all-server-values
                                 get-all-values
                                 unbind-all
                                 unbind1
                                 defpar1
                                 defcon1
                                 server-function
                                 get-all-server-functions
                                 get-all-functions
                                 fcall
                                 undefine-all
                                 undefine1
                                 define1
                                 evaluate1
                                 set-racer-parameter)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function (first expr)) (rest expr)))
               output-string-stream)))
    (otherwise
     (cond ((minilisp-server-function-p (first expr))
            (answer expr
                    state
                    stream
                    n
                    (apply #'thematic-substrate::call-function expr)
                    output-string-stream))
           (t
            (let ((res :hook-not-found))
              (loop as hook in thematic-substrate:*server-hooks*
                    do (let ((res1 (funcall hook expr)))
                         (unless (eq res1 :hook-not-found)
                           (setf res res1)
                           (return))))
              (case res
                (:hook-not-found (error "Illegal operator in ~A" expr))
                (otherwise
                 (answer expr
                         state
                         stream
                         n
                         res
                         output-string-stream)))))))))

(defun server-patch-hook (expr)
  (declare (ignorable expr))
  :hook-not-found)

(thematic-substrate:server-hook 'server-patch-hook)
