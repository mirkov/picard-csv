(in-package :csv-parser)

(export '(next-csv-record))

(define-test next-csv-record
  (with-input-from-string (stream "1, 2, 3")
    (multiple-value-bind (values count)
	(next-csv-record stream t 0)
      (assert-equal
       '("1" "2" "3") values)
      (assert-numerical-equal 3 count)))
  (with-input-from-string (stream "1, 2, 3")
    (multiple-value-bind (values count)
	(next-csv-record stream :read-from-string 0 :type 'double-float)
      (assert-equal
       '(1d0 2d0 3d0) values)
      (assert-numerical-equal 3 count)))
  (with-input-from-string (stream "1, 2, 3")
    (multiple-value-bind (values count)
	(next-csv-record stream :read-from-string 0
			 :type 'double-float)
      (assert-equal
       '(1d0 2d0 3d0) values)
      (assert-numerical-equal 3 count)))
  (with-input-from-string (stream "1, 2, 3")
    (multiple-value-bind (values count)
	(next-csv-record stream
			 #'(lambda (value column row)
			     (+ (expt (read-from-string
				       value)
				      column)
				(/ row 10.0)))
			 1)
      (assert-equal
       '(1.1 2.1 9.1) values)
      (assert-numerical-equal 3 count))))

(define-test next-csv-record--incomplete
  (with-input-from-string (stream "1, 2, 3")
    (multiple-value-bind (values count)
	(next-csv-record stream t 0 :length 2)
      (assert-equal '("1" "2") values)
      (assert-equal 2 count)))
  (with-input-from-string (stream "1, 2,")
    (multiple-value-bind (values count)
	(next-csv-record stream t 0 :length 3
			 :missing-field-value "8")
      (assert-equal '("1" "2" "8") values )
      (assert-equal 3 count)))
  (with-input-from-string (stream "1,, 3")
    (multiple-value-bind (values count)
	(next-csv-record stream t 0 :missing-field-value "8")
      (assert-equal '("1" "8" "3") values)
      (assert-equal 3 count))))


(defmacro read&process-next-record (&optional process)
  "Expand into code that will return the next record, properly
cleaned-up and processed.

Cleaning up is identical in all the methods of next-csv-record.
It involves:
 - Checking the length and signaling an error if so specified
 - Fixing the length
 - Fixing the missing elements

The processing is specified by the `process' argument on the fields%%
variable.

The final step is to return the cleaned-up record, and the number of
values it stores.

See the comments in the code for the cleanup steps.
"
  `(multiple-value-bind (fields count)
       (csv-parser:read-csv-line stream)
     ;; Signal error if insufficient length and eor-error-p/
     (when (and length
		eor-error-p
		(< count length))
       (error "Row ~a is too short" row))
     ;; Start of cleanup and processing. I name the list as fields,
     ;; fields% fields%% and fields%%% during various steps
     (let ((fields%
	    ;; Adjust the record length, filling it with nil's if need
	    ;; be
	    (if length
		(if (= count length)
		    fields
		    (adjust-field-count fields count length))
		fields)))
       (let ((fields%%
	      ;; replace nil's with missing-field-value
	      (if missing-field-value
		  (replace-nils fields% missing-field-value row)
		  fields%)))
	 ,(if process
	      ;; if process is provided, insert code here and execute
	      ;; return
	      `(let ((fields%%% ,process))
		 (if length
		     (values fields%%% length)
		     (values fields%%% count)))
	      ;; return if process is not provided.
	      `(values fields%%
		       (if length length count)))))))

(defgeneric next-csv-record (stream key row &key type length
				    eor-error-p missing-field-value)
  (:documentation
   "Read next record from csv-file bound to `stream' and return the
record contents as a list.

The second returned value is the number of found records.

Keyword `length' can be used to set the list length.  If the number of
fields in the record exceeds length, the return list is truncated.  If
it is shorter than length, the rest of the list is populated by the
`missing-field-value' (default nil).

We use csv-parser:read-csv-line from picard-csv to read csv records.
This function returns the record contents as a list of strings, NILs and
is optionally terminated by a nil.  The NIL's are converted to the
`missing-field value'.

`eor-error-p' (default t) and `missing-field-value' control behavior
if a record has fewer than the required number of fields.

If eor-error-p is t (default), an error is signaled.  If it is nil,
`missing-field-value' is stored in that cell.

`missing-field-value' can be an atom or a function.

If `missing-field-value' is a function, the function is called on the
column index and the row index of the missing cell, and its return
value is stored.

In case that parameter `key' is :read-from-string (see below) the
`missing-field-value' should be (or return) a string, as it will be
read using `read-from-string'.  It can be set to an empty string, in
which case, the returned value will be nil.

Parameters `key' and `type' can be used to convert the string to a
desired type.  NIL fields are left uncoverted.

Depending on the value of `key' the record strings can be extracted
and post-processed as follows:
 - key (eql t) returns the string
 - key (eql :read-from-string) applies `read-from-string' to the string
 - key can be a two-argument function (string and column number) to 
   process the string

In case of `key' being :read-from-string, type can be used to coerce
the result of read string to a desired type")
  (:method ((stream stream) (key (eql t)) row &key type length
	    (eor-error-p t) (missing-field-value nil))
    (declare (ignore type))
    (read&process-next-record))
  (:method ((stream stream) (key (eql :read-from-string)) row
	    &key type length (eor-error-p t) (missing-field-value "nil"))
    (read&process-next-record
     (mapcar #'(lambda (string)
		 (coerce (read-from-string string)
			 type))
	     fields%%)))
  (:method ((stream stream) (key function) row
	    &key type length
	    (eor-error-p t) missing-field-value)
    (declare (ignore type))
    (read&process-next-record
     (loop for string in fields%%
	for column upfrom 0
	collect (funcall key string column row)))))


(define-test adjust-field-count
    (assert-equal
     '(1 2 3 nil 5) (adjust-field-count '(1 2 3 nil 5) 5 5))
    (assert-equal
     '(1 2 3 nil) (adjust-field-count '(1 2 3 nil 5) 5  4))
    (assert-equal
     '(1 2 3 nil 5 nil nil) (adjust-field-count '(1 2 3 nil 5) 5 7)))


  
(defun adjust-field-count (fields current-length new-length)
  "Adjust length of list fields to new-length.  Its original length is
old-length."
  (cond
    ((< current-length new-length)
     (append fields (make-list (- new-length current-length)
			       :initial-element nil)))
    ((> current-length new-length)
     (subseq fields 0 new-length))
    (t fields)))



(define-test replace-nils
  (assert-numerical-equal
   '(1 2 3 4)
   (replace-nils (list 1 2 nil 4) 3))
  (assert-numerical-equal
   '(1 2 4 4)
   (replace-nils (list 1 2 nil 4) #'(lambda (column)
				      (expt column 2))))
  (assert-numerical-equal
   '(1 2 8 4)
   (replace-nils (list 1 2 nil 4) #'(lambda (column row)
				      (expt column row))
		 3)))

(defgeneric replace-nils (fields missing-field-value &optional
				 row)
  (:documentation
   "Return a list in which NIL's are replaced with `missing-field-value'.

`missing-field-value' can be a constant, or a function.  In the latter
case, it is a function of two arguments, the field index and the `row'")
  (:method ((fields list) (missing-field-value t) &optional row)
    (declare (ignore row))
    (substitute missing-field-value nil fields))
  (:method  ((fields list) (missing-field-value function) &optional row)
    (if row
	(loop for field in fields
	     for column upfrom 0
	     collect (if field field
			 (funcall missing-field-value
				  column row)))
	(loop for field in fields
	   for column upfrom 0
	   collect (if field field
		       (funcall missing-field-value
				column))))))