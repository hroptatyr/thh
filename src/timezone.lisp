;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; LOCAL-TIME
;;;
;;; A package for manipulating times and dates.
;;;
;;; Based on Erik Naggum's "A Long, Painful History of Time" (1999)
;;;
;;; Authored by Daniel Lowe <dlowe@bitmuse.com>
;;;
;;; Copyright (c) 2005-2010 Daniel Lowe
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining
;;; a copy of this software and associated documentation files (the
;;; "Software"), to deal in the Software without restriction, including
;;; without limitation the rights to use, copy, modify, merge, publish,
;;; distribute, sublicense, and/or sell copies of the Software, and to
;;; permit persons to whom the Software is furnished to do so, subject to
;;; the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "package")
(in-package :local-time)

;;; Types

(defclass timestamp ()
  ((day :accessor day-of :initarg :day :initform 0 :type integer)
   (sec :accessor sec-of :initarg :sec :initform 0 :type integer)
   (nsec :accessor nsec-of :initarg :nsec :initform 0 :type (integer 0 999999999))))

(defstruct subzone
  (abbrev nil)
  (offset nil)
  (daylight-p nil))

(defstruct timezone
  (transitions #(0) :type simple-vector)
  (indexes #(0) :type simple-vector)
  (subzones #() :type simple-vector)
  (leap-seconds nil :type list)
  (path nil)
  (name "anonymous" :type string)
  (loaded nil :type boolean))

(deftype timezone-offset ()
  '(integer -43199 50400))


;;; Variables

(defparameter *default-timezone-repository-path*
  #P"/usr/share/zoneinfo/")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +rotated-month-days-without-leap-day+
    #.(coerce #(31 30 31 30 31 31 30 31 30 31 31 28)
              '(simple-array fixnum (*))))

  (defparameter +rotated-month-offsets-without-leap-day+
    (coerce
     (cons 0
           (loop with sum = 0
                 for days :across +rotated-month-days-without-leap-day+
                 collect (incf sum days)))
     '(simple-array fixnum (*)))))

(defun %read-binary-integer (stream byte-count &optional (signed nil))
  "Read BYTE-COUNT bytes from the binary stream STREAM, and return an integer which is its representation in network byte order (MSB).  If SIGNED is true, interprets the most significant bit as a sign indicator."
  (loop
    :with result = 0
    :for offset :from (* (1- byte-count) 8) :downto 0 :by 8
    :do (setf (ldb (byte 8 offset) result) (read-byte stream))
    :finally (if signed
                 (let ((high-bit (* byte-count 8)))
                   (if (logbitp (1- high-bit) result)
                       (return (- result (ash 1 high-bit)))
                       (return result)))
                 (return result))))

(defun %string-from-unsigned-byte-vector (vector offset)
  "Returns a string created from the vector of unsigned bytes VECTOR starting at OFFSET which is terminated by a 0."
  (declare (type (vector (unsigned-byte 8)) vector))
  (let* ((null-pos (or (position 0 vector :start offset) (length vector)))
         (result (make-string (- null-pos offset) :element-type 'base-char)))
    (loop for input-index :from offset :upto (1- null-pos)
          for output-index :upfrom 0
          do (setf (aref result output-index) (code-char (aref vector input-index))))
    result))

(defun %find-first-std-offset (timezone-indexes timestamp-info)
  (let ((subzone-idx (find-if 'subzone-daylight-p
                              timezone-indexes
                              :key (lambda (x) (aref timestamp-info x)))))
    (subzone-offset (aref timestamp-info (or subzone-idx 0)))))

(defun %tz-verify-magic-number (inf zone)
  ;; read and verify magic number
  (let ((magic-buf (make-array 4 :element-type 'unsigned-byte)))
    (read-sequence magic-buf inf :start 0 :end 4)
    (when (string/= (map 'string #'code-char magic-buf) "TZif" :end1 4)
      (error 'invalid-timezone-file :path (timezone-path zone))))
  ;; skip 16 bytes for "future use"
  (let ((ignore-buf (make-array 16 :element-type 'unsigned-byte)))
    (read-sequence ignore-buf inf :start 0 :end 16)))

(defun %tz-read-header (inf)
  `(:utc-count ,(%read-binary-integer inf 4)
         :wall-count ,(%read-binary-integer inf 4)
         :leap-count ,(%read-binary-integer inf 4)
         :transition-count ,(%read-binary-integer inf 4)
         :type-count ,(%read-binary-integer inf 4)
         :abbrev-length ,(%read-binary-integer inf 4)))

(defun %tz-read-transitions (inf count)
  (make-array count
              :initial-contents
              (loop for idx from 1 upto count
                 collect (%read-binary-integer inf 4 t))))

(defun %tz-read-indexes (inf count)
  (make-array count
              :initial-contents
              (loop for idx from 1 upto count
                 collect (%read-binary-integer inf 1))))

(defun %tz-read-subzone (inf count)
  (loop for idx from 1 upto count
     collect (list (%read-binary-integer inf 4 t)
                   (%read-binary-integer inf 1)
                   (%read-binary-integer inf 1))))

(defun %tz-read-leap-seconds (inf count)
  (loop for idx from 1 upto count
     collect (list (%read-binary-integer inf 4)
                   (%read-binary-integer inf 4))))

(defun %tz-read-abbrevs (inf length)
  (let ((a (make-array length :element-type '(unsigned-byte 8))))
    (read-sequence a inf
                   :start 0
                   :end length)
    a))

(defun %tz-read-indicators (inf length)
  ;; read standard/wall indicators
  (let ((buf (make-array length :element-type '(unsigned-byte 8))))
    (read-sequence buf inf
                   :start 0
                   :end length)
    (make-array length
                :element-type 'bit
                :initial-contents buf)))

(defun %tz-make-subzones (raw-info abbrevs gmt-indicators std-indicators)
  (declare (ignore gmt-indicators std-indicators))
  ;; TODO: handle TZ environment variables, which use the gmt and std
  ;; indicators
  (make-array (length raw-info)
              :element-type 'subzone
              :initial-contents
              (loop for info in raw-info collect
                   (make-subzone
                    :offset (first info)
                    :daylight-p (/= (second info) 0)
                    :abbrev (%string-from-unsigned-byte-vector abbrevs (third info))))))

(defun %realize-timezone (zone &optional reload)
  "If timezone has not already been loaded or RELOAD is non-NIL, loads the timezone information from its associated unix file.  If the file is not a valid timezone file, the condition INVALID-TIMEZONE-FILE will be signaled."
  (when (or reload (not (timezone-loaded zone)))
    (with-open-file (inf (timezone-path zone)
                         :direction :input
                         :element-type 'unsigned-byte)
      (%tz-verify-magic-number inf zone)

      ;; read header values
      (let* ((header (%tz-read-header inf))
             (timezone-transitions (%tz-read-transitions inf (getf header :transition-count)))
             (subzone-indexes (%tz-read-indexes inf (getf header :transition-count)))
             (subzone-raw-info (%tz-read-subzone inf (getf header :type-count)))
             (leap-second-info (%tz-read-leap-seconds inf (getf header :leap-count)))
             (abbreviation-buf (%tz-read-abbrevs inf (getf header :abbrev-length)))
             (std-indicators (%tz-read-indicators inf (getf header :wall-count)))
             (gmt-indicators (%tz-read-indicators inf (getf header :utc-count)))
             (subzone-info (%tz-make-subzones subzone-raw-info
                                              abbreviation-buf
                                              gmt-indicators
                                              std-indicators)))

        (setf (timezone-transitions zone) timezone-transitions)
        (setf (timezone-indexes zone) subzone-indexes)
        (setf (timezone-subzones zone) subzone-info)
        (setf (timezone-leap-seconds zone) leap-second-info))
      (setf (timezone-loaded zone) t)))
  zone)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %make-simple-timezone (name abbrev offset)
    (let ((subzone (local-time::make-subzone :offset offset
					     :daylight-p nil
					     :abbrev abbrev)))
      (local-time::make-timezone
       :subzones (make-array 1 :initial-contents (list subzone))
       :path nil
       :name name
       :loaded t))))

(defparameter +utc-zone+ (%make-simple-timezone "Coordinated Universal Time" "UTC" 0))

(defparameter +gmt-zone+ (%make-simple-timezone "Greenwich Mean Time" "GMT" 0))

(defparameter +none-zone+ (%make-simple-timezone "Explicit Offset Given" "NONE" 0))

(defmacro define-timezone (zone-name zone-file)
  "Define zone-name (a symbol or a string) as a new timezone, lazy-loaded from zone-file (a pathname designator relative to the zoneinfo directory on this system.  If load is true, load immediately."
  (declare (type (or string symbol) zone-name))
  (let ((zone-sym (if (symbolp zone-name) zone-name (intern zone-name))))
    `(prog1
	 (defparameter ,zone-sym
	   (make-timezone :path ,zone-file
			  :name ,(if (symbolp zone-name)
				     (string-downcase (symbol-name zone-name))
				   zone-name)))
       ,@(%realize-timezone zone-sym))))

(defun transition-position (needle haystack &optional (start 0) (end (1- (length haystack))))
  (let ((middle (floor (+ end start) 2)))
    (cond
      ((> start end)
       (if (minusp end)
           0
           end))
      ((= needle (elt haystack middle))
       middle)
      ((> needle (elt haystack middle))
       (transition-position needle haystack (1+ middle) end))
      (t
       (transition-position needle haystack start (1- middle))))))

(defconstant +unix-epoch+ (encode-universal-time 0 0 0 1 1 1970))

(defun stamp-offset (stamp zone)
  "Return as multiple values the time zone as the number of seconds east of UTC, a boolean daylight-saving-p, and the customary abbreviation of the timezone."
  (declare (type (integer stamp))
	   (type (or null zone) zone))
  (let* ((zone (%realize-timezone zone))
	 (stamp (- stamp +unix-epoch+))
         (subzone-idx
	  (if (zerop (length (timezone-indexes zone)))
	      0
	    (elt (timezone-indexes zone)
		 (transition-position stamp
				      (timezone-transitions zone)))))
         (subzone (elt (timezone-subzones zone) subzone-idx)))
    (values
     (subzone-offset subzone)
     (subzone-daylight-p subzone)
     (subzone-abbrev subzone))))

(provide "timezone")
