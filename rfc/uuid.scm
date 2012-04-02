;;; -*- coding: utf-8; mode: scheme -*-
;;;
;;;  A Universally Unique IDentifier (UUID) URN Namespace
;;;    http://www.ietf.org/rfc/rfc4122.txt
;;;
;;; This module supports only UUID version 1 and 4.
;;;
;;; *  usage
;;;    (uuid) => "6FA2EEB9-9691-4B0B-BEA5-37095562C3DB"
;;;    (uuid 4) => "6FA2EEB9-9691-4B0B-BEA5-37095562C3DB"
;;;    (uuid4) => <uuid> object
;;;    (x->string <uuid>) => "6FA2EEB9-9691-4B0B-BEA5-37095562C3DB"
;;;    (x->integer <uuid>) => 148390301540502438315791606672724771803
;;;    (x->uuid "6FA2EEB9-9691-4B0B-BEA5-37095562C3DB") => <uuid> object
;;;    (x->uuid 148390301540502438315791606672724771803) => <uuid> object
;;;    (uuid-version? <uuid>) => 1-4 uuid version number
;;;    (uuid-variant? <uuid>) => uuid variant symbol like 'RFC4122

(define-module rfc.uuid
  (use gauche.parameter)
  (use srfi-19)
  (use srfi-27)
  (export
   <uuid>
   <uuid-error>
   uuid
   uuid1
   uuid3
   uuid4
   uuid5
   fake-node
   x->string
   x->integer
   x->uuid
   uuid-version?
   uuid-variant?
   uuid?
   nil-uuid))

(select-module rfc.uuid)

(define-condition-type <uuid-error> <error>
  uuid-error?
  (reason uuid-error-reason))

(define-class <uuid> ()
  ((time_low :allocation :instance
	     :init-keyword :time_low)
   (time_mid :allocation :instance
	     :init-keyword :time_mid)
   (time_hi_and_version 
    :allocation :instance
    :init-keyword :time_hi_and_version)
   (clock_seq_hi_and_reserved  
    :allocation :instance
    :init-keyword :clock_seq_hi_and_reserved)
   (clock_seq_low :allocation :instance
		  :init-keyword :clock_seq_low)
   (node :allocation :instance
	 :init-keyword :node)))

(define (random-bit bit)
  ((random-source-make-integers default-random-source) (expt 2 bit)))

(define (make-fake-node)
  (copy-bit 40 (random-bit 48) 1))

(define fake-node
  (make-parameter (make-fake-node)
		  (^a (make-fake-node))))

;;Current time in 100-nanoseconds resolution from 00:00:00.00, 15 October 1582.
(define (timestamp)
  (let ((pt (date->time-utc (make-date 0 0 0 0 15 10 1582 0)))
	(ct (current-time)))
    (/ (+ (* (abs (~ pt 'second)) 1000000000)
	  (+ (* (~ ct 'second) 1000000000)
	     (~ ct 'nanosecond))) 
       100)))

(define (uuid . version)
  (if (pair? version)
      (case (car version)
	([1] (x->string (uuid1)))
	([3] (x->string (uuid3)))
	([4] (x->string (uuid4)))
	([5] (x->string (uuid5)))
	(else (error <uuid-error> "Not Correct UUID version.")))
      (x->string (uuid4))))

(define (uuid1 . mac)
  (let ((ts (timestamp))
	(cs (random-bit 14))
	(node
	 (if (pair? mac)
	     (let1 m (car mac)
	       (cond ((and (integer? m) 
			   (<= (integer-length m) 48))
		      (bit-field m 0 48))
		     ((and (string? m) 
			   (#/^([\dA-F]{2}:){5}[\dA-F]{2}$/i m))
		      (string->number (regexp-replace-all #/:/ m "") 16))
		     (else (error <uuid-error> "Not IEEE 802 MAC for node value."))))
	     (fake-node))))
    (make <uuid>
      :time_low (bit-field ts 0 32)
      :time_mid (bit-field ts 32 48)
      :time_hi_and_version (+ (ash 1 12) (bit-field ts 48 60))
      :clock_seq_hi_and_reserved (+ (ash 2 6) (bit-field cs 9 15))
      :clock_seq_low (bit-field cs 0 8)
      :node node)))

(define (uuid3)
  (error <uuid-error> "Not support UUID version 3."))

(define (uuid4)
  (make <uuid>
    :time_low (random-bit 32)
    :time_mid (random-bit 16)
    :time_hi_and_version (+ (ash 4 12)(random-bit 12))
    :clock_seq_hi_and_reserved (+ (ash 2 6) (random-bit 6))
    :clock_seq_low (random-bit 8)
    :node (random-bit 48)))

(define (uuid5)
  (error <uuid-error> "Not support UUID version 5."))

(define-constant nil-uuid
  (make <uuid>
    :time_low 0
    :time_mid 0
    :time_hi_and_version 0
    :clock_seq_hi_and_reserved 0
    :clock_seq_low 0
    :node 0))

(define-method x->string ((uuid <uuid>))
  (string-append
   (format #f "~8,'0x" (~ uuid 'time_low)) "-"
   (format #f "~4,'0x" (~ uuid 'time_mid)) "-"
   (format #f "~4,'0x" (~ uuid 'time_hi_and_version)) "-"
   (format #f "~2,'0x" (~ uuid 'clock_seq_hi_and_reserved))
   (format #f "~2,'0x" (~ uuid 'clock_seq_low)) "-"
   (format #f "~12,'0x" (~ uuid 'node))))

(define-method x->integer ((uuid <uuid>))
  (+ (ash (~ uuid 'time_low) 96)
     (ash (~ uuid 'time_mid) 80)
     (ash (~ uuid 'time_hi_and_version) 64)
     (ash (~ uuid 'clock_seq_hi_and_reserved) 56)
     (ash (~ uuid 'clock_seq_low) 48)
     (~ uuid 'node)))

(define-method x->uuid ((uuid <string>))
  (cond ((#/^(?:urn:uuid:)?00000000\-0000\-0000\-0000\-000000000000$/ uuid)
	 nil-uuid)
	((#/^(?:urn:uuid:)?([\dA-F]{8})\-([\dA-F]{4})\-([\dA-F]{4})\-([\dA-F]{4})\-([\dA-F]{12})$/i uuid)
	 => (^a 
	     (let ((time_hi_and_version (string->number (a 3) 16))
		   (clock_seq_hi_and_reserved (bit-field (string->number (a 4) 16) 8 16)))
	       (if (and (<= 1 (bit-field time_hi_and_version 12 16) 5)
			(= (bit-field clock_seq_hi_and_reserved 6 8) 2))
		   (make <uuid> 
		     :time_low (string->number (a 1) 16)
		     :time_mid (string->number (a 2) 16)
		     :time_hi_and_version time_hi_and_version
		     :clock_seq_low (bit-field (string->number (a 4) 16) 0 8)
		     :clock_seq_hi_and_reserved clock_seq_hi_and_reserved
		     :node (string->number (a 5) 16))
		   (error <uuid-error> "Not UUID string.")))))
	(else (error <uuid-error> "Not UUID string."))))

(define-method x->uuid ((uuid <integer>))
  (if (zero? uuid) nil-uuid
      (let ((time_hi_and_version (bit-field uuid 64 80))
	    (clock_seq_hi_and_reserved (bit-field uuid 56 64)))
	(if (and (<= 1 (bit-field time_hi_and_version 12 16) 5)
		 (= (bit-field clock_seq_hi_and_reserved 6 8) 2))
	    (make <uuid> 
	      :time_low (bit-field uuid 96 128)
	      :time_mid (bit-field uuid 80 96)
	      :time_hi_and_version time_hi_and_version
	      :clock_seq_hi_and_reserved clock_seq_hi_and_reserved
	      :clock_seq_low (bit-field uuid 48 56)
	      :node (bit-field uuid 0 48))
	    (error <uuid-error> "Not UUID number.")))))

(define-method uuid-version? ((uuid <uuid>))
  (bit-field (~ uuid 'time_hi_and_version) 12 16))

(define-method uuid-variant? ((uuid <uuid>))
  (let1 vari (~ uuid 'clock_seq_hi_and_reserved)
    (if (logbit? 7 vari)
	(if (logbit? 6 vari) 
	    (if (logbit? 8 vari) 'RESERVED-FOR-FUTURE
		'MICROSOFT-BACKWARD-COMPATIBILITY)
	    'RFC4122)
	'NCS-BACKWARD-COMPATIBILITY)))

(define (uuid? uuid)
  (is-a? uuid <uuid>))

;; Epilogue
(provide "rfc/uuid")
