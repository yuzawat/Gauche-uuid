;;; -*- coding: utf-8; mode: scheme -*-
;;;
;;;  A Universally Unique IDentifier (UUID) URN Namespace
;;;    http://www.ietf.org/rfc/rfc4122.txt
;;;
;;; Last Updated: "2012/04/03 15:35.18"
;;;
;;;  Copyright (c) 2012  yuzawat <suzdalenator@gmail.com>

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
  (use gauche.uvector)
  (use gauche.collection)
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
(define-constant origin-time (date->time-utc (make-date 0 0 0 0 15 10 1582 0)))
(define-constant origin-number (* (abs (~ origin-time 'second)) 1000000000))
(define (timestamp)
  (let1 ct (current-time)
    (/ (+ origin-number
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
  (uuid->string5 uuid))

;; time "/usr/local/bin/gosh" -I. -I. ./perf.scm 100000
;;
;; real	0m10.324s
;; user	0m17.575s
;; sys	0m4.693s
(define (uuid->string1 uuid)
  (string-append
   (format #f "~8,'0x" (~ uuid 'time_low)) "-"
   (format #f "~4,'0x" (~ uuid 'time_mid)) "-"
   (format #f "~4,'0x" (~ uuid 'time_hi_and_version)) "-"
   (format #f "~2,'0x" (~ uuid 'clock_seq_hi_and_reserved))
   (format #f "~2,'0x" (~ uuid 'clock_seq_low)) "-"
   (format #f "~12,'0x" (~ uuid 'node))))

;; time "/usr/local/bin/gosh" -I. -I. ./perf.scm 100000
;;
;; real	0m8.136s
;; user	0m13.465s
;; sys	0m3.107s
(define (uuid->string2 uuid)
  (format #f "~8,'0x-~4,'0x-~4,'0x-~2,'0x~2,'0x-~12,'0x"
	  (~ uuid 'time_low) (~ uuid 'time_mid) (~ uuid 'time_hi_and_version)
	  (~ uuid 'clock_seq_hi_and_reserved) (~ uuid 'clock_seq_low) (~ uuid 'node)))

;; time "/usr/local/bin/gosh" -I. -I. ./perf.scm 100000
;;
;; real	0m7.807s
;; user	0m13.017s
;; sys	0m2.921s
(define (uuid->string3 uuid)
  (let1 v (x->integer uuid)
    (format #f "~8,'0x-~4,'0x-~4,'0x-~4,'0x,-~12,'0x"
	    (bit-field v 96 128) (bit-field v 80 96) (bit-field v 64 80) (bit-field v 48 64) (~ uuid 'node))))

;; time "/usr/local/bin/gosh" -I. -I. ./perf.scm 100000
;;
;; real	0m3.224s
;; user	0m5.410s
;; sys	0m1.094s
(define (uuid->string4 uuid)
  (let* ((v (x->integer uuid))
	 (s (format #f "~32,'0x" v)))
    (string-append
     (substring s 0 8) "-"
     (substring s 8 12) "-"
     (substring s 12 16) "-"
     (substring s 16 20) "-"
     (substring s 20 32))))

;; time "/usr/local/bin/gosh" -I. -I. ./perf.scm 100000
;;
;; real	0m3.088s
;; user	0m3.748s
;; sys	0m0.386s
(define-constant buffer-template (string->u8vector "00000000-0000-0000-0000-000000000000"))
(define-constant xdigit-table (string->u8vector "0123456789abcdef"))
(define (uuid->string5 uuid)
  (let ((v (x->integer uuid))
	(buf (u8vector-copy buffer-template)))
    (for-each (^ (i p) (u8vector-set! buf i (u8vector-ref xdigit-table (bit-field v p (+ 4 p)))))
	      '(35 34 33 32 31 30 29 28 27 26 25 24 22 21 20 19 17 16 15 14 12 11 10  9  7   6   5   4   3   2   1   0)
	      '( 0  4  8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120 124))
    (u8vector->string buf)))

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
