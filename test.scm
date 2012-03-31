;;; -*- coding: utf-8; mode: scheme -*-
;;;
;;; Test rfc.uuid
;;;

(use gauche.test)

(test-start "rfc.uuid")
(use rfc.uuid)
(test-module 'rfc.uuid)

(test* "<uuid> by uuid1" #t
       (is-a? (uuid1) <uuid>))

(test* "<uuid> by uuid4" #t
       (is-a? (uuid4) <uuid>))

(test* "uuid" #t
      (regmatch? (#/^[\dA-F]{8}\-[\dA-F]{4}\-4[\dA-F]{3}\-[89ab][\dA-F]{3}\-[\dA-F]{12}$/i (uuid))))

(test* "uuid 1" #t
      (regmatch? (#/^[\dA-F]{8}\-[\dA-F]{4}\-1[\dA-F]{3}\-[89ab][\dA-F]{3}\-[\dA-F]{12}$/i (uuid 1))))

(test* "uuid 2" (test-error <uuid-error>)
       (uuid 2))

(test* "uuid 3" (test-error <uuid-error>)
       (uuid 3))

(test* "uuid 4" #t
      (regmatch? (#/^[\dA-F]{8}\-[\dA-F]{4}\-4[\dA-F]{3}\-[89ab][\dA-F]{3}\-[\dA-F]{12}$/i (uuid 4))))

(test* "uuid 5" (test-error <uuid-error>)
       (uuid 5))

(test* "x->string uuid1" #t
      (regmatch? (#/^[\dA-F]{8}\-[\dA-F]{4}\-1[\dA-F]{3}\-[89ab][\dA-F]{3}\-[\dA-F]{12}$/i (x->string (uuid1)))))

(test* "x->string uuid4" #t
      (regmatch? (#/^[\dA-F]{8}\-[\dA-F]{4}\-4[\dA-F]{3}\-[89ab][\dA-F]{3}\-[\dA-F]{12}$/i (x->string (uuid4)))))

(test* "uuid1 operand MAC str" #t
      (regmatch? (#/^[\dA-F]{8}\-[\dA-F]{4}\-1[\dA-F]{3}\-[89ab][\dA-F]{3}\-08002770b114$/i
		     (x->string (uuid1 "08:00:27:70:b1:14")))))

(test* "uuid1 operand MAC int" #t
      (regmatch? (#/^[\dA-F]{8}\-[\dA-F]{4}\-1[\dA-F]{3}\-[89ab][\dA-F]{3}\-08002770b114$/i
		     (x->string (uuid1 8796754718996)))))

(test* "x->uuid str" #t
       (let ((str "185d0fea-3c8d-4a1d-a4dd-cda16614ceff")
	     (int 32384678300955712163857616517001957119))
	 (and
	  (equal? str (x->string (x->uuid str)))
	  (equal? int (x->integer (x->uuid str))))))

(test* "x->uuid int" #t
       (let ((str "185d0fea-3c8d-4a1d-a4dd-cda16614ceff")
	     (int 32384678300955712163857616517001957119))
	 (and
	  (equal? str (x->string (x->uuid int)))
	  (equal? int (x->integer (x->uuid int))))))

(test* "x->uuid invalid str0" (test-error <uuid-error>)
       (let ((str "185d0fea-3c8d-0a1d-a4dd-cda16614ceff"))
	 (x->uuid str)))

(test* "x->uuid invalid str1" (test-error <uuid-error>)
       (let ((str "185d0fea-3c8d-0a1d-a4dd-cda16614ceff"))
	 (x->uuid str)))

(test* "x->uuid invalid str2" (test-error <uuid-error>)
       (let ((str "185d0fea-3c8d-0a1d-a4dd-cda16614ceff"))
	 (x->uuid str)))

(test* "x->uuid invalid int" (test-error <uuid-error>)
       (let ((int 32384678300955863279585068345648795391))
	 (x->uuid int)))

(test* "uuid-variant? 0" 'RFC4122
       (uuid-variant? (uuid1)))

(test* "uuid-variant? 1" 'RFC4122
       (uuid-variant? (uuid4)))

(test* "uuid-variant? 2" 'RFC4122
       (uuid-variant? (x->uuid "73a45c27-7b35-11e1-9ea0-2b3243eded62")))

(test* "uuid-variant? 3" 'RFC4122
       (uuid-variant? (x->uuid "185d0fea-3c8d-4a1d-a4dd-cda16614ceff")))

(test* "uuid-version? 0" 1
       (uuid-version? (uuid1)))

(test* "uuid-version? 1" 4
       (uuid-version? (uuid4)))

(test* "uuid-version? 2" 1
       (uuid-version? (x->uuid "73a45c27-7b35-11e1-9ea0-2b3243eded62")))

(test* "uuid-version? 3" 4
       (uuid-version? (x->uuid "185d0fea-3c8d-4a1d-a4dd-cda16614ceff")))

;; epilogue
(test-end)
