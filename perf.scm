(use gauche.lazy)
(use rfc.uuid)

(define (make-uuid-lseq :optional count)
  (if (undefined? count)
      (generator->lseq uuid)
      (ltake (generator->lseq uuid) count)))

(define (main args)
  (for-each values (make-uuid-lseq (x->integer (cadr args))))
  0)
