#lang racket/base
;; Copyright: Ben Greenman 2016.
;; Used with permission.

(require
  (only-in racket/file file->value)
  racket/serialize)

(provide (all-defined-out))

;; If #f, do not cache anything
;; auto-caching off by default
(define *CACHE?* (make-parameter #f))
(define *CACHE-VERBOSE?* (make-parameter #f))

(define ((cache-read-error cache-file) exn)
  (printf "[WARNING] Failed to read cachefile '~a', got exception:\n\t~a\n" cache-file (exn-message exn))
  #f)

(define (with-cache cache-file thunk #:read [read-proc #f] #:write [write-proc #f])
  (let ([read-proc (or read-proc values)]
        [write-proc (or write-proc values)])
    (or (and (*CACHE?*)
             (file-exists? cache-file)
             (let ([v (with-handlers ([exn:fail? (cache-read-error cache-file)])
                        (read-proc (file->value cache-file)))])
               (and v
                    (when (*CACHE-VERBOSE?*) (printf "[INFO] reading cachefile '~a'~n" cache-file))
                    v)))
        (let ([r (thunk)])
          (when (*CACHE-VERBOSE?*) (printf "[INFO] writing cachefile '~a'~n" cache-file))
          (and (*CACHE?*)
               (with-output-to-file cache-file #:exists 'replace
                 (lambda () (writeln (write-proc r)))))
          r))))

;; =============================================================================
;; Below are a few uses of `with-cache` scraped from my code
;
;(define (render-table render-proc)
;  (with-cache cache-file
;   #:read uncache-table
;   #:write cache-table
;   render-proc)
;
;(define (render-data-lattice bm v #:tag [tag "*"])
;  (with-cache (lattice-cache-file bm v tag)
;    #:read deserialize
;    #:write serialize
;    (lambda () (file->performance-lattice (data-path bm v tag)))))
;
;(define (render-lnm-table)
;  (with-cache (lnm-table-cache)
;   #:read (lambda (tag+data)
;            (let ([d (uncache-table tag+data)])
;              (and d (deserialize d))))
;   #:write (compose1 cache-table serialize)
;   new-lnm-bars))
;
;;; -----------------------------------------------------------------------------
;;; Functions to use in #:read and #:write
;
;(define BENCHMARK-NAMES '(a b c))
;
;(define (cache-table T)
;  (cons BENCHMARK-NAMES T))
;
;(define (uncache-table tag+data)
;  (and (equal? (car tag+data) BENCHMARK-NAMES)
;       (cdr tag+data)))
