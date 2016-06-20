#lang racket/base

(require
 (for-syntax
  racket/base
  racket/list
  syntax/parse)
 scribble/base
 pict
 typeset-rewriter
 (only-in racket/class new send is-a?/c make-object)
 scribble-latex-utils/utils
 racket/draw
 racket/function
 racket/list
 redex/pict
 "with-cache.rkt"
 (only-in rackunit require/expose)
 racket/serialize)

(require/expose scribble/run (current-dest-directory))
(require/expose redex/private/core-layout (compound-rewrite-table atomic-rewrite-table))

(provide
 (all-defined-out))

;; Not yet properly defined; need more research into (La)TeX. And probably would be better to render
;; directly to (La)TeX from Redex.
(define textbf-style (cons "LM Roman 10, CMU Serif, bold" 'roman))
(define textrm-style (cons "LM Roman 10, CMU Serif" 'roman))
(define textsf-style (cons "LM Sans 10, CMU Sans Serif, Symbola" 'swiss))
(define mathsf-style (cons "LM Sans 10, CMU Sans Serif, Neo Euler, Symbola" 'swiss))
(define mathrm-style (cons "LM Roman 10, Neo Euler, CMU Serif, Symbola" 'roman))

(define greek-style (make-parameter (cons "CMU Serif, LM Roman 10, CMU Serif, Symbola, Neo Euler" 'roman)))
(define upgreek-style (make-parameter (cons "CMU Serif, Neo Euler, LM Roman 17, CMU Serif" 'roman)))

;; LaTeX fonts screw up the bounding boxes; correct them
(current-render-pict-adjust
 (λ (x sym)
   (case sym
     [('language-line) x]
     [else (inset x 0 -4 0 0)])))

(default-font-size 12)
(metafunction-font-size 12)
(label-font-size 10)
(metafunction-style textsf-style)
(label-style textsf-style)
(default-style mathrm-style)
(literal-style mathrm-style)
(paren-style mathrm-style)
(grammar-style textbf-style)

;; auto-caching off by default
(*CACHE-VERBOSE?* #f)
(*CACHE?* #f)

(define fresh-term-cache
  (let ([n 0])
    (lambda ()
      (set! n (add1 n))
      (format "~aretex-cache/cache~a.fasl" (current-dest-directory) n))))

(define (serialize-color e)
  (if ((is-a?/c color%) e)
      (list (send e red) (send e blue) (send e green) (send e alpha))
      e))

(define (serialize-style st)
  (let loop ([st st])
    (if (pair? st)
        (cons
         (serialize-color (car st))
         (serialize-style (cdr st)))
        (serialize-color st))))

(define ((check-cache e) cache)
  (let ([cache (deserialize cache)])
    (and (equal? (first cache) e)
         (equal?
          (second cache)
          (list
           (default-font-size)
           (metafunction-font-size)
           (label-font-size)
           (serialize-style (default-style))
           (serialize-style (literal-style))
           (serialize-style (non-terminal-superscript-style))
           (serialize-style (non-terminal-subscript-style))
           (serialize-style (non-terminal-style))
           (serialize-style (upgreek-style))
           (serialize-style (greek-style))))
         (equal?
          (third cache)
          (map car (atomic-rewrite-table)))
         (equal?
          (fourth cache)
          (map car (compound-rewrite-table)))
         (equal?
          (fifth cache)
          (judgment-form-cases))
         (last cache))))

(define ((write-cache e) t)
  (serialize
   (list
    e
    (list
     (default-font-size)
     (metafunction-font-size)
     (label-font-size)
     (serialize-style (default-style))
     (serialize-style (literal-style))
     (serialize-style (non-terminal-superscript-style))
     (serialize-style (non-terminal-subscript-style))
     (serialize-style (non-terminal-style))
     (serialize-style (upgreek-style))
     (serialize-style (greek-style)))
    (map car (atomic-rewrite-table))
    (map car (compound-rewrite-table))
    (judgment-form-cases)
    t)))

(define (_render-term-cache e f)
  (with-cache (fresh-term-cache)
    #:read (check-cache e)
    #:write (write-cache e)
    f))

(define-syntax-rule (render-term-cache lang e)
  (_render-term-cache '(lang e) (thunk (render-term lang e))))

(define-syntax-rule (render-judgment-form-cache e ...)
  (_render-term-cache '(e ...) (thunk (render-judgment-form e ...))))

(define-syntax-rule (render-language-cache e ...)
  (_render-term-cache '(e ...) (thunk (render-language e ...))))

(define-syntax (render-reduction-relation-cache syn)
  (syntax-parse syn
    [(_ e (~optional (~seq #:style s) #:defaults ([s #''vertical])))
     #'(_render-term-cache '(e s) (thunk (render-reduction-relation e #:style s)))]))

(define (style-remq e style)
  (if (pair? style)
      (if (eq? e (car style))
          (style-remq (cdr style) style)
          (cons (car style) (style-remq e (cdr style))))
      (if (eq? e style)
          null
          e)))

;; TODO: Magic numbers
(define (rule->table rule)
  (list
    (rule-pict-info-lhs rule)
    (hc-append 0
     (scale (arrow->pict (rule-pict-info-arrow rule)) .5)
     (text (format "~a" (rule-pict-info-label rule)) (label-style) (label-font-size)))
    (rule-pict-info-rhs rule)
    (blank) (blank) (rule-pict-info->side-condition-pict rule 24)))

(define (table-reduction-style rules)
  (table 3 (flatten (map rule->table rules))
         `(,rc-superimpose . ,lc-superimpose)
         cc-superimpose
         6 (flatten (map (λ _ `(0 0)) rules))))

; TODO: Determine number of cases in judgment form n.
; Attempt to render n side-by-side, decreasing n until width of pic is less than max-width
; stack vertically until all n rendered
#;(define (render-judgment-form-vertical id max-width)
  (define n
    (let loop ([n 0])
      (with-handlers ([_ (sub1 n)])
        (parameterize ([judgment-form-cases (build-list n values)])
          (render-judgment-form id)
          (loop (add1 n))))))
  (let loop ([p (blank)]
             [m 0]
             [i n])
    (parameterize ([judgment-form-cases (build-list n values)]
                   ;; TODO: Abstract 20, hp-append
                   [relation-clauses-combine (lambda (l) (apply hb-append 20 l))])
      (render-judgment-form id))))

;; TODO: Should not be a macro, but need macro to get id; factor out body
(define-syntax-rule (render-judgment-form-rows id ls)
  (parameterize (;; TODO: Abstract 20, hp-append
                 [relation-clauses-combine (lambda (l) (apply hb-append 20 l))])
    (define-values (rows _)
      (for/fold ([l '()]
                 [start 0])
                ([in-row ls])
        (values
         (cons (parameterize ([judgment-form-cases (build-list in-row (curry + start))])
                 (render-judgment-form id))
               l)
         (+ start in-row))))
    ;; TODO: Abstract 20, vc-append
    (apply vc-append 20 (reverse rows))))

;; TODO: Refactor macros

;; Seems to create a little too much bottom padding
;; TODO: Magic numbers
(define mathpar-judgment-negspace (make-parameter "-1.5em"))
(define-for-syntax (case-decls->cases more)
  (syntax-parse more
    [(n:nat)
     (in-range (syntax->datum #'n))]
    [((s:str ...))
     (map syntax->datum (attribute s))]
    [((n:nat ...))
     (map syntax->datum (attribute n))]))

;; TODO: Magic numbers
(define-for-syntax (judgment->pict-list j cases)
  (for/list ([i cases])
    (list
     #`(parameterize ([judgment-form-cases '(#,i)]
                      ;; TODO: LaTeX fonts screw up bounding boxes, adjust
                      ;; NB: current-render-pict-adjust does not affect where-combine, manually correct.
                      [where-combine
                       (compose
                        (λ (x) (inset x 0 -7 0 0))
                        (where-combine))]
                      [current-render-pict-adjust
                       (λ (x sym)
                         (inset x 0 -6 0 0))])
         (render-judgment-form-cache #,j))
     "\\and"))
  )
(define-syntax (render-mathpar-judgment syn)
  (syntax-parse syn
    [(_ j:id . more)
     #'(render-mathpar-judgment (j . more))]
    [(_ (j:id . more) ...)
     #`(elem
        (mathpar
         #,@(flatten
             (for/list ([j (attribute j)]
                        [more (attribute more)])
               (flatten (judgment->pict-list j (case-decls->cases more))))))
        (exact (format "\\vspace{~a}" (mathpar-judgment-negspace))))]))

;; Useful for rewriters
(define (set-column l1 f l2)
  (struct-copy lw l1 [column (f (lw-column l2))]))

(define (supscript-nt s1 s2)
  (hc-append
   0
   (text s1 (non-terminal-style) (default-font-size))
   (text s2 (non-terminal-superscript-style) (default-font-size))))

(define (atomic-upgreek str)
  (text str (upgreek-style) (default-font-size)))

(define (atomic-greek str)
  (text str (greek-style) (default-font-size)))

;; TODO: Add some rewriters for standard forms, like type-check, validity, lambda, app, snoc-envs
