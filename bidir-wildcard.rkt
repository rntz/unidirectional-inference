#lang racket

;; fuzzy types A, B are of the forms:
;;  | num | bool  -- base typed
;;  | A -> B      -- functions
;;  | _           -- wildcard
;;
;; exact types E, F are the same, but disallow wildcards.
(define (type/c exact?)
  (flat-named-contract (if exact? 'exact-type 'fuzzy-type)
   (flat-rec-contract type?
    (if exact? none/c '_)
    'num 'bool
    (list/c type? '-> type?))))

(define exact-type? (type/c #t))
(define fuzzy-type? (type/c #f))

;; A typing environment associates variable names with exact types.
(define env? (listof (list/c symbol? exact-type?)))

;; terms M, N are one of:
;;  | L          -- a literal (boolean or number)
;;  | (the A M)  -- an annotated term
;;  | x          -- variable
;;  | (lambda (x) M)  -- function literal
;;  | (M N)      -- function application
(define term?
  (flat-rec-contract term?
    boolean? number?                        ; literals
    (list/c 'the fuzzy-type? term?)         ; type ascription
    symbol?                                 ; variable
    (list/c 'lambda (list/c symbol?) term?) ; lambda
    (list/c term? term?)                    ; application
    ))

;; Merges two fuzzy types, if they're compatible. Otherwise, errors.
;;
;; waitaminute, is this just unification? would this break if we had to deal
;; with polymorphic types?
;;
;; it's /like/ unification, but NO VARIABLES. So you can never require different
;; bits of a type to be _equal_. Maybe that's the crucial difference?
;;
;; for what is this necessary?
;; - nested type ascription: (the (A -> _) (the (_ -> B) M))
;; - ... any other reason?
(define/contract (fuzzy-type-merge A B)
  (-> fuzzy-type? fuzzy-type? fuzzy-type?)
  (match* (A B)
    [(A '_) A]
    [('_ B) B]
    [(`(,A1 -> ,A2) `(,B1 -> ,B2))
     `(,(fuzzy-type-merge A1 A2) -> ,(fuzzy-type-merge B1 B2))]
    [('num 'num) 'num]
    [('bool 'bool) 'bool]
    ;; TODO: useful error message on failure.
    [(A B)
     (error (format "cannot unify ~s with ~s" A B))]))


;; Returns the inferred type.
(define/contract (check env term type)
  (-> env? term? fuzzy-type? exact-type?)

  (define (synth inferred-type)
    (unless (exact-type? inferred-type) (error "I fucked up."))
    (fuzzy-type-merge type inferred-type)
    inferred-type)

  (match term
    [(? boolean?) (synth 'bool)]
    [(? number?)  (synth 'num)]
    [`(the ,A ,M)
     (check env M (fuzzy-type-merge A type))]
    [(? symbol? x)
     (match (assoc x env)
       [#f (error "no such variable in scope")]
       [`(,_ ,A) (synth A)])]
    [`(lambda (,x) ,M)
     ;; find the argument and return types. the argument type must be exact; the
     ;; return type can be fuzzy.
     (define-values (A B)
       (match type
         [`(,A -> ,B) #:when (exact-type? A) (values A B)]
         [`(,A -> ,_)
          (error "cannot type-infer lambda with incomplete argument type")]
         [_ (error "cannot synthesize type for lambda expression")]))
     ;; check the body
     `(,A -> ,(check `((,x ,A) ,@env) M B))]
    [`(,M ,N)
     (match-define `(,A -> ,B) (check env M `(_ -> ,type)))
     (check env N A)
     B]))

(define (test M #:type [type '_] #:env [env '()])
  (check env M type))
