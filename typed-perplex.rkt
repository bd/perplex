#lang typed/racket

(require math/base)


;how a missive looks, and what makes
;it a missive, though what letters
;to type is the province of another 
;procedure
(struct: Missive ([date : String] 
                  [salutation : String] 
                  [sentences : (Listof Sentence)] 
                  [ valediction : String]))

;after phoneme, lexeme, morpheme ... meme
;an abstract unit of language
(struct: Eme ([type : EmeType]
              [tier : Tier]
              [representation : String]
              [productions : (Listof Production)]))

;a level in the tower of abstractions we are building toward babble...
;it represents something like the _level_ abstraction, i.e. the 
;notion of a phoneme, rather than a particular phoneme
(define-type Tier Integer)
(define-predicate Tier? Tier)

;can be thought of as the particular name of the 
;abstract unit of language within a tier, i.e. fricatives
(define-type EmeType Symbol)
(define-predicate EmeType? EmeType)

;a valid unit of grammar
;a production that is a List of EmeTypes can be expanded
;a List of Atoms is the base level, and can be printed
(define-type Production  [Listof (U EmeType Atom)])

;a parse is a PARTICULAR instance of an eme
(define-type Parse (Listof Eme))

(struct: Sentence ([parse : Parse]
                   [representation : String]))

(define-type Grammar (HashTable EmeType (Listof EmeType)))
(define-type Lexicon (HashTable EmeType (Setof Parse)))

(struct: Language ([grammar : Grammar]
                   [lexicon : Lexicon]))

(define-type Atom Symbol)
(define-type CharacterSet [Listof Atom])

(struct: Range ([lower : Integer] [upper : Integer]))

;generates an arbitrary, unique symbol to use as the type name
(: new-emetype (-> EmeType))
(define (new-emetype)
  (gensym 'EmeType))

;generates a list of types, the exact number being randomly determined within a specified range
(: make-random-emetype-list (Range -> (Listof EmeType)))
(define (make-random-emetype-list range )
  (letrec: ([lower : Integer (Range-lower range)]
            [upper : Integer (Range-upper range)]
            [how-many : Integer (random-integer lower upper)]
            [bld-lst : (Integer (Listof EmeType) -> (Listof EmeType))
                     (λ: ([remaining : Integer]
                          [lst : (Listof EmeType)]) 
                       (cond [(< remaining 1 ) lst]
                             [else (bld-lst (- remaining 1) (cons (new-emetype) lst))]))])
    (bld-lst how-many '())))

(: evaluate-eme (Eme -> String))
(define (evaluate-eme eme)
  (Eme-representation eme))

;produces the groundwork of our language, i.e. consonants v. vowels
(: bootstrap-base-tier (CharacterSet -> (Listof Eme)))
(define (bootstrap-base-tier atoms)
  (letrec: ([distribute-atoms : (CharacterSet (Listof EmeType) (Listof Eme) -> (Listof Eme))
                              (λ: ([atoms : CharacterSet]
                                   [types : (Listof EmeType)]
                                   [emes : (Listof Eme)])
                                (cond [(null? atoms) emes]
                                      [else (distribute-atoms 
                                             (cdr atoms) 
                                             types 
                                             (cons (Eme (select-random types) 1 (symbol->string (car atoms)) (list (list (car atoms)))) emes))
                                            ]))])
    (distribute-atoms atoms (make-random-emetype-list (Range 2 5)) '())))

(: update-lexicon (Lexicon EmeType Parse -> Lexicon))
(define (update-lexicon lexicon type parse)
  (cond [(hash-has-key? lexicon type) (hash-set lexicon type (set-add (hash-ref lexicon type) parse))]
        [else (hash-set lexicon type (set parse))]))
                                       
;creates the lexicon anew from a list of (assumed to be base-level) emes
;thus, this returns a mapping from EmeType to sets of Parses
(: bootstrap-lexicon ((Listof Eme) -> Lexicon))
(define (bootstrap-lexicon base-tier)
  (letrec: ([build-lex : ((Listof Eme) Lexicon -> Lexicon)
            (λ: ([emes : (Listof Eme)]
                 [lex : Lexicon])
              (cond [(null? emes) lex]
                    [else (build-lex (cdr emes) 
                                     (update-lexicon lex (Eme-type (car emes)) (list (car emes))))]))])
    (build-lex (cdr base-tier) (make-immutable-hash (list (cons (Eme-type (car base-tier)) (set (list (car base-tier)))))))))
   

;randomly creates a grammar rule (Production) given a set of types 
(: make-arbitrary-grammar-rule ((Setof EmeType) ->  Production))
(define (make-arbitrary-grammar-rule composing-types)
  (letrec: ([types : (Listof EmeType) (set->list composing-types)]
            [grammar-rule : (Production Real Real -> Production)
                          (λ: ([production : Production]
                               [probability : Real]
                               [decay-rate : Real])
                            (cond [(done? production probability) production]
                                  [else (grammar-rule 
                                         (cons (select-random types) production) 
                                         (+  probability decay-rate)
                                         decay-rate)]))])
    (grammar-rule '() 0.2 0.2))) ;TODO extract decay/probability to constants or dynamic lookup

;randomly decides if a production is to be considered complete, given a probability
;thus, the caller may generate a decay function s.t. the likelihood of the production's 
;being extended is diminished with each successive call
(: done? (Production Real -> Boolean))
(define (done? production probability)
  (cond [(null? production) #f]
        [else  (< (random) probability)]))

;utility to get a list of unique EmeTypes from a list of Emes
(: extract-types ((Listof Eme) -> (Listof EmeType)))
(define (extract-types emes)
  (letrec: ([extract : ((Listof Eme) (Setof EmeType) -> (Setof EmeType))
                     (λ: ([emes : (Listof Eme)]
                          [types : (Setof EmeType)])
                       (cond [(null? emes) types]
                             [else (extract (cdr emes) (set-add types (Eme-type (car emes))))]))])
    (set->list (extract emes (set )))))
            
;utility function to select a random element of a list
(: select-random (All (T) ((Listof T) -> T)))
(define (select-random lst) 
  (list-ref lst (random-integer 0 (length lst))))

; the atoms of our generated language
; determined by the character set of the Selectric II (at least in this instance)
(: SELECTRIC-ATOMS CharacterSet)
(define SELECTRIC-ATOMS '(q w e r t y u i o p a s d f g h j k l z x c v b n m))