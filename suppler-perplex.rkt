#lang typed/racket
(require math/base)

;the smallest unit of language, here, represented as a 1 character string
(define-type Atom String)
(define-predicate Atom? Atom)
;after phoneme, lexeme, morpheme, etc.
;a very abstract notion--the name of a member of a type of linguistic unit
;here represented as a racket symbol
(define-type EmeType (U Symbol Atom))
(define-predicate EmeType? EmeType)
(define-predicate ComplexType? Symbol)
;a grammatically valid sequence of EmeTypes, i.e. "subject verb object"
(define-type Production (Listof EmeType))
;The whole kitten kaboodle, grammatically speaking--our generated language's grammar
;constructed as a list of mappings from a type to valid productions 
;the order of the list provides implicit hierarchy, s.t. (first Grammar)
;is composed entirely from the types defined in (rest Grammar)
(define-type Grammar (Listof SubGrammar))
;a single level of a grammar
(define-type SubGrammar (HashTable EmeType  (Listof Production)))
;the container for the specific tokens of given EmeTypes
; organized by EmeType
; i.e. all the words (phrases, phonemes, etc.) in the language
(define-type Lexicon (HashTable EmeType (Listof String)))  

(: empty-lexicon ( -> Lexicon))
(define (empty-lexicon)
  (make-immutable-hash `()))

;turns a production into text. That is, given a grammatical outline of a unit of language
;it non-deterministically generates a text that conforms to the rules presented.
(: evaluate (Production Grammar Lexicon -> String))
(define (evaluate production grammar lexicon)
  (letrec: ([eval : (Production Grammar -> String)
                  (λ: ([production : Production]
                       [grammar : Grammar])                      
                    (cond [(null? production) ""]
                          [(Atom? (first production)) (first production)]
                          [(lookup-in-lexicon? lexicon (first production)) (string-append 
                                                                            (select-random (lookup-in-lexicon lexicon (first production))) 
                                                                            (eval (rest production) grammar))]
                          [else (string-append (eval (select-random (lookup-productions grammar (first production)))
                                                     grammar )
                                               (eval (rest production) grammar))]))])
    (eval production grammar)))




;rather than evaluate directly, we need to preserve the particulars of an evaluation 
;so they can be interred in the lexicon for future use
;the flow would be something like build-grammar -> generate production->expand-production->inter-production-evaluations->return text
(define-type Expansion (Pairof String Lexicon))
;(: expand-production (Production Grammar Lexicon -> Expansion))
;(define (expand-production production grammar lexicon)
;  (letrec: ([lookup : (EmeType Lexicon -> String)
;                    (λ: ([term : EmeType]
;                         [lexicon : Lexicon])
;                      (select-random (lookup-in-lexicon lexicon term)))]
;            [invent : (EmeType Grammar Lexicon -> String)
;                    (λ: ([term : EmeType]
;                         [grammar : Grammar]
;                         [lexicon : Lexicon])
;                      (
;                         

;(define g (build-grammar SELECTRIC-CHARS))
;(first (select-random (lookup-productions g (first (hash-keys (last g))))))

(define PROB-NEW-TOKEN .23)

(define-type Expander (EmeType Grammar Lexicon -> Expansion))

(: expand Expander)
(define (expand type grammar lexicon)
  ((odds-on new-token old-token PROB-NEW-TOKEN) grammar lexicon))

(: new-token Expander)
(define (new-token type grammar lexicon)
  (pass))

(: old-token Expander)
(define (old-token type grammar lexicon)
  (let: ([token : String (select-random (lookup-in-lexicon lexicon type))])
    `(,token . ,lexicon)))

(: isAtomType? (EmeType Grammar -> Boolean))
(define (isAtomType? type grammar)
  (let: ([productions : (Listof Production) (lookup-productions grammar type)])
    (and (not (empty? productions)) (Atom? (first (first productions))))))

(: make-atom-emetype (EmeType Grammar -> Expansion))
(define (make-atom-emetype type grammar)
  (let: ([token : String (cast (first (select-random (lookup-productions grammar type))) String)])
    `(,token . ,(update (empty-lexicon) type token))))


(define GRAMMAR-INITIAL-PROBABILITY .01)
(define GRAMMAR-DECAY-RATE .02)

;constructs a grammar for the language 
(: build-grammar ((Setof Atom) -> Grammar))
(define (build-grammar atomset)
  (letrec: ([base-grammar : SubGrammar (bootstrap atomset)]
            [build : (Grammar Real Real -> Grammar)
                   (λ: ([grammar : Grammar]
                        [probability : Real]
                        [decay-rate : Real])
                     (cond [(done? grammar probability) grammar]
                           [else (build (cons (extend-grammar (hash-keys (first grammar))) grammar)
                                        (+ probability decay-rate)
                                        decay-rate)]))])
    (build (list base-grammar) GRAMMAR-INITIAL-PROBABILITY GRAMMAR-DECAY-RATE)))

(define MIN_CHAR_TYPES 2)
(define MAX_CHAR_TYPES 3)

;assigns the character set to arbitrary types, as the starting point for the grammar
;this is the foundation of the tower to babble we're constructing
(: bootstrap ((Setof Atom) -> SubGrammar ))
(define (bootstrap atomset)
  (letrec: ([types : (Listof EmeType) (make-random-emetype-list MIN_CHAR_TYPES MAX_CHAR_TYPES)]
            [atoms : (Listof Atom) (set->list atomset)]
            [assign-atoms : ((Listof EmeType) (Listof Atom) SubGrammar -> SubGrammar)
                          (λ (types atoms base-grammar)
                            (cond [(null? atoms) base-grammar]
                                  ;TODO | allow a few atoms to appear as valid productions of more than one base type, i.e. "sometimes y"
                                  ;TODO | allow multiple atoms to compose one base type, i.e. "ll"
                                  [else (assign-atoms types (rest atoms) (update base-grammar (select-random types) (list (first atoms))))]))])
    (assign-atoms types  atoms (make-immutable-hash '()))))

;arbitrarily creates productions from the typeset argument and 
;and assigns  them to a randomly generated list of new higher-order EmeTypes
(: extend-grammar ((Listof EmeType) -> SubGrammar))
(define (extend-grammar typeset)
  (letrec: ([new-types : (Listof EmeType) (make-random-emetype-list 6 60)]
            [composing-types : (Listof EmeType) typeset]
            [compose-productions : ( (Listof EmeType) SubGrammar -> SubGrammar)
                                 (λ: ([types : (Listof EmeType)]
                                      [grammar : SubGrammar])
                                   (cond [(null? types) grammar]
                                         [else (compose-productions 
                                                (rest types) 
                                                (hash-set grammar (first types) (random-production-set composing-types)))]))])
    (compose-productions new-types (make-immutable-hash '()))))

(define PRODUCTION-PROBABILITY .005)
(define PRODUCTION-DECAY .03)

;constructs a set of Productions (though returned as a list type for convenience)
(: random-production-set ((Listof EmeType) -> (Listof Production)))
(define (random-production-set composing-types)
  (letrec: ([make-productions : ((Setof Production) Real Real -> (Setof Production))
                              (λ: ([productions : (Setof Production)]
                                   [probability : Real]
                                   [decay-rate : Real])
                                (cond [(done? (set->list productions) probability) productions]
                                      [else (make-productions
                                             (set-add productions (random-production (list->set composing-types)))
                                             (+ probability decay-rate)
                                             decay-rate)]))])
    (set->list (make-productions (set ) PRODUCTION-PROBABILITY PRODUCTION-DECAY))))

(define PRODUCTION-COMPLETION-PROBABILITY .005)
(define PRODUCTION-COMPLETION-DECAY .02)

;constructs a random Production from a set of types
(: random-production ((Setof EmeType) -> Production))
(define (random-production composing-types)
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
    (grammar-rule '() PRODUCTION-COMPLETION-PROBABILITY PRODUCTION-COMPLETION-DECAY))) 

;randomly decides if a list (i.e. Production, list of Production, etc. is to be considered complete, given a probability
;thus, the caller may generate a decay function s.t. the likelihood of the production's 
;being extended is diminished with each successive call
;this function is meant to put off the determination of the range of variability until the last moment
(: done? (All (T) (Listof T) Real -> Boolean))
(define (done? production probability)
  (cond [(null? production) #f]
        [else  (< (random) probability)]))

;the character set [here: provided by the IBM Selectric II typewriter]
(define SELECTRIC-CHARS (set "q" "w" "e" "r" "t" "y" "u" "i" "o" "p" "a" "s" "d" "f" "g" "h" "j" "k" "l" "z" "x" "c" "v" "b" "n" "m" ))

;utility to find the productions for the given EmeType
(: lookup-productions (Grammar EmeType ->  (Listof Production)))
(define (lookup-productions grammar type)
  (cond [(Atom? type) (error "Cannot use Atom as Key to SubGrammar")] 
        [(null? grammar) (error (string-append "no such type exists in grammar: " (symbol->string type)))]
        [(hash-has-key? (first grammar) type) (hash-ref (first grammar) type)]
        [else (lookup-productions (rest grammar) type)]))

;utility function to simplify adding values to a data structure that will be  used frequently throughout
(: update  (All (A B) (HashTable A (Listof B)) A B -> (HashTable A (Listof B))))
(define (update table key value)
  (cond [(hash-has-key? table key) (hash-set table key (cons value (hash-ref table key)))]
        [else (hash-set table key (list value))]))


;utility function to select a random element of a list
(: select-random (All (T) ((Listof T) -> T)))
(define (select-random lst) 
  (list-ref lst (random-integer 0 (length lst))))

;utility
(: lookup-in-lexicon (Lexicon EmeType -> (Listof String)))
(define (lookup-in-lexicon lexicon type)
  (hash-ref lexicon type))

;generates an arbitrary, unique symbol to use as the type name
(: new-emetype (-> EmeType))
(define (new-emetype)
  (gensym 'EmeType))

;generates a list of types, the exact number being randomly determined within a specified range
(: make-random-emetype-list (Integer Integer -> (Listof EmeType)))
(define (make-random-emetype-list lower upper )
  (letrec: ([how-many : Integer (random-integer lower upper)]
            [bld-lst : (Integer (Listof EmeType) -> (Listof EmeType))
                     (λ: ([remaining : Integer]
                          [lst : (Listof EmeType)]) 
                       (cond [(< remaining 1 ) lst]
                             [else (bld-lst (- remaining 1) (cons (new-emetype) lst))]))])
    (bld-lst how-many '())))

;randomly decides whether the eme to write will be one that has been used before or not
(: lookup-in-lexicon? (Lexicon EmeType -> Boolean))
(define (lookup-in-lexicon? lexicon type)
  (and (not (generate-new-eme?)) (hash-has-key? lexicon type)))

(: generate-new-eme? ( -> Boolean))
(define (generate-new-eme?)
  (> (random) .333))

; TODO: use odds-on to refactor perplex 
;Refactor with a "might" HOF, takes a two functions, and a probability, 
;returns a function which calls the first function with probability, 
;calls the second function with 1/probability
(: odds-on ((Any * -> Any) (Any * -> Any) Real -> (Any * -> Any)))
(define (odds-on choice alternative odds)
  (cond [(< (random) odds) choice]
        [else alternative]))

(define-type Odds (Pairof (Any * -> Any) Real))
(: odds-that ((Listof Odds) -> (Any * -> Any)))
;admittedly, I can't prove that this is an accurate algorithm for generating the specified distribution
;since it's all going to be hand-tuned odds anyhow, initial tests show that it is fit for its purpose
(define (odds-that odds)
  (letrec: ([total-probability : Real (apply + (map (λ: ([x : Odds]) (cdr x)) odds))]
            [pick : ((Listof Odds) -> (Any * -> Any))
                  (λ (odds)
                    (cond [(empty? odds) (error "No arguments to odds-that")]
                          [(empty? (rest odds)) (car (first odds))] ; never hit on alternates, this is the event
                          [(< (random) (cdr (first odds))) (caar odds)] ; hit! return this procedure
                          [else (pick (rest odds))]))])
    (if (= 1.0 total-probability)
        (pick (reverse (sort odds (λ: ([one : Odds] [other : Odds]) (> (cdr one) (cdr other))))))
        (error "Odds in argument to odds-that must total 1.0"))))

(define (pass)
  (error "NOT YET IMPLEMENTED"))

