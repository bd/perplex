#lang typed/racket
(require math/base)
(require racket/set)



;;the smallest unit of language, here, represented as a 1 character string
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

(define-type Expansion (Pairof String Lexicon))
;rather than evaluate directly, we need to preserve the particulars of an evaluation 
;so they can be interred in the lexicon for future use
;the Expansion type provides this 

; TODO: refactor/shadow as a stateful function
; which reduces its probability over time
; ultimately, may need a more involved strategy--
; want to constrain lower-level type->tokens to 
; top out early, but keep making new higher-level tokens
(define PROB-NEW-TOKEN .60) ; because _that's_ the probability of a new token

(define-type Expander (EmeType Grammar Lexicon -> Expansion))

(: expand Expander)
; expands a type into a string and lexicon pair (Expansion)
; this is a non-deterministic function
(define (expand type grammar lexicon)
  ((odds-on new-token old-token PROB-NEW-TOKEN) type grammar lexicon))

(: make-expansion (String Lexicon -> Expansion))
; constructor convenience function for Expansion
(define (make-expansion token lex)
  `(,token . ,lex))

(: token-of (Expansion -> String))
; accessor function for Expansion
(define (token-of e)
  (car e))

(: lexicon-of (Expansion -> Lexicon))
; accessor function for Expansion
(define (lexicon-of e)
  (cdr e))

; TODO: next iteration of perplex might be nicer 
; with more struct types--localizing information 
; about where in the grammatical hierarchy we are
; may simplify making some of the probabalistic
; decisions 

(: new-token Expander)
; generates a new token-- a new part of the language is used for the first time
; a new word is coined, a phoneme uttered
(define (new-token type grammar lexicon)
  (cond [(isAtomType? type grammar) (make-atom-emetype type grammar)]
        [else (let ([exp (collapse (expand-all (random-rhs-of type grammar) grammar lexicon))])
                (make-expansion (token-of exp) (update (lexicon-of exp) type (token-of exp))))]))

(: make-atom-emetype (EmeType Grammar -> Expansion))
; create an expansion with a random atomic token
(define (make-atom-emetype type grammar)
  (let: ([token : String (cast (first (select-random (lookup-productions grammar type))) String)])
    (make-expansion token (update (empty-lexicon) type token))))

(: expand-all (Production Grammar Lexicon -> (Listof Expansion)))
;expands each type in turn 
(define (expand-all types grammar lexicon)
  (map (λ: ((t : EmeType)) (expand t grammar lexicon)) types))

(: random-rhs-of (EmeType Grammar -> Production))
; pull a random production out of the grammar
(define (random-rhs-of type grammar)
  (select-random (lookup-productions grammar type)))

(: collapse ((Listof Expansion) -> Expansion))
; flatten a list of expansions into a single expansion representing the whole thing--
(define (collapse expansions)
  (letrec: ([token : String   (foldl (λ: ([e : Expansion] [working : String]) (string-append working (token-of e))) "" expansions)]
            [normalized-token  : String (clean-punctuation token)]
            [lexicon : Lexicon (foldl (λ: ([e : Expansion] [working : Lexicon]) (merge-lexica working (lexicon-of e))) (empty-lexicon) expansions)])
    (make-expansion normalized-token lexicon)))

(: clean-punctuation (String -> String))
; formats the punctuation characters in a more sensible manner
(define (clean-punctuation str)
  (string-normalize-spaces #:trim? #f (normalize-marks  str) #px" +" " "))


(: normalize-marks (String -> String))
; clean up punctuation: only the final punctuation mark in a run,
; except for elipses, which we first rewrite as % then back 
(define (normalize-marks str)
  (cast (regexp-replaces str '([#px"\\.{3}" "%"]                    ; ... -> %
                               [#px"\\W+([.:,.!?;])(.)"  "\\1 \\2"] ; run of marks -> final mark
                               [#px"\\W?%" "..."]                       ; % -> ...
                               [#px"(\\w)\\1+" "\\1"]               ; no runs of letters longer than 3
                               [#px"([:;])\\s+" "\\1 "]             ; no colon or semi-colon before newline
                               [#px" -" "-"]))                      ; no space before hyphen
        String))

(: old-token Expander)
; a pre-existing token is employed. 
(define (old-token type grammar lexicon)
  (cond [(inLexicon? type lexicon)(make-expansion (select-random (lookup-in-lexicon lexicon type)) lexicon)]
        [else (new-token type grammar lexicon)]))

(: inLexicon? (EmeType Lexicon -> Boolean))
(define (inLexicon? type lexicon)
  (hash-has-key? lexicon type))

(: isAtomType? (EmeType Grammar -> Boolean))
; true if the emetype is indivisible within a given grammar.
(define (isAtomType? type grammar)
  (let: ([productions : (Listof Production) (lookup-productions grammar type)])
    (and (not (empty? productions)) (Atom? (first (first productions))))))


(: merge-lexica (Lexicon Lexicon -> Lexicon))
; utility to combine two lexica into a single master lexicon
(define (merge-lexica a b)
  (letrec: ([merge-list : ((Listof String) (Listof String) -> (Listof String))
                        (λ: ([lst-a : (Listof String)]
                             [lst-b : (Listof String)])
                          (set->list (set-union (list->set lst-a) (list->set lst-b))))]
            [a-keys : (Listof EmeType) (hash-keys a)]
            [b-keys : (Listof EmeType) (hash-keys b)]
            [default : (-> '()) (λ () '())]) ;if we don't find the key, just return an empty list
    (cond [(empty? a-keys) b]
          [(empty? b-keys) a]
          [else (merge-lexica (hash-set a (first b-keys) (merge-list (hash-ref a (first b-keys) default)
                                                                     (hash-ref b (first b-keys)))) ;should always hit, I want an error if it doesn't
                              (hash-remove b (first b-keys)))])))
;; ---------- example of merge-lexica: -----------
;; > (define t (new-emetype)
;; > (define tokens-a (list "alpha" "beta"))
;; > (define tokens-b (list "beta" "gamma" "delta))
;; > (merge-lexica (make-immutable-hash (list `(,t . ,tokens-a))) (make-immutable-hash (list `(,t . ,tokens-b))))
;; - : Lexicon
;; '#hash((EmeType60344 . ("beta" "alpha" "delta" "gamma")))

(define GRAMMAR-INITIAL-PROBABILITY .005)
(define GRAMMAR-DECAY-RATE .055)

(: build-grammar ( -> Grammar))
;constructs a grammar for the language 
(define (build-grammar)
  (letrec: ([base-grammar : SubGrammar (root-grammar) ]
            [build : (Grammar Real Real -> Grammar)
                   (λ: ([grammar : Grammar]
                        [probability : Real]
                        [decay-rate : Real])
                     (cond [(done? grammar probability) grammar]
                           [else (build (cons (extend-grammar (hash-keys (first grammar))) grammar)
                                        (+ probability decay-rate)
                                        decay-rate)]))])
    (build (list base-grammar) GRAMMAR-INITIAL-PROBABILITY GRAMMAR-DECAY-RATE)))

(: root-grammar (-> SubGrammar))
;introducing more structure for aesthetic purposes 
; intended to make more "readable" gibberish. the 
; consonant/vowel divide simply makes the patterns
; a little more accessible on first glance.
;   | create a type for vowels, 
;   | a type for consonants, 
;   | and assign frequency distributions for that good human language feel
; the conceptual artist in me says this is adding too much preconceived notions of "language"
; the aesthete says fuckit, looks better. the aesthete is correct.
(define (root-grammar)
  (cast (make-immutable-hash (list `(,(new-emetype) . ,(frequency-distribute (list  '("w") '("z") '("v")  '("s") '("p") '("r") '("l") '("k") '("n") '("h") '("g") '("j")  '("c") '("f") '("x") '("t") '("q") '("m") '("d") '("b"))
                                                                             CONSONANT-FREQUENCIES))
                                   `(,(new-emetype) . ,(frequency-distribute (list '("i") '("u") '("o") '("e") '("y") '("a"))
                                                                             VOWEL-FREQUENCIES)))) SubGrammar))

(: perturb-distribution ((Listof Integer) Integer Integer -> (Listof Integer)))
; add variability within a range to a frequency distribution
; randomly adds or subtracts an integer between 'lower and 'upper
; to each frequency in 'distribution
(define (perturb-distribution distribution lower upper)
  (pass))

;both roughly based on old ETAOIN SHRDLCU
(define CONSONANT-FREQUENCIES '(9 7 6 6 6 4 4 3 2 2 2 2 2 2 1 1 1 1 1 1 ))
(define VOWEL-FREQUENCIES '(13 8 8 7 3 2))

(: repeat (All (T) (T Integer -> (Listof T))))
; create a list with 'item occuring 'times 
(define (repeat item times)
  (cond [(< times 1) '()]
        [else (cons item (repeat item (- times 1)))]))

(: frequency-distribute ((Listof (Listof String)) (Listof Integer) -> (Listof (Listof String))))
; returns a list of atomic productions (root grammar) with the 
; characters arbitrarily assigned repeated to reflect the frequency 
; distribution passed as the second argument
(define (frequency-distribute subgrammar histogram)
  (foldl (λ: ([c : (Listof String)] [f : Integer] [acc : (Listof (Listof String))])
           `(,@(repeat c f) ,@acc))
         '()
         (shuffle subgrammar)
         histogram))

(: extend-grammar ((Listof EmeType) -> SubGrammar))
;arbitrarily creates productions from the typeset argument and 
;and assigns  them to a randomly generated list of new higher-order EmeTypes
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

(define PRODUCTION-PROBABILITY .1)
(define PRODUCTION-DECAY .02)


(: random-production-set ((Listof EmeType) -> (Listof Production)))
;constructs a set of Productions (though returned as a list type for convenience)
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

(define PRODUCTION-COMPLETION-PROBABILITY .3)
(define PRODUCTION-COMPLETION-DECAY .2)


;
;(: random-production-2 ((Listof EmeType) Integer Integer -> Production ))
;; produces a random production of length between upper and lower
;(define (random-production-2 composing-types lower upper)
;  (letrec: ([histogram : (Listof Emetype) (frequency-distribute composing-types (generate-distribution (length composing-types)))]
;            [options : (Listof (Listof Production)) (permutations histogram)]
;            [grammar-rule: ((Listof (Listof Production)) -> Production)
;                           (λ: ([options : (Listof (Listof Production))]) (pass))])
;            (pass)))


(: random-production ((Setof EmeType) -> Production))
;constructs a random Production from a set of types
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


(: done? (All (T) (Listof T) Real -> Boolean))
;randomly decides if a list (i.e. Production, list of Production, etc. is to be considered complete, given a probability
;thus, the caller may generate a decay function s.t. the likelihood of the production's 
;being extended is diminished with each successive call
;this function is meant to put off the determination of the range of variability until the last moment
(define (done? production probability)
  (cond [(null? production) #f]
        [else  (< (random) probability)]))

;the character set [here: provided by the IBM Selectric II typewriter]
;(define SELECTRIC-CHARS (set "q" "w" "e" "r" "t" "y" "u" "i" "o" "p" "a" "s" "d" "f" "g" "h" "j" "k" "l" "z" "x" "c" "v" "b" "n" "m" ))


(: lookup-productions (Grammar EmeType ->  (Listof Production)))
; utility to find the productions for the given EmeType
(define (lookup-productions grammar type)
  (cond [(Atom? type) (error "Cannot use Atom as Key to SubGrammar")] 
        [(null? grammar) (error (string-append "no such type exists in grammar: " (symbol->string type)))]
        [(hash-has-key? (first grammar) type) (hash-ref (first grammar) type)]
        [else (lookup-productions (rest grammar) type)]))


(: update  (All (A B) (HashTable A (Listof B)) A B -> (HashTable A (Listof B))))
; utility function to simplify adding values to a data structure that will be  used frequently throughout
(define (update table key value)
  (cond [(hash-has-key? table key) (hash-set table key (cons value (hash-ref table key)))]
        [else (hash-set table key (list value))]))



(: select-random (All (T) ((Listof T) -> T)))
; utility function to select a random element of a list
(define (select-random lst) 
  (list-ref lst (random-integer 0 (length lst))))


(: lookup-in-lexicon (Lexicon EmeType -> (Listof String)))
; utility
(define (lookup-in-lexicon lexicon type)
  (hash-ref lexicon type))


(: new-emetype (-> EmeType))
; generates an arbitrary, unique symbol to use as the type name
; no one besides a maintainer of the program should encouter these names
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

(: odds-on (All (X Y) (X Y Real -> (U X Y))))
; - given two procedures and a probability, probabalistically return one procedure
; | choice is returned odds*100 percent of the time
; | alternative is returned (1 - odds)*100 percent of the time
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

; because I like that feature of python
; TODO: implement #pass syntax
(define (pass)
  (error "NOT YET IMPLEMENTED"))


(define NONE '(""))
(define SPACE '(" "))
(define INTRA-SENTENCE '(" " " " " " ", " ", " "-"))
(define INTER-SENTENCE '(". " ". " ". " ". " "! " "; " ": " "... " "? "))
(: append-newline (String -> String))
(define (append-newline x) (string-append x "\n"))
(define WITH-NEWLINE (map append-newline INTER-SENTENCE))
(define DOUBLE-NEWLINE (map append-newline WITH-NEWLINE))
(define ALL-PUNCTUATION `(,NONE ,NONE ,NONE ,SPACE ,INTRA-SENTENCE ,INTER-SENTENCE ,WITH-NEWLINE ,DOUBLE-NEWLINE))

(define-type PunctuationLayer (Pairof EmeType (Listof (Listof String))))

(: make-punctuation-layer ((Listof String) -> PunctuationLayer))
(define (make-punctuation-layer marks)
  `(,(new-emetype) . ,(map (λ: ([x : String]) (list x)) marks)))


;TODO: refactor punctuation phase into grammar build phase for clarity
(: punctuate ((Listof PunctuationLayer) Grammar -> Grammar))
; adds punctuate to an existing grammar
; | assumes a reverse-order (smallest units first) grammar
; | assumes a normal-order (smallest units first) list of PunctuationLayers
; | then applies the punctuation layer to the grammer, s.t. the smallest units 
;   of punctuation applied first
(define (punctuate punctuation grammar)
  (cond [(> (length punctuation) (length grammar)) (punctuate punctuation (lengthen-grammar grammar))]
        [(< (length punctuation) (length grammar)) (punctuate (lengthen-punctuation punctuation) grammar)]
        [else (foldl (λ: ([marks : PunctuationLayer] [subgrammar : SubGrammar] [acc : Grammar]) `(,(punctuate-subgrammar marks subgrammar) ,@acc)) 
                     '()
                     punctuation
                     `(,(add-punctuation-types punctuation (first grammar)) ,@(rest grammar)))]))

(: add-punctuation-types ((Listof PunctuationLayer) SubGrammar -> SubGrammar))
(define (add-punctuation-types punctuation subgrammar)
  (make-immutable-hash `(,@punctuation ,@(hash->list subgrammar))))


(: punctuate-subgrammar (PunctuationLayer SubGrammar -> SubGrammar))
; appends the EmeType of marks to the end of each production in subgrammar
(define (punctuate-subgrammar marks subgrammar)
  (letrec: ([keys : (Listof EmeType) (hash-keys subgrammar)]
            [punctuation-type : EmeType (car marks)]
            [punctuate-lhs : ((Listof EmeType) SubGrammar EmeType -> SubGrammar)
                           (λ (types sg punc-type)
                             (cond [(empty? types) sg]
                                   [else (punctuate-lhs (rest types) 
                                                        (hash-set sg (first types) (map (λ: ([prod : Production]) (cond [(not (string? (first prod)))`(,@prod ,punc-type)]
                                                                                                                        [else prod]))
                                                                                        (hash-ref sg (first types))))
                                                        punc-type)]))])
    (punctuate-lhs keys subgrammar punctuation-type)))

(: lengthen-grammar (Grammar -> Grammar))
; extends a grammar by adding a layer of grammar above the top
(define (lengthen-grammar grammar)
  `(,(extend-grammar (hash-keys (first grammar))) ,@grammar))

(: lengthen-punctuation ((Listof PunctuationLayer) -> (Listof PunctuationLayer)))
; used when there are not enough punctuation layers to punctuate a grammar
; simply extend with double-newline punctuation layers.
; TODO: consider a probabalistic function to alternately add NONE layers...
(define (lengthen-punctuation punctuation)
  `(,@punctuation ,(make-punctuation-layer DOUBLE-NEWLINE)))

(: top-lhs-of (Grammar -> (Listof EmeType)))
; convenience function for composing
(define (top-lhs-of grammar)
  (hash-keys (first grammar)))

(: random-top-lhs-of (Grammar -> EmeType))
; convenience function for composing 
(define (random-top-lhs-of grammar)
  (select-random (top-lhs-of grammar)))

(define ALL-PUNCTUATION-LAYERS (map (λ: ([x : (Listof String)]) (make-punctuation-layer x)) ALL-PUNCTUATION))
(define G (punctuate ALL-PUNCTUATION-LAYERS (reverse (build-grammar))))
(define e (expand (random-top-lhs-of G) G (empty-lexicon)))
(displayln  (string-trim (token-of e)))

(: compose (Grammar Lexicon -> Expansion))
(define (compose grammar lexicon)
  (expand (random-top-lhs-of grammar) grammar lexicon))

(: write-some (Language -> Expansion))
(define (write-some language)
  (compose (mcar language) (mcdr language)))

(define-type Language (MPairof Grammar Lexicon))

(: make-language (Grammar Lexicon -> Language))
(define (make-language grammar lexicon)
  (mcons grammar lexicon))

(define L (make-language G (lexicon-of e)))

(: more ( -> Void))
(define (more) 
  (begin (define e-prime (write-some L)) 
         (set-mcdr! L (lexicon-of e-prime))
         (displayln (token-of e-prime))))



