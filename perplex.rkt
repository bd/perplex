#lang racket
(require racket/set)
(require math/base)

;these are the absolute smallest units of composition for the perplex.
;they are changeable depending on the target of the perplex's output.
;in this case they are determined by the character set of the IBM selec-
;tric II typewriter's type element
(define atoms '(q w e r t y u i o p a s d f g h j k l z x c v b n m 1 2 3 4 5 6 7 8 9 0))

(define (rand-atom)
  (random-element atoms))

;generates a new uninterned symbol prefixed with "type" to be used as a compoundType name
(define (new-unique-identifier)
  (gensym "type"))
           
        

; -> string
(define (rand-char-lower)
  (let [(letters '(q w e r t y u i o p a s d f g h j k l z x c v b n m ))]
    (symbol->string (random-element letters))))

;integer -> string  
(define (rand-word length)
  (local ([define build-rand-word (lambda (length word)
           (cond [(> 1 length) word]
                 [else (build-rand-word (- length 1) (string-append word (rand-char-lower)))]))])
    (build-rand-word length "")))

; ->string
(define (make-new-word)
  (let [(n (+ 1 (random 12)))]
    (rand-word n)))


(define (random-boolean) (random-element (list #t #f)))



; listof string -> listof string
;the car of the return value is the word to use
(define (lexicon lex)
  (let ([new-word? (or (random-boolean)(< (length lex) 10)) ])
  (cond [new-word? (cons (make-new-word) lex)]
        [else (cons (string-copy (random-element lex)) lex)])))

(define (mk-sentence sentence-lex)
  (local ([define sentence (car sentence-lex)]
          [define lex (cdr sentence-lex)]
          [define updated-lex (lexicon lex)])
    (cond [(random-boolean)  (mk-sentence (cons (cat sentence (whitespace-pad (car updated-lex)))
                                                 updated-lex))]
          [else (cons sentence (list lex))])))

(define lex '())


(define (write letter)
  (local ([define updated-lex (lexicon lex)]
          [define next-word (car updated-lex)])
  (cond [(or (random-boolean) 
             (random-boolean)
             (random-boolean)) (begin   
                                (set! lex updated-lex)
                                (write (cat ( whitespace-pad letter ) next-word)))]
         [( < .80 (random)) (write (punctuate letter))]
         [else (cat letter ".")])))
 

(define (new-sentence lex)
    (punctuate ( car (mk-sentence (cons  "" lex)))))        


(define (random-element collection)
  (local ([define (rand-elem list) 
            (list-ref list (random (length list)))])
  (cond [(set? collection) (rand-elem (set->list collection))]
        [(list? collection) (rand-elem collection)]
        [else (error "Collection type not supported...yet.")])))

; integer -> string
(define (rand-sentence length)
  (local ([define build-rand-sentence (lambda (length sentence)
                                        (cond [(> 1 length) sentence]
                                              [else (build-rand-sentence (- length 1) 
                                                                         (string-append (whitespace-pad sentence)
                                                                                        (make-new-word)))]))])
    (punctuate (build-rand-sentence length (make-new-word)))))

;string-> string
(define (cat s1 s2)
  (string-append s1 s2))

;string -> string
(define (punctuate sentence)
  (cat sentence (symbol->string (random-element '(\. : \; \, ?)))))

;string -> string
(define (whitespace-pad string)
  (cat string " "))
                                             
;-> string
(define (make-new-sentence)
  (rand-sentence (+ 1 (random 12))))

;import python feature:
(define pass '())

(struct compound (representation type))
(struct compoundType (name rules))



;these grammars describe the bedrock of what can be considered a language
;and will be commmon to any language produced by the perplex.
(define baseGrammar (list
                     (compoundType 'numeral '(1 2 3 4 5 6 7 8 9 0))
                     (compoundType 'number '(numeral (maybe \.) (may-repeat (maybe numeral))))
                     (compoundType 'atom atoms)
                     (compoundType 'space '(may-repeat space))
                     ))

;this is the grammar set the perplex is producing as it writes the text
;the language that emerges produces and is produced by the act of 
;writing in the language
(define theGrammar (set baseGrammar))

;this is the lexicon the perplex is producing.
;it is a mapping from compoundTypes to components which match those types
;i.e., the lexicon 
(define theLexicon (make-hash))



;creates a new pseudolinguistic compoundType, which is an 
;abstract representation of some level of linguistic struc-
;ture
;
;i.e., a sentence type might be created with the representation
;(compoundType 'sentence ((compoundType 'word (<word grammar goes here>)
(define (make-new-type)
  (compoundType (new-unique-identifier) (new-random-grammar theGrammar)))
   
;a list of lists describing trees
;of compoundTypes
;[can add statistical behavior here, later,
;by creating weighted edges ]
(define (new-random-grammar)
  (pass))


