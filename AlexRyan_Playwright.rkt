#lang mzscheme

(#%require (only srfi/13 string-trim-both string-tokenize string-titlecase))
(#%require (only srfi/14 char-set char-set-complement))
(#%require srfi/69)
(#%require mzlib/defmacro)
(#%require "General.scm")

(define (_case key . cases) 
  (if (not (null? cases))
      (if (eq? key (caar cases))
          (eval (cadar cases))
          (apply _case key (cdr cases)))))

;; vector utilities
(define (vector-map-index i proc vec)
  (if (< i (vector-length vec))
      (begin 
        (apply proc (list (vector-ref vec i)))
        (vector-map-index (+ i 1) proc vec))))

(define (vector-map proc vec)
  (vector-map-index 0 proc vec))
  
;; string utilities
(define (string-chop-last str n)
  (substring str 0 (- (string-length str) n)))

(define (string-repeat-last str n)
    (string-append str (substring str (- (string-length str) n))))

(define (char-next ch)
  (integer->char (+ (char->integer ch) 1)))

;;; ===== DICTIONARY DATA STRUCTURES ===== ;;;

;; STRUCTURE: WORD
; The first structure is what I am calling a "word." It has a string
; representation, and a tag used for inflexions
(define (word-make str pron tag syllables)
  (vector str pron tag syllables))

(define (word-punc str)
  (word-make str "" "" "0"))

(define (is-punc? word)
  (eq? 0 (word-syllables word)))

(define (word-string word)
  (vector-ref word 0))

(define (word-pronunciation word)
  (vector-ref word 1))

(define (word-tag word)
  (vector-ref word 2))

(define (word-syllables word)
  (string->number (substring (vector-ref word 3) 0 1)))

(define no-commas (char-set-complement (char-set #\,)))
(define (list-words str pron tags syllables)
  (map (lambda x (word-make str pron (car x) syllables)) (string-tokenize tags no-commas)))

;; word rarities
(define (word-rarity word)
  (string-ref (word-tag word) 2))

(define COMMON-WORD '#\*)
(define UNCOMMON-WORD '#\%)
;(define RARE-WORD #\$)

;; word parts of speech
  
(define (word-part-of-speech word)
  (string-ref (word-tag word) 0))

(define POS-ANOMALOUS-VERB #\G)
(define POS-TRANSITIVE-VERB #\H)
(define POS-INTRANSITIVE-VERB #\I)
(define POS-TRANS-INTRANS-VERB #\J)

(define POS-COUNTABLE-NOUN #\K)
(define POS-UNCOUNTABLE-NOUN #\L)
(define POS-COUNT-UNCOUNT-NOUN #\M)
(define POS-PROPER-NOUN #\N)

(define POS-ADJECTIVE #\O)
(define POS-ADVERB #\P)
(define POS-PRONOUN #\Q)
(define POS-DEFINITE-ARTICLE #\R)
(define POS-INDEFINITE-ARTICLE #\S)
(define POS-PREPOSITION #\T)
(define POS-PREFIX #\U)
(define POS-CONJUNCTION #\V)
(define POS-INTERJECTION #\W)
(define POS-PARTICLE #\X)
(define POS-ABBREVIATION #\Y)
(define POS-NOT-CLASSIFIED #\Z)

(define POS-FIRST POS-ANOMALOUS-VERB)
(define POS-LAST POS-NOT-CLASSIFIED)

;; word conjugation information

(define (word-conj word)
  (string-ref (word-tag word) 1))
                            
;; STRUCTURE: WORD SUBCOLLECTION
; The second structure that I make extensive use of is a "word subcollection," which
; encapsulates a comparatively collection of words which are externally categorized

(define (word-subcollection-make) (vector #f '()))
(define (word-subcollection-add sub word)
  (vector-set! sub 1 (cons word (word-subcollection-data sub))))
(define (word-subcollection-data sub) (vector-ref sub 1))
(define (word-subcollection-finalized? sub) (vector-ref sub 0))
(define (word-subcollection-finalize sub)
  (begin
    (vector-set! sub 1 (list->vector (word-subcollection-data sub)))
    (vector-set! sub 0 #t)))
(define (word-subcollection-ref sub pos)
  (vector-ref (word-subcollection-data sub) pos))
(define (word-subcollection-random sub) 
  (let ((len (vector-length (word-subcollection-data sub))))
    (word-subcollection-ref sub (random len))))
(define (word-subcollection-empty? sub)
  (= (vector-length (word-subcollection-data sub)) 0))

;;; STRUCTURE: WORD COLLECTION
; A word collection is simply three word subcollections, for each of three word
; rarity categories as specified in the data that I am using.

(define (word-collection-make)
  (vector 0 (vector 20 (word-subcollection-make)) (vector 35 (word-subcollection-make)) (vector 10 (word-subcollection-make))))

(define (word-collection-sub collection i)
  (vector-ref (vector-ref collection i) 1))
(define (word-collection-add collection word)
  (let ((sub 
         (case (word-rarity word)
           ((#\*) (word-collection-sub collection 1))
           ((#\%) (word-collection-sub collection 2))
           ((#\$) (word-collection-sub collection 3)))))
    (word-subcollection-add sub word)))

(define (word-collection-finalize-index collection i)
  (if (< i 4)
      (let ((sub (vector-ref (vector-ref collection i) 1)))
        (word-subcollection-finalize sub)
        (if (word-subcollection-empty? sub)
            (vector-set! (vector-ref collection i) 0 0))
        (word-collection-finalize-index collection (+ i 1)))))

(define (word-collection-finalize collection)
  (begin
    (word-collection-finalize-index collection 1)
    (vector-set! collection 0 (fold-left (lambda (x y) (+ x (vector-ref y 0))) 0 (cdr (vector->list collection))))))

(define (word-collection-random-index collection i rand)
  (let* ((elem (vector-ref collection i))
         (weight (vector-ref elem 0)))
    (if (< rand weight)
        (word-subcollection-random (vector-ref elem 1))
        (word-collection-random-index collection (+ i 1) (- rand weight)))))

(define (word-collection-random collection)
  (word-collection-random-index collection 1 (random (vector-ref collection 0))))

(define (dictnode-make func . nodes)
  (if (eq? func 0)
      (vector 0 (word-collection-make))
      (let ((node-map (make-hash-table)))
        (map (lambda x (hash-table-set! node-map (caar x) (apply dictnode-make (cdar x)))) nodes)
        (vector func node-map))))

(define (dictnode-func node)
  (vector-ref node 0))

(define (dictnode-child node)
  (vector-ref node 1))

(define (dictnode-add node word)
  (let* ((func (dictnode-func node))
         (val (dictnode-child node)))
    (if (eq? func 0)
        (word-collection-add val word)
        (dictnode-add (hash-table-ref val (func word)) word))))

(define (dictnode-get node path)
  (if (null? path)
      node
      (dictnode-get (hash-table-ref (dictnode-child node) (car path)) (cdr path))))

(define (dictnode-random node path)
  (let ((child (dictnode-get node path)))
    (word-collection-random (dictnode-child child))))

(define (dictnode-addall node words)
  (map (lambda x (dictnode-add node (car x))) words))

(define (dictnode-finalize node)
  (if (eq? (dictnode-func node) 0)
      (word-collection-finalize (dictnode-child node))
      (hash-table-walk (dictnode-child node) (lambda (x y) (dictnode-finalize y)))))



;;; ===== STRUCTURE: DICTIONARY =====
; A dictionary is a very complex manifestation of a 'dictnode'. See DICTIONARY-HIERARCHY.

;; -- VERB DEFINITIONS --

(define (is-infinitive? verb)
  (char-numeric? (word-conj verb)))

(define VERB-INFINITIVE #t)
(define VERB-CONJUGATED #f)

(define CONJ-3RD-SING #\a)
(define CONJ-PRESENT-PART #\b)
(define CONJ-PAST #\c)
(define CONJ-PAST-PART #\d)
(define CONJ-UNKNOWN #\e)

(define CONJ-CONTRACT-PRONOUN #\f)
(define CONJ-CONTRACT-NOT #\g)
(define CONJ-OTHER-CONTRACTION #\h)

(define VERB-HIERARCHY 
  `(,is-infinitive?
    (,VERB-INFINITIVE 0)
    (,VERB-CONJUGATED ,word-conj 
                      (,CONJ-3RD-SING 0) 
                      (,CONJ-PRESENT-PART 0) 
                      (,CONJ-PAST 0) 
                      (,CONJ-PAST-PART 0) 
                      (,CONJ-UNKNOWN 0) 
                    ; - only anomalous verbs below
                      (,CONJ-CONTRACT-PRONOUN 0) 
                      (,CONJ-CONTRACT-NOT 0) 
                      (,CONJ-OTHER-CONTRACTION 0))))

;; -- NOUN DEFINITIONS --

(define (is-singular? noun)
  (let ((ch (word-conj noun)))
    (or (char-numeric? ch) (char=? ch #\i) (char=? ch #\k))))

(define NOUN-SINGULAR #t)
(define NOUN-PLURAL #f)

(define PROPER-NOUN-FORENAME #\l)
(define PROPER-NOUN-REGION #\m)
(define PROPER-NOUN-CITY #\n)
(define PROPER-NOUN-OTHER #\o)

(define COMMON-NOUN-HIERARCHY
  `(,is-singular?
    (,NOUN-SINGULAR 0)
    (,NOUN-PLURAL 0)))

(define PROPER-NOUN-HIERARCHY
  `(,word-conj
     (,PROPER-NOUN-FORENAME 0)
     (,PROPER-NOUN-REGION 0)
     (,PROPER-NOUN-CITY 0)
     (,PROPER-NOUN-OTHER 0)))

;; -- ADJECTIVE DEFINITIONS --

(define ADJECTIVE-PREDICATE #\p)
(define ADJECTIVE-ATTRIBUTE #\q)
(define ADJECTIVE-COMPARE #\r)
(define ADJECTIVE-SUPERLATIVE #\s)
(define ADJECTIVE-HYPHEN #\t)

(define (is-attribute? word)
  (eq? (word-conj word) #\q))

(define ADJECTIVE-OTHER #f)
(define ADJECTIVE-GOOD #t)

(define ADJECTIVE-HIERARCHY
  `(,is-attribute?
    (,ADJECTIVE-GOOD 0)
    (,ADJECTIVE-OTHER 0)))

;; -- ADVERB DEFINITIONS --

(define ADVERB-NORMAL #\u)
(define ADVERB-CONJUNCTIVE #\^)
(define ADVERB-INTERROGATIVE #\v)
(define ADVERB-RELATIVE #\w)
(define ADVERB-PARTICLE #\+)

(define ADVERB-HIERARCHY
  `(,word-conj
    (,ADVERB-NORMAL 0)
    (,ADVERB-CONJUNCTIVE 0)
    (,ADVERB-INTERROGATIVE 0)
    (,ADVERB-RELATIVE 0)
    (,ADVERB-PARTICLE 0)))

;; -- CONJUNCTION DEFINITIONS --

(define CONJUNCTION-COORDINATING #\~)
(define CONJUNCTION-SUBORDINATING #\^)
(define CONJUNCTION-OTHER #\-)

(define CONJUNCTION-HIERARCHY
  `(,word-conj
    (,CONJUNCTION-COORDINATING 0)
    (,CONJUNCTION-SUBORDINATING 0)
    (,CONJUNCTION-OTHER 0)))

;; -- PRONOUN DEFINITIONS --

(define PRONOUN-NORMAL #\x)
(define PRONOUN-INTERROGATIVE #\y)
(define PRONOUN-RELATIVE #\z)
(define PRONOUN-POSSESSIVE #\A)

(define PRONOUN-HIERARCHY
  `(,word-conj
    (,PRONOUN-NORMAL 0)
    (,PRONOUN-INTERROGATIVE 0)
    (,PRONOUN-RELATIVE 0)
    (,PRONOUN-POSSESSIVE 0)))

;; ------------------ LE GRAND DICTIONARY HIERARCHY ------------------ ;;

(define DICTIONARY-HIERARCHY
  `(,word-part-of-speech
    ,(cons POS-ANOMALOUS-VERB       VERB-HIERARCHY)
    ,(cons POS-TRANSITIVE-VERB      VERB-HIERARCHY)
    ,(cons POS-INTRANSITIVE-VERB    VERB-HIERARCHY)
    ,(cons POS-TRANS-INTRANS-VERB   VERB-HIERARCHY)
    
    ,(cons POS-COUNTABLE-NOUN       COMMON-NOUN-HIERARCHY)
    (,POS-UNCOUNTABLE-NOUN 0)
    ,(cons POS-COUNT-UNCOUNT-NOUN   COMMON-NOUN-HIERARCHY)
    ,(cons POS-PROPER-NOUN          PROPER-NOUN-HIERARCHY)
    
    ,(cons POS-ADJECTIVE            ADJECTIVE-HIERARCHY)
    
    ,(cons POS-ADVERB               ADVERB-HIERARCHY)
    
    ,(cons POS-PRONOUN              PRONOUN-HIERARCHY)
    
    (,POS-DEFINITE-ARTICLE 0)
    (,POS-INDEFINITE-ARTICLE 0)
    (,POS-PREPOSITION 0)
    (,POS-PREFIX 0)
    ,(cons POS-CONJUNCTION          CONJUNCTION-HIERARCHY)
    (,POS-INTERJECTION 0)
    (,POS-PARTICLE 0)
    (,POS-ABBREVIATION 0)
    (,POS-NOT-CLASSIFIED 0)))

(define (dictionary-make)
  (dictnode-make DICTIONARY-HIERARCHY))

(define (dictionary-finalize dict)
  (dictnode-finalize dict))

;;; ===== FILE READING -> DICTIONARY ===== ;;;

(define (dictionary-read dict input-file)
  (let ((port (open-input-file input-file)))
    (begin
      (dictionary-readall dict port)
      (close-input-port port))))

(define (dictionary-readall dict port)
  (let ((line (read-line port)))
    (if (not (eof-object? line))
      (begin
        (dictionary-add-input dict line)
        (dictionary-readall dict port)))))

(define (word-data-tokenize line)
  (list (substring line 0 23) (substring line 23 46) (substring line 46 69) (substring line 69 127)))

(define (word-data-tokenize-trim line)
  (map string-trim-both (word-data-tokenize line)))

(define (dictionary-add-input dict line)
  (dictnode-addall dict (apply list-words (word-data-tokenize-trim line))))

(define my-dict (apply dictnode-make DICTIONARY-HIERARCHY))
(define (rand-word . body)
  (dictnode-random my-dict body))


;(dictionary-read my-dict "dict.txt")
(dictionary-read my-dict "base.txt")
;(dictionary-read my-dict "h.txt")
;(dictionary-read my-dict "b.txt")
;(dictionary-read my-dict "f.txt")
(dictionary-read my-dict "p.txt")
(dictionary-finalize my-dict)

;;; ===== RANDOM FUNCTION APPLICATION ===== ;;;
;;;
;;; Allows for definition of functions as:
;;;
;;;     (define-rand (random-function) '((function-1 weight-1) (function-2 weight-2) ...))
;;;
;;; where weight-1, ..., weight-n are nonnegative integers and represent the relative
;;; frequencies that the functions function-1, ..., function-n will be called.
;;; 

; the weight of a specific element of the random function list, 
; i.e. weight-i in the list (function-i weight-i)
(define (func-weight elem)
  (cadr elem))

; finds the weight-sum of the list
(define (assoc-sum lst)
  (if (null? lst)
      0
      (+ (func-weight (car lst)) (assoc-sum (cdr lst)))))

; evaluates the appropriate randomized function in the random function list 'lst',
; at the randomized position 'weight' in the range of [0, (assoc-sum lst)).
(define (retrieve-elem lst weight)
  (let ((first-weight (func-weight (car lst))))
    (if (< weight first-weight)
        (caar lst)
        (retrieve-elem (cdr lst) (- weight first-weight)))))

; randomized function macro definition
(defmacro define-rand-func (func . body)
  `(define (,(car func))
    (let ((body-sum (assoc-sum ,@body)))
     (apply (eval (retrieve-elem ,@body (random body-sum))) '()))))

(defmacro define-rand-elem (func . body)
  `(define (,(car func))
    (let ((body-sum (assoc-sum ,@body)))
     (retrieve-elem ,@body (random body-sum)))))

(define (string-append-lambda . args)
  (lambda () (apply string-append args)))

;;; ============================================================================================= ;;;
;;; =================================== ENGLISH GRAMMAR RULES =================================== ;;;
;;; ============================================================================================= ;;;

(define %blank (word-punc ""))
(define %nil '())
(define (%nil-func0) %nil)
(define (%nil-func2 x y) %nil)

(define %comma (word-punc ","))
(define %period (word-punc "."))
(define %question (word-punc "?"))
(define %exclamation (word-punc "!"))
(define %semicolon (word-punc ";"))

(define %and (word-make "and" "&nd" "V~*" "1"))
(define %that (word-make "that" "D&t" "Pu%,Qx%,Qz*,V^*" "1"))
(define %which (word-make "which" "wItS" "OA*,Qy*,Qz*" "1"))
(define %not (word-make "not" "n0t" "Pu*" "1"))

(define %a (word-make "a" "@" "S-*" "1"))
(define %an (word-make "an" "@n" "S-*" "1"))
(define %the (word-make "the" "D@" "Pu$,R-*" "1"))

(define %would (word-make "would" "wUd" "Gc*,Hc%" "1"))
(define %will (word-make "will" "wIl" "G5*,J0%,M6%" "1"))
(define %should (word-make "should" "SUd" "G5*" "1"))
(define %may (word-make "may" "meI" "G5*,Ga*" "1"))
(define %might (word-make "might" "maIt" "Gc*,L@%" "1"))
(define %could (word-make "could" "kUd" "Gc*" "1"))
(define %must (word-make "must" "mVst" "G5*,M6%" "1"))

(define %am (word-make "am" "&m" "Ge*,Ie%" "1"))
(define %is (word-make "is" "Iz" "Ga*,Ia%" "1"))
(define %are (word-make "are" "AR" "Ge*,Ie*,K6$" "1"))
(define %was (word-make "was" "w0z" "Gc*,Ic%" "1"))
(define %were (word-make "were" "w3R" "Gc*,Ic%" "1"))

(define %be (word-make "be" "bi" "G5*,I5%" "1"))
(define %been (word-make "been" "bin" "Gd*,Id%" "1"))

(define %has (word-make "has" "h&z" "Ga*,Ja*" "1"))
(define %have (word-make "have" "h&v" "G5*,J5*" "1"))
(define %had (word-make "had" "h&d" "Gc*,Gd*,Jc*,Jd*" "1"))

;;; STRUCT: phrase
(define (phrase . words)
  (phrase-condense words))

(define (phrase-condense words)
  (if (null? words)
      words
      (if (list? words)
          (append (phrase-condense (car words)) (phrase-condense (cdr words)))
          (list words))))

(define (phrase-syllables phrase)
  (fold-right (lambda (x y) (+ (word-syllables x) y)) 0 phrase))

(define (str-overwrite-last orig new)
  (string-append (substring orig 0 (- (string-length orig) 1)) new))

(define (phrase-parse-part str begin? punc? phrase)
  (if (null? phrase)
      str
      (let ((token (car phrase)))
        (if (is-punc? token)
            (if punc?
                (phrase-parse-part (str-overwrite-last str (word-string token)) #f #t (cdr phrase))
                (phrase-parse-part (string-append str (word-string token)) #f #t (cdr phrase)))
            (if begin?
                (phrase-parse-part (string-titlecase (word-string token)) #f #f (cdr phrase))
                (phrase-parse-part (string-append str " " (word-string token)) #f #f (cdr phrase)))))))

(define (phrase-parse phrase)
  (phrase-parse-part "" #t #f phrase))

(define (rand-sentence) 
  (phrase-parse (%sentence)))

;;; ====== GRAMMAR RULES ======


;;; --- CLAUSES ---

(define (%adverb-punc begin end)
  (if (eq? %nil begin)
      (phrase (rand-word POS-ADVERB ADVERB-NORMAL) end)
      (rand-word POS-ADVERB ADVERB-NORMAL)))

(define (%sub-clause-punc begin end) (phrase begin (%sub-clause) end))
(define-rand-elem (%possible-sub-clause-func) `((,%sub-clause-punc 1) (,%nil-func2 16) (,%adverb-punc 4)))
(define (%possible-sub-clause begin end) ((%possible-sub-clause-func) begin end))

(define-rand-elem (%possible-adverb-func) `((,%adverb-punc 1) (,%nil-func2 2)))
(define (%possible-adverb begin end) ((%possible-adverb-func) begin end))

(define-rand-func (%sub-clause-start) '(((lambda () (rand-word POS-CONJUNCTION CONJUNCTION-SUBORDINATING)) 1)))
(define (%sub-clause) (phrase (%sub-clause-start) (%indep-clause)))

(define-rand-elem (rand-plural) '((#t 1) (#f 2)))
(define (%indep-clause) (%indep-clause-pl (rand-plural)))
(define-rand-elem (%indep-clause-pl-random) 
  `(
    (,%indep-clause-pl-intrans 2) 
    (,%indep-clause-pl-trans 5)
    ))

(define (%indep-clause-pl plural?) ((%indep-clause-pl-random) plural?))

(define (%indep-clause-pl-trans-no-obj plural?) 
  (phrase ((%noun plural?)) (%possible-adverb %nil %nil) ((%verb plural?) POS-TRANSITIVE-VERB)))

(define (%indep-clause-pl-intrans plural?)
  (phrase (%possible-adverb %nil %nil) ((%noun plural?)) (%possible-adverb %nil %nil) ((%verb plural?) POS-INTRANSITIVE-VERB) (%possible-adverb %nil %nil) (%possible-prepositional-phrase) (%possible-prepositional-phrase)))

(define (%indep-clause-pl-trans plural?)
  (phrase (%possible-adverb %nil %nil) (%indep-clause-pl-trans-no-obj plural?) 
          ((%noun (rand-plural))) (%possible-prepositional-phrase) (%possible-prepositional-phrase) (%possible-adverb %nil %comma)))

(define (%noun plural)
  (if (eq? plural #t)
      %noun-pl
      %noun-sing))

(define (%verb plural)
  (if (eq? plural #t)
      %verb-pl
      %verb-sing))

;;; --- SENTENCES ---
(define (%sentence) (phrase (%sentence-random) (%end-punc-random)))
(define-rand-elem (%end-punc-random) `((,%exclamation 1) (,%period 9)))                            
(define-rand-func (%sentence-random) '((%sentence-single 5) (%sentence-compound 2)))
(define (%sentence-single) (phrase (%possible-sub-clause %nil %comma) (%indep-clause) (%possible-sub-clause %comma %nil) %comma))
(define (%sentence-compound-conjunction) (phrase (%sentence-single) (rand-word POS-CONJUNCTION CONJUNCTION-COORDINATING) (%sentence-single)))
(define (%sentence-compound-adverb) (phrase (%sentence-single) %semicolon (rand-word POS-ADVERB ADVERB-CONJUNCTIVE) %comma (%sentence-single)))
(define-rand-func (%sentence-compound) '((%sentence-compound-conjunction 5) (%sentence-compound-adverb 1)))

;; prepositional phrases
(define (%prepositional-phrase) (phrase (rand-word POS-PREPOSITION) (%noun-pl)))
(define-rand-func (%possible-prepositional-phrase) '((%prepositional-phrase 1) (%nil-func0 6)))

;; adjectives
(define-rand-func (%possible-adj) '((%nil-func0 15) (%adj-1 9) (%adj-2 3) (%adj-3 1)))
(define (%adj-1) (rand-word POS-ADJECTIVE ADJECTIVE-GOOD))
(define (%adj-2) (phrase (rand-word POS-ADJECTIVE ADJECTIVE-GOOD) %comma (rand-word POS-ADJECTIVE ADJECTIVE-GOOD)))
(define (%adj-3) (phrase (rand-word POS-ADJECTIVE ADJECTIVE-GOOD) %comma (rand-word POS-ADJECTIVE ADJECTIVE-GOOD) %comma %and (rand-word POS-ADJECTIVE ADJECTIVE-GOOD)))

(define-rand-func (%possible-adj-clause) '((%nil-func0 20) (%adj-clause 1)))
(define-rand-func (%adj-clause) '((%that-adj-clause 3) (%which-adj-clause 1)))
(define (%that-adj-clause) (phrase %that (%indep-clause-pl-trans-no-obj (rand-plural))))
(define (%which-adj-clause) (phrase %comma %which (%indep-clause-pl-trans-no-obj (rand-plural)) %comma))

;; nouns
(define-rand-func (%noun-pl) 
  '(((lambda () (%noun-phrase NOUN-PLURAL)) 10) 
    ((lambda () (phrase (%noun-phrase-no-clause NOUN-SINGULAR) %and (%noun-phrase NOUN-SINGULAR))) 2)
    ((lambda () (phrase (%noun-phrase-no-clause NOUN-SINGULAR) %comma (%noun-phrase-no-clause NOUN-SINGULAR) %comma %and (%noun-phrase NOUN-SINGULAR))) 2)))

(define (%noun-sing) (%noun-phrase NOUN-SINGULAR))
(define (%noun-phrase-no-clause plurality) (phrase %the (%possible-adj) (rand-word POS-COUNTABLE-NOUN plurality)))
(define (%noun-phrase plurality) (phrase (%noun-phrase-no-clause plurality) (%possible-adj-clause)))

;; === VERB TENSES === 
  
(define-rand-elem (%verb-pl-func) `((,%verb-simple-present-pl 4) 
                                    (,%verb-simple-past 4) 
                                    (,%verb-simple-future 3) 
                                    (,%verb-present-continuous-pl 3) 
                                    (,%verb-past-continuous-pl 3) 
                                    (,%verb-future-continuous 3)
                                    (,%verb-present-perfect-pl 2)
                                    (,%verb-past-perfect 2)
                                    (,%verb-future-perfect 2)
                                    (,%verb-present-perfect-continuous-pl 1)
                                    (,%verb-past-perfect-continuous 1)
                                    (,%verb-future-perfect-continuous 1)))

(define-rand-elem (%verb-sing-func) `((,%verb-simple-present-sing 4) 
                                      (,%verb-simple-past 4) 
                                      (,%verb-simple-future 3) 
                                      (,%verb-present-continuous-sing 3) 
                                      (,%verb-past-continuous-sing 3) 
                                      (,%verb-future-continuous 3)
                                      (,%verb-present-perfect-sing 2)
                                      (,%verb-past-perfect 2)
                                      (,%verb-future-perfect 2)
                                      (,%verb-present-perfect-continuous-sing 1)
                                      (,%verb-past-perfect-continuous 1)
                                      (,%verb-future-perfect-continuous 1)))


(define (%verb-pl trans) ((%verb-pl-func) trans))
(define (%verb-sing trans) ((%verb-sing-func) trans))

(define-rand-func (%random-inflect) `(((lambda () (phrase (%random-modal) %not)) 2) (%random-modal 5)))
(define-rand-elem (%random-modal) `((,%would 3) (,%will 8) (,%should 2) (,%may 1) (,%might 2) (,%could 2) (,%must 2)))
(define (%verb-simple-present-sing trans) (rand-word trans VERB-CONJUGATED CONJ-3RD-SING))
(define (%verb-simple-present-pl trans) (rand-word trans VERB-INFINITIVE))
(define (%verb-simple-past trans) (rand-word trans VERB-CONJUGATED CONJ-PAST))
(define (%verb-simple-future trans) (phrase (%random-inflect) (rand-word trans VERB-INFINITIVE)))

(define (%verb-present-continuous-sing trans) (phrase %is (rand-word trans VERB-CONJUGATED CONJ-PRESENT-PART)))
(define (%verb-present-continuous-pl trans) (phrase %are (rand-word trans VERB-CONJUGATED CONJ-PRESENT-PART)))
(define (%verb-past-continuous-sing trans) (phrase %was (rand-word trans VERB-CONJUGATED CONJ-PRESENT-PART)))
(define (%verb-past-continuous-pl trans) (phrase %were (rand-word trans VERB-CONJUGATED CONJ-PRESENT-PART)))
(define (%verb-future-continuous trans) (phrase (%random-inflect) %be (rand-word trans VERB-CONJUGATED CONJ-PRESENT-PART)))

(define (%verb-present-perfect-sing trans) (phrase %has (rand-word trans VERB-CONJUGATED CONJ-PAST-PART)))
(define (%verb-present-perfect-pl trans) (phrase %have (rand-word trans VERB-CONJUGATED CONJ-PAST-PART)))
(define (%verb-past-perfect trans) (phrase %had (rand-word trans VERB-CONJUGATED CONJ-PAST-PART)))
(define (%verb-future-perfect trans) (phrase (%random-inflect) %have (rand-word trans VERB-CONJUGATED CONJ-PAST-PART)))

(define (%verb-present-perfect-continuous-sing trans) (phrase %has %been (rand-word trans VERB-CONJUGATED CONJ-PRESENT-PART)))
(define (%verb-present-perfect-continuous-pl trans) (phrase %have %been (rand-word trans VERB-CONJUGATED CONJ-PRESENT-PART)))
(define (%verb-past-perfect-continuous trans) (phrase %had %been (rand-word trans VERB-CONJUGATED CONJ-PRESENT-PART)))
(define (%verb-future-perfect-continuous trans) (phrase (%random-inflect) %have %been (rand-word trans VERB-CONJUGATED CONJ-PRESENT-PART)))
         