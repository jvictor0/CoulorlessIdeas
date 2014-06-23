#lang mzscheme

(#%require (only srfi/13 string-trim-both string-tokenize string-titlecase))
(#%require (only srfi/14 char-set char-set-complement))
(#%require srfi/69)
(#%require mzlib/defmacro)
(#%require srfi/1)
(#%require mzlib/string)

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
    (vector-set! collection 0 (fold (lambda (y x) (+ x (vector-ref y 0))) 0 (cdr (vector->list collection))))))

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
(define (is-adjective-good? verb)
  (or (eq? (word-conj verb) #\A)
      (eq? (word-conj verb) #\B)
      (eq? (word-conj verb) #\C)
      (eq? (word-conj verb) #\D)
      (eq? (word-conj verb) #\E)))

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
  `(,is-adjective-good?
    (,ADJECTIVE-GOOD 0)
    (,ADJECTIVE-OTHER ,word-conj
		      (,ADJECTIVE-COMPARE 0)
		      (,ADJECTIVE-PREDICATE 0)
		      (,ADJECTIVE-ATTRIBUTE 0)
		      (,ADJECTIVE-SUPERLATIVE 0)
		      (,ADJECTIVE-HYPHEN 0))))
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
(define (rand-word-lit . body)
  (dictnode-random my-dict body))
(define (rand-word . body)
  (let ((nr (apply rand-word-lit body)))
    (vector (string-append ;(vector-ref nr 0) 
			   "("
			   (apply string-append 
				  (map (lambda (x) 
					 (if (boolean? x)
					     (if x "t" "f")
					     (format "~a" x)))
				       body))
			   ")")
	    (vector-ref nr 1)
	    (vector-ref nr 2)
	    (vector-ref nr 3))))


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
  `(define (,@func)
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
(define %dotdotdot (word-punc "..."))
(define %question (word-punc "?"))
(define %exclamation (word-punc "!"))
(define %semicolon (word-punc ";"))

(define %and (word-make "and" "&nd" "V~*" "1"))
(define %that (word-make "that" "D&t" "Pu%,Qx%,Qz*,V^*" "1"))
(define %which (word-make "which" "wItS" "OA*,Qy*,Qz*" "1"))
(define %not (word-make "not" "n0t" "Pu*" "1"))

(define %I (word-make "I" "@" "S-*" "1"))
(define %we (word-make "we" "@" "S-*" "1"))

(define %a (word-make "a" "@" "S-*" "1"))
(define %an (word-make "an" "@n" "S-*" "1"))
(define %the (word-make "the" "D@" "Pu$,R-*" "1"))
(define %than (word-make "than" "D@" "Pu$,R-*" "1"))
(define %some (word-make "some" "" "Pu$,R-*" "1"))
(define %those (word-make "those" "" "Pu$,R-*" "1"))



(define %would (word-make "would" "wUd" "Gc*,Hc%" "1"))
(define %wouldnt (word-make "wouldn't" "wUd" "Gc*,Hc%" "1"))
(define %will (word-make "will" "wIl" "G5*,J0%,M6%" "1"))
(define %should (word-make "should" "SUd" "G5*" "1"))
(define %may (word-make "may" "meI" "G5*,Ga*" "1"))
(define %might (word-make "might" "maIt" "Gc*,L@%" "1"))
(define %could (word-make "could" "kUd" "Gc*" "1"))
(define %must (word-make "must" "mVst" "G5*,M6%" "1"))

(define %am (word-make "am" "&m" "Ge*,Ie%" "1"))
(define %is (word-make "is" "Iz" "Ga*,Ia%" "1"))
(define %isnt (word-make "isn't" "Iz" "Ga*,Ia%" "1"))
(define %arent (word-make "aren't" "Iz" "Ga*,Ia%" "1"))
(define %like (word-make "like" "Iz" "Ga*,Ia%" "1"))
(define %are (word-make "are" "AR" "Ge*,Ie*,K6$" "1"))
(define %was (word-make "was" "w0z" "Gc*,Ic%" "1"))
(define %were (word-make "were" "w3R" "Gc*,Ic%" "1"))
(define %to (word-make "to" "w3R" "Gc*,Ic%" "1"))


(define %be (word-make "be" "bi" "G5*,I5%" "1"))
(define %been (word-make "been" "bin" "Gd*,Id%" "1"))

(define %has (word-make "has" "h&z" "Ga*,Ja*" "1"))
(define %have (word-make "have" "h&v" "G5*,J5*" "1"))
(define %had (word-make "had" "h&d" "Gc*,Gd*,Jc*,Jd*" "1"))

(define %one (word-make "one" "" "" "1"))
(define %two (word-make "two" "" "" "1"))
(define %three (word-make "three" "" "" "1"))
(define %four (word-make "four" "" "" "1"))
(define %five (word-make "five" "" "" "1"))
(define %six (word-make "six" "" "" "1"))

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
                (phrase-parse-part (string-first-upper (word-string token)) #f #f (cdr phrase))
                (phrase-parse-part (string-append str " " (word-string token)) #f #f (cdr phrase)))))))


(define (string-first-upper str)
  (string-append (string-titlecase (substring str 0 1))
		 (substring str 1 (string-length str))))

(define (phrase-parse phrase)
  (phrase-parse-part "" #t #f (drop-while is-punc? phrase)))

(define (mkfakeword x)
  (word-make x "" "" "1"))

(define (lit-phrase ph)
  (apply phrase (map mkfakeword (regexp-split " " ph))))

(define (rand-sentence) 
  (phrase-parse (%sentence)))
(define (rand-sentence-bother) 
  (phrase-parse (%sentence-bother)))

(define (rand-conversation) 
  (phrase-parse (%advice))
  (phrase-parse (%question-sentence)))

;;; ====== GRAMMAR RULES ======


;;; --- CLAUSES ---

(define (%adverb-punc begin end)
  (if (eq? %nil begin)
      (phrase (rand-word POS-ADVERB ADVERB-NORMAL) end)
      (rand-word POS-ADVERB ADVERB-NORMAL)))

(define (%sub-clause-punc begin end) (phrase begin (%sub-clause) end))
(define-rand-elem (%possible-sub-clause-func) `((,%sub-clause-punc 1) (,%nil-func2 12)))
(define (%possible-sub-clause begin end) ((%possible-sub-clause-func) begin end))

(define-rand-elem (%possible-adverb-func) `((,%adverb-punc 1) (,%nil-func2 3)))
(define (%possible-adverb begin end) ((%possible-adverb-func) begin end))

(define-rand-func (%sub-clause-start) '(((lambda () (%subordinating-conjunction)) 1)))
(define (%sub-clause) (phrase (%sub-clause-start) (%indep-clause)))

(define-rand-elem (rand-plural) '((#t 1) (#f 2)))
(define (%indep-clause) (%indep-clause-pl (rand-plural)))
(define (%subjunctive-clause) (%subjunctive-clause-pl (rand-plural)))
(define (%conditional-clause) (%conditional-clause-pl (rand-plural)))
(define-rand-elem (%indep-clause-pl-random) 
  `(
    (,%indep-clause-pl-intrans-2 3) 
    (,%indep-clause-pl-intrans-3 2) 
    (,%indep-clause-pl-trans-2 6)
    (,%indep-clause-pl-trans-3 3)

    (,%indep-clause-pl-intrans-ego-2 5) 
    (,%indep-clause-pl-intrans-ego-3 2) 
    (,%indep-clause-pl-trans-ego-2 6)
    (,%indep-clause-pl-trans-ego-3 3)
    
    (,%being-clause 7)
    ))

(define-rand-func (%command) 
  `(
    ((lambda () (phrase (rand-word POS-INTRANSITIVE-VERB VERB-INFINITIVE) (%possible-adverb %nil %nil))) 1)
    ((lambda () (phrase (rand-word POS-TRANSITIVE-VERB VERB-INFINITIVE) (%object (rand-plural) ''you) (%possible-adverb %nil %nil))) 1)
    ((lambda () (phrase (%possible-adverb %nil %nil) (rand-word POS-TRANSITIVE-VERB VERB-INFINITIVE) (%object (rand-plural) ''you))) 1)
    ((lambda () (phrase %be (%being-target #f ''you))) 3)
    ))



(define (%present-question)
  (let ((sub-and-tag (%subject-and-tag (rand-plural))))
    (phrase (%present-question-word (cdr sub-and-tag)) (car sub-and-tag) (%present-question-suffix (cdr sub-and-tag)) %question)))

(define (%transitive-question)
  (let ((sub-and-tag (%subject-and-tag (rand-plural))))
    (phrase (%transitive-question-word) (rand-word POS-COUNTABLE-NOUN NOUN-SINGULAR)
	    (%simple-question-word (cdr sub-and-tag)) (car sub-and-tag)
	    (rand-word POS-TRANSITIVE-VERB VERB-INFINITIVE)
	    (%likely-prepositional-phrase)
	    (%transitive-question-suffix) %question)))

(define (%being-question)
  (let* ((plurality (rand-plural))
         (subj-and-tag (%subject-and-tag plurality)))
    (phrase (%to-be (%fix-am-not (cdr subj-and-tag))) (car subj-and-tag)  (%being-target plurality (cdr subj-and-tag))
	    %question)))

(define (%fix-am-not p)
  (if (equal? p (lit-phrase "am not"))
      (lit-phrase "aren't")
      p))

(define-rand-func (%present-question-word pronoun)
  `(((lambda () (lit-phrase "did")) 1)
    ((lambda () (lit-phrase "when did")) 1)
    ((lambda () (lit-phrase "where did")) 1)
    ((lambda () (lit-phrase "why did")) 1)
    ((lambda () (lit-phrase "how did")) 1)
    ((lambda () (lit-phrase "didn't")) 1)
    ((lambda () (lit-phrase "why didn't")) 1)
    ((lambda () (lit-phrase "why can't")) 1)
    ((lambda () (lit-phrase "can't")) 1)
    ((lambda () (lit-phrase "can")) 1)  
    ((lambda () (lit-phrase "how can")) 1)  
    ((lambda () (lit-phrase "where can")) 1)  
    ((lambda () (lit-phrase "when can")) 1)  
    ((lambda () (%do-verb ',pronoun)) 2)
    ((lambda () (phrase (lit-phrase "why") (%do-verb ',pronoun))) 1)
    ((lambda () (phrase (lit-phrase "where") (%do-verb+ ',pronoun))) 1)
    ((lambda () (phrase (lit-phrase "how") (%do-verb+ ',pronoun))) 1)
    ((lambda () (phrase (lit-phrase "when") (%do-verb+ ',pronoun))) 1)
    ))
(define-rand-func (%simple-question-word pronoun)
  `(((lambda () (lit-phrase "did")) 1)
    ((lambda () (lit-phrase "didn't")) 1)
    ((lambda () (lit-phrase "can't")) 1)
    ((lambda () (lit-phrase "can")) 1)  
    ((lambda () (%do-verb ',pronoun)) 2)
    ))
(define-rand-func (%transitive-question-word)
  `(((lambda () (lit-phrase "which")) 1)
    ((lambda () (lit-phrase "whose")) 1)
    ((lambda () (lit-phrase "what")) 1)
    ))

(define-rand-func (%do-verb pronoun)
  `(((lambda () (%do-verb+ ',pronoun)) 4)
    ((lambda () (%do-verb- ',pronoun)) 1)))
(define (%do-verb+ pronoun)
  (lit-phrase 
   (cond 
    ((equal? pronoun ''I) "do")
    ((equal? pronoun ''you) "do")
    ((equal? pronoun ''he) "does")
    ((equal? pronoun ''she) "does")
    ((equal? pronoun ''it) "does")
    ((equal? pronoun ''they) "do")
    ((equal? pronoun ''we) "do")
    ((equal? pronoun ''sing) "does")
    ((equal? pronoun ''plural) "do")
    (else (error pronoun)))))
(define (%do-verb- pronoun)
  (lit-phrase (cond 
	       ((equal? pronoun ''I) "don't")
	       ((equal? pronoun ''you) "don't")
	       ((equal? pronoun ''he) "doesn't")
	       ((equal? pronoun ''she) "doesn't")
	       ((equal? pronoun ''it) "doesn't")
	       ((equal? pronoun ''they) "don't")
	       ((equal? pronoun ''we) "don't")
	       ((equal? pronoun ''sing) "doesn't")
	       ((equal? pronoun ''plural) "don't")
	       (else (error pronoun)))))

(define-rand-func (%present-question-suffix pronoun) 
  `(
    ((lambda () (phrase (rand-word POS-INTRANSITIVE-VERB VERB-INFINITIVE) (%possible-adverb %nil %nil))) 1)
    ((lambda () (phrase (rand-word POS-TRANSITIVE-VERB VERB-INFINITIVE) (%object (rand-plural) ',pronoun) (%possible-adverb %nil %nil))) 1)
    ((lambda () (phrase (%possible-adverb %nil %nil) (rand-word POS-TRANSITIVE-VERB VERB-INFINITIVE) (%object (rand-plural) ',pronoun))) 1)
    ))


(define-rand-elem (%subjunctive-clause-pl-random) 
  `(
    (,%subjunctive-clause-pl-intrans 5) 
    (,%subjunctive-clause-pl-trans 8)
    (,%subjunctive-clause-pl-infin-intrans 5) 
    (,%subjunctive-clause-pl-infin-trans 8)

    (,%being-clause-subjunctive 3)
    ))
(define-rand-elem (%conditional-clause-pl-random) 
  `(
    (,%conditional-clause-pl-intrans 5) 
    (,%conditional-clause-pl-trans 8)
    (,%conditional-clause-pl-infin-intrans 5) 
    (,%conditional-clause-pl-infin-trans 8)

    (,%being-clause-conditional 3)
    ))


(define (%indep-clause-pl plural?) ((%indep-clause-pl-random) plural?))
(define (%subjunctive-clause-pl plural?) ((%subjunctive-clause-pl-random) plural?))
(define (%conditional-clause-pl plural?) ((%conditional-clause-pl-random) plural?))

(define (%conditional-helper)
  (phrase (%would-or-wouldnt) (%possible-conditional-filler)))

;; These could be generalized, but why?
;;  
(define-rand-elem (%would-or-wouldnt)
  `((,%would 8)
    (,%wouldnt 2)
    (,(lit-phrase "would never") 1)
    (,(lit-phrase "would maybe") 2)
    (,(lit-phrase "would perhaps") 1)
    (,(lit-phrase "would always") 1)
    (,(lit-phrase "would almost surely") 1)
    (,(lit-phrase "would almost always") 1)
    (,(lit-phrase "would at least sometimes") 1)
    (,(lit-phrase "would at least occasionally") 1)))
(define-rand-elem (%possible-conditional-filler)
  `((,%nil 15)
    (,(lit-phrase "be able to") 2)
    (,(lit-phrase "have enough to") 1)
    (,(lit-phrase "have what it takes to") 1)
    (,(lit-phrase "want to") 2)
    (,(lit-phrase "refuse to") 1)
    (,(lit-phrase "need to") 2)
    (,(lit-phrase "have to") 1)
    (,(lit-phrase "get to") 1)
    (,(lit-phrase "try to") 1)
    (,(lit-phrase "think I could") 1)
    (,(lit-phrase "think I should") 1)))

(define (%indep-clause-pl-trans-no-obj plural? . no-adverb) 
  (phrase (%noun plural?) 
	  (fix-helper (if (null? no-adverb) (%possible-adverb %nil %nil) %nil)
		      ((%verb plural?) POS-TRANSITIVE-VERB))))

(define (%indep-clause-pl-intrans-1 plural?)
  (phrase (%possible-adverb %nil %nil) (%noun plural?)  ((%verb plural?) POS-INTRANSITIVE-VERB) (%possible-prepositional-phrase)))
(define (%indep-clause-pl-intrans-2 plural?)
  (phrase  (%noun plural?) (fix-helper (%possible-adverb %nil %nil) ((%verb plural?) POS-INTRANSITIVE-VERB)) (%possible-prepositional-phrase)))
(define (%indep-clause-pl-intrans-3 plural?)
  (phrase (%noun plural?) ((%verb plural?) POS-INTRANSITIVE-VERB) (%possible-adverb %nil %nil) (%possible-prepositional-phrase)))

(define (%subjunctive-clause-pl-intrans plural?)
  (phrase (%subject plural?) %were (%possible-adverb %nil %nil) 
	  (rand-word POS-INTRANSITIVE-VERB VERB-CONJUGATED CONJ-PRESENT-PART)
	  (%possible-prepositional-phrase)))
(define (%subjunctive-clause-pl-infin-intrans plural?)
  (phrase (%subject plural?) %were %to (%possible-adverb %nil %nil) 
	  (rand-word POS-INTRANSITIVE-VERB VERB-INFINITIVE)
	  (%possible-prepositional-phrase)))

(define (%conditional-clause-pl-intrans plural?)
  (phrase (%subject plural?) (%conditional-helper) %be (%possible-adverb %nil %nil) 
	  (rand-word POS-INTRANSITIVE-VERB VERB-CONJUGATED CONJ-PRESENT-PART)
	  (%possible-prepositional-phrase)))
(define (%conditional-clause-pl-infin-intrans plural?)
  (phrase (%subject plural?) (%conditional-helper) (%possible-adverb %nil %nil) 
	  (rand-word POS-INTRANSITIVE-VERB VERB-INFINITIVE)
	  (%possible-prepositional-phrase)))



(define (%indep-clause-pl-intrans-ego-1 plural?)
  (phrase (%possible-adverb %nil %nil) (if plural? %we %I)
	  ((%verb-ego plural?) POS-INTRANSITIVE-VERB) (%possible-prepositional-phrase)))
(define (%indep-clause-pl-intrans-ego-2 plural?)
  (phrase  (if plural? %we %I) (fix-helper (%possible-adverb %nil %nil) ((%verb-ego plural?) POS-INTRANSITIVE-VERB)) (%possible-prepositional-phrase)))
(define (%indep-clause-pl-intrans-ego-3 plural?)
  (phrase (if plural? %we %I) ((%verb-ego plural?) POS-INTRANSITIVE-VERB) (%possible-adverb %nil %nil) (%possible-prepositional-phrase)))


(define (%indep-clause-pl-trans-1 plural?)
  (phrase (%possible-adverb %nil %nil) (%indep-clause-pl-trans-no-obj plural? 'no-adverb) 
          (%object (rand-plural)) (%possible-prepositional-phrase)))
(define (%indep-clause-pl-trans-2 plural?)
  (phrase (%indep-clause-pl-trans-no-obj plural?) 
          (%object (rand-plural)) (%possible-prepositional-phrase)))
(define (%indep-clause-pl-trans-3 plural?)
  (phrase (%indep-clause-pl-trans-no-obj plural? 'no-adverb) 
          (%object (rand-plural)) (%possible-adverb %nil %nil) (%possible-prepositional-phrase)))

(define (%subjunctive-clause-pl-trans plural?)
  (let ((subj-and-tag (%subject-and-tag plural?)))
    (phrase (car subj-and-tag) %were (%possible-adverb %nil %nil) 
	    (rand-word POS-TRANSITIVE-VERB VERB-CONJUGATED CONJ-PRESENT-PART)
	    (%object (rand-plural) (cdr subj-and-tag)) (%possible-prepositional-phrase))))
(define (%subjunctive-clause-pl-infin-trans plural?)
  (let ((subj-and-tag (%subject-and-tag plural?)))
    (phrase (car subj-and-tag) %were %to (%possible-adverb %nil %nil) 
	    (rand-word POS-TRANSITIVE-VERB VERB-INFINITIVE)
	    (%object (rand-plural) (cdr subj-and-tag)) (%possible-prepositional-phrase))))


(define (%conditional-clause-pl-trans plural?)
  (let ((subj-and-tag (%subject-and-tag plural?)))
    (phrase (car subj-and-tag) (%conditional-helper) %be (%possible-adverb %nil %nil) 
	    (rand-word POS-TRANSITIVE-VERB VERB-CONJUGATED CONJ-PRESENT-PART)
	    (%object (rand-plural) (cdr subj-and-tag)) (%possible-prepositional-phrase))))
(define (%conditional-clause-pl-infin-trans plural?)
  (let ((subj-and-tag (%subject-and-tag plural?)))
    (phrase (car subj-and-tag) (%conditional-helper) (%possible-adverb %nil %nil) 
	    (rand-word POS-TRANSITIVE-VERB VERB-INFINITIVE)
	    (%object (rand-plural) (cdr subj-and-tag)) (%possible-prepositional-phrase))))

  

(define (%indep-clause-pl-trans-ego-1 plural?)
  (phrase (%possible-adverb %nil %nil) (if plural? %we %I)  ((%verb-ego plural?) POS-TRANSITIVE-VERB)
          (%object (rand-plural) (if plural? ''we ''I)) (%possible-prepositional-phrase)))
(define (%indep-clause-pl-trans-ego-2 plural?)
  (phrase (if plural? %we %I)  (fix-helper (%possible-adverb %nil %nil) ((%verb-ego plural?) POS-TRANSITIVE-VERB))
          (%object (rand-plural) (if plural? ''we ''I)) (%possible-prepositional-phrase)))
(define (%indep-clause-pl-trans-ego-3 plural?)
  (phrase (if plural? %we %I) ((%verb-ego plural?) POS-TRANSITIVE-VERB)
          (%object (rand-plural) (if plural? ''we ''I)) (%possible-adverb %nil %nil) (%possible-prepositional-phrase)))


(define (%being-clause plurality)
  (let ((subj-and-tag (%subject-and-tag plurality)))
    (phrase (car subj-and-tag) (%to-be (cdr subj-and-tag)) (%being-target plurality (cdr subj-and-tag)))))
(define (%being-clause-subjunctive plurality)
  (let ((subj-and-tag (%subject-and-tag plurality)))
    (phrase (car subj-and-tag) %were (%being-target plurality (cdr subj-and-tag)))))
(define (%being-clause-conditional plurality)
  (let ((subj-and-tag (%subject-and-tag plurality)))
    (phrase (car subj-and-tag) (%conditional-helper) %be (%being-target plurality (cdr subj-and-tag)))))



(define-rand-func (%to-be tag) 
  `(((lambda () (%to-be+ ',tag)) 3)
    ((lambda () (%to-be- ',tag)) 1)))
(define (%to-be+ tag)
  (cond ((equal? tag ''I) %am)
	((equal? tag ''sing) %is)
	((equal? tag ''he) %is)
	((equal? tag ''she) %is)
	((equal? tag ''it) %is)
	((equal? tag ''plural) %are)
	((equal? tag ''you) %are)
	((equal? tag ''they) %are)
	((equal? tag ''we) %are)))
(define (%to-be- tag)
  (cond ((equal? tag ''I) (lit-phrase "am not"))
	((equal? tag ''sing) %isnt)
	((equal? tag ''he) %isnt)
	((equal? tag ''she) %isnt)
	((equal? tag ''it) %isnt)
	((equal? tag ''plural) %arent)
	((equal? tag ''you) %arent)
	((equal? tag ''they) %arent)
	((equal? tag ''we) %arent)))


(define-rand-func (%being-target plurality pronoun)
 `(((lambda () (rand-word POS-PROPER-NOUN PROPER-NOUN-FORENAME)) 1)
   ((lambda () (cond 
		((equal? ,pronoun 'I) (lit-phrase "myself"))
		((equal? ,pronoun 'we) (lit-phrase "ourselves"))
		(else (if ,plurality (lit-phrase "us") (lit-phrase "me")))))
    1)
   ((lambda () (cond 
		((equal? ,pronoun 'you) (lit-phrase "yourself"))
		(else (lit-phrase "you")))) 
    1)		   
   ((lambda () (phrase (%article (not ,plurality)) (rand-word POS-COUNTABLE-NOUN (not ,plurality)))) 1)
   ((lambda () (phrase %like (%article (not ,plurality)) (rand-word POS-COUNTABLE-NOUN (not ,plurality)))) 1)
   ((lambda () (%prepositional-phrase)) 1)
   ((lambda () (%adj)) 1)
   ((lambda () (phrase %like (%article (not ,plurality)) (rand-word POS-COUNTABLE-NOUN (not ,plurality)) (%adj-clause))) 1)
   ((lambda () (phrase (%article (not ,plurality)) (rand-word POS-COUNTABLE-NOUN (not ,plurality)) (%adj-clause))) 1)
   ((lambda () (phrase (%adj-comparative) %than (%object ,plurality ',pronoun))) 1)
   ((lambda () (phrase (%adj-comparative-1) %than (%object ,plurality ',pronoun) %and 
		       (%adj-comparative-1) %than (%object ,plurality ',pronoun))) 1)
   ((lambda () (phrase %the (%adj-superlative (not ,plurality)))) 1)))

(define (fix-helper adverb verb)
  (if (not (list? verb))
      (phrase adverb verb)
      (phrase (take-while is-helper verb) adverb (drop-while is-helper verb))))

(define (%noun plural . simple)
  (if (null? simple)
      (%noun plural #f)
      (if plural
	  (%noun-pl (car simple))
	  (%noun-sing (car simple)))))

(define (%verb plural)
  (if (eq? plural #t)
      %verb-pl
      %verb-sing))

(define-rand-elem (%possible-prefix)
  `((,%nil 30) 
    (,(lit-phrase "I think") 2)
;    (,(phrase (lit-phrase "as a taxpayer") %comma) 2)
    (,(lit-phrase "I'm bothered that") 2)
    (,(lit-phrase "I believe that") 2)
    (,(lit-phrase "I don't think") 2) 
    (,(phrase (lit-phrase "imo") %comma) 1)
    (,(phrase (lit-phrase "hey") %comma) 1)
    (,(phrase (lit-phrase "yo") %comma) 1)	
    (,(phrase (lit-phrase "imho") %comma) 1)
    (,(lit-phrase "I heard") 2)
    (,(lit-phrase "they say") 2)
    (,(lit-phrase "no offense but") 2)
    (,(phrase (lit-phrase "honestly") %comma) 2)
    (,(phrase (lit-phrase "you know what") %comma) 2)
    (,(phrase (lit-phrase "however") %comma) 2)
    (,(lit-phrase "but") 2)
    (,(lit-phrase "I hate that") 2)
    (,(lit-phrase "I love that") 2)
    (,(phrase (lit-phrase "eww") %comma) 1)
    ))

(define-rand-elem (%second-conditional-prefix)
  `((,(lit-phrase "if") 5)
    (,(lit-phrase "if only") 1)))

(define-rand-elem (%transitive-question-suffix)
  `((,%nil 8)
    (,(lit-phrase "and why") 2)
    (,(lit-phrase "and what for") 2)
    (,(lit-phrase "and for what") 2)
    (,(phrase %comma (lit-phrase "seriously")) 1)
    (,(phrase %comma (lit-phrase "really")) 1)))

  
(define-rand-elem (%subjunctive-prefix)
  `((,(lit-phrase "but what if") 2)
    (,(lit-phrase "imagine if") 2)    
    (,(lit-phrase "but imagine if") 2)    
    (,(lit-phrase "what would happen if") 2)))


(define-rand-elem (%possible-suffix)
  `((,%nil 25) 
    (,(phrase %comma (lit-phrase "you know")) 1)
    (,(phrase %comma (lit-phrase "but I don't know why")) 1)
    (,(phrase %comma (lit-phrase "at least I think")) 1)
    (,(phrase %comma (lit-phrase "and I'm excited about it")) 1)
    (,(phrase %comma (lit-phrase "and I'm stoked")) 1)
    (,(phrase %comma (lit-phrase "and I don't care")) 1)
    (,(phrase %comma (lit-phrase "but don't take my word for it")) 1)
    (,(phrase %dotdotdot (lit-phrase "nevermind")) 1)
    ))


(define-rand-elem (%question-adapted-prefix)
  `((,(lit-phrase "do you think") 1)
    (,(lit-phrase "do you believe") 1)
    (,(lit-phrase "did you know") 1)
    (,(lit-phrase "did you hear") 1)
    (,(lit-phrase "are you saying that") 1)
    (,(lit-phrase "why do you think") 1)
    (,(lit-phrase "do you know if") 1)
    (,(lit-phrase "don't you hate that") 1)
    (,(lit-phrase "don't you love that") 1)
    (,(lit-phrase "what if") 1)
    ))
  
(define-rand-elem (%advice-prefix)
  `((,(lit-phrase "I think you should") 1)
    (,(lit-phrase "you should") 1)
    (,(lit-phrase "you ought to") 1)
    (,(lit-phrase "you ought not") 1)
    (,(lit-phrase "you shoudn't") 1)
    (,(lit-phrase "please") 2)
    (,(lit-phrase "please don't") 1)
    (,(lit-phrase "you need to") 1)
    (,(lit-phrase "you don't have to") 1)
    (,(lit-phrase "I have to ask you to") 1)
    ))
  

;;; --- SENTENCES ---
(define-rand-func (%sentence) 
  `(((lambda () (phrase (%possible-prefix) (%sentence-random) (%end-punc-random))) 3)
    ((lambda () (phrase (%sentence-random) (%possible-suffix) (%end-punc-random))) 2)
    ((lambda () (phrase (%second-conditional-prefix) (%subjunctive-clause) %comma (%conditional-clause) (%end-punc-random))) 3)
    ((lambda () (phrase (%conditional-clause) (%second-conditional-prefix) (%subjunctive-clause) (%end-punc-random))) 3)
    ((lambda () (phrase (%subjunctive-prefix) (%subjunctive-clause) %question)) 2)
    (,%advice 2)
    (,%question-sentence 6))) 
 
(define-rand-func (%sentence-bother) 
  `(((lambda () (phrase (%possible-prefix) (%sentence-random) (%end-punc-random))) 3)
    ((lambda () (phrase (%sentence-random) (%possible-suffix) (%end-punc-random))) 2)
    ((lambda () (phrase (%second-conditional-prefix) (%subjunctive-clause) %comma (%conditional-clause) (%end-punc-random))) 3)
    ((lambda () (phrase (%conditional-clause) (%second-conditional-prefix) (%subjunctive-clause) (%end-punc-random))) 3)
    ((lambda () (phrase (%subjunctive-prefix) (%subjunctive-clause) %question)) 2)
    (,%advice 15)
    (,%question-sentence 20)))  

(define-rand-elem (%end-punc-random) `((,%exclamation 1) (,%period 9)))                            
(define-rand-func (%sentence-random) '((%sentence-single 5) (%sentence-compound 2)))
(define (%sentence-single) (phrase  (%possible-sub-clause %comma %comma) (%indep-clause) (%possible-sub-clause %comma %nil) %comma))
(define (%sentence-compound-conjunction) (phrase (%sentence-single) (%coordinating-conjunction) (%sentence-single)))
(define (%sentence-compound-adverb) (phrase (%sentence-single) %semicolon (rand-word POS-ADVERB ADVERB-CONJUNCTIVE) %comma (%sentence-single)))
(define-rand-func (%sentence-compound) '((%sentence-compound-conjunction 5) (%sentence-compound-adverb 1)))
(define (%advice) (phrase (%advice-prefix) (%command) (%possible-sub-clause %nil %nil) (%end-punc-random)))
(define-rand-elem (%question-sentence)
  `((,(%present-question) 2)
    (,(%transitive-question) 1)
    (,(%being-question) 3)))


;; prepositional phrases
(define (%prepositional-phrase) (phrase (%preposition-sing) (%noun-phrase-no-clause (rand-plural))))
(define-rand-func (%possible-prepositional-phrase) '((%prepositional-phrase 1)(%nil-func0 6)))
(define-rand-func (%likely-prepositional-phrase) '((%prepositional-phrase 1)(%nil-func0 1)))

(define-rand-elem (%preposition-sing)
  `((,(lit-phrase "across from") 1)
    (,(lit-phrase "around") 1)
    (,(lit-phrase "beyond") 1)
    (,(lit-phrase "by") 1)
    (,(lit-phrase "for") 2)
    (,(lit-phrase "like") 3)
    (,(lit-phrase "near") 1)
    (,(lit-phrase "with") 1)
    (,(lit-phrase "without") 1)))



;; articles
(define (%article plurality)
  (if plurality (%article-sing) (%article-pl)))
(define (%article-indef plurality)
  (if plurality (%article-sing-indef) (%article-pl-indef)))
(define-rand-elem (%article-pl) `((,%the 10) (,%some 8) (,%those 16) (,%two 1) (,%three 1) (,%four 1) (,%five 1) (,%six 1)))
(define-rand-elem (%article-sing) `((,%the 2) (,%a 2) (,%that 4) (,%some 1)))
(define-rand-elem (%article-pl-indef) `((,%some 8) (,%those 16) (,%two 1) (,%three 1) (,%four 1) (,%five 1) (,%six 1)))
(define-rand-elem (%article-sing-indef) `((,%a 3) (,%some 1)))


;;conjunctions 

(define-rand-elem (%coordinating-conjunction)
  `((,(lit-phrase "and") 1)
    (,(lit-phrase "or") 1)
    (,(lit-phrase "but") 1)
    (,(lit-phrase "yet") 1)
    (,(lit-phrase "for" ) 1)
    (,(lit-phrase "so") 1)))

(define-rand-elem (%subordinating-conjunction)
  `((,(lit-phrase "after") 1)
    (,(lit-phrase "till") 1)
    (,(lit-phrase "if" ) 1)
    (,(lit-phrase "unless") 1)
    (,(lit-phrase "as" ) 1)
    (,(lit-phrase "inasmuch") 1)   
    (,(lit-phrase "until") 1)
    (,(lit-phrase "as if" ) 1)
    (,(lit-phrase "in order that") 1) 
    (,(lit-phrase "when") 1)
    (,(lit-phrase "as long as") 1) 
    (,(lit-phrase "lest" ) 1)
    (,(lit-phrase "whenever") 1)
    (,(lit-phrase "as much as" ) 1)
    (,(lit-phrase "now that" ) 1)
    (,(lit-phrase "where") 1)
    (,(lit-phrase "as soon as") 1) 
    (,(lit-phrase "provided" ) 1)
    (,(lit-phrase "wherever") 1)
    (,(lit-phrase "as though" ) 1)
    (,(lit-phrase "since" ) 1)
    (,(lit-phrase "while") 1)
    (,(lit-phrase "because" ) 1)
    (,(lit-phrase "so that") 1)
    (,(lit-phrase "before" ) 1)
    (,(lit-phrase "even if") 1)
    (,(lit-phrase "even though") 1)))

;; adjectives
(define-rand-func (%possible-adj) '((%nil-func0 100) (%adj-1 8) (%adj-2 1) (%adj-2-and 1) (%adj-3 1)))
(define-rand-func (%adj) '((%adj-1 8) (%adj-2-and 3) (%adj-3 1)))
(define (%adj-1) (rand-word POS-ADJECTIVE ADJECTIVE-GOOD))
(define (%adj-2) (phrase (rand-word POS-ADJECTIVE ADJECTIVE-GOOD) %comma (rand-word POS-ADJECTIVE ADJECTIVE-GOOD)))
(define (%adj-2-and) (phrase (rand-word POS-ADJECTIVE ADJECTIVE-GOOD) %and (rand-word POS-ADJECTIVE ADJECTIVE-GOOD)))
(define (%adj-3) (phrase (rand-word POS-ADJECTIVE ADJECTIVE-GOOD) %comma (rand-word POS-ADJECTIVE ADJECTIVE-GOOD) %comma %and (rand-word POS-ADJECTIVE ADJECTIVE-GOOD)))
(define (%adj-comparative-1) (rand-word POS-ADJECTIVE ADJECTIVE-OTHER ADJECTIVE-COMPARE))
(define (%adj-comparative-2) (phrase (rand-word POS-ADJECTIVE ADJECTIVE-OTHER ADJECTIVE-COMPARE) %and (rand-word POS-ADJECTIVE ADJECTIVE-OTHER ADJECTIVE-COMPARE)))
(define-rand-func (%adj-comparative) '((%adj-comparative-1 6) (%adj-comparative-2 1)))
(define-rand-func (%adj-superlative plural) 
  `(((lambda () (phrase (rand-word POS-ADJECTIVE ADJECTIVE-OTHER ADJECTIVE-SUPERLATIVE))) 8)
    ((lambda () (phrase (rand-word POS-ADJECTIVE ADJECTIVE-OTHER ADJECTIVE-SUPERLATIVE) (%superlative-modify))) 1)
    ((lambda () (phrase (rand-word POS-ADJECTIVE ADJECTIVE-OTHER ADJECTIVE-SUPERLATIVE) 
			(rand-word POS-COUNTABLE-NOUN ,plural) (%superlative-modify))) 1)
    ((lambda () (phrase (rand-word POS-ADJECTIVE ADJECTIVE-OTHER ADJECTIVE-SUPERLATIVE))) 2)))

(define-rand-elem (%superlative-modify)
  `((,(lit-phrase "ever") 1)
    (,(lit-phrase "anywhere") 1)
    (,(lit-phrase "yet") 1)
    (,(lit-phrase "I've seen") 1)
    (,(lit-phrase "I've know") 1)
    (,(lit-phrase "I've heard of") 1)))

(define-rand-func (%possible-adj-clause) '((%nil-func0 2) (%adj-clause 3)))
(define-rand-func (%adj-clause) '((%that-adj-clause 3) (%which-adj-clause 1)))
(define (%that-adj-clause) (phrase %that (%indep-clause-pl-trans-no-obj (rand-plural))))
(define (%which-adj-clause) (phrase %which (%indep-clause-pl-trans-no-obj (rand-plural))))

;; nouns
(define-rand-func (%noun-pl simple) 
  `(((lambda () (%noun-phrase NOUN-PLURAL ,simple)) 100) 
    ((lambda () (phrase (%noun-phrase-no-clause NOUN-SINGULAR) %and (%noun-phrase NOUN-SINGULAR ,simple))) 25)
    ((lambda () (phrase (%noun-phrase-no-clause NOUN-SINGULAR) %comma (%noun-phrase-no-clause NOUN-SINGULAR) %comma %and (%noun-phrase NOUN-SINGULAR ,simple))) 1)))

(define (%noun-sing simple) (%noun-phrase NOUN-SINGULAR simple))
(define (%noun-phrase-no-clause plurality) 
  (if (not plurality)
      (%noun-phrase-no-clause-pl)
      (%noun-phrase-no-clause-sing)))
(define-rand-func (%noun-phrase-no-clause-pl)
  `(((lambda () (phrase (%article #f) (%possible-adj) (rand-word POS-COUNTABLE-NOUN #f))) 8)
    ((lambda () (phrase %the (rand-word POS-ADJECTIVE ADJECTIVE-OTHER ADJECTIVE-SUPERLATIVE) (rand-word POS-COUNTABLE-NOUN #f))) 1)
    ))
(define-rand-func (%noun-phrase-no-clause-sing)
  `(((lambda () (phrase (%article #t) (%possible-adj) (rand-word POS-COUNTABLE-NOUN #t))) 8)
    ((lambda () (phrase %the (rand-word POS-ADJECTIVE ADJECTIVE-OTHER ADJECTIVE-SUPERLATIVE) (rand-word POS-COUNTABLE-NOUN #t))) 1)
    ((lambda () (rand-word POS-PROPER-NOUN PROPER-NOUN-FORENAME)) 2)))
(define (%noun-phrase plurality simple) 
  (if simple
      (%noun-phrase-no-clause plurality)
      (phrase (%noun-phrase-no-clause plurality))))

(define-rand-func (%object-sing is-I)
  `(((lambda () (%noun #f)) 20)
    ((lambda () (if (equal? ,is-I 'I) 
		     (lit-phrase "myself") 
		     (if (equal? ,is-I 'we)
			 (%object-sing ''we)
			 (lit-phrase "me")))) 5)
    ((lambda () (if (equal? ,is-I 'he) (lit-phrase "himself") (lit-phrase "him"))) 1)
    ((lambda () (if (equal? ,is-I 'she) (lit-phrase "herself") (lit-phrase "her"))) 1)
    ((lambda () (if (equal? ,is-I 'it) (lit-phrase "itself") (lit-phrase "it"))) 2)
    ((lambda () (if (equal? ,is-I 'you) (lit-phrase "yourself") (lit-phrase "you"))) 2)))

(define-rand-func (%object-pl is-we)
  `(((lambda () (%noun #t)) 20)
    ((lambda () (if (equal? ,is-we 'we) (lit-phrase "ourselves") (lit-phrase "us"))) 3)
    ((lambda () (if (equal? ,is-we 'them) (lit-phrase "themselves") (lit-phrase "them"))) 2)
    ((lambda () (lit-phrase "em")) 2)))
(define (%object plurality . is-ego)
  (if (null? is-ego)
      (%object plurality #f)
      (if plurality
	  (%object-pl (car is-ego))
	  (%object-sing (car is-ego)))))


(define (%subject-and-tag plurality)
  (if plurality
      (%subj-and-tag-pl)
      (%subj-and-tag-sing)))

(define-rand-func (%subj-and-tag-sing)
  `(((lambda () (cons (%noun #f) ''sing)) 20)
    ((lambda () (cons (lit-phrase "I") ''I)) 5)
    ((lambda () (cons (lit-phrase "he") ''he)) 1)
    ((lambda () (cons (lit-phrase "she") ''she)) 1)
    ((lambda () (cons (lit-phrase "it") ''it)) 2)
    ((lambda () (cons (lit-phrase "you") ''you)) 15)))
(define-rand-func (%subj-and-tag-pl)
  `(((lambda () (cons (%noun #t) ''plural)) 20)
    ((lambda () (cons (lit-phrase "we") ''we)) 5)
    ((lambda () (cons (lit-phrase "they") ''they)) 1)))
(define (%subject plurality) (car (%subject-and-tag plurality)))

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

(define-rand-elem (%verb-ego-func) `((,%verb-simple-present-ego 4) 
                                      (,%verb-simple-past 4) 
                                      (,%verb-simple-future 3) 
                                      (,%verb-present-continuous-ego 3) 
                                      (,%verb-past-continuous-sing 3) 
                                      (,%verb-future-continuous 3)
                                      (,%verb-present-perfect-ego 2)
                                      (,%verb-past-perfect 2)
                                      (,%verb-future-perfect 2)
                                      (,%verb-present-perfect-continuous-ego 1)
                                      (,%verb-past-perfect-continuous 1)
                                      (,%verb-future-perfect-continuous 1)))

(define-rand-elem (%verb-we-func) `((,%verb-simple-present-ego 4) 
                                      (,%verb-simple-past 4) 
                                      (,%verb-simple-future 3) 
                                      (,%verb-present-continuous-pl 3) 
                                      (,%verb-past-continuous-pl 3) 
                                      (,%verb-future-continuous 3)
                                      (,%verb-present-perfect-ego 2)
                                      (,%verb-past-perfect 2)
                                      (,%verb-future-perfect 2)
                                      (,%verb-present-perfect-continuous-ego 1)
                                      (,%verb-past-perfect-continuous 1)
                                      (,%verb-future-perfect-continuous 1)))


(define (is-helper x)
  (member x (list %not %would %will %should %may %might %could %must
		  %is %are %am %was %were %be %has %have %had %been)))

(define (%verb-pl trans) ((%verb-pl-func) trans))
(define (%verb-sing trans) ((%verb-sing-func) trans))
(define (%verb-ego plural) 
  (if plural (%verb-we-func) (%verb-ego-func)))

(define-rand-func (%random-inflect) `(((lambda () (phrase (%random-modal) %not)) 2) (%random-modal 5)))
(define-rand-elem (%random-modal) `((,%would 3) (,%will 8) (,%should 2) (,%may 1) (,%might 2) (,%could 2) (,%must 2)))
(define (%verb-simple-present-sing trans) (rand-word trans VERB-CONJUGATED CONJ-3RD-SING))
(define (%verb-simple-present-pl trans) (rand-word trans VERB-INFINITIVE))
(define (%verb-simple-present-ego trans) (rand-word trans VERB-INFINITIVE))
(define (%verb-simple-past trans) (rand-word trans VERB-CONJUGATED CONJ-PAST))
(define (%verb-simple-future trans) (phrase (%random-inflect) (rand-word trans VERB-INFINITIVE)))

(define (%verb-present-continuous-sing trans) (phrase %is (rand-word trans VERB-CONJUGATED CONJ-PRESENT-PART)))
(define (%verb-present-continuous-pl trans) (phrase %are (rand-word trans VERB-CONJUGATED CONJ-PRESENT-PART)))
(define (%verb-present-continuous-ego trans) (phrase %am (rand-word trans VERB-CONJUGATED CONJ-PRESENT-PART)))
(define (%verb-past-continuous-sing trans) (phrase %was (rand-word trans VERB-CONJUGATED CONJ-PRESENT-PART)))
(define (%verb-past-continuous-pl trans) (phrase %were (rand-word trans VERB-CONJUGATED CONJ-PRESENT-PART)))
(define (%verb-future-continuous trans) (phrase (%random-inflect) %be (rand-word trans VERB-CONJUGATED CONJ-PRESENT-PART)))

(define (%verb-present-perfect-sing trans) (phrase %has (rand-word trans VERB-CONJUGATED CONJ-PAST-PART)))
(define (%verb-present-perfect-pl trans) (phrase %have (rand-word trans VERB-CONJUGATED CONJ-PAST-PART)))
(define (%verb-present-perfect-ego trans) (phrase %have (rand-word trans VERB-CONJUGATED CONJ-PAST-PART)))
(define (%verb-past-perfect trans) (phrase %had (rand-word trans VERB-CONJUGATED CONJ-PAST-PART)))
(define (%verb-future-perfect trans) (phrase (%random-inflect) %have (rand-word trans VERB-CONJUGATED CONJ-PAST-PART)))

(define (%verb-present-perfect-continuous-sing trans) (phrase %has %been (rand-word trans VERB-CONJUGATED CONJ-PRESENT-PART)))
(define (%verb-present-perfect-continuous-pl trans) (phrase %have %been (rand-word trans VERB-CONJUGATED CONJ-PRESENT-PART)))
(define (%verb-present-perfect-continuous-ego trans) (phrase %have %been (rand-word trans VERB-CONJUGATED CONJ-PRESENT-PART)))
(define (%verb-past-perfect-continuous trans) (phrase %had %been (rand-word trans VERB-CONJUGATED CONJ-PRESENT-PART)))
(define (%verb-future-perfect-continuous trans) (phrase (%random-inflect) %have %been (rand-word trans VERB-CONJUGATED CONJ-PRESENT-PART)))
         

(define (doit n fn out)
  (if (= n 0)
      '()
      (begin 
	(write-string (fn) out)
	(write-string "\n" out)
	(doit (- n 1) fn out))))

(define (main)
  (begin
    (delete-file "Phrases.txt")
    (delete-file "PhrasesBother.txt")
    (doit 1000 rand-sentence (open-output-file "Phrases.txt"))
    (doit 1000 rand-sentence-bother (open-output-file "PhrasesBother.txt"))))

