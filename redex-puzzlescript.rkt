#lang racket
(require redex)

; incomplete grammar of puzzlescript
; TODO: colors, pixels, late, rigid, random, probably more stuff
(define-language puzzlescript
  (game (preamble objects legend sounds layers rules winconditions levels))
  (preamble (string ...))
  (objects (object ...))
  (object ((label ...) colors pixels))
  (label variable-not-otherwise-mentioned)
  (legend (decl ...))
  (decl (label =or (label ...)) (label =and (label ...))) ; no precedence for now
  (sounds (string ...))
  (layers ((label ...) ...))
  (rules (rule ...))
  (rule (-> blockseqpair ...))
  (blockseqpair (blockseq blockseq) (dir blockseq blockseq))
  (dir up down left right horizontal vertical)
  (blockseq (& block ...)) ; using & instead of [] because [] are just parens
  (block (: item ...)) ; using : instead of | because | does something in racket
  (item label (movement label))
  (movement up down left right stationary action v < > ^)
  (winconditions (string ...))
  (levels (string ...)))

(define-metafunction puzzlescript
  puzzle->string : any -> string
  ; game
  [(puzzle->string (preamble objects legend sounds layers rules winconditions levels))
   ,(string-join (term ("(Compiled from redex-puzzlescript)"
                        (puzzle->string preamble)
                        ""
                        "========"
                        "OBJECTS"
                        "========"
                        ""
                        (puzzle->string objects)
                        ""
                        "======="
                        "LEGEND"
                        "======="
                        ""
                        (puzzle->string legend)
                        ""
                        "======="
                        "SOUNDS"
                        "======="
                        ""
                        (puzzle->string sounds)
                        ""
                        "================"
                        "COLLISIONLAYERS"
                        "================"
                        ""
                        (puzzle->string layers)
                        ""
                        "======"
                        "RULES"
                        "======"
                        ""
                        (puzzle->string rules)
                        ""
                        "=============="
                        "WINCONDITIONS"
                        "=============="
                        ""
                        (puzzle->string winconditions)
                        ""
                        "======="
                        "LEVELS"
                        "======="
                        ""
                        (puzzle->string levels))) "\n")]
  ; preamble, sounds, winconditions, levels
  [(puzzle->string (string ...)) ,(string-join (term (string ...)) "\n")]
  ; objects
  [(puzzle->string (object ...)) ,(string-join (term ((puzzle->string object) ...)) "\n\n")]
  ; object
  [(puzzle->string ((label ...) colors pixels))
   ,(string-join (list (string-join (map symbol->string (term (label ...))) " ")
                       "blue white"
                       "00100"
                       "01100"
                       "11100"
                       "00100"
                       "11111")
                 "\n")]
  ; legend
  [(puzzle->string (decl ...)) ,(string-join (term ((puzzle->string decl) ...)) "\n")]
  ; decl
  [(puzzle->string (label_1 =or (label_2 ...)))
   ,(string-append (symbol->string (term label_1))
                   " = "
                   (string-join (map symbol->string (term (label_2 ...))) " or "))]
  [(puzzle->string (label_1 =and (label_2 ...)))
   ,(string-append (symbol->string (term label_1))
                   " = "
                   (string-join (map symbol->string (term (label_2 ...))) " and "))]
  ; layers
  [(puzzle->string ((label ...) ...))
   ,(string-join (map (λ (ls) (string-join (map symbol->string ls) " "))
                      (term ((label ...) ...)))
                 "\n")]
  ; rules
  [(puzzle->string (rule ...)) ,(string-join (term ((puzzle->string rule) ...)) "\n")]
  ; rule, blockseqpair -- rule->strings helper does most of the work
  [(puzzle->string rule)
   ,(redex-let puzzlescript
               ([(string_1 string_2) (term (rule->strings rule))])
               (if (string=? (term string_1) (term string_2) "")
                   ""
                   (string-append (term string_1) " -> " (term string_2))))]
  ; blockseq
  [(puzzle->string (& block ...))
   ,(string-append "[ " (string-join (term ((puzzle->string block) ...)) " | ") " ]")]
; block
  [(puzzle->string (: item ...)) ,(string-join (term ((puzzle->string item) ...)) " ")]
  ; item
  [(puzzle->string label) ,(symbol->string (term label))]
  [(puzzle->string (movement label))
   ,(string-append (symbol->string (term movement)) " " (symbol->string (term label)))])

(define-metafunction puzzlescript
  rule->strings : any -> (string string)
  [(rule->strings (->)) ("" "")]
  [(rule->strings (-> (blockseq_1 blockseq_2) blockseqpair ...))
   ,(redex-let puzzlescript
               ([(string_1 string_2) (term (rule->strings (-> blockseqpair ...)))])
               (term (,(string-join (term (string_1 (puzzle->string blockseq_1))) " ")
                      ,(string-join (term (string_2 (puzzle->string blockseq_2))) " "))))]
  [(rule->strings (-> (dir blockseq_1 blockseq_2) blockseqpair ...))
   ,(redex-let puzzlescript
               ([(string_1 string_2) (term (rule->strings (-> blockseqpair ...)))])
               (term (,(string-join (term (,(symbol->string (term dir))
                                           (puzzle->string blockseq_1) string_1)) " ")
                      ,(string-join (term ((puzzle->string blockseq_2) string_2)) " "))))])
