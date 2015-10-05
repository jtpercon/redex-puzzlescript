#lang racket
(require redex)

; incomplete grammar of puzzle script
; TODO: late, rigid, random, probably more stuff
(define-language puzzlescript
  (game (preamble tileset legend sounds layers rules winconditions levels))
  (label variable-not-otherwise-mentioned)
  ; preamble
  (preamble (string ...))
  ; tileset
  (tileset (tile ...))
  (tile (label label ... color color ... pixels))
  (color (rgb natural natural natural) ; should be 0-255 but this is not checked (yet?)
         ; color list from http://www.puzzlescript.net/Documentation/objects.html
         black white grey darkgrey lightgrey gray darkgray lightgray red darkred lightred brown darkbrown
         lightbrown orange yellow green darkgreen lightgreen blue lightblue darkblue purple pink transparent)
  (pixels ((px px px px px) (px px px px px) (px px px px px) (px px px px px) (px px px px px)))
  (px 0 1 2 3 4 5 6 7 8 9 dot)
  ; legend
  (legend (decl ...))
  (decl (label =or (label ...)) (label =and (label ...))) ; no precedence for now
  ; sounds
  (sounds (string ...))
  ; layers
  (layers ((label ...) ...))
  ; rules
  (rules (rule ...))
  (rule (-> blockseqpair ...))
  (blockseqpair (blockseq blockseq) (dir blockseq blockseq))
  (dir up down left right horizontal vertical)
  (blockseq (& block ...)) ; using & instead of [] because [] are just parens
  (block (: item ...)) ; using : instead of | because | does something in racket
  (item label (movement label))
  (movement up down left right stationary action v < > ^)
  ; winconditions
  (winconditions (string ...))
  ; levels
  (levels (string ...)))

; -----------
; - compile -
; -----------

; convert redex representation to actual Puzzle Script source
(define-metafunction puzzlescript
  puzzle->string : any -> string
  ; game
  [(puzzle->string (preamble tileset legend sounds layers rules winconditions levels))
   ,(string-join (term ("(Compiled from redex-puzzlescript)"
                        (puzzle->string preamble)
                        ""
                        "========"
                        "OBJECTS"
                        "========"
                        ""
                        (puzzle->string tileset)
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
  ; tileset
  [(puzzle->string (tile ...)) ,(string-join (term ((puzzle->string tile) ...)) "\n\n")]
  ; tile
  [(puzzle->string (label ... color ... pixels))
   ,(string-append (string-join (map symbol->string (term (label ...))) " ")
                   "\n"
                   (string-join (term ((puzzle->string color) ...)) " ")
                   "\n"
                   (term (puzzle->string pixels)))]
  ; color
  [(puzzle->string (rgb natural_1 natural_2 natural_3))
   ,(string-append "#"
                   (number->string (term natural_1) 16)
                   (number->string (term natural_2) 16)
                   (number->string (term natural_3) 16))]
  [(puzzle->string color) ,(symbol->string (term color))]
  ; pixels
  [(puzzle->string ((px_1 ...) (px_2 ...) (px_3 ...) (px_4 ...) (px_5 ...)))
   ,(string-join (append (term ((puzzle->string px_1) ...))
                         '("\n")
                         (term ((puzzle->string px_2) ...))
                         '("\n")
                         (term ((puzzle->string px_3) ...))
                         '("\n")
                         (term ((puzzle->string px_4) ...))
                         '("\n")
                         (term ((puzzle->string px_5) ...)))
                 "")]
  ; px
  [(puzzle->string dot) "."]
  [(puzzle->string px) ,(number->string (term px))]
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

; get LHS and RHS of a rule as strings
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

; ----------------------
; - correctness checks -
; ----------------------

; returns #t if all the pixels in the tile are valid for the number of colors given
(define-metafunction puzzlescript
  wf-tile : tile -> bool
  [(wf-tile (label ... color ... pixels))
   ,(let ([len (length (term (color ...)))])
      (empty? (filter (λ (px) (and (number? px) (>= px len))) (flatten (term pixels)))))])