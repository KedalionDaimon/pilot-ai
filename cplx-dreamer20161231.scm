; This is a variation of the Complex Piloting System.
; This system "sleeps" and "dreams" - i.e. it "defers some computation"
; until later, and upon reaching the end of its "deference capacity",
; it "dreams", i.e. it evaluates the deferred computation.
; During "dreaming", I/O with the user is suspended.

; COMPLEX PILOTING SYSTEM:

; It contains the function "sub-adjust-knowledge-by-pairs", in order
; to evaluate with all knowledge not only the final "pilot corridor ending",
; but EACH STEP of the piloting system.

; Look for the word TERMINATION:
; Each human sentence and each machine reply should end with '().
; However:
; I am still not quite sure how to "make it reply normally".
; Given its more complex reasoning, if I set the "termination sign" '()
; too early, its conclusions lead nowhere - it replies just with '(());
; but if I set it too late - it replies to the maximum length.
; This looks like a reasonable compromise:
; repeated reasoning is "free" and not terminated by force; but each "final"
; internal reply is terminated with a ().
; The human reply is terminated upon "reading in".
; But if you want to experiment with other variants, check the marker
; TERMINATION to see where you can adjust something.
; Bear in mind : (append somelist '()) = somelist, whereas
; (append somelist '(())) = (somelist + ()). I.e. set '(()) to "activate"
; a termination marker and '() to "deactivate" it.

(define x-human '())
(define x-human-bkp '())
(define x-machine '())
(define x-history '(() () () () () ())) ; KEEP IN SYNC WITH x-window BELOW!
(define x-max-reply-length 20) ; it cannot tell anything longer than 20 words.
(define x-window 6) ; smaller or equal to x-max-reply-length

; dreaming:
(define x-dreams '())
(define x-waking-capacity 100)

(define x-knowledge (with-input-from-file "pilots2.txt" read))

; prototype for four -- x-window:
; ((() () () ()) ())
; prototype for eight -- x-window:
; ((() () () () () () () ()) ())

; This is adjusted from series 2.

; This function is used for "updating" the knowledge base:
; knowledge is a list, and whatever is at the front of the list, is "favoured";
; whatever is close to the end of the list is at risk of being forgotten.
(define (proto-rotate-or-shift challenge-reply-pair knowledge seen)
  (if (or (null? challenge-reply-pair) (equal? '(()) challenge-reply-pair))
    knowledge
    (if (null? knowledge)
      (cons challenge-reply-pair (reverse (cdr seen))) ; pair not found - forget last element, learn new pair
      (if (equal? (caar knowledge) (car challenge-reply-pair)) ; challenge-part of the pair is found
        (cons challenge-reply-pair (append (reverse seen) (cdr knowledge))) ; re-learn pair (possibly with new consequence)
        (proto-rotate-or-shift challenge-reply-pair (cdr knowledge) (cons (car knowledge) seen))))))

(define (rotate-or-shift challenge-reply-pair knowledge)
  (proto-rotate-or-shift challenge-reply-pair knowledge '()))

; (rotate-or-shift '((m) k) '(((p a b c d e) f) ((p q x b q) r) ((k c l e c l) s) ((o w x y) z) ((m) n) ((x x x y y y) z)))
; -->
; (((m) k) ((p a b c d e) f) ((p q x b q) r) ((k c l e c l) s) ((o w x y) z) ((x x x y y y) z))
; i.e. the consequence k replaced the consequence n for the known window (m).

; a few auxiliary functions:

; take the n first elements of a list:
(define (proto-takefirst fromwhere howmany resultlist)
  (if (or (null? fromwhere) (zero? howmany)) (reverse resultlist)
    (proto-takefirst (cdr fromwhere) (- howmany 1) (cons (car fromwhere) resultlist))))

(define (takefirst fromwhere howmany) (proto-takefirst fromwhere howmany '()))

; take the n last elements of a list:
(define (takelast fromwhere howmany) (reverse (takefirst (reverse fromwhere) howmany)))

; take n elements AFTER some elements:
(define (takeafter fromwhere howmany)
  (if (or (null? fromwhere) (zero? howmany)) fromwhere
    (takeafter (cdr fromwhere) (- howmany 1))))
; (takeafter '(a b c d e f) 2) --> (c d e f)

; remove last element of a list - Common Lisp has that built-in:
(define (butlast lis) (reverse (cdr (reverse lis))))


; auxiliary
(define (ecdr lis) (if (null? lis) lis (if (not (list? lis)) (list lis) (cdr lis))))

; the pilotize function has the role to turn continuous lists of input into
; windows with replies so that this knowledge can be learned; it assumes
; there to be a "window" already, and continues the window step by step
; with each history element
(define (proto-pilotize window historie pairlist)
  (if (null? historie) (reverse pairlist)
    (proto-pilotize
      (append (ecdr window) (list (car historie)))
      (cdr historie)
      (cons (list window (car historie)) pairlist))))

; How should history "end", finally? What is the sign of "termination"?
; I am choosing it to be (), the empty list.
(define (pilotize window historie)
  (proto-pilotize window historie '()))
; (pilotize '(a b c) '(d e f g h))      
; --> (((a b c) d) ((b c d) e) ((c d e) f) ((d e f) g) ((e f g) h))

; Compare two lists, element by element; the more elements are
; "the same" at "the same" positions, the more similar two lists are.
; This is so to say a vector comaprison, and
; it is assumed that the lists are of same length
; (I used lists, though, and not vectors).
(define (compare-lists lis1 lis2)
  (apply + (map (lambda (x y) (if (equal? x y) 1 0)) lis1 lis2)))
; (compare-lists '(a b c d) '(a b x d)) --> 3

; Check a "problem" and how well it matches each of the knowledge elements.
(define (check-all-matches challenge knowledge)
  (map (lambda (x) (compare-lists challenge (car x))) knowledge))
; (check-all-matches '(a b c) '(((a x y) z) ((r p x) b) ((a b q) l) ((q b c) v)))
; --> (1 0 2 2)

; Try to "select the best match" for the problem, i.e. the known element
; matching it "best"; here, the TAIL is favoured (see >=) - in order to
; USE knowledge elements before they get forgotten.
(define (proto-select-maximum lis vallis valcand candidate)
  (if (null? lis)
    (if (zero? valcand) ; a "candidate" without any match would be nonsense
      '()
      candidate)
    (if (>= (car vallis) valcand)
      (proto-select-maximum (cdr lis) (cdr vallis) (car vallis) (car lis))
      (proto-select-maximum (cdr lis) (cdr vallis) valcand candidate))))
; Currently, there is no knowledge rotation upon selection of reply,
; but only - if at all - during observation. I do that because re-observation
; of the machine reply leads to circulation of the valuable results anyway.

; assume non-zero knowledge
(define (select-maximum lis vallis)
    (proto-select-maximum (cdr lis) (cdr vallis) (car vallis) (car lis)))
; (select-maximum '(a b c d) '(1 3 2 0)) --> b

(define (find-best-match window knowledge)
  (select-maximum knowledge (check-all-matches window knowledge)))
; (find-best-match '(a b c) '(((a x y) z) ((r p x) b) ((a b q) l) ((q b c) v)))
; --> ((q b c) v) -- ((a b q) l) would be the alternative for "early" matching
; if we used > instead of >= in proto-select-maximum above
; (find-best-match '(g g g) '(((a x y) z) ((r p x) b) ((a b q) l) ((q q c) v)))
; --> () -- no match at all

; This one is tricky "in practice", as I am fooling around with the
; "termination sign".
; Basically, this function "creeps along" knowledge in order to
; produce a reply. The reply is collected step by step into the
; result. It terminates if:
; (i) NO match was found;
; (ii) the match ends with a '();
; (iii) the maximum reply length has been reached.
(define (proto-free-flight window knowledge maxlen currlen result)
  (let ((match (find-best-match window knowledge)))

;   SHALL THE SYSTEM USE THE TERMINATION SIGN WITH ITS ANSWERS?

    (if (or (null? match) (> currlen maxlen))

;     TERMINATION
;     (reverse (cons '() result)); I wanted a uniform reply type
      (reverse result) ; i.e. a () is NOT appended to the machine reply

      (if (null? (cadr match))

;       TERMINATION:
;       (reverse (cons '() result)) ; attach the terminator to the result
        (reverse result) ; i.e. a () is NOT appended to the machine reply

        (proto-free-flight
          (append (cdr window) (list (cadr match)))
          knowledge
          maxlen
          (+ 1 currlen)
          (cons (cadr match) result))))))

(define (free-flight window knowledge)
  (proto-free-flight window knowledge x-max-reply-length 0 '()))

; (free-flight '(a b c)
; '(((c y r) b) ((i i n) g) ((a p c) d) ((b c s) e) ((c d d) f)
; ((d x f) g) ((y y g) h) ((f g h) ()) ((g h ()) i) ((h () i) j)))
; --> (d e f g h ())

; REALISTICALLY - challenges will end in a terminator:
; (free-flight '(a b ())
; '(((c y r) b) ((i i n) g) ((a p c) d) ((b c s) e) ((c d d) f)
; ((d x f) g) ((y y g) h) ((f g h) ()) ((g h ()) i) ((h () i) j)))
; --> (i j g h ())

; Above, you know now how to reply - but you also need to
; LEARN the challenge-reply-pairs - i.e. not only to "reply",
; but also to LEARN THE CONCLUSIONS. (This is really like
; triangulation inheritance in the logical triangulatin systems.)

(define (sub-adjust-knowledge-by-pairs challenge-reply-pairs knowledge)
  (if (null? challenge-reply-pairs)
    knowledge
    (sub-adjust-knowledge-by-pairs
      (cdr challenge-reply-pairs)
      (rotate-or-shift (car challenge-reply-pairs) knowledge))))

(define (adjust-knowledge-by-pairs challenge-reply-pairs knowledge)
  (if (null? challenge-reply-pairs)
    knowledge
    (adjust-knowledge-by-pairs
      (cdr challenge-reply-pairs)
        (if (not (null? (car challenge-reply-pairs)))
          (rotate-or-shift (car challenge-reply-pairs)
            (sub-adjust-knowledge-by-pairs
              (pilotize x-history (free-flight (caar challenge-reply-pairs) knowledge))
                knowledge))
          (rotate-or-shift (car challenge-reply-pairs)
                knowledge)))))

; (adjust-knowledge-by-pairs
; '(((a b c) d) ((b c d) e) ((c d e) f))
; '(((a x y) z) ((r p x) b) ((a b q) l) ((q b c) v)))
; -->
; (((c d e) f) ((b c d) e) ((a b c) d) ((a x y) z))
; - here, everything except one pair got forgotten...

; ------------------------------------------------------------

; Using text is "cheating"... your actions equal your perceptions.
; Normally, perceptions would need translations into actions.
; You don't lift your hand because you watch it, but because
; a certain experience of lifting it... actually lifts it.
; Yet even with text, a few adjustments are necessary.
(define INSTINCTS '((I . YOU)
(i . you)
(ME . YOU)
(me . you)
(YOU . ME)
(you . me)
(MYSELF . YOURSELF)
(myself . yourself)
(YOURSELF . MYSELF)
(yourself . myself)
(MY . YOUR)
(my . your)
(YOUR . MY)
(your . my)
(MINE . YOURS)
(mine . yours)
(YOURS . MINE)
(yours . mine)
(AM . ARE)
(am . are)
(ARE . |AM/ARE|)
(are . |am/are|)
(WAS . |WAS/WERE|)
(was . |was/were|)
(WERE . |WAS/WERE|)
(were . |was/were|)
(|I'M| . |YOU'RE|)
(|i'm| . |you're|)
(|YOU'RE| . |I'M|)
(|you're| . |i'm|)))

(define (proto-instinct-element someelement listofinstincts)
  (if (null? listofinstincts) someelement
    (if (equal? (caar listofinstincts) someelement) (cdar listofinstincts)
      (proto-instinct-element someelement (cdr listofinstincts)))))

(define (proto-instinct-list somelist listofinstincts resultlist)
  (if (null? somelist) (reverse resultlist)
    (proto-instinct-list (cdr somelist) listofinstincts
      (cons (proto-instinct-element (car somelist) listofinstincts)
             resultlist))))

(define (instinct-list somelist) (proto-instinct-list somelist INSTINCTS '()))

; This is the main "thinking" function, just without I/O.
; Leaving out I/O has the purpose to keep thinking "internal".
(define (pseudo-run) ; no reply printing, no reading of x-human
  (begin
    ; learn series of challenge-reply-pairs
    (let ((adjustment
           (adjust-knowledge-by-pairs
             (pilotize x-history x-human)
             x-knowledge)))
        (set! x-knowledge adjustment))
    (set! x-history (takelast (append x-history x-human) x-window))

    ; TERMINATION
    (set! x-machine (append (free-flight x-history x-knowledge) '(())))))

(define (eexxiitt)
(begin (newline)
(with-output-to-file "pilots2.txt" (lambda () (display x-knowledge)))
(exit)))

(define (run)
  (begin
  (set! x-human (append (read) '(()))) ; TERMINATION
; (()) means you append a ();  if you just set () and not (()), this is
; annihilated during "append".
; optional termination sign - removed, was '(()) rather than '()

  (if (or (equal? '(()) x-human) (null? x-human)) ; TERMINATION
  (eexxiitt)
  (begin

; FROM HERE...

  (set! x-human-bkp x-human)

  ; 3 reslides, 2 snow-flakes

  (pseudo-run) (set! x-human x-machine)
  (pseudo-run) (set! x-human x-machine)

  (set! x-human x-human-bkp)
  (pseudo-run) (set! x-human x-machine)
  (pseudo-run) (set! x-human x-machine)

  (set! x-human x-human-bkp)
  (pseudo-run) (set! x-human x-machine)
  (pseudo-run)

  (set! x-human x-human-bkp)
; ... TILL HERE THE SYSTEM DOES "INTERNAL THINKING":
; It generates replies just fine, and learns from its thoughts -
; but it does not yet reply to the user.

; NOW it replies "really":
  (pseudo-run)
  (set! x-machine (instinct-list x-machine));

  ; TERMINATION: - here, set the "answer delimiters" - () means no terminator
  ; whereas (()) appends the terminator () to the previous list.
  ; around the machine reply (or not)         end-human       end-machine
; (set! x-history (takelast (append x-history '(()) x-machine '(())) x-window))
; (set! x-history (takelast (append x-history '() x-machine '(())) x-window))
; (set! x-history (takelast (append x-history '() x-machine '()) x-window))
  (set! x-history (takelast (append x-history '() x-machine '()) x-window))

  (display x-machine)
  (newline)

; DREAMING SECTION:
; setting up the dreams is similar to setting the history:
  (begin
    (set! x-dreams (append x-dreams x-human x-machine))
; do nothing if there is still capacity to stay awake:
    (if (< (length x-dreams) x-waking-capacity) '()
; else - "dream":
      (begin (set! x-human x-dreams) (pseudo-run)
; Wake up, Neo... resetting the dream properties:
      (set! x-dreams '())
      (set! x-human '())
      (set! x-history '(() () () () () ())))))


  (run)))))

; (run)
; (a b c d)
; ...
; ()
; E-X-I-T

(run)


