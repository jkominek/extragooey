#lang racket/base

(require racket/class racket/gui
         racket/contract)

(provide/contract
 [improved-tab-panel%
  (class/c
   [rename-tab
    (->m (or/c string? natural-number/c) string? void?)]
   [append-tab
    (->*m (string? (is-a?/c subwindow<%>))
         (#:show boolean?)
         void?)]
   [remove-tab
    (->m (or/c string? natural-number/c) void?)])])

(define improved-tab-panel%
  (class tab-panel%
    (define labels '())
    (define children '())
    
    (define (cb panel e)
      (define selected-pos (send this get-selection))
      (send this change-children
            (lambda (current-children)
              (if (< selected-pos (length children))
                  (list (list-ref children selected-pos))
                  '()))))

    (define/public (append-tab label child #:show [show #f])
      (set! labels (append labels (list label)))
      (set! children (append children (list child)))
      (send this set labels)
      (when show
        (send this set-selection (sub1 (length labels))))
      (cb this (void))
      (void))
    
    (define (find identifier)
      (for/fold ([t #f]
                 [ti #f])
                ([c children]
                 [l labels]
                 [i (in-naturals)])
        (if (or (equal? i identifier)
                (equal? l identifier))
            (values c i)
            (values t ti))))
    
    (define/public (rename-tab identifier new-name)
      (define-values (target-child target-i)
        (find identifier))
      (set! labels
            (for/list ([l labels]
                       [i (in-naturals)])
              (if (= i target-i)
                  new-name
                  l)))
      (define current-pos (send this get-selection))
      (send this set labels)
      (send this set-selection current-pos)
      (void))

    (define/public (remove-tab identifier)
      (define start-pos (send this get-selection))
      (define-values
        (target-child target-i)
        (find identifier))
        
      (when (member target-child
                    (send this get-children))
        (send this delete-child target-child))

      (set!-values (labels children)
       (for/lists (a b)
         ([l labels]
          [c children]
          #:unless (equal? c target-child))
         (values l c)))
      
      (send this set labels)
      (if (<= target-i start-pos)
          (send this set-selection (- start-pos 1))
          (send this set-selection start-pos))
      (void))
    
    (super-new [callback cb] [choices '("")])
    ))

#;(define myframe%
  (class frame%
    #;(define/override (on-subwindow-char r e)
      (printf "~a~n" (send e get-key-code))
      #f)

    (super-new)
    ))

#;(define f (new myframe% [label "foo"] [width 320] [height 240]))
#;(define (cb panel e)
  (send panel change-children
        (lambda (cs)
          (define l (send panel get-item-label (send panel get-selection)))
          (filter (lambda (c)
                    (equal? (send c get-label) l))
                  cs))))
#;(define tp (new improved-tab-panel%
                [parent f]))

;(send f show #t)

;(send tp append-tab "A" (new message% [label "A"] [parent tp]))
;(send tp append-tab "B" (new message% [label "B"] [parent tp]))
;(send tp append-tab "C" (new text-field% [label "C"] [parent tp]) #:show #t)

;(send tp remove-tab "C")