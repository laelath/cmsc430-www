#lang racket
(require "a86/ast.rkt")

;; Asm -> Asm
(define (remove-zero-jumps p)
  (match p
    ['() '()]
    [(cons (Jmp l) rst)
     (if (member l (labels rst))
         (remove-zero-jumps rst)
         (cons (Jmp l) (remove-zero-jumps rst)))]
    [(cons i rst) (cons i (remove-zero-jumps rst))]))

;; Asm -> (listof symbol)
(define (labels p)
  (match p
    ['() '()]
    [(cons (Label l) rst) (cons l (labels rst))]
    [(cons _ _) '()]))