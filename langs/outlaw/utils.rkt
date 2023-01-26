#lang racket
(provide (struct-out Frame) new-frame extend-frame pad-frame symbol->label symbol->data-label lookup pad-stack unpad-stack)
(require "a86/ast.rkt" "registers.rkt")

;; Symbol -> Label
;; Produce a symbol that is a valid Nasm label
(define (symbol->label s)
  (to-label "label_" s))

(define (symbol->data-label s)
  (to-label "data_" s))

;; Char -> String
(define (char-encode c)
  (if (or (char<=? #\a c #\z)
          (char<=? #\A c #\Z)
          (char<=? #\0 c #\9)
          (memq c '(#\_ #;#\$ #\# #\@ #\~ #\. #\?)))
      (make-string 1 c)
      (string-append "$" (number->string (char->integer c) 16))))

(define (to-label prefix s)
  (string->symbol
   (string-append prefix
                  (apply string-append
                         (map char-encode
                              (string->list (symbol->string s)))))))

(struct Frame (ret vars))

(define (new-frame ret)
  (Frame ret '()))

(define (extend-frame f xs)
  (Frame (Frame-ret f) (append xs (Frame-vars f))))

(define (pad-frame f)
  (Frame (Frame-ret f) (cons #f (Frame-vars f))))

;; Id CEnv -> [Maybe Integer]
#;(define (lookup x cenv)
  (match cenv
    ['() #f]
    [(cons y rest)
     (match (eq? x y)
       [#t 0]
       [#f (match (lookup x rest)
             [#f #f]
             [i (+ 8 i)])])]))

(define (lookup x cenv)
  (match cenv
    ['() #f]
    [(cons (Frame ret vars) rest)
     (lookup-list x vars rest)]))

(define (lookup-list x vars cenv)
  (match vars
    ['() (lookup x cenv)]
    [(cons y rest)
     (match (eq? x y)
       [#t 0]
       [#f (match (lookup-list x rest cenv)
             [#f #f]
             [i (+ 8 i)])])]))

;; -> Asm
;; Dynamically pad the stack to be aligned for a call
(define (pad-stack)
  (seq (Mov r15 rsp)
       (And r15 #b1000)
       (Sub rsp r15)))

;; -> Asm
;; Undo the stack alignment after a call
(define (unpad-stack)
  (seq (Add rsp r15)))
