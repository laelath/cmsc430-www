#lang racket
(let ([t (make-continuation-prompt-tag)])
  (reset-at t (+ 1 (reset (+ 2 (shift-at t _ (shift _ 42)))))))
