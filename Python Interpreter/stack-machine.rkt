#lang racket
(require "opcodes.rkt")
(provide make-stack-machine)
(provide run-stack-machine)
(provide get-stack)
(provide get-varnames)
(provide get-consts)
(provide get-names)
(provide get-code)
(provide get-IC)
(provide empty-stack)
(provide make-stack)
(provide push)
(provide pop)
(provide top)


;; TODO 1:
;; Alegeți metoda de reprezentarea a unei stive.
;; Implementați:
(define empty-stack '())
(define (make-stack) '())

(define (push element stack) (cons element stack))

(define (top stack)
  (if (null? stack)
      '()
      (car stack)))

(define (pop stack)
  (if (null? stack)
      '()
      (cdr stack)))

(define (top-m stack-machine)
  (top (get-stack stack-machine)))

(define (top1-m stack-machine)
  (top (pop (get-stack stack-machine))))


;; TODO 2:
;; Alegeți metoda de reprezentare a unei mașini stivă.
;; Definiți make-stack-machine, acesta trebuie sa primeasca cele 4 segmente de date
;; Veți avea nevoie de o stivă pentru execuție și un counter ca să stiți
;; la ce instrucțiune sunteți.
(define (make-stack-machine stack co-varnames co-consts co-names co-code IC)
  (list stack co-varnames co-consts co-names co-code IC))

;; Definiți funcțiile `get-varnames`, `get-consts`, `get-names`,
;; `get-code`, `get-stack`, `get-IC` care primesc o mașina stivă și întorc
;; componenta respectivă

;; ex:
;; > (get-varnames (make-stack-machine empty-stack 'dummy-co-varnames (hash) (hash) (list) 0))
;; 'dummy-co-varnames
(define (get-varnames stack-machine)
  (cadr stack-machine))

;; ex:
;; > (get-consts (make-stack-machine empty-stack (hash) 'dummy-co-consts (hash) (list) 0))
;; 'dummy-co-consts
(define (get-consts stack-machine)
  (caddr stack-machine))

;; ex:
;; > (get-names (make-stack-machine empty-stack (hash) (hash) 'dummy-co-names (list) 0))
;; 'dummy-co-names
(define (get-names stack-machine)
  (caddr (reverse stack-machine)))

;; ex:
;; > (get-code (make-stack-machine empty-stack (hash) (hash) (hash) 'dummy-co-code 0))
;; dummy-co-code
(define (get-code stack-machine)
  (cadr (reverse stack-machine)))

;; Întoarce stiva de execuție.
;; ex:
;; > (get-code (make-stack-machine 'dummy-exec-stack (hash) (hash) (hash) (list) 0))
;; dummy-exec-stack
(define (get-stack stack-machine)
  (car stack-machine))

;; Întoarce instruction counterul.
;; ex:
;; > (get-code (make-stack-machine empty-stack (hash) (hash) (hash) (list) 0))
;; 0
(define (get-IC stack-machine)
  (car (reverse stack-machine)))



(define symbols (list 'STACK 'CO-VARNAMES 'CO-CONSTS 'CO-NAMES 'CO-CODE 'INSTRUCTION-COUNTER))

;; TODO 3:
;; Definiți funcția get-symbol-index care gasește index-ul simbolului in listă.

(define (get-symbol-index symbol)
  (index-of symbols symbol))

;; Definiți funcția update-stack-machine care intoarce o noua mașina stivă
;; înlocuind componenta corespondentă simbolului cu item-ul dat în paremetri.
;; > (get-varnames (update-stack-machine "new-varnames" 'CO-VARNAMES stack-machine))
;; "new-varnames"
;; > (get-varnames (update-stack-machine "new-names" 'CO-NAMES stack-machine))
;; "new-names"
(define (update-stack-machine item symbol stack-machine)
  (list-set stack-machine (get-symbol-index symbol) item))

;; Definiți funcția push-exec-stack care primește o masină stivă și o valoare
;; și intoarce o noua mașina unde valoarea este pusă pe stiva de execuție
(define (push-exec-stack value stack-machine)
  (update-stack-machine (push value (get-stack stack-machine)) 'STACK stack-machine))

;;  Definiți funcția pop-exec-stack care primește o masină stivă
;;  și intoarce o noua mașina aplicând pop pe stiva de execuție.
(define (pop-exec-stack stack-machine)
  (update-stack-machine (pop (get-stack stack-machine)) 'STACK stack-machine))

;; TODO 4:
;; Definiți funcția run-stack-machine care execută operații pană epuizează co-code.
(define (run-stack-machine stack-machine)
  (cond
    ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'RETURN_VALUE)
     stack-machine)

    ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'POP_TOP)
     (run-stack-machine (update-stack-machine (add1 (get-IC stack-machine)) 'INSTRUCTION-COUNTER (pop-exec-stack stack-machine))))

    ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'LOAD_CONST)
     (run-stack-machine (update-stack-machine (add1 (get-IC stack-machine)) 'INSTRUCTION-COUNTER (load-const stack-machine))))

    ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'LOAD_FAST)
     (run-stack-machine (update-stack-machine (add1 (get-IC stack-machine)) 'INSTRUCTION-COUNTER (load-fast stack-machine))))

    ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'STORE_FAST)
     (run-stack-machine (update-stack-machine (add1 (get-IC stack-machine)) 'INSTRUCTION-COUNTER (store-fast stack-machine))))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'BINARY_ADD)
     (run-stack-machine (update-stack-machine (add1 (get-IC stack-machine)) 'INSTRUCTION-COUNTER (binary-add stack-machine))))

    ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'BINARY_SUBTRACT)
     (run-stack-machine (update-stack-machine (add1 (get-IC stack-machine)) 'INSTRUCTION-COUNTER (binary-sub stack-machine))))

    ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'BINARY_MODULO)
     (run-stack-machine (update-stack-machine (add1 (get-IC stack-machine)) 'INSTRUCTION-COUNTER (binary-modulo stack-machine))))

    ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'INPLACE_ADD)
     (run-stack-machine (update-stack-machine (add1 (get-IC stack-machine)) 'INSTRUCTION-COUNTER (binary-add stack-machine))))

    ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'INPLACE_SUBTRACT)
     (run-stack-machine (update-stack-machine (add1 (get-IC stack-machine)) 'INSTRUCTION-COUNTER (binary-sub stack-machine))))

    ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'INPLACE_MODULO)
     (run-stack-machine (update-stack-machine (add1 (get-IC stack-machine)) 'INSTRUCTION-COUNTER (binary-modulo stack-machine))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

    ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'JUMP_ABSOLUTE)
     (run-stack-machine (jump-absolute stack-machine
                                       (cdr (list-ref (get-code stack-machine) (get-IC stack-machine))))))

    ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'COMPARE_OP)
     (run-stack-machine (update-stack-machine (add1 (get-IC stack-machine)) 'INSTRUCTION-COUNTER (compare-op stack-machine
                                                                                                             (cdr (list-ref (get-code stack-machine) (get-IC stack-machine)))))))
    
    ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'POP_JUMP_IF_FALSE)
     (run-stack-machine (pop-jump-false stack-machine
                                        (cdr (list-ref (get-code stack-machine) (get-IC stack-machine))))))
    
    ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'POP_JUMP_IF_TRUE)
     (run-stack-machine (pop-jump-true stack-machine
                                       (cdr (list-ref (get-code stack-machine) (get-IC stack-machine))))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'FOR_ITER)
     (run-stack-machine  (for-iter stack-machine
                                   (cdr (list-ref (get-code stack-machine) (get-IC stack-machine))))))
    
    ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'GET_ITER)
     (run-stack-machine (update-stack-machine (add1 (get-IC stack-machine)) 'INSTRUCTION-COUNTER stack-machine)))

    ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'SETUP_LOOP)
     (run-stack-machine (update-stack-machine (add1 (get-IC stack-machine)) 'INSTRUCTION-COUNTER stack-machine)))

    ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'POP_BLOCK)
     (run-stack-machine (update-stack-machine (add1 (get-IC stack-machine)) 'INSTRUCTION-COUNTER stack-machine)))

            
    ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'LOAD_GLOBAL)
     (run-stack-machine (update-stack-machine (add1 (get-IC stack-machine)) 'INSTRUCTION-COUNTER (load-global stack-machine
                                                                                                              (cdr (list-ref (get-code stack-machine) (get-IC stack-machine)))))))

    ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'CALL_FUNCTION)
     (run-stack-machine (update-stack-machine (add1 (get-IC stack-machine)) 'INSTRUCTION-COUNTER (call-function stack-machine
                                                                                                              (cdr (list-ref (get-code stack-machine) (get-IC stack-machine)))))))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (load-const stack-machine)
  (push-exec-stack
   (hash-ref (get-consts stack-machine) (cdr (list-ref (get-code stack-machine) (get-IC stack-machine)))) stack-machine))

(define (load-fast stack-machine)
  (push-exec-stack
   (hash-ref (get-varnames stack-machine) (cdr (list-ref (get-code stack-machine) (get-IC stack-machine)))) stack-machine))

(define (store-fast stack-machine)
  (update-stack-machine
   (hash-set (get-varnames stack-machine) (cdr (list-ref (get-code stack-machine) (get-IC stack-machine))) (top (get-stack stack-machine)))
   'CO-VARNAMES
   (pop-exec-stack stack-machine)
   ))

(define (binary-add stack-machine)
  (update-stack-machine
   (push (+ (top1-m stack-machine) (top-m stack-machine)) (pop (pop (get-stack stack-machine)))) 'STACK stack-machine))

(define (binary-sub stack-machine)
  (update-stack-machine
   (push (- (top1-m stack-machine) (top-m stack-machine)) (pop (pop (get-stack stack-machine)))) 'STACK stack-machine))

(define (binary-modulo stack-machine)
  (update-stack-machine
   (push (modulo (top1-m stack-machine) (top-m stack-machine)) (pop (pop (get-stack stack-machine)))) 'STACK stack-machine))

(define (jump-absolute stack-machine target)
  (update-stack-machine
   (/ target 2) 'INSTRUCTION-COUNTER stack-machine))

(define (compare-op stack-machine i)
  (update-stack-machine
   (push ((get-cmpop i) (top1-m stack-machine) (top-m stack-machine))
         (pop (pop (get-stack stack-machine))))
   'STACK
   stack-machine
   ))

(define (pop-jump-false stack-machine target)
  (if (equal? (top-m stack-machine) #t)
      (update-stack-machine (add1 (get-IC stack-machine)) 'INSTRUCTION-COUNTER (pop-exec-stack stack-machine))
      (update-stack-machine (pop (get-stack stack-machine)) 'STACK (jump-absolute stack-machine target))
      )
  )

(define (pop-jump-true stack-machine target)
  (if (equal? (top-m stack-machine) #t)
      (update-stack-machine (pop (get-stack stack-machine)) 'STACK (jump-absolute stack-machine target))
      (update-stack-machine (add1 (get-IC stack-machine)) 'INSTRUCTION-COUNTER (pop-exec-stack stack-machine))
      )
  )

(define (for-iter stack-machine delta)
  (if (empty? (top-m stack-machine))
      (update-stack-machine (+ (add1 (/ delta 2)) (get-IC stack-machine)) 'INSTRUCTION-COUNTER  (pop-exec-stack stack-machine))
      (update-stack-machine (push (car (top-m stack-machine)) (push (cdr (top-m stack-machine)) (pop (get-stack stack-machine)))) 'STACK
                            (update-stack-machine (add1 (get-IC stack-machine)) 'INSTRUCTION-COUNTER stack-machine))
      ))

(define (load-global stack-machine func_i)
  (push-exec-stack (hash-ref (get-names stack-machine) func_i) stack-machine))

(define (call-function stack-machine n)
  (update-stack-machine (push ((get-function (car (drop (get-stack stack-machine) n))) (take (get-stack stack-machine) n)) (drop (get-stack stack-machine) (add1 n))) 'STACK stack-machine)
  )


;(trace run-stack-machine)










