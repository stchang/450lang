#lang racket

(provide parse
         run
         exn:fail:syntax:cs450?
         lm-result?
         INIT-ENV-add!
         INIT-ENV-show
         NaN ; TODO: don't provide (but makes testing easier)
         circular-err
         undefined-var-err
         arity-err
         not-fn-err
         (rename-out [nan? NaN?]
                     [circular-err? CIRCULAR-ERR?]
                     [undefined-var-err? UNDEF-ERR?]
                     [arity-err? ARITY-ERR?]
                     [not-fn-err? NOT-FN-ERR?]))

(require rackunit)

;; A Variable is a Symbol

;; A 450LangExpr (Expr) is one of
;; - Atom
;; - Variable
;; - `(bind [,Var ,Expr] ,Expr)
;; - `(bind/rec [,Var ,Expr] ,Expr)
;; - `(iffy ,Expr ,Expr ,Expr)
;; - `(lm ,List<Var> ,Expr)
;; - `(∨ Expr ...)
;; - `(∧ Expr ...)
;; - (cons Expr List<Expr>)

;; An Atom is one of
;; - Number
;; - String
;; - Bool!

;; A Bool is one of
;; - 'TRUE!
;; - 'FALSE!

(define (atom? x)
  (or (number? x) (string? x) (symbool? x)))

(define (symbool? x)
  (and (symbol? x)
       (or (symbol=? x 'TRUE!)
           (symbol=? x 'FALSE!))))
(define (var? x) (symbol? x))
(define (Expr? x)
  (or (atom? x)
      (var? x)
      (cons? x))
  #;(disjoin number? string? symbol? cons?))

;; A TestExpr is a:
;; - (list 'chk= 450Expr 450Expr)
;;   interp: test passes if the two results are (Racket) equal?
;;           This is equiv to rackunit's check-equal?

;; - (list 'chkerr 450Expr 450Expr)
;;   interp: special case for testing error-producing programs
;;           - first Expr should be a predicate
;;           - second Expr should be error-producing program
;;           test passes if applying predicate to second arg produces true
;;           This is equiv to rackunit (check-true (expr1 expr2))
;; - (list 'chk 450Expr)
;; interp: equiv to check-true
;; interp: These are special-case testing forms built into the language
;; They must be primitives and not functions because they need to
;; bypass the automatic error-propagation of 450Lang

;; A 450LangAST (AST) is one of:
;; - (num Number)
;; - (str String)
;; - (boo Boolean)
;; - (ite AST AST AST)
;; - (vari Symbol)
;; - (bind Symbol AST List<AST>)
;; - (recb Symbol AST List<AST>)
;; - (call AST List<AST>)
;; - (lm-ast List<Var> AST)
(struct AST () #:transparent)
(struct num AST [val] #:transparent)
(struct str AST [val] #:transparent)
(struct boo AST [val] #:transparent)
(struct ite AST [test then else] #:transparent)
(struct vari AST [name] #:transparent)
(struct bind AST [name expr body] #:transparent)
(struct recb AST [name expr body] #:transparent)
(struct call AST [fn args] #:transparent)
(struct land AST [args] #:transparent)
(struct lor AST [args] #:transparent)
(struct lm-ast AST [params body] #:transparent)

(struct exn:fail:syntax:cs450 exn:fail:syntax [])

;; special case testing forms built into the language
(struct test-form AST [] #:transparent)
(struct chk=? test-form (expected actual))
(struct chktrue test-form [test])
(struct chkfalse test-form [test])
(struct chkerr test-form (p? err-expr))

;; parse : Expr -> AST
(define/contract (parse s)
  (-> Expr? AST?)
  (match s
    [(? number?) (num s)]
    [(? string?) (str s)]
    ['TRUE! (boo true)]
    ['FALSE! (boo false)]
    [(? symbol?) (vari s)]
    [`(bind [,x ,e] . ,bods) (bind x (parse e) (map parse bods))]
    [`(bind . ,_)
     (raise-syntax-error
      'parse "bind: invalid syntax, expected: (bind [x e] body)" s
      #:exn exn:fail:syntax:cs450)]
    [`(bind/rec [,x ,e] . ,bods) (recb x (parse e) (map parse bods))]
    [`(bind/rec . ,_)
     (raise-syntax-error
      'parse "bind/rec: invalid syntax, expected: (bind/rec [x e] body)" s
      #:exn exn:fail:syntax:cs450)]
    [`(∨ . ,args) (lor (map parse args))]
    [`(∧ . ,args) (land (map parse args))]
;    [`(+ ,x ,y) (add (parse x) (parse y))]
;    [`(- ,x ,y) (sub (parse x) (parse y))]
;    [`(=== ,x ,y) (eq (parse x) (parse y))]
    [`(iffy ,tst ,thn ,els) (ite (parse tst) (parse thn) (parse els))]
    [`(lm ,(and (list (? symbol?) ...) args) ,body) (lm-ast args (parse body))]
    #;[`(lm ,(and (list x ...)
                  (list (? symbol?) ...)) ,body) (lm-ast x (parse body))]
    [`(lm . ,_)
     (raise-syntax-error
      'parse "invalid lm syntax, expected (lm (x ..) body)" s
      #:exn exn:fail:syntax:cs450)]
    [`(chk= ,e1 ,e2) (chk=? (parse e1) (parse e2))]
    [`(chk ,e) (chktrue (parse e))]
    [`(chknot ,e) (chkfalse (parse e))]
    [`(chkerr ,e1 ,e2) (chkerr (parse e1) (parse e2))]
    [`(,(or 'chk= 'chkerr) . ,_)
     (raise-syntax-error ; this must be before fn app case
      'chk= "wrong number of arguments to chk= or chkerr" s
      #:exn exn:fail:syntax:cs450)]
    [`(,fn . ,args) (call (parse fn) (map parse args))]
    [_ (raise-syntax-error
        'parse "not a valid CS450 Lang program" s
        #:exn exn:fail:syntax:cs450)]))

(check-equal? (parse 1) (num 1))
(check-equal? (parse '(+ 1 2))
              (call (vari '+) (list (num 1) (num 2)))
              #;(add (num 1) (num 2)))

(check-equal? (parse '(+ (- 1 2) (- 3 4)))
              (call (vari '+)
                    (list
                     (call (vari '-) (list (num 1) (num 2)))
                     (call (vari '-) (list (num 3) (num 4)))))
              #;(add (sub (num 1) (num 2))
                   (sub (num 3) (num 4))))
(check-equal? (parse '(* 1 2))
              (call (vari '*) (list (num 1) (num 2)))) ; unbound vars not parse err
(check-equal? (parse 'TRUE!) (boo true))
(check-equal? (parse 'FALSE!) (boo false))
(check-equal? (parse '(=== TRUE! FALSE!))
              (call (vari '===) (list (boo true) (boo false))))
(check-equal? (parse '(iffy (=== 10 10) 100 200))
              (ite (call (vari '===) (list (num 10) (num 10)))
                   (num 100)
                   (num 200)))

(check-equal? (parse '(bind [x 1] x))
              (bind 'x (num 1) (list (vari 'x))))

(check-exn exn:fail? (lambda () (parse '())))
(check-exn exn:fail:syntax:cs450? (lambda () (parse '(bind [x]))))
(check-exn
 exn:fail:syntax:cs450?
 (lambda () (parse '(bind/rec x x))))

;; A 450LangResult (Result) is one of
;; - Number
;; - String
;; - Boolean
;; - NaN
;; - ErrorResult
;; - RacketFn
;; Interp: possible results of a CS450Lang program
;; Note: NaN is a valid result, but not a valid program (for now)
(struct nan [])
(define NaN (nan))

;; An ErrorResult is one of
;; - ERROR-RESULT
;; - UNDEFINED-ERROR
;; - ARITY-ERROR
;; - CIRCULAR-ERROR
(struct ErrorResult [] #:transparent)
(struct arity-err ErrorResult [] #:transparent)
(struct undefined-var-err ErrorResult [name] #:transparent)
(struct not-fn-err ErrorResult [val] #:transparent)
(struct circular-err ErrorResult [val] #:transparent)
(define ERROR-RESULT (ErrorResult))
(define UNDEFINED-ERROR (undefined-var-err 'unknown))
(define ARITY-ERROR (arity-err))
(define NOT-FN-ERROR (not-fn-err 'unknown))
(define CIRCULAR-ERROR (circular-err 'unknown))

(struct lm-result [params code env] #:transparent)

(define (Result? x)
  ((disjoin number?
            string?
            boolean?
            nan?
            ErrorResult?
            procedure?
            lm-result?
            list?
            image?
            void?) ; need void for testing forms
   x))

(define (bool->str b)
  (if b "TRUE!" "FALSE!"))

(define (bool->num b)
  (if b 1 0))

;; 450LangResult -> String
(define (res->str x)
  (cond
    [(string? x) x]
    [(boolean? x) (bool->str x)]
    [(nan? x) "NaN"]
    [else (~a x)]))

;; 450LangResult -> Number or NaN
(define (res->num x)
  (cond
    [(number? x) x]
    [(string? x) (or (string->number x) ; #f for non-string-nums
                     NaN)]
    [(boolean? x) (bool->num x)]
    [else NaN]))

;; Result -> bool
(define (res->bool x)
  (not (or (nan? x)
           (false? x)
           (empty? x)
           (and (number? x) (zero? x))
           (and (string? x) (string=? x ""))))
  #;(cond
    [(nan? x) false]
    [(false? x) false]
    [(and (number? x) (zero? x)) false]
    [(and (string? x) (string=? x "")) false]
    [else true]))

;; 450+ : 450LangResult ... 450LangResult -> 450LangResult
(define/contract (450+ . args)
  (-> Result? ... Result?)
  (cond
    [(findf undefined-var-err? args) => (lambda (e) e)]
    [(findf ErrorResult? args) => (lambda (e) e)]
;    [(ErrorResult? x) x]
;    [(ErrorResult? y) y]
    [(ormap string? args) (apply string-append (map res->str args))]
    [(andmap list? args) (apply append args)]
    ;[(or (string? x) (string? y)) (string-append (res->str x) (res->str y))]
    [else
     (define nums (map res->num args))
;     (define xnum (res->num x))
;     (define ynum (res->num y))
     (if (ormap nan? nums) #;(or (nan? xnum) (nan? ynum))
         NaN
         (apply + nums))]))

;; 450- : 450LangResult ... 450LangResult -> 450LangResult
(define/contract (450- . args)
  (-> Result? ... Result?)
  (cond
    [(findf undefined-var-err? args) => (lambda (e) e)]
    [(findf ErrorResult? args) => (lambda (e) e)]
;    [(ErrorResult? x) x]
;    [(ErrorResult? y) y]
    ;[(or (string? x) (string? y)) (string-append (res->str x) (res->str y))]
    ;[(ormap string? args) (apply string-append (map res->str args))]
    [else
     (define nums (map res->num args))
;     (define xnum (res->num x))
;     (define ynum (res->num y))
     (if (ormap nan? nums) #;(or (nan? xnum) (nan? ynum))
         NaN
         (apply - nums))]))

;; 450*: Result ...  -> Result
(define/contract (450* . args)
  (-> Result? ... Result?)
  (cond
    ;[(findf undefined-var-err? args) => (lambda (e) e)]
    [(findf ErrorResult? args) => (lambda (e) e)]
    [else
     (define nums (map res->num args))
     (if (ormap nan? nums)
         NaN
         (apply * nums))]))

;; 450= : Result ... -> Result
(define (450loose= . args)
  (-> Result? ... Result?)
  (cond
    [(findf undefined-var-err? args) => (lambda (e) e)]
    [(findf ErrorResult? args) => (lambda (e) e)]
    [(ormap nan? args) false]
    [(or (andmap number? args)
         (andmap boolean? args)
         (andmap string? args))
     (apply equal? args)]
    [(ormap boolean? args)
     (apply 450loose=
            (map
             (lambda (x) (if (boolean? x) (bool->num x) x))
             args))]
    [else
     (apply equal? (map res->str args))]))

(define/contract (450not arg)
  (-> Result? Result?)
  (not (res->bool arg)))

(define/contract (450abs arg)
  (-> Result? Result?)
  (let ([res (res->num arg)])
    (if (number? res)
        (abs res)
        NaN)))

;; 450= : Result ... -> Result
(define/contract (450= . args)
  (-> Result? ... Result?)
  (cond
    [(findf undefined-var-err? args) => (lambda (e) e)]
    [(findf ErrorResult? args) => (lambda (e) e)]
    ;[(ErrorResult? x) x]
    ;[(ErrorResult? y) y]
    [else #;(or (andmap number? args)
         (andmap boolean? args)
         (andmap string? args))
     (apply equal? args)]
    #;[(or (and (number? x) (number? y))
         (and (boolean? x) (boolean? y))
         (and (string? x) (string? y)))
     (equal? x y)]
    ;[(ormap boolean? args) (apply 450= (map bool->num args))]
    ;[(boolean? x) (450= (bool->num x) y)]
    ;[(boolean? y) (450= x (bool->num y))]
    #;[else (equal? (res->str x) (res->str y))]))

;; An Environment (Env) is a List<EnvVal>
;; - represents in-scope variables while running a 450jsLang AST
;; - ids earlier in the list shadow later ones with the same name

;; An EnvVal is one of:
;; - Result
;; - Box<Result>

;; lookup : Var Env -> Result
;; looks up the given var in the given env
;; unbound vars result in (undefined-err x)
(define (lookup x env)
  (or (and (assoc x env)
           (envval->result (second (assoc x env))))
      (undefined-var-err x)))

;; envval->result : EnvVal -> Result
(define (envval->result ev)
  (cond
    [(box? ev) (unbox ev)]
    [else ev]))

;; env-add : Var EnvVal Env -> Env
(define (env-add x res env)
  (cons (list x res) env))

;; env-add/many : List<Var> List<EnvVal> Env -> Env
(define (env-add/many xs ress env)
  (append (map list xs ress) env))

;; INIT-ENV
(require 2htdp/image)

(define (INIT-ENV-show)
  (displayln (INIT-ENV)))
(define (INIT-ENV-add! x f)
  (INIT-ENV (cons (list x f) (INIT-ENV))))

(define INIT-ENV
  (make-parameter
   `((+ ,450+)
     (- ,450-)
    ; (* ,*)
     (× ,450*)
     ;(=== ,450=)
     (~= ,450loose=)
     (++ ,add1)
     (-- ,sub1)
     ;(chk= ,check-equal?) ; cant do this because we dont want errs propagating
     (cns ,cons)
     (mt ,empty)
     (li ,list)
     (< ,<)
     (<= ,<=)
     (> ,>)
     (>= ,>=)
     (¬ ,450not)
     (1st ,first)
     (2nd ,second)
     ;(3rd ,third)
     ;(4th ,fourth)
     (rst ,rest)
     (len ,length)
     ;(π ,pi)
     ;(pi ,pi)
     ;(* ,450*)
     ;(/ ,/)
     (abs ,450abs)
     ;(cos ,cos)
     ;(sin ,sin)
     ;(add-line ,add-line)
     ;(empty-image ,empty-image)
     ;(flip-vertical ,flip-vertical)
     (star ,star)
     [NaN? ,nan?]
     [CIRCULAR-ERR? ,circular-err?]
     [UNDEF-ERR? ,undefined-var-err?]
     [ARITY-ERR? ,arity-err?]
     [NOT-FN-ERR? ,not-fn-err?]
     (lm-result? ,lm-result?)
     )))


;; run : 450LangAST -> 450LangResult
;; Computes the result of running the given CS450Lang program AST
(define/contract (run p)
  (-> AST? Result?)

  (define (450apply f args)
    (match f
      [(? procedure?) (apply f args)]
      [(lm-result params body fnenv)
       (if (= (length params) (length args))
           (run/env body (append (map list params args) fnenv))
           ARITY-ERROR)]
      [(? undefined-var-err?) f]
      [_ (not-fn-err f)]))

     ;; accumulator : env : Env
  ;; invariant : represents variable bindings seen so far
  (define (run/env p env)
    (match p
      [(num n) n]
      [(str s) s]
      [(boo b) b]
      [(vari x) (lookup x env)]
      #;[(bind x e body) (run/env body (env-add x (run/env e env) env))]
      [(bind x e bodys)
       (define new-env (env-add x (run/env e env) env))
       (last
        (map (curryr run/env new-env) bodys))]
      [(recb x e bodys)
       (define placeholder (box (circular-err x)))
       (define env/placeholder (env-add x placeholder env))
       (define x-result (run/env e env/placeholder))
       (set-box! placeholder x-result)
       (last
        (map (curryr run/env env/placeholder) bodys))]
;      [(add x y) (450+ (run/env x env) (run/env y env))]
;      [(sub x y) (450- (run/env x env) (run/env y env))]
;      [(eq x y) (450= (run/env x env) (run/env y env))]
      [(ite tst thn els)
       (define tst-res (run/env tst env))
       (if (ErrorResult? tst-res)
           tst-res
           (if (res->bool (run/env tst env))
               (run/env thn env)
               (run/env els env)))]
      [(land args)
       (cond [(empty? args) #t]
             [(= (length args) 1) (run/env (first args) env)]
             [else
              (let ([res (run/env (first args) env)])
                (if (res->bool res)
                    (run/env (land (rest args)) env)
                    res))])]           
      [(lor args)
       (cond [(empty? args) #f]
             [(= (length args) 1) (run/env (first args) env)]
             [else
              (let ([res (run/env (first args) env)])
                (if (res->bool res)
                    res
                    (run/env (lor (rest args)) env)))])]           
      [(lm-ast args body) (lm-result args body env)] ; dont eval body
      [(call fn args)
       (define fn-res (run/env fn env))
       (if (ErrorResult? fn-res)
           fn-res
           (450apply
            fn-res
            (map (curryr run/env env) args)))]
      [(chk=? expected actual)
       (check-equal? (run/env expected env) (run/env actual env))]
      [(chktrue tst) (check-true (run/env tst env))]
      [(chkfalse tst) (check-false (run/env tst env))]
      [(chkerr p? err)
       (check-true ((run/env p? env) (run/env err env)))]
  ))

  (run/env p (INIT-ENV)))

(define eval450 (compose run parse))

(check-equal? (eval450 1) 1)
(check-equal? (eval450 '(+ 1 2)) 3)
(check-equal? (eval450 '(+ (- 1 2) (- 3 4))) -2)

;; with js-style coercions: string, number

;; "adding" strings
(check-equal? (eval450 '(+ "hello" " world")) "hello world")

;; "adding" strings and numbers
(check-equal? (eval450 '(+ 100 "grand")) "100grand")
(check-equal? (eval450 '(+ "cs" 450)) "cs450")

;; "sub" with string-nums "works", but is NaN for non-string-nums
(check-equal? (eval450 '(- "3" "1")) 2)
(check-equal? (eval450 '(- 3 "1")) 2)
(check-equal? (eval450 '(- "3" 1)) 2)

;; "sub" with non-string-nums NaN
(check-equal? (eval450 '(- "hello" "world")) NaN)
(check-equal? (eval450 '(- "hello" 100)) NaN)
(check-equal? (eval450 '(- 100 "world")) NaN)


;; bind examples
(check-equal? (parse '(bind [x 1] (bind [y 2] (+ x y))))
              (bind 'x (num 1)
                    (list
                     (bind 'y (num 2)
                           (list (call (vari '+) (list (vari 'x) (vari 'y))))))))
;; var and bind
(check-equal? (eval450 'x) (undefined-var-err 'x))
(check-equal? (eval450 '(bind [x 1] x)) 1)
(check-equal? (eval450 '(bind [x 1] (bind [y 2] (+ x y)))) 3)
(check-equal? (eval450 '(bind [x (- 4 3)] (bind [y (+ 5 6)] (+ x y)))) 12)

;; check shadowing, proper variable capture

;; x in-scope should be captured with function def
(check-equal? (parse '((bind [y 10] (lm (x) (+ x y))) 100))
              (call (bind 'y (num 10) (list (lm-ast '(x) (call (vari '+) (list (vari 'x) (vari 'y))))))
                   (list (num 100))))
(check-equal? (eval450 '(bind [y 10] (lm (x) (+ x y))))
              (lm-result
               '(x)
               (call (vari '+) (list (vari 'x) (vari 'y)))
               (cons '(y 10) (INIT-ENV))))
(check-equal? (eval450 '((bind [y 10] (lm (x) (+ x y)))
                         100))
              110)

;; different xs for fn and args should not get shadowed
(check-equal? (eval450 '((bind [x 10] (lm (y) (+ x y)))
                         (bind [x 11] x)))
              21)

;; multiple lambdas
(check-equal? (eval450 '(((lm (x) (lm (x) (+ 1 x))) 10) 11)) 12)

;; second x is shadowed in arg
(check-equal? (eval450 '(bind [x 10] ((lm (y) (+ x y)) (bind [x 11] x))))
              21)

;; dynamic scope not supported - should be error
(check-equal? (eval450 '(bind [f (lm (x) (+ x y))] (bind [y 10] (f 11))))
              (undefined-var-err 'y))
 

(check-equal? (eval450 '(bind [x 10] (bind [x (+ x 1)] (+ x 2))))
              13)

(check-exn exn:fail:syntax:cs450? (lambda () (parse '(lm (x)))))
(check-exn exn:fail:syntax:cs450? (lambda () (parse '(lm (1 2) x))))
(check-exn exn:fail:syntax:cs450? (lambda () (parse '(lm (1) 3))))
(check-exn exn:fail:syntax:cs450? (lambda () (parse '(lm (x 1) 3))))

;; bind/rec

(check-equal?
 (parse '(bind/rec [f f] f))
 (recb 'f (vari 'f) (list (vari 'f))))

(check-true (circular-err?
             (eval450 '(bind/rec [f f] f))))

(define (factorial n)
  (if (zero? n)
      1
      (* n (factorial (sub1 n)))))
(for ([n 6])
  (check-equal?
   (eval450
    `(bind/rec [fac
                (lm (n)
                    (iffy n (× n (fac (- n 1))) 1))]
               (fac ,n)))
   (factorial n)))

;(check-true (eval450 '(=== mt (li))))

;; ternary ? should propagate err
;; issue #2 (h/t Gustavo Aguiar)
(check-equal?
 (eval450
  '(bind/rec
    [reduce
     (lm (f y lst)
         (iffy (empty? lst)
          y
          (reduce f (f y (first lst)) (rst lst))))]
    (reduce + 0 (list 1 2 3 4))))
 (undefined-var-err 'empty?))

(check-equal?
 (eval450
  '(bind/rec
    [reduce
     (lm (f y lst)
         (iffy lst
          (reduce f (f y (first lst)) (rst lst))
          y))]
    (reduce + 0 (list 1 2 3 4))))
 (undefined-var-err 'list))

(check-equal?
 (eval450
  '(bind/rec
    [reduce
     (lm (f y lst)
         (iffy lst
          (reduce f (f y (first lst)) (rst lst))
          y))]
    (reduce + 0 (li 1 2 3 4))))
 (undefined-var-err 'first))

(check-equal?
 (eval450
  '(bind/rec
    [reduce
     (lm (f y lst)
         (iffy lst
          (reduce f (f y (1st lst)) (rst lst))
          y))]
    (reduce + 0 (li 1 2 3 4))))
 10)
