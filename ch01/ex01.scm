;;; 1.1
10 ; 10
(+ 5 3 4) ; 12
(- 9 1) ; 8
(/ 6 2) ; 3
(+ (* 2 4) (- 4 6)) ; 6
(define a 3)
(define b (+ a 1))
(+ a b (* a b)) ; 19
(= a b) ; #f
(if (and (> b a) (< b (* a b)))
    b
    a) ; 4
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25)) ; 16
(+ 2 (if (> b a) b a)) ; 6
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1)) ; 16

;;; 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7))) ; -37/150

;;; 1.3
(define (square x) (* x x))
(define (sum-of-square x y) (+ (square x) (square y)))
(define (f a b c)
  (cond ((and (= a b) (= b c)) (sum-of-square a b))
        ((and (= a b) (< b c)) (sum-of-square b c))
        ((and (= a b) (< c b)) (sum-of-square a b))
        ((and (= b c) (< c a)) (sum-of-square c a))
        ((and (= b c) (< a b)) (sum-of-square b c))
        ((and (= c a) (< a b)) (sum-of-square a b))
        ((and (= c a) (< b a)) (sum-of-square a c))
        ((and (< a b) (< b c)) (sum-of-square b c))
        ((and (< b c) (< c a)) (sum-of-square c a))
        ((and (< c a) (< a b)) (sum-of-square a b))))

;;; 1.4
((define (a-plus-abs-b a b))
 ((if (> b 0) + -) a b))
;; (> b 0) の評価結果によって演算子自体を返却する

;;; 1.5
(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y))

(test 0 (p))

;;; 1.6
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(sqrt-iter 1.0 9) ; not response
;; if は述語式predicateを最初に評価する特殊形式なので、good-enough?が評価されるが、
;; new-if はgood-enough?が評価されず無限に (sqrt-iter (improve guess x) x) が呼ばれてしまう

;;; 1.7
;; good-enough? で想定している許容誤差0.001より小さい数については計算できない
;; また、非常に大きい数では計算が終わらない
;; guessがどれだけ変化するかに着目して再実装
;; (define (improve guess x)
;;   (average guess (/ x guess)))
;; (define (average x y)
;;   (/ (+ x y) 2))
(define (good-enough2? guess pre-guess)
  (< (/ (abs (- guess pre-guess)) guess) 0.001))

(define (sqrt-iter2 guess pre-guess x)
  (if (good-enough2? guess pre-guess)
      guess
      (sqrt-iter2 (improve guess x) guess x)))

;;; 1.8
(define (improve2 guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))

(define (cube-iter guess pre-guess x)
  (if (good-enough2? guess pre-guess)
      guess
      (cube-iter (improve2 guess x) guess x)))

(define (cube x)
  (cube-iter 0.001 100.0 x))

(cube 8) ; 2.000000136961482

;;; 1.9
(define (inc x) (+ x 1))
(define (dec x) (- x 1))

(define (p1 a b)
  (if (= a 0) b (inc (p1 (dec a) b))))
(define (p2 a b)
  (if (= a 0) b (p2 (dec a) (inc b))))

(p1 4 5)
;; (if (= 4 0) 5 (inc (p1 (- 4 1) 5)))
;; (inc (p1 3 5))
;; (inc (if (= 3 0) 5 (inc (p1 (- 3 1) 5))))
;; (inc (inc (p1 2 5)))
;; (inc (inc (if (= 2 0) 5 (inc (p1 (- 2 1) 5)))))
;; (inc (inc (inc (p1 1 5))))
;; (inc (inc (inc (if (= 1 0) 5 (inc (p1 (- 1 1) 5))))))
;; (inc (inc (inc (inc (p1 0 5)))))
;; (inc (inc (inc (inc (if (= 0 0) 5 (inc (p1 (- 0 1) 5)))))))
;; (inc (inc (inc (int 5))))
;; (inc (inc (inc 6)))
;; (inc (inc 7))
;; (inc 8)
;; 9
(p2 4 5)
;; (if (= 4 0) 5 (p2 (dec 4) (inc 5)))
;; (p2 (dec 4) (inc 5))
;; (p2 3 6)
;; (if (= 3 0) 6 (p2 (dec 3) (inc 6)))
;; (p2 (dec 3) (inc 6))
;; (p2 2 7)
;; (if (= 2 0) 7 (p2 (dec 2) (inc 7)))
;; (p2 (dec 2) (inc 7))
;; (p2 1 8)
;; (if (= 1 0) 8 (p2 (dec 1) (inc 8)))
;; (p2 (dec 1) (inc 8))
;; (p2 0 9)
;; (if (= 0 0) 9 (p2 (dec 0) (inc 9)))
;; 9

;;; 1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))
(A 1 10) ; 1024
(A 2 4) ; 65536
(A 3 3) ; 65536

(define (f n) (A 0 n)) ;; => 2n
(define (g n) (A 1 n)) ;; => 2^n
(define (h n) (A 2 n)) ;; => 2^2^2^...^2（2のn回のべき乗）
(define (k n) (* 5 n n))

;;; 1.11
;; f(n) = n (if n < 3) | f(n-1) + 2f(n-2) + 3f(n-3) (if n >= 3)
;; recursion
(define (f1 n)
  (if (< n 3)
        n
        (+ (f1 (- n 1)) (* 2 (f1 (- n 2))) (* 3 (f1 (- n 3))))))
;; iteration
;; a <- b
;; b <- c
;; c <- f(n-1) + 2f(n-2) + 3f(n-3) = c + 2b + 3a
(define (f2 n)
  (f2-iter 0 1 2 n))
(define (f2-iter a b c count)
  (if (= count 0)
      a
      (f2-iter b c (+ c (* 2 b) (* 3 a)) (- count 1))))

;;; 1.12
;; Pascal's triangle
(define (pt h n) ; height number
  (if (or (= n 1) (= n h))
      1
      (+ (pt (- h 1) (- n 1)) (pt (- h 1) n))))
(pt 1 1) ; 1
(pt 2 1) ; 1
(pt 2 2) ; 1
(pt 3 2) ; 2
(pt 5 3) ; 6

;;; 1.13 略

;;; 1.14
;; http://community.schemewiki.org/?sicp-ex-1.14

;;; 1.15
(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

;; (a)
(sine 12.15)
;; (p (sine 4.05))
;; (p (p (sine 1.35)))
;; (p (p (p (sine 0.45))))
;; (p (p (p (p (sine 0.15)))))
;; (p (p (p (p (p (sine 0.05))))))

;; (b)
(sine a)
;; a / 3.0 ごとにブレイクダウンされていく
;; -> a が3倍されるごとにステップ数が1増える
;; -> O(log3(a))

;;; 1.16
(define (fast-expt2 b n)
  (fast-expt-iter b n 1))
(define (even? n)
  (= (remainder n 2) 0))
;; b が偶 = a * (b^2)^(n/2)   => a * b^n
;; b が奇 = (a * b) * b^(n-1) => a * b^n
(define (fast-expt-iter b n a)
  (cond ((= n 0) a)
        ((even? n) (fast-expt-iter (* b b) (/ n 2) a))
        (else (fast-expt-iter b (- n 1) (* b a)))))
(fast-expt2 2 5) ; 32
;; 各状態を通じて一定であるような不変量invariant quantityを定義するのが、反復アルゴリズムを設計する上で一般的かつ強力なテクニック

;;; 1.17
(define (double x)
  (* x 2))
(define (halve x)
  (/ x 2))
(define (even? n)
  (= (remainder n 2) 0))
;; a*b
;; b が偶 = (a*2) * (b/2)           => a*b
;; b が奇 = ((a*2) * ((b-1)/2)) + a => a*b
(define (fast-multi b n a)
  (cond ((= n 0) 0)
        ((even? n) (fast-multi (double a) (halve b)))
        (else (+ (fast-multi (double a) (halve (- b 1))) a))))

;;; 1.18
;; 不変量 a*b+n を考える（不変量が思いつかない。。）
;; b が偶 = ((a*2) * (b/2)) + n         => a*b+n
;; b が奇 = ((a*2) * ((b-1)/2)) + a + n => a*b+n
(define (fast-multi-iter a b n)
  (cond ((= b 0) n)
        ((even? b) (fast-multi-iter (double a) (halve b) n))
        (else (+ (fast-multi (double a) (halve (- b 1))) a n))))
(define (fast-multi2 a b n)
  (fast-multi-iter a b 0))

;;; 1.19
;; フィボナッチ数を対数的ステップで計算する
;; a <- a + b
;; b <- a
;; を、以下のように考える
;; a <- b*q + a*q + a*p
;; b <- b*p + a*q
;; Tpq の一回目と二回目の差分を地道に計算して 'p と 'q を検証する
;; https://www.serendip.ws/archives/381
(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* p p) (* q q))   ;; 'p
                   (+ (* 2 p p) (* q q)) ;; 'q
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

;;; 1.20
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
(gcd 206, 40)
;; remainder = %

;; 正規順序評価
;; (if (= 40 0) 206 (gcd 40 (% 206 40))) #f
;; (gcd 40 (% 206 40))
;; (if (= (% 206 40) 0) 40 (gcd (% 206 40) (% 40 (% 206 40)))) #f
;; (gcd (% 206 40) (% 40 (% 206 40)))
;; (if (= (% 40 (% 206 40)) 0) (% 206 40) (gcd (% 40 (% 206 40)) (% (% 206 40) (% 40 (% 206 40))))) #f
;; (gcd (% 40 (% 206 40)) (% (% 206 40) (% 40 (% 206 40))))
;; (if (= (% (% 206 40) (% 40 (% 206 40))) 0) (% 40 (% 206 40)) (gcd (% (% 206 40) (% 40 (% 206 40))) (% (% 40 (% 206 40)) (% (% 206 40) (% 40 (% 206 40)))))) #f
;; (gcd (% (% 206 40) (% 40 (% 206 40))) (% (% 40 (% 206 40)) (% (% 206 40) (% 40 (% 206 40)))))
;; (if (= (% (% 40 (% 206 40)) (% (% 206 40) (% 40 (% 206 40)))) 0) (% (% 206 40) (% 40 (% 206 40))) (gcd (% (% 40 (% 206 40)) (% (% 206 40) (% 40 (% 206 40)))) (% (% (% 206 40) (% 40 (% 206 40))) (% (% 40 (% 206 40)) (% (% 206 40) (% 40 (% 206 40))))))) #t
;; (% (% 206 40) (% 40 (% 206 40)))
;; (% 6 (% 40 6))
;; (% 6 4)
; => 2

;; 適用順序評価
;; (if (= 40 0) 206 (gcd 40 (% 206 40)))
;; (if (= 40 0) 206 (gcd 40 6)) #f
;; (if (= 6 0) 40 (gcd 6 (% 40 6)))
;; (if (= 6 0) 40 (gcd 6 4)) #f
;; (if (= 4 0) 6 (gcd 4 (% 6 4)))
;; (if (= 4 0) 6 (gcd 4 2)) #f
;; (if (= 2 0) 4 (gcd 2 (% 4 2))
;; (if (= 2 0) 4 (gcd 2 0)) #f
;; (if (= 0 0) 2 (gcd 0 (% 2 0))) #t
; => 2

;;; 1.21
(define (smallest-divisor n) (find-divisor n 2))
(define (square n) (* n n))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divide? n test-divisor) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divide? a b)
  (= (remainder a b) 0))

(smallest-divisor 199)   ; 199
(smallest-divisor 1999)  ; 1999
(smallest-divisor 19999) ; 7

;;; 1.22
(define (prime? n)
  (= (smallest-divisor n) n))

; https://github.com/suzuken/sicp/blob/master/chapter1/q1.22.scm#L18
(define (runtime)
  (use srfi-11)
  (let-values (((a b) (sys-gettimeofday)))
              (+ (* a 1000000) b)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes a b)
  (search-for-primes-iter a b))

(define (search-for-primes-iter a b)
  (if (= (remainder a 2) 0)
      (search-for-primes (+ a 1) b)
      (if (prime? a)
          (timed-prime-test a)
          (search-for-primes (+ a 2) b))))

(search-for-primes 1000 1050)       ; 1009 *** 10#<undef>
(search-for-primes 10000 10050)     ; 10007 *** 44#<undef>
(search-for-primes 100000 100050)   ; 100003 *** 112#<undef>
(search-for-primes 1000000 1000050) ; 1000003 *** 299#<undef>
;; 1009  -> 10007   : 44/10  = 4.4
;; 10007 -> 1000003 : 112/44 = 2.5454545454545454
;; (sqrt 10) : 3.1622776601683795
;; 実行するごとに runtime 結果が変わるのでなんとも言えない

;;; 1.23
(define (smallest-divisor n) (find-divisor2 n 2))
(define (square n) (* n n))

(define (find-divisor2 n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divide? n test-divisor) test-divisor)
        (else (find-divisor2 n (next test-divisor)))))

(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

(define (divide? a b)
  (= (remainder a b) 0))

(define (prime? n)
  (= (smallest-divisor n) n))

(define (runtime)
  (use srfi-11)
  (let-values (((a b) (sys-gettimeofday)))
              (+ (* a 1000000) b)))

(define (timed-prime-test n)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(timed-prime-test 1009)    ; 1009 *** 19#<undef>
(timed-prime-test 1013)    ; 1013 *** 11#<undef>
(timed-prime-test 1019)    ; 1019 *** 13#<undef>
(timed-prime-test 10007)   ; 10007 *** 21#<undef>
(timed-prime-test 10009)   ; 10009 *** 22#<undef>
(timed-prime-test 10037)   ; 10037 *** 13#<undef>
(timed-prime-test 100003)  ; 100003 *** 87#<undef>
(timed-prime-test 100019)  ; 100019 *** 42#<undef>
(timed-prime-test 100043)  ; 100043 *** 32#<undef>
(timed-prime-test 1000003) ; 1000003 *** 93#<undef>
(timed-prime-test 1000033) ; 1000033 *** 94#<undef>
(timed-prime-test 1000037) ; 1000037 *** 93#<undef>

;; 速くなったり遅くなったり。。。
;; 1000037 のケースは繰り返すと155, 93と交互に出力された（なぜ）

;;; 1.24
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else
         (remainder (* base (expmod base (- exp 1) m)) m))))
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (prime? n)
  (= (fast-prime? n) n))

(define (runtime)
  (use srfi-11)
  (let-values (((a b) (sys-gettimeofday)))
              (+ (* a 1000000) b)))

(define (timed-prime-test n)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(timed-prime-test 1009)    ; 1009 *** 15#<undef>
(timed-prime-test 1013)    ; 1013 *** 36#<undef>
(timed-prime-test 1019)    ; 1019 *** 12#<undef>
(timed-prime-test 10007)   ; 10007 *** 35#<undef>
(timed-prime-test 10009)   ; 10009 *** 23#<undef>
(timed-prime-test 10037)   ; 10037 *** 34#<undef>
(timed-prime-test 100003)  ; 100003 *** 87#<undef>
(timed-prime-test 100019)  ; 100019 *** 124#<undef>
(timed-prime-test 100043)  ; 100043 *** 82#<undef>
(timed-prime-test 1000003) ; 1000003 *** 236#<undef>
(timed-prime-test 1000033) ; 1000033 *** 331#<undef>
(timed-prime-test 1000037) ; 1000037 *** 199#<undef>

;;; 1.25
(define (expmod2 base exp m)
  (remainder (fast-expt base exp) m))
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))
(define (even? n)
  (= (remainder n 2) 0))

;; fast-expt を使用する場合、base^exp の計算結果に対して remainder を適用する
;; => exp が大きな値の場合、ステップ数が多く必要になるので終わらない

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else
         (remainder (* base (expmod base (- exp 1) m)) m))))

;; ステップごとに remainder が適用されるので、 exp が大きな値でも計算が完了する

;;; 1.26
;; Louis's code
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (* (expmod base (/ exp 2) m)
                       (expmod base (/ exp 2) m))
                    m)
         (else (remainder (* base (expmod base (- exp 1) m))
                          m)))))
;; (square (expmod base (/ exp 2) m)) が
;; (* (expmod base (/ exp 2) m) (expmod base (/ exp 2) m))
;; になっているので、 expmod の呼び出しが倍になっているので、
;; プロセスは O(logN^2) => O(N) となる

;;; 1.27
(define (square n) (* n n))
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else
         (remainder (* base (expmod base (- exp 1) m)) m))))

(define (cmike? n)
  (define (try-it n a)
    (cond ((= a 1) #t) ;; cmike!
          ((not (= (expmod a n n) a)) #f) ;; not cmike
          (else (try-it n (- a 1)))))
  (try-it n (- n 1)))
;; n が素数であるかを n-1 からデクリメントしながらフェルマーの小定理を使用してテストしていく
(cmike? 561)  ; #t
(cmike? 1105) ; #t
(cmike? 1729) ; #t
(cmike? 2465) ; #t
(cmike? 2821) ; #t
(cmike? 6601) ; #t

;;; 1.28
(define (square n) (* n n))
(define (check-square-expmod m n)
  ;; 法nに関して自明でない1の平方根を見つけたかチェック
  (if (and (not (= m 1))
           (not (= m (- n 1)))
           (= (remainder (square m) n) 1))
      0
      (remainder (square m) n)))
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (check-square-expmod (expmod base (/ exp 2) m) m))
        (else
         (remainder (* base (expmod base (- exp 1) m)) m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (random x)
  (modulo (sys-random) x))

(miller-rabin-test 559) ; #f
(miller-rabin-test 561) ; #f

;;; 1.29
(define (integral2 f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (add x) (+ x 1))
  (define (integral2-term x)
    (* (if (= (remainder x 2) 0) 2 4)
       (y x)))
  (/
   (* (+ (y 0)
         (sum integral2-term 1 add (- n 1))
         (y n))
      h)
   3.0))

(integral2 cube 0 1 100)  ; 0.25
(integral2 cube 0 1 1000) ; 0.25

;;; 1.30
;; (define (<name> a b)
;;    (if (> a b)
;;        0
;;        (+ (<term> a)
;;           (<name> (<next> a) b))))
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc x) (+ x 1))
(define (cube x) (* x x x))
(define (sum-cubes a b)
  (sum cube a inc b))

(sum-cubes 1 10) ; 3025
;; (sum cube 1 inc 10)
;; (if (> 1 10) 0 (+ (cube 1) (sum cube 2 inc 10)))
;; (+ 1 (sum cube 2 inc 10))
;; (+ 1 (if (> 2 10) 0 (+ (cube 2) (sum cube 3 inc 10))))
;; (+ 1 8 (sum cube 3 inc 10))
;; ...

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

(define (sum-cubes-iter a b)
  (sum-iter cube a inc b))

(sum-cubes-iter 1 10) ; 3025
;; (sum-iter cube 1 inc 10)
;; (if (> 1 10) 0 (iter (inc 1) (+ (cube 1) 0)))
;; (if (> 2 10) 1 (iter (inc 2) (+ (cube 2) 1)))
;; (if (> 3 10) (+ 8 1)  (iter (inc 3) (+ (cube 3) (+ 8 1))))
;; ...

;;; 1.31
;; 1.31 a
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (inc x) (+ x 1))
(define (factorial n)
  (define (term x) x)
  (product term 1 inc n))

(factorial 4)  ; 24
(factorial 10) ; 3628800

(define (pi-product n)
  (define (term x)
    (/ (* x (+ x 2.0))
       (* (+ x 1.0) (+ x 1.0))))
  (define (next x) (+ x 2))
  (product term 2.0 next n))

(* 4.0 (pi-product 30))   ; 3.191057433389645
(* 4.0 (pi-product 100))  ; 3.157030176455167
(* 4.0 (pi-product 1000)) ; 3.1431607055322752

;; 1.31 b
(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

(define (factorial-iter n)
  (define (term x) x)
  (define (next x) (+ x 1))
  (product-iter term 1 next n))

(factorial-iter 4)  ; 24
(factorial-iter 10) ; 3628800

;;; 1.32
;; 1.32 a
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (sum term a next b)
  (define (combiner x y) (+ x y))
  (accumulate combiner 0 term a next b))

(define (inc x) (+ x 1))
(define (cube x) (* x x x))
(define (sum-cubes a b)
  (sum cube a inc b))

(sum-cubes 1 10) ; 3025

(define (product term a next b)
  (define (combiner x y) (* x y))
  (accumulate combiner 1 term a next b))

(define (inc x) (+ x 1))
(define (factorial n)
  (define (term x) x)
  (product term 1 inc n))

(factorial 4)  ; 24
(factorial 10) ; 3628800

;; 1.32 b
(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (product-iter term a next b)
  (define (combiner x y) (* x y))
  (accumulate-iter combiner 1 term a next b))

(define (inc x) (+ x 1))
(define (factorial-iter n)
  (define (term x) x)
  (product-iter term 1 inc n))

(factorial-iter 4)  ; 24
(factorial-iter 10) ; 3628800

;;; 1.33
(define (filtered-accumulate filter combiner null-value term a next b)
  (if (> a b)
      null-value
      (if (filter a)
          (combiner (term a) (filtered-accumulate filter combiner null-value term (next a) next b))
          (filtered-accumulate filter combiner null-value term (next a) next b))))

;; 1.33 a
(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divide? n test-divisor) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (square x) (* x x))
(define (divide? a b)
  (= (remainder a b) 0))

(define (prime? n)
  (= (smallest-divisor n) n))

(define (prime-sum a b)
  (define (combiner x y) (+ x y))
  (define (term x) (* x x))
  (define (next x) (+ x 1))
  (filtered-accumulate prime? combiner 0 term a next b))

(prime-sum 3 10) ; 9+25+49=83

;; 1.33 b
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (gcd-sum n)
  (define (filter x)
    (if (= (gcd x n) 1)
        #t
        #f))
  (define (combiner x y) (* x y))
  (define (term x) x)
  (define (next x) (+ x 1))
  (filtered-accumulate filter combiner 1 term 1 next n))

(gcd-sum 10) ; 189

;;; 1.34
(define (f g) (g 2))

(f square) ; 4
(f (lambda (z) (* z (+ z 1)))) ; 6
(f f) ; invalid application (2 2)

;;; 1.35
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (golden-ratio x)
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))
(golden-ratio 10) ; 1.6180327868852458

;;; 1.36
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display next)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))
(define (average x y) (/ (+ x y) 2.0))
;; 平均緩和法を使用しない場合
(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)
;; 9.965784284662087
;; 3.004472209841214
;; 6.279195757507157
;; 3.759850702401539
;; 5.215843784925895
;; 4.182207192401397
;; 4.8277650983445906
;; 4.387593384662677
;; 4.671250085763899
;; 4.481403616895052
;; 4.6053657460929
;; 4.5230849678718865
;; 4.577114682047341
;; 4.541382480151454
;; 4.564903245230833
;; 4.549372679303342
;; 4.559606491913287
;; 4.552853875788271
;; 4.557305529748263
;; 4.554369064436181
;; 4.556305311532999
;; 4.555028263573554
;; 4.555870396702851
;; 4.555315001192079
;; 4.5556812635433275
;; 4.555439715736846
;; 4.555599009998291
;; 4.555493957531389
;; 4.555563237292884
;; 4.555517548417651
;; 4.555547679306398
;; 4.555527808516254
;; 4.555540912917957
;; 4.555532270803653
;; 4.555532270803653
;; 平均緩和法を使用する場合
(fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2.0)
;; 5.9828921423310435
;; 4.922168721308343
;; 4.628224318195455
;; 4.568346513136242
;; 4.5577305909237005
;; 4.555909809045131
;; 4.555599411610624
;; 4.5555465521473675
;; 4.555537551999825
;; 4.555537551999825

;;; 1.37
;; 1.37 a
(define (cont-frac n d k)
  (define (cont-frac-rep i result)
    (if (= i k)
        result
        (cont-frac-rep (+ i 1) (/ (n i) (+ (d i) result)))))
  (cont-frac-rep 1 0))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           10) ; 0.6181818181818182
(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           11) ; 0.6179775280898876

;; 1.37 b
(define (cont-frac n d k)
  (define (cont-frac-iter i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (cont-frac-iter (+ i 1))))))
  (cont-frac-iter 1))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           10) ; 0.6179775280898876
(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           11) ; 0.6180555555555556

;;; 1.38
;; e = 2.71828182846...
;; D について
;;  i mod 3 = 2 のとき i - ((i-2)/3) <- i - (i/3) で間違えた
;;  それ以外は 1
(+ (cont-frac (lambda (i) 1.0)
           (lambda (i) (if (= (remainder i 3) 2) (- i (/ (- i 2)  3))
                           1.0))
           10)
   2) ; 2.7182817182817183

;;; 1.39
(define (cont-frac n d k)
  (define (cont-frac-iter i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (- (d i) (cont-frac-iter (+ i 1)))))) ;; 減算
  (cont-frac-iter 1))
(define (tan-cf x k)
  (cont-frac (lambda (i) (if (= i 1)
                             x
                             (* x x)))
             (lambda (i) (- (* i 2) 1)) ;; 1, 3, 5, ... -> 2i - 1
             k))

(tan-cf 1.0 10) ; 1.557407724654902
