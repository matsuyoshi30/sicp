;;;;; 1. Building Abstractions with Procedures

;;;; 1.1 The Elements of Programming

;;; 1.1.1 Expressions
486 ; 486
(+ 137 349) ; 486
(- 1000 334) ; 666
(* 5 99) ; 495
(/ 10 5) ; 2
(+ 2.7 10) ; 12.7
;; (operator argument)
;; argument = operand*
(+ 21 35 12 7) ; 75
(* 25 4 12) ; 1200

;;; 1.1.2 Naming and Environment
(define size 2)
size ; 2
(* 5 size) ; 10

;;; 1.1.3 Evaluating Combinations
;; 組み合わせの評価
;; 1) 組み合わせの部分式の評価
;; 2) 部分式を部分式の残り値（引数）に適用する
;; => 一般的評価規則（例外は特殊形式 special form と呼ばれる）
(* (+ 2 (* 4 6))
   (+ 3 5 7))
;; 上の例の場合、4つの組み合わせに対して評価規則が再帰的に適用される

;;; 1.1.4 Compound Procedures
(define (square x) (* x x))
;; (define (name parameter) (body))
(square 21) ; 441
(square (+ 2 5)) ; 49
(square (square 3)) ; 81
(define (sum-of-square x y) (+ (square x) (square y)))
(sum-of-square 3 4) ; 25
(define (f a) (sum-of-square (+ a 1) (* a 2)))
(f 5) ; 136

;;; 1.1.5 The substitution model for procedure application
;; 複合手続きを引数に適用するには、手続き本体に出てくる仮引数parameterを対応する引数argumentで置換し、それを評価する
;; 簡単なモデル
(f 5)
;; -> (sum-of-square (+ 5 1) (* 5 2))
;; -> (+ (square 6) (square 10))
;; -> (+ (* 6 6) (* 10 10))
;; -> (+ 36 100)
;; => 136
;; 「値が必要になるまで被演算子を評価しない」という適用順序の考え方もある（結果は同じ）
;; 完全に展開してから簡約する = 正規順序評価 normal-order evaluation
;; 引数を評価してから適用する = 適用順序評価 application-order evaluation （Lispはこっち）

;;; 1.1.6 Conditional Expressions and Predicates
(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) -x)))
;; (cond (p1 e1) (p2 e2) ... (pn en))
(define (abx x)
  (cond ((< x 0) -x)
        (else x)))
;; (if predicate consequent alternative)
(define (abs x)
  (if (< x 0)
      (- x)
      x))
;; (and e1 ... en)
;; (or e1 ... en)
;; (not e)

;;; 1.1.7 Example of square root by newton's method
;; 数学の関数とコンピュータの手続きの違い = 手続きは実効的effectiveでなければならない
;; 関数は「物事の属性について説明するもの」、手続きは「どうやって物事を行うかについて説明するもの」
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))
(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 9) ; 3.00009155413138
(sqrt (+ 100 37)) ; 11.704699917758145
(sqrt (+ (sqrt 2) (sqrt 3))) ; 1.7739279023207892
(square (sqrt 1000)) ; 1000.000369924366

;;; 1.1.8 Procedures as black-box abstractions
;; 手続き分割のポイント
;; 分割したそれぞれの手続きが特定のタスクを成し遂げていて、それが他の手続きを定義する際にもモジュールとして使用できる
;; -> 使用するモジュールはブラックボックスとして、「どうするか」は気にせず「何をするか」だけ知っていれば良い

;; 内部定義とブロック構造を用いて sqrt を定義する
;; 補助手続きを内部化することにより、sqrt に束縛されている x を補助手続きに明示的に渡す必要がない
;; ref. レキシカルスコープ
(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess) (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0 x))

;;;; 1.2 Procedures and the processes they generate

;;; 1.2.1 Linear recursion and iteration
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))
;; 線形再帰プロセス
;; (factorial 6)
;; (* 6 (factorial 5))
;; (* 6 (* 5 (factorial 4)))
;; (* 6 (* 5 (* 4 (factorial 3))))
;; (* 6 (* 5 (* 4 (* 3 (factorial 2)))))
;; (* 6 (* 5 (* 4 (* 3 (* 2 (factorial 1))))))
;; (* 6 (* 5 (* 4 (* 3 (* 2 1)))))
;; (* 6 (* 5 (* 4 (* 3 2))))
;; (* 6 (* 5 (* 4 6)))
;; (* 6 (* 5 24))
;; (* 6 120)
;; 720
;; 展開から縮約
;; n に対してステップ数が線形に変化し、メモリ消費も再帰呼び出し回数に比例する
(define (factorial n)
  (fact-iter 1 1 n))
(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))
;; 線形反復プロセス
;; (factorial 6)
;; (fact-iter 1 1 6)
;; (fact-iter 1 2 6)
;; (fact-iter 2 3 6)
;; (fact-iter 6 4 6)
;; (fact-iter 24 5 6)
;; (fact-iter 120 6 6)
;; (fact-iter 720 7 6)
;; 720
;; 状態が限られた数の状態変数に集約される
;; 再帰手続きで実装されていても固定の空間で実行できる

;;; 1.2.2 Tree recursion
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))))
;; 木構造で表すことができる再帰
;; 無駄が多い
;; a, bという整数のペアを使って、反復プロセスとして定式化できる
;; a <- a + b
;; b <- a
(define (fib n)
  (fib-iter 1 0 n))
(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

;; 例）両替パターン
;; 50, 25, 10, 5, 1セントの硬貨が使用できる場合
(define (count-change amount) (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination
                         kinds-of-coins))
                     kinds-of-coins)))))
;; 使用できる硬貨の種類数が与えられ、1つ目の種類のコインの額面を返す
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))
;; n種類のコインを使って金額aを両替するやり方のパターン数
;;  1) 1つ目の種類のコイン以外のすべての種類のコインを使って金額aを両替
;;  2) n種類の硬貨すべてを使って、金額a-dを両替（dは1つ目の種類のコインの額面）

;;; 1.2.3 Orders of growth
;; 入力が増えるに従ってプロセスが必要とする計算リソースはどの程度になるか

;;; 1.2.4 Exponentation
;; recursion
(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))
;; O(n)のステップ数、O(n)の空間を要する
;; iteration
(define (expt b n)
  (expt-iter b n 1))
(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b (- counter 1) (* b product))))
;; O(n)のステップ数、O(1)の空間を要する
;; 指数が2のべき乗である場合、例えばb^8のときは以下のように3回で計算できる
;; b^2 = b * b
;; b^4 = b^2 * b^2
;; b^8 = b^4 * b^4
;; 偶奇を考慮することでより一般的な定義が可能
;; n が偶 = b^n = (b^(n/2))^2 => b^n
;; n が奇 = b^n = b*b^(n-1)   => b^n
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))
(define (even? n)
  (= (remainder n 2) 0))
;; 空間、ステップ数ともに対数的な増加となる

;;; 1.2.5 Greatest Common Divisors
;; Euclid's algorithm
;; GCD(206, 40) = GCD(40, 6)
;;              = GCD(6, 4)
;;              = GCD(4, 2)
;;              = GCD(2, 0)
;;              = 2
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
;; ref) Lameの定理

;;; 1.2.6 Example: testing for primality
;; 約数を探すパターン
(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divide? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divide? a b)
  (= (remainder a b) 0))

(define (prime n)
  (= (smallest-divisor n) n))
;; -> O(root-n)の増加オーダー
;; フェルマーの小定理を使用するパターン
;; nが素数で、aがnより小さい任意の正の整数であるとき、aのn乗は法nに関してaと合同
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
;; 与えられたtimes分フェルマーテストを実行する -> 確率的手法（times分実行して素数と判定されても正しいとは限らない）
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))
;; フェルマーテストを騙す数 = カーマイケル数


;;;; 1.3 Formulating Abstractions with Higher-Order Procedures

;; 手続きを操作する手続き = 高階手続き

;;; 1.3.1 Procedures as arguments
;; a から b までの整数の和を計算
(define (sum-integer a b)
  (if (> a b)
      0
      (+ a (sum-integer (+ a 1) b))))
;; a から b までの整数の三乗の和を計算
(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a)
         (sum-cubes (+ a 1) b))))
;; 1(1*3) + 1/(5*7) + 1/(9*11) + ... を計算(π/8に収束する
(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2)))
         (pi-sum (+ a 4) b))))

;; これらの手続きには同様のパターンがみられる
;; (define (<name> a b)
;;    (if (> a b)
;;        0
;;        (+ (<term> a)
;;           (<name> (<next> a) b))))

;; 「特定の総和を計算する手続き」から「総和という概念そのもの」を抽象化して表現する
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

(define (identity x) x)
(define (sum-integer a b)
  (sum identity a inc b))

(sum-integer 1 10) ; 55

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(* 8 (pi-sum 1 1000)) ; 3.139592655589783

;; sum を定義することによって別の概念（定積分）の近似が定義できる
(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

(integral cube 0 1 0.01)  ; 0.24998750000000042
(integral cube 0 1 0.001) ; 0.249999875000001
;; 0 から 1 の cube の正確な定積分は 1/4
