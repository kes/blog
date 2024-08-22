---
title: "Project Euler - Number 6"
author: ["Karl Stump"]
date: 2024-08-22
tags: ["euler", "lisp"]
draft: false
---

The problem can be found here: <https://projecteuler.net/problem=6>

> Find the difference between the sum of the squares of the first one hundred natural numbers and the
> square of the sum.

Once the formula for both is known, the sum of the squares, [n(n+1)(2n+1)]/6 and the sun of the
first n numbers, (2n(n+1))/2 the solution is trival:

<a id="code-snippet--formula-diff"></a>
{{< highlight lisp >}}
(defun formula/sum-of-squares (n)
  (/ (* n (+ n 1) (+ (* 2 n) 1)) 6))

(defun formula/square-of-the-sum (n)
  (expt (/ (* n (+ n 1)) 2) 2))

(defun formula/diff (n)
  (- (formula/square-of-the-sum n) (formula/sum-of-squares n)))

(formula/diff 100)
{{< /highlight >}}

```text
25164150
```

On the other hand, a recursive solution also beckons:

<a id="code-snippet--diff-r"></a>
{{< highlight lisp >}}
(defun recursive/sum-of-the-squares (n)
  (if (= n 0) n
      (+ (expt n 2)
         (recursive/sum-of-the-squares (- n 1)))))

(defun recursive/sum-of (n)
  (if (= n 0) n
      (+ n (recursive/sum-of (- n 1)))))

(defun recursive/diff (n)
  (- (expt (recursive/sum-of n) 2)
     (recursive/sum-of-the-squares n)))

(recursive/diff 100)
{{< /highlight >}}

```text
25164150
```

And we might want to look at the trace:

<a id="code-snippet--trace"></a>
{{< highlight lisp >}}

(trace recursive/sum-of-the-squares)
(recursive/sum-of-the-squares 5)
{{< /highlight >}}

and that gives == &#x2013; and the trace output:

{{< highlight text >}}
0: (RECURSIVE/SUM-OF-THE-SQUARES 5)
  1: (RECURSIVE/SUM-OF-THE-SQUARES 4)
    2: (RECURSIVE/SUM-OF-THE-SQUARES 3)
      3: (RECURSIVE/SUM-OF-THE-SQUARES 2)
        4: (RECURSIVE/SUM-OF-THE-SQUARES 1)
          5: (RECURSIVE/SUM-OF-THE-SQUARES 0)
          5: RECURSIVE/SUM-OF-THE-SQUARES returned 0
        4: RECURSIVE/SUM-OF-THE-SQUARES returned 1
      3: RECURSIVE/SUM-OF-THE-SQUARES returned 5
    2: RECURSIVE/SUM-OF-THE-SQUARES returned 14
  1: RECURSIVE/SUM-OF-THE-SQUARES returned 30
0: RECURSIVE/SUM-OF-THE-SQUARES returned 55
{{< /highlight >}}

Fun!
