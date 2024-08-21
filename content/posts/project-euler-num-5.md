---
title: "Project Euler - Number 5"
author: ["Karl Stump"]
date: 2024-08-20
tags: ["euler", "lisp"]
draft: false
---

Problem number five can be found here:  <https://projecteuler.net/problem=5>

Problem statement:

> 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
>
> What is the smallest positive number that is evenly divisible by all the numbers 1 to 20?

What's wanted is the least common multiple of all numbers 2-20.

We need to factor all the number from 2 to 20. So, let's do some prime factoring!

| Num | Prime Factors      |
|-----|--------------------|
| 20  | 5 \* 2<sup>2</sup> |
| 19  | 19                 |
| 18  | 3<sup>2</sup> \* 2 |
| 17  | 17                 |
| 16  | 2<sup>4</sup>      |
| 15  | 5 \* 3             |
| 14  | 7 \* 2             |
| 13  | 13                 |
| 12  | 3 \* 2<sup>2</sup> |
| 11  | 11                 |
| 10  | 5 \* 2             |
| 9   | 3<sup>2</sup>      |
| 8   | 2<sup>3</sup>      |
| 7   | 7                  |
| 6   | 3 \* 2             |
| 5   | 5                  |
| 4   | 2<sup>2</sup>      |
| 3   | 3                  |
| 2   | 2                  |

Now, for each prime number, take the one with the greatest exponent.

Therefore:

19 \* 17 \* 13 \* 11 \* 7 \* 5 \* 3<sup>2</sup> \* 2<sup>4</sup>

In Lisp we can just do this:

{{< highlight elisp >}}
(* 19 17 13 11 7 5 (expt 3 2) (expt 2 4))
{{< /highlight >}}

```text
232792560
```

So, that's the answer. But I'd like to write a program. How?

The key insight is that we need to know how many, the count, of each prime factor.

But this is just another way saying we need to calculate the exponent for each prime factor.

How to do that?


<div class="equation-container">
<span class="equation">
<img src="/ltximg/whatisalambda_6c806a4b658426b0f13b894b51c816fc89525337.png" alt="\begin{equation*}
 2^x \le 20
\end{equation*}
" />
</span>
<span class="equation-label">
1
</span>
</div>

Ah, so, we need to take the log of both sides.


<div class="equation-container">
<span class="equation">
<img src="/ltximg/whatisalambda_5dd7a65e0bc8c30becb8e4b4037fe173e4881e4e.png" alt="\begin{equation*}
\log 2^x \le \log 20
\end{equation*}
" />
</span>
<span class="equation-label">
2
</span>
</div>

And then observe that:


<div class="equation-container">
<span class="equation">
<img src="/ltximg/whatisalambda_1d04d7ac248426c35fa0ac5560921350012b0cda.png" alt="\begin{equation*}
x \log 2 \le \log 20
\end{equation*}
" />
</span>
<span class="equation-label">
3
</span>
</div>

And finally:


<div class="equation-container">
<span class="equation">
<img src="/ltximg/whatisalambda_21c153857fdfbde7ee52c83aec973b47f8341e73.png" alt="\begin{equation*}
x \le \frac{\log 20}{\log 2}
\end{equation*}
" />
</span>
<span class="equation-label">
4
</span>
</div>

And that gives `4.321928`. But of course, only the integer is needed,
so, we'll take the `floor` of that value.

So, this will work:

<a id="code-snippet--get-exponent-floor"></a>
{{< highlight lisp >}}

(floor (/ (log 20) (log 2)))
{{< /highlight >}}

Now we need to do this calculation for each prime number in the range 1-20. We can use the
Common Lisp `loop` macro.

<a id="code-snippet--get-primes-exp"></a>
{{< highlight lisp >}}
(defun get-primes-exp ()
  (loop for x in '(2 3 5 7 11 13 17 19)
        collect (list x (floor (/ (log 20) (log x))))))

;; and then we can call with:
(get-primes-exp)
{{< /highlight >}}

`get-primes-exp` returns a list of lists of the form `((p1 e1) (p2 e2) ... )`, and here are the results
in a Lisp list:

```text
((2 4) (3 2) (5 1) (7 1) (11 1) (13 1) (17 1) (19 1))
```

Or as a table:

| Prime Factor | Exponent |
|--------------|----------|
| 2            | 4        |
| 3            | 2        |
| 5            | 1        |
| 7            | 1        |
| 11           | 1        |
| 13           | 1        |
| 17           | 1        |
| 19           | 1        |

But we need to calculate a result.

That could be done in the loop, but I"d rather keep the generation of the list separate from the
calculation.

I can use `reduce` for the calculation. If you want a refresher on reduce take a look at the [examples](http://clhs.lisp.se/Body/f_reduce.htm).

`reduce` passes the first two elements of a list to a calculator (a function of some kind). The calculator
calculates and returns a value. After the first call `reduce` will
feed into the calculator the previously calculated value and the next value off the list.

<img src="/ltximg/whatisalambda_df48593dcaa9b5c2ea82f69b90927eb6ef17cf4f.png" alt="\( (2,3,4,5)\rightarrow R \rightleftarrows calculator(x,y) \)" />

The calculator will have to accept two parameters x and y. On the first call x and y will both be of
the form of a list, `(p e)`, where p is the prime factor, and e the exponent. Thus the first
calculation is p<sub>1</sub><sup>(e<sub>1</sub>)</sup> \* p<sub>2</sub><sup>(e<sub>2</sub>)</sup>. On the second call, reduce will pass to x the previously
calculated value, and the next item off the list (which is a list) will go to y. Thus, only y has to
cope with a list and intermediately calculation every time.

So, this is the result code:

<a id="code-snippet--calc-it"></a>
{{< highlight lisp >}}
(defun calc-it ()
  (reduce (lambda (x y)
            (if (numberp x) (* x (expt (car y) (cadr y)))
                (* (expt (car x) (cadr x)) (expt (car y) (cadr y)))))
                (get-primes-exp)))
;; and to call it
(format t "Results: The LCM of all integers 1-20 is: ~d" (calc-it))
{{< /highlight >}}

And it prints out:

```text
Results: The LCM of all integers 1-20 is: 232792560
```

Excellent.

Notice in the `lambda` the `if` statement handles the variant case of  x. So, `x` will not be a number
on the very first call, but on all subsequent calls, it will be.  This is fundamental to how `reduce`
works. Let's put in a print statement and take a look.

{{< highlight lisp >}}
(reduce (lambda (x y)
          (progn
            (format t "X: ~s Y: ~s~%" x y)
            (if (numberp x) (* x (expt (car y) (cadr y)))
                (* (expt (car x) (cadr x)) (expt (car y) (cadr y))))))
        (get-primes-exp))
{{< /highlight >}}

```text
X: (2 4) Y: (3 2)
X: 144 Y: (5 1)
X: 720 Y: (7 1)
X: 5040 Y: (11 1)
X: 55440 Y: (13 1)
X: 720720 Y: (17 1)
X: 12252240 Y: (19 1)
```

And there it is. On the first call _only_ does `reduce` pass into x a list.

So the first call x = (2 3) and y = (3 2) and 144 is returned.

On the subsequent call the calculation from the previous call is passed in as x, so, x = 144, and y
= (5 1) and 720 is returned.

On the next call x = 720 &#x2026; and so on, until on the finally call x = 12252240 and y = 19 and 232792560
is returned.

Fun!
