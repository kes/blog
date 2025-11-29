---
title: "Project Euler - Number 7"
author: ["Karl Stump"]
date: 2024-08-22
tags: ["euler", "lisp"]
draft: false
math: true
---

You can find the problem here: <https://projecteuler.net/problem=7>

The problem statement:

> By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13 we can see that the 6th prime is 13.
>
> What is the 10001st prime number?

Consider an array like this:

<style>.table-1 table { text-align: center;  width: 30%;  margin: 0 auto;  }</style>

<div class="ox-hugo-table table-1">

| n  | a[n] |
|----|------|
| 0  | 0    |
| 1  | 1    |
| 2  | 1    |
| 3  | 1    |
| 4  | 0    |
| 5  | 1    |
| 6  | 0    |
| 7  | 1    |
| 8  | 0    |
| 9  | 0    |
| 10 | 0    |
| 11 | 1    |
| 12 | 0    |
| 13 | 1    |
|    |      |

</div>

where n (the index of the array) is the number of interest -- So, is the number prime? If
a[n] is 1 the number is prime, and if 0 it is not prime.

Generating an array initialized to 1 is easy enough. We can then go through all the multiples of the
primes set the value at that index to 0. This procedure is called the Sieve of Eratosthenes, a
description of which is easily findable. For example, here:
<https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes>

If I follow the given algorithm, then I get the following:

<a id="code-snippet--defun-generate-primes"></a>
```lisp { linenos=true, linenostart=1 }
(defun generate-primes (n)
  "Returns a list of primes from 2 to n"
    ;; Sieve of Eratosthenes
  (loop for i from 2 to (floor (sqrt n))
	with numbers = (make-array (1+ n) :initial-element 1)
	when (= 1 (aref numbers i))
	  do
	     ;; now we'll iterate over every (i^2 + i*x) number
	     (loop for x from 0
		   with j = 0 and i-squared = (expt i 2)
		   while (<= (setf j (+ i-squared (* i x))) n)
		   do
		      (setf (aref numbers j) 0))
	finally
	   ;; generate the list of prime numbers from the array.
	   (return (loop for x from 2 to n
			 when (= (aref numbers x) 1)
			   collect x))))
```

Starting with 2, we will eliminate all the multiples up to number n. When we get to the next number,
3, we start eliminating at \\( 3^2 \\). Why? Because the previous iteration using the number 2 will
have eliminated all of the numbers less than \\( 3^2 \\) that have 3 as a factor, which is \\( 3 \* 2
\\). When we move to 4, we don't process it at all because it's not a prime and has been eliminated
(by placing a 0 at that index). Then we come to 5, again we start looking for multiples at \\( 5^2
\\). Why?  Because the previous iterations on 2 and 3 will have eliminated \\(5 \* 2\\) and \\(5 \*
3\\). The same will be true for each prime.

I've always been a bit ambivalent about using `loop` in Common Lisp. Perhaps I should be using scheme?
But that attitude has also caused me not to study `loop` syntax as I should.

But once you study loop at [CLHS](https://www.lispworks.com/documentation/HyperSpec/Front/index.htm) you're confronted with a really powerful [DSL](https://en.wikipedia.org/wiki/Domain-specific_language). And one gets the feeling
that it is both understandable and yet inexhaustible. I know I've spent more time looking at the
documentation for `loop` in order to write the sieve. I think the result is concise and to the point.

Does it work? Let's test it.

<a id="code-snippet--run-generate-primes"></a>
```lisp
(generate-primes 20)
```

```text
(2 3 5 7 11 13 17 19)
```

Perfect! A list of primes. But I want the 10001th prime.

I can just turn the list into an array, and then index to the 10001th prime.

`coerce` is the answer for this.

<a id="code-snippet--defun-get-prime"></a>
```lisp
;; now the following will get the nth prime number

(defun get-prime (i &optional n)
  "Generate a list of n prime numbers and returns the ith prime number.
An n of 105,000 is sufficient to get the 10001th prime."
  (progn (if (null n) (setf n 105000))
	 (aref (coerce (generate-primes n) 'vector) (1- i))))
```

So, what is the 10001th prime? Will we get the right answer?

Invoking ...

<a id="code-snippet--run-get-prime"></a>
```lisp
(get-prime 10001)
```

we get...

```text
104743
```

Yup!

Fun!
