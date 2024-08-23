---
title: "Project Euler - Number 7"
author: ["Karl Stump"]
date: 2024-08-22
tags: ["euler", "lisp"]
draft: false
---

You can find the problem here: <https://projecteuler.net/problem=7>

The problem statement:

> By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13 we can see that the 6th prime is 13.
>
> What is the 10001st prime number?

Consider an array like this:

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

where n (the index of the array) is the number of interest &#x2013; So, is the number prime? If
a[n] is 1 the number is prime, and if 0 it is not prime.

Generating an array initialized to 1 is easy enough. We can then go through all the multiples of the
primes set the value at that index to 0. This procedure is called the Sieve of Eratosthenes, a
description of which is easily findable. For example, here:
<https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes>

If I follow the given algorithm, then I get the following:

{{< highlight lisp "linenos=true, linenostart=1" >}}
(defun generate-primes (n)
  "Returns a list of primes from 2 to n"
  (let ((numbers (make-array n :initial-element 1)))
    ;;
    ;; note: the index of the array, is the number of
    ;; interest, while the value at array[index] is whether
    ;; it's prime or not
    ;;
    ;; starting at the prime number 2 (that is, we consider 2 to be
    ;; the first prime number)

    ;; Sieve of Eratosthenes
    (loop for i from 2 below (sqrt n)
      while (< i (sqrt n))
      when (= 1 (aref numbers i))
        do
           ;; now we'll iterate over every (i^2 + i*x) number
           (loop for x from 0
             with j = 0
             while (< (setf j (+ (expt i 2) (* i x))) n)
             do
                (setf (aref numbers j) 0)))
    ;; generate the list of prime numbers from the array.
    (loop for x from 2 to (1- n)
          when (= (aref numbers x) 1)
            collect x)))
{{< /highlight >}}

I've always been a bit ambivalent about using `loop` in Common Lisp. Perhaps I should be using scheme?
But that attitude has also caused me not to study `loop` syntax as I should.

But once you study loop at [CLHS](https://www.lispworks.com/documentation/HyperSpec/Front/index.htm) you're confronted with a really powerful [DSL](https://en.wikipedia.org/wiki/Domain-specific_language). And one gets the feeling
that it is both understandable and yet inexhaustible.

As can be seen, this is a concise expression of the sieve.

So what's returned is a list of primes. But I want the 10001th prime.

I can just turn the list into an array, and then index to the 10001th prime.

`coerce` is the answer for this.

<a id="code-snippet--get-prime"></a>
{{< highlight lisp >}}
;; now the following will get the nth prime number

(defun get-prime (i &optional n)
  "Generate a list of n prime numbers and returns the ith prime number.
An n of 105,000 is sufficient to get the 10001th prime."
  (progn (if (null n) (setf n 105000))
         (aref (coerce (generate-primes n) 'vector) (1- i))))

(get-prime 10001)
{{< /highlight >}}

So, what is the 10001th prime? Will we get the right answer?

Invoking we get&#x2026;

```text
104743
```

Yup!

Fun!
