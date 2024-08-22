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
  "Returns a list of n prime numbers"
  (let ((numbers (make-array n :initial-element 1))
        (i 2)
        (j 1)
        (x 0))
    ;;
    ;; note: the index of the array, is the number of
    ;; interest, while the value at array[index] is whether
    ;; it's prime or not
    ;;
    ;; starting at the prime number 2 (that is, we consider 2 to be
    ;; the first prime number)

    ;; Behold! The Sieve of Eratosthenes!  :)
    (loop while (< i (sqrt n))
          do (progn
               ;; if array[i] is a prime number, continue, but if not
               ;; there's nothing to do so we'll only need to increase i
               ;; i -- notice that this _if_ does not have an _else_

               (if (= 1 (aref numbers i))
                   (progn
                     ;; since we are on a prime, we'll iterate over
                     ;; the multiples of the prime (starting at the
                     ;; square of the prime), so (i^2 + i*x)
                     (setf x 0) ;; notice starting x = 0
                     (loop while (< (setf j (+ (expt i 2) (* i x))) n)
                           do(progn
                               (setf (aref numbers j) 0) ;; not a prime!
                               (setf x (1+ x)))))) ;; increment x for
                                                   ;; the next
                                                   ;; multiple
               ;;
               ;; if we are not on a prime, then we just skip to the
               ;; next number by incrementing the index.
               (setf i (1+ i))))
    ;;
    ;; notice that we are still in the let, so generate the list of
    ;; prime numbers
    (loop for x from 2 to (1- n)
          when (= (aref numbers x) 1)
            collect x)))
{{< /highlight >}}

Is this the best? I'm not sure. Is it the most readable? Probably not. I'm always a bit ambivalent
about using `loop` in Common Lisp. I shouldn't be since it's actually a pretty impressive [DSL](https://en.wikipedia.org/wiki/Domain-specific_language). Loop or
not, I'd rather pull some of this apart,

But, for the moment, I'll leave it.

So what's returned is a list of primes. But I want the 10001th prime.

I need to turn the list into an array, and then index to the 10001th prime.

`coerce` is the answer.

<a id="code-snippet--get-prime"></a>
{{< highlight lisp >}}
;; now the following will get the nth prime number

(defun get-prime (i &optional n)
  "Generate a list of n prime numbers and returns the ith prime number.
An n of 1600000 is more than sufficient to get the 10001th prime."
  (progn (if (null n) (setf n 1600000))
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
