---
title: "FRACTRAN: John Conway’s Ridiculous Little Programming Language"
author: ["Karl Stump"]
date: 2025-11-16
tags: ["euler", "lisp"]
draft: false
math: true
---

Some months back I learned about an esoteric programming language created by mathematician [John Conway](https://en.wikipedia.org/wiki/John_Horton_Conway). Conway also created the well-known [Game of Life](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life).

The language is called [FRACTRAN](https://en.wikipedia.org/wiki/FRACTRAN) (there’s a wonderful introductory lecture by Conway himself [here](https://www.uctv.tv/shows/Fractran-A-Ridiculous-Logical-Language-with-John-Conway-23320)).

Very simply, a `FRACTRAN` program is an ordered list of positive fractions

\\[ f\_1, f\_2, \ldots, f\_k \\]

and an initial integer \\( n \\).

The program runs like this:

1.  Find the first fraction \\( f\_i \\) in the list such that \\( n \cdot f\_i \\) is an integer, and replace \\( n \\) with that new integer.
2.  Repeat step (1) until no fraction in the list produces an integer when multiplied by \\( n \\); then halt.

Using only this mechanism, Conway wrote a FRACTRAN program that generates a sequence of numbers in which certain terms are powers of two whose exponents are prime. For example, starting from 2, the program eventually produces values like

\\[ 2^2,\\; 2^3,\\; 2^5,\\; 2^7,\\; 2^{11},\\; 2^{13}, \ldots \\]

The exponents are prime, and they appear in increasing order. In between those special values, the program produces a lot of "other" numbers --- noise. The job ahead will be to navigate that noise efficiently.

In this post, we’ll look at [Project Euler Problem 308](https://projecteuler.net/problem=308), which asks:

> If someone uses Conway’s `FRACTRAN` prime-generating program to "solve" Project Euler Problem 7 (find the 10,001st prime), how many iterations of the program are needed before we reach the power of two whose exponent is that prime?

We can start with a simple `FRACTRAN` simulator in Common Lisp, then gradually uncover the finite state machine hiding inside Conway’s fraction list, and finally arrive at an optimized implementation that can handle the enormous iteration count.


## The Idea of a FRACTRAN Simulator {#the-idea-of-a-fractran-simulator}

In Common Lisp, we can define the program easily as a list of lists. For each list in the list `*fp*` below, the `car` (i.e., the first element of the list) is the numerator, while the `cadr` (i.e., the second element of the list) is the denominator.

<a id="code-snippet--fractran-program"></a>
```lisp
(defparameter *fp*
  '((17 91) (78 85) (19 51) (23 38) (29 33)
    (77 29) (95 23) (77 19) (1 17) (11 13)
    (13 11) (15 2) (1 7) (55 1)))
```

We can then define a function that runs the program for one iteration.

To review a bit: we've been told that we are to pass in a seed number, also called the state, and that number is multiplied by the first fraction. If we get an integer, we return that integer. If not, we discard the calculation and move on to the next fraction and follow the same process:

-   multiply the state by the fraction, and if we get an integer then return it, otherwise, discard the calculation and move on.

If we exhaust all the fractions and have not gotten an integer, the program halts. (However, if you look closely at Conway's ~~list of fractions~~ program, you can see that we will always get a new integer. _Hint_ look at the last fraction.)

So, our function looks like this:

```lisp
(defun fractran-simulator (state program)
  "Apply STATE, to the PROGRAM using the multiplication operator.  Return
the value of the first operation that evaluates to an integer,
otherwise return nil."
  (if (null program) nil ;; no more program? Return nil
      (let ((temp (/ (* state (caar program)) (cadar program))))
	(if (integerp temp) temp
	    ;; tail call recurse
	    (fractran-simulator state (cdr program))))))
```

Let's use an initial seed value of 2 (as the problem description says).

<a id="code-snippet--fractran-simulator"></a>
```lisp
(fractran-simulator 2 *fp*)
```

What do we get back from our simulator?

```text
15
```

This is the correct result for the first iteration. So, we're off to a good start. Let's do more iterations:

<a id="code-snippet--simulator-run"></a>
```lisp

(loop for i from 1 to 20
      with state = 2
      do (setf state (fractran-simulator state *fp*))
      collect (list i state) into results
      finally (return (cons '(iteration state) results)))
```

Running the above code gives us the following:

<style>
.my-table-1 tr:nth-child(even){
background-color: #3b3f4a;
}
/*
.my-table-1 thead tr th:nth-child(12) {
  background-color:red;
}
*/
.my-table-1 th{
font-weight:normal;
text-align: center;
background-color: #3b3f4a;
padding:5px;
border: 0;
}
/* Body cells */
.my-table-1 td{
padding:5px;
border: 1px black solid;
}
</style>

<style>.my-table-1 table { text-align: center;  width: 60%;  margin: 0 auto;  }</style>

<div class="ox-hugo-table my-table-1">

| ITERATION | STATE |
|-----------|-------|
| 1         | 15    |
| 2         | 825   |
| 3         | 725   |
| 4         | 1925  |
| 5         | 2275  |
| 6         | 425   |
| 7         | 390   |
| 8         | 330   |
| 9         | 290   |
| 10        | 770   |
| 11        | 910   |
| 12        | 170   |
| 13        | 156   |
| 14        | 132   |
| 15        | 116   |
| 16        | 308   |
| 17        | 364   |
| 18        | 68    |
| 19        | 4     |
| 20        | 30    |

</div>

The state follows the sequence given in the problem description, and so, we can have some confidence. Notice that on iteration 19 we get \\( 2^2 \\), which is the first prime exponent of \\( 2 \\).

Theoretically, the simulator is sufficient to find the answer. All we have to do is keep calling the simulator. So, can we just run it and declare victory?

Sadly, no. It turns out that we'll need something more powerful. Having already   solved this problem I can tell you that the number of iterations is on the order of \\( 10^{15} \\) ---  in the quadrillions --- a rather big number.

More analysis is needed. So, . . .


## More Analysis {#more-analysis}

Taking the prime factors in the `FRACTRAN` program:

\\( \dfrac{17}{7 \times 13}, \dfrac{2 \times 3 \times 13}{5 \times 17}, \dfrac{19}{3 \times 17}, \dfrac{23}{2 \times 19}, \dfrac{29}{3 \times 11}, \dfrac{7 \times 11}{29}, \dfrac{5 \times 19}{23}, \dfrac{7 \times 11}{19}, \dfrac{1}{17}, \dfrac{11}{13}, \dfrac{13}{11}, \dfrac{3 \times 5}{2}, \dfrac{1}{7}, \dfrac{5 \times 11}{1} \\)

We need to consider exactly what is happening when we run the program. So, we start with \\( 2 \\). This is then multiplied by the fraction \\( \dfrac{17}{7 \times 13} \\). But what does that really mean? Multiplication is putting in factors, while division is taking out factors. So, we get,

\\[ \dfrac{2 \times 17}{7 \times 13} \\]

Clearly, this is not an integer. Let's move on to the next fraction, \\(\dfrac{2 \times 3 \times 13}{5 \times 17} \\), and multiplying by 2, we get

\\[ \dfrac{2^2 \times 3 \times 13}{5 \times 17} \\]

Also, not an integer.

Let's back up and put this process in tabular form:

<style>
.my-table-2 {
  border-collapse: collapse;
  margin: 0 auto;
  font-size: 0.95rem;        /* slightly smaller, feels more “set” */
}

/* Header row */
.my-table-2 thead th {
  background-color: #163515; /* light grey, like LaTeX booktabs header */
  color: #dfe61b;
  font-weight: normal;
  text-align: center;
  padding: 6px 10px;
  border: 0;
  /* border-bottom: 1px solid #cccccc; */
}

/* Body cells */
.my-table-2 tbody td {
  padding: 6px 10px;
  text-align: center;
  border: 0;
  /* border-bottom: 1px solid #e0e0e0; */
}

/* Subtle row striping */
.my-table-2 tbody tr:nth-child(even) {
  background-color: #3b3f4a;
}

/* Optional: first column slightly left-aligned to feel “texty” */
.my-table-2 tbody td:first-child {
/*text-align: left; */
}
/* Optional: narrower arrow column */
.my-table-2 tbody td:nth-child(2),
.my-table-2 thead th:nth-child(2) {
  /*width: 2.5rem; */
}
</style>

<style>.my-table-2 table { text-align: center;  width: 60%;  margin: 0 auto;  }</style>

<div class="ox-hugo-table my-table-2">

| \\(2 \times \cdots \\)                           |                        | Result                                                      |                                                                         |
|--------------------------------------------------|------------------------|-------------------------------------------------------------|-------------------------------------------------------------------------|
| \\( \dfrac{17}{7 \times 13} \\)                  | \\(\longrightarrow \\) | \\( \dfrac{2 \times 17}{7 \times 13} \\)                    | \\( \bbox[5px, border:2px solid green]{\unicode{x274c}} \\)             |
| \\( \dfrac{2 \times 3 \times 13}{5 \times 17}\\) | \\(\longrightarrow \\) | \\( \dfrac{2^2 \times 3 \times 13}{5 \times 17} \\)         | \\( \bbox[5px, border:2px solid green]{\unicode{x274c}} \\)             |
| \\( \dfrac{19}{3 \times 17} \\)                  | \\(\longrightarrow \\) | \\(  \dfrac{ 2 \times 19}{3 \times 17}\\)                   | \\( \bbox[5px, border:2px solid green]{\unicode{x274c}} \\)             |
| \\( \dfrac{23}{2 \times 19} \\)                  | \\(\longrightarrow \\) | \\( \dfrac{ \cancel{2} \times 23}{\cancel{2} \times 19} \\) | \\( \bbox[5px, border:2px solid green]{\unicode{x274c}} \\)             |
| \\( \dfrac{29}{3 \times 11} \\)                  | \\(\longrightarrow \\) | \\( \dfrac{ 2 \times 29}{3 \times 11} \\)                   | \\( \bbox[5px, border:2px solid green]{\unicode{x274c}} \\)             |
| \\( \dfrac{7 \times 11}{29} \\)                  | \\(\longrightarrow \\) | \\( \dfrac{ 2 \times 7 \times 11}{29} \\)                   | \\( \bbox[5px, border:2px solid green]{\unicode{x274c}} \\)             |
| \\( \dfrac{5 \times 19}{23} \\)                  | \\(\longrightarrow \\) | \\( \dfrac{ 2 \times 5 \times 19}{23} \\)                   | \\( \bbox[5px, border:2px solid green]{\unicode{x274c}} \\)             |
| \\( \dfrac{7 \times 11}{19} \\)                  | \\(\longrightarrow \\) | \\( \dfrac{ 2 \times 7 \times 11}{19} \\)                   | \\( \bbox[5px, border:2px solid green]{\unicode{x274c}} \\)             |
| \\( \dfrac{1}{17}           \\)                  | \\(\longrightarrow \\) | \\( \dfrac{ 2 \times 1}{17} \\)                             | \\( \bbox[5px, border:2px solid green]{\unicode{x274c}} \\)             |
| \\( \dfrac{11}{13}          \\)                  | \\(\longrightarrow \\) | \\( \dfrac{ 2 \times 11}{13} \\)                            | \\( \bbox[5px, border:2px solid green]{\unicode{x274c}} \\)             |
| \\( \dfrac{13}{11}          \\)                  | \\(\longrightarrow \\) | \\( \dfrac{ 2 \times 13}{11} \\)                            | \\( \bbox[5px, border:2px solid green]{\unicode{x274c}} \\)             |
| \\( \dfrac{3 \times 5}{2}   \\)                  | \\(\longrightarrow \\) | \\( \dfrac{ \cancel{2} \times 3 \times 5}{\cancel{2}} \\)   | \\( \bbox[5px, border:2px solid green;color:green]{\unicode{x2714}} \\) |

</div>

Finally, we get an integer, \\( 15 \\), which we will feed back into the `FRACTRAN` program.

It shouldn't be too hard to see that in the `FRACTRAN` program the last fraction \\( \dfrac{5 \times 11}{1} \\) is a catchall, and when we feed \\( 15 \\) back into the program, it is only this fraction that when multiplied by \\( 15 \\) returns an integer, and so, iteration two gives us, \\[ \dfrac{3 \times 5^2 \times 11}{1} \longrightarrow  825 \\]

Now, a quiz. If we feed in \\( 825 \\) into the program, what is the first fraction that will return an integer? And what is the resulting integer value?

If you said \\( \dots \\)

\\[ \dfrac{3 \times 5^2 \times 11}{1} \times \dfrac{29}{3 \times 11} \longrightarrow \dfrac{\cancel{3} \times 5^2 \times \cancel{11} \times 29}{\cancel{3} \times \cancel{11}} \longrightarrow 725 \\]

\\(\dots \\) you understand the process perfectly.


## Prime Factorization {#prime-factorization}

Let's see if we can write a function to do a prime factorization of the results we get from the `FRACTRAN` program.

We can try and get the prime factors for \\( 2275 \\) the number that occurs on the fifth iteration.

<a id="code-snippet--get-prime-factors"></a>
```lisp
  (defparameter primes '(2 3 5 7 11 13 17 19 23 29))

  (defun get-prime-factors (num)
    (loop for p in primes
  	if  (eq 0 (mod num p))
	  ;; we have identified p as a factor -- now how many times?
  	  collect (loop
  		    with i = num
  		    while (= 0 (mod i p))
  		    do (setf i (/ i p))
  		    collect p into caught
  		    finally (return (list p (length caught))))
	  else
	    collect (list p 0)))
;; 2275 occurs on the 5th iteration of the FRACTRAN program
;; (see above)
(get-prime-factors 2275)
```

Testing...

```text
((2 0) (3 0) (5 2) (7 1) (11 0) (13 1) (17 0) (19 0) (23 0) (29 0))
```

Or, \\[ 5^2 \times 7 \times 13  =  2275 \\] \\[ \bbox[border:3px solid green;color:green,2pt]{\unicode{x2714}} \\]

Perfect.


## Prime Factorized Results in Tabular Form {#prime-factorized-results-in-tabular-form}

Now let's put the factorization in a tabular form.

```lisp
(defun eval-pf (l)
  (reduce (lambda (x y)
            (if (numberp  x)
            	(* x (expt (car y) (cadr y)))
            	(* (expt (car x) (cadr x))
            	   (expt (car y) (cadr y)))))
          l))

(defun get-status (iter state)
  (loop for f in (get-prime-factors state)
	collect (cadr f) into fs
	finally (return (append (list iter) fs (list state)))))
(get-status 5 2275)
```

```text
(5 0 0 2 1 0 1 0 0 0 0 2275)
```

Okay, that's a step in the right direction. Can we make a table of the first twenty iterations? Let's try.

<a id="code-snippet--create-table"></a>
```lisp
(loop for i from 1 to 20
      with state = 2
      collect (get-status i (setf state (fractran-simulator state *fp*))))
```

And this gives:

<style>
.fractran-factors-table-no-header tr:nth-child(even){
background-color: #3b3f4a;
}
/*
.fractran-factors-table-no-header thead tr th:nth-child(12) {
  background-color:red;
}
*/
.fractran-factors-table-no-header th{
font-weight:normal;
text-align: center;
background-color: #3b3f4a;
padding:5px;
border: 0;
}
/* Body cells */
.fractran-factors-table-no-header td{
padding:5px;
border: 1px black solid;
}
</style>

<style>.fractran-factors-table-no-header table { text-align: center;  width: 80%;  margin: 0 auto;  }</style>

<div class="ox-hugo-table fractran-factors-table-no-header">

| 1  | 0 | 1 | 1 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 15   |
|----|---|---|---|---|---|---|---|---|---|---|------|
| 2  | 0 | 1 | 2 | 0 | 1 | 0 | 0 | 0 | 0 | 0 | 825  |
| 3  | 0 | 0 | 2 | 0 | 0 | 0 | 0 | 0 | 0 | 1 | 725  |
| 4  | 0 | 0 | 2 | 1 | 1 | 0 | 0 | 0 | 0 | 0 | 1925 |
| 5  | 0 | 0 | 2 | 1 | 0 | 1 | 0 | 0 | 0 | 0 | 2275 |
| 6  | 0 | 0 | 2 | 0 | 0 | 0 | 1 | 0 | 0 | 0 | 425  |
| 7  | 1 | 1 | 1 | 0 | 0 | 1 | 0 | 0 | 0 | 0 | 390  |
| 8  | 1 | 1 | 1 | 0 | 1 | 0 | 0 | 0 | 0 | 0 | 330  |
| 9  | 1 | 0 | 1 | 0 | 0 | 0 | 0 | 0 | 0 | 1 | 290  |
| 10 | 1 | 0 | 1 | 1 | 1 | 0 | 0 | 0 | 0 | 0 | 770  |
| 11 | 1 | 0 | 1 | 1 | 0 | 1 | 0 | 0 | 0 | 0 | 910  |
| 12 | 1 | 0 | 1 | 0 | 0 | 0 | 1 | 0 | 0 | 0 | 170  |
| 13 | 2 | 1 | 0 | 0 | 0 | 1 | 0 | 0 | 0 | 0 | 156  |
| 14 | 2 | 1 | 0 | 0 | 1 | 0 | 0 | 0 | 0 | 0 | 132  |
| 15 | 2 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 1 | 116  |
| 16 | 2 | 0 | 0 | 1 | 1 | 0 | 0 | 0 | 0 | 0 | 308  |
| 17 | 2 | 0 | 0 | 1 | 0 | 1 | 0 | 0 | 0 | 0 | 364  |
| 18 | 2 | 0 | 0 | 0 | 0 | 0 | 1 | 0 | 0 | 0 | 68   |
| 19 | 2 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 4    |
| 20 | 1 | 1 | 1 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 30   |

</div>

That's getting there. Let's add some headers.

<a id="code-snippet--FORMAT-TABLE"></a>
```elisp
(let (value)
  (dolist (p table value)
    (setf (nth 5 p) (if (= (nth 5 p) 0) '-- (nth 5 p)))
    (setf (nth 6 p) (if (= (nth 6 p) 0) '-- (nth 6 p)))
    (setf (nth 7 p) (if (= (nth 7 p) 0) '-- (nth 7 p)))
    (setf (nth 8 p) (if (= (nth 8 p) 0) '-- (nth 8 p)))
    (setf (nth 9 p) (if (= (nth 9 p) 0) '-- (nth 9 p)))
    (setf (nth 10 p) (if (= (nth 10 p) 0) '-- (nth 10 p)))
    (setf value (cons p value)))
  (setf value (reverse value))
  (setf value (cons 'hline value))
  (setf value (cons '(Iteration 2 3 5 7 11 13 17 19 23 29 Value) value)))
```

This gives:

<style>
/* Base table styling */
.fractran-factors-with-header {
  border-collapse: collapse;
}

/* Header cells */
.fractran-factors-with-header thead th {
  font-weight: normal;
  text-align: center;
  background-color: #575551;   /* overall header background */
  color: #f0f0f0;
  padding: 6px 8px;
  border-bottom: 1px white solid;
  border-top: 2px white solid;
  border-right: 2px white solid;
  border-left: 2px white solid;
}

/* Body cells */
.fractran-factors-with-header td {
  padding: 4px 8px;
  text-align: center;
  border: 1px black solid;
}

/* Row striping (body only) */
.fractran-factors-with-header tbody tr:nth-child(even) {
  background-color: #2f2f2f;
}

/* Highlight factor columns 11–29: columns 6–11 (header + body) */
.fractran-factors-with-header thead th:nth-child(n+6):nth-child(-n+11),
.fractran-factors-with-header tbody td:nth-child(n+6):nth-child(-n+11) {
  background-color: #42403c;  /* slightly different tone */
}

/* Optional: make the "Value" column pop a bit */
.fractran-factors-with-header thead th:nth-child(12),
.fractran-factors-with-header tbody td:nth-child(12) {
  background-color: #3b3f4a;
  font-weight: bold;
}
</style>

<style>.fractran-factors-with-header table { text-align: center;  width: 80%;  margin: 0 auto;  }</style>

<div class="ox-hugo-table fractran-factors-with-header">

| Iteration | 2 | 3 | 5 | 7 | 11 | 13 | 17 | 19 | 23 | 29 | Value |
|-----------|---|---|---|---|----|----|----|----|----|----|-------|
| 1         | 0 | 1 | 1 | 0 | -- | -- | -- | -- | -- | -- | 15    |
| 2         | 0 | 1 | 2 | 0 | 1  | -- | -- | -- | -- | -- | 825   |
| 3         | 0 | 0 | 2 | 0 | -- | -- | -- | -- | -- | 1  | 725   |
| 4         | 0 | 0 | 2 | 1 | 1  | -- | -- | -- | -- | -- | 1925  |
| 5         | 0 | 0 | 2 | 1 | -- | 1  | -- | -- | -- | -- | 2275  |
| 6         | 0 | 0 | 2 | 0 | -- | -- | 1  | -- | -- | -- | 425   |
| 7         | 1 | 1 | 1 | 0 | -- | 1  | -- | -- | -- | -- | 390   |
| 8         | 1 | 1 | 1 | 0 | 1  | -- | -- | -- | -- | -- | 330   |
| 9         | 1 | 0 | 1 | 0 | -- | -- | -- | -- | -- | 1  | 290   |
| 10        | 1 | 0 | 1 | 1 | 1  | -- | -- | -- | -- | -- | 770   |
| 11        | 1 | 0 | 1 | 1 | -- | 1  | -- | -- | -- | -- | 910   |
| 12        | 1 | 0 | 1 | 0 | -- | -- | 1  | -- | -- | -- | 170   |
| 13        | 2 | 1 | 0 | 0 | -- | 1  | -- | -- | -- | -- | 156   |
| 14        | 2 | 1 | 0 | 0 | 1  | -- | -- | -- | -- | -- | 132   |
| 15        | 2 | 0 | 0 | 0 | -- | -- | -- | -- | -- | 1  | 116   |
| 16        | 2 | 0 | 0 | 1 | 1  | -- | -- | -- | -- | -- | 308   |
| 17        | 2 | 0 | 0 | 1 | -- | 1  | -- | -- | -- | -- | 364   |
| 18        | 2 | 0 | 0 | 0 | -- | -- | 1  | -- | -- | -- | 68    |
| 19        | 2 | 0 | 0 | 0 | -- | -- | -- | -- | -- | -- | 4     |
| 20        | 1 | 1 | 1 | 0 | -- | -- | -- | -- | -- | -- | 30    |

</div>

And the results look better. What I've done for primes \\( 11 \\) through \\( 29 \\), if the exponent is 0 then I've
replaced it with "--". This removes a lot of clutter and make the table easier to read.

Examining the table it looks like primes \\( 11 \\) through \\( 29 \\) are mutually exclusive (an `XOR`). Only
one can be on at a time.

So primes \\( 11 \\) through \\( 29 \\) function as _control nodes_ in a finite state machine, while primes \\( 2,3,5, \text{ and } 7 \\) encode the data state (their exponents are the “registers” we’re updating).

This distinction between the primes \\( 11 \\) through \\( 29 \\) and the primes \\( 2 \\) through \\( 7 \\) will become really important, like, right now!


## Grokking FRACTRAN {#grokking-fractran}

Let's define a finite state machine (FSM) with a starting node, labeled \\( 1 \\), and further that there are nodes numbered, \\( 11, 13, 17, 19, 23, 29 \\), and that these nodes have edges.

The FSM, also, has state, carried in factors, \\( 2, 3, 5, \text{ and } 7 \\) (or in variables representing those factors). Simply, think of exponents. So, the factor \\( 2 \\) we might say, has a value of \\( 3 \\), and this simply means, \\( 2^3 \\)

In general moving along an edge, from node to node, will increment or decrement some, all, or no, factors. However, in no case, is a move permitted along any edge that causes the value of a factor to be less than zero.

Every fraction in Conway's program defines a node and an edge.

Consider the first fraction in the program: \\[ \dfrac{17}{7 \times 13} \\]

This fraction tells us that there is a node \\( 13\\) with an edge to node \\( 17 \\). Further movement along that edge decrements factor \\( 7 \\).

If we look for other fractions with \\( 13 \\) in the denominator, we find: \\[ \dfrac{11}{13} \\]

This means that node \\( 13 \\) also has a null edge to node \\( 11 \\).

Now, since there are no other fractions with \\( 13 \\) in the denominator, this finishes the definition of node \\( 13 \\).

So, there is in the FSM, node \\(13\\) with two edges. The first is to node \\( 17 \\) (allowed only if the current state value has a factor of \\( 7 \\) ), and a second null edge, to node \\( 11 \\).

Note that the `FRACTRAN` program runs from left to right, so in the event of more than one edge from a node, there is a priority. In programming languages this is called _associativity_. In `FRACTRAN` the _associativity_ is left to right. In this example, the edge from node \\( 13 \\) to \\( 17 \\) takes priority over the edge to node \\( 11 \\).

Working in this way, we can define our FSM as follows (if you are not conversant with dot code, don't worry about it, the actual diagram follows):

```dot
digraph finite_state_machine {
    graph [pad="0.5", nodesep="1", ranksep="2"];
    splines=true;
	fontname="Helvetica,Arial,sans-serif"
	node [fontname="Helvetica,Arial,sans-serif"]
	edge [fontname="Helvetica,Arial,sans-serif"]

	node [shape = doublecircle]; 2
	node [shape = circle];
2->S1
S1:n -> S1:n[margin=1;headlabel="(1) -2, +3, +5";color=blue];
S1:e -> S1[label="(2) -7";color=red];
S1 -> S11[label="(3) +5"; color=green];

S11 -> S29[label="(1) -3";color=blue];
S11 -> S13[label="(2) null";color=red];

S13:se -> S17:sw[label="(1) -7";color=blue];
S13 -> S11[label="(2) null";color=red];


S17 -> S13[label="(1) -5, +2, +3";color=blue];
S17 -> S19:sw[label="(2) -3";color=red];
S17 -> S1[label="(3) null"; color=green];

S19 -> S23:nw[label="(1) -2";color=blue];
S19:w -> S11:e[label="(2) +7";color=red];


S23:e -> S19:e[label="(1) +5";labelangle=5;color=blue];
S29 -> S11[label="(1) +7";color=blue];

{ rank=min; S1;2  }
{rank=same; S11;S19};
{ rank=max;  S23;S13; S17; }
}
```

From the fraction list, the following is the equivalent finite state machine, where each “node” corresponds to one of the large primes \\( 11,13,17,19,23,29 \\) and the factors \\( 2,3,5,7 \\) the “registers” in the system.

{{< figure src="/ox-hugo/test.png" >}}

Here's how it works. On start, you pass in a \\( 2 \\) to `S1`.

In the diagram you can see the edges to the other nodes, and you can see the priority given by the number in parenthesis.

The effect upon the state of the FSM is given by state factors. A positive state factor means that the factor's value will be incremented, while a negative sign means that the factor's value will be decremented.

So, from node `S1` edge (1) requires that factor \\( 2 \\) be decremented by one. But this edge also requires that factors \\( 3 \text{ and } 5 \\) be incremented.

If following edge (1) is not possible, then we go to edge (2), which is a transition into the same state `S1` but requires that factor \\( 7 \\) reduced. If that's not possible then there is edge (3) which increments factor \\(5\\) (And if none of edges were possible, then the program would simply halt.)

So, going back to the start, in `S1` the first option works and so factor \\( 2 \\) is decremented, and factors \\( 3 \\) and \\( 5 \\) are incremented. Hence, on the first iteration we get \\( 15 \\). This  comports (totally) with the result of our program's first iteration.

The FSM works.

Every node, is read in the same way.

Now, let's do the second iteration. Our number is \\( 15 \\) (i.e., \\( 3^1 \times 5^1 \\) ) and we are still in the `S1` node.

We have no factors of \\( 2 \\) at this point (i.e., \\( 2^0 \\) ), so option (1) is not available. What about option (2)? The answer is no, decrementing the factor  \\( 7 \\) would result in a negative number. That leaves option (3), and so we increment factor \\( 5 \\) and transition to state `S11`. So, now we have now  \\( 3^1 5^2 \\) and \\( 11^1 \\). And again, this is exactly the value we expect.

NOTE: **When we enter into a node, we effectively factor in the state number we are entering, and when we leave a node, we factor out the node number.** This is important for the code we will be demonstrate coming up, for producing tables and values. In the end however, we won't care about these values. The goal after all is to find the number of iterations it takes to get \\( 2^{10001^{st}\text{ prime }} \\).

So that's the second iteration.

And that's the process, moving from state to state, incrementing and decrementing state factors.


## Implementation of the FSM {#implementation-of-the-fsm}

What follows is my first attempt at developing the FSM. It works in the sense that it produces correct results. But it's too slow.

You can happily skip this section if you are so inclined. Below, you can find a working solution:  [FRACTRAN For Real](#fractran-for-real)

```lisp
(defparameter *fractran-program* '((17 91) (78 85) ( 19 51) ( 23 38) (29 33) (77 29) (95 23) (77 19) (1 17) (11 13) (13 11) (15 2) (1 7) (55 1)))
(defparameter *fractran-registers* '(:rs 1 :rp 0 :rc 0 :r1 0 :r2 1 :r3 0 :r5 0 :r7 0 :r11 0 :r13 0 :r17 0 :r19 0 :r23 0 :r29 0 ))

(defmacro r (x)
  `(getf reg ,(read-from-string (concatenate 'string ":r" (write-to-string x)))))

(defmacro fr (x)
  `(getf *fractran-registers* ,(read-from-string (concatenate 'string ":r" (write-to-string x)))))

(defmacro state-change (os ns)
  (list 'progn `(decf (r ,os)) `(incf (r ,ns)) `(setf (r s) ,ns)))
```

In the above the FSM state factors are: r2, r3, r5, r7, and the nodes r11, r13, r17, r19, r23 and r29.

As I was developing the code, I didn't realize that r11-r29 could be ignored, and so, in my code I kept incrementing and decrementing them as I moved from node to node. This is very inefficient.

rs is the current node, rp is the count of primes, rc is iteration count.

And r1 is not needed at all.

You can also see a few convenience macros that made setting the exponents and changing from node to node much easier. This is a nice technique, but is far too slow for what we want to accomplish.  (You can see from the name of the macro, "state-change" that I had not settled on what to call "nodes" -- initially, I used the word, "state" but now prefer to think of moving among nodes and carrying state).

So, in the following code I simply go to whatever state the register says I'm in.

```lisp
(defun fractran (reg)
"Calls the required state with the register. returns a list of state and register."
(cond ((= (r s) 1) (s1 reg))
	((= (r s) 11) (s11 reg))
	((= (r s) 13) (s13 reg))
	((= (r s) 17) (s17 reg))
	((= (r s) 19) (s19 reg))
	((= (r s) 23) (s23 reg))
	((= (r s) 29) (s29 reg))))
```


### S1 {#s1}

Remember what we can do in `S1`. We must follow the specific order:

1.  Can I decrement 2?
    1.  Yes: Decrement 2, increment 3, increment 7, and stay in `S1`
    2.  No. Can I decrement 7?
        1.  Yes. Decrement 7, and stay in `S1`
        2.  No. Increment 5 and move to `S11`

Also, in `S1` a check is made for a power of two.

<a id="code-snippet--S1"></a>
```lisp
(defun s1 (reg)
  (if (and (= (r 3) 0)
	   (= (r 5) 0)
	   (= (r 7) 0)
	   (> (r c) 1))
	   (incf (r p)))
  (incf (r c))
  (cond ((> (r 2) 0) ;; note don't change state
	 (decf (r 2))
	 (incf (r 3))
	 (incf (r 5)))
	((> (r 7) 0) ;; don't change state
	 (decf (r 7)))
	(t (incf (r 5))
	   (state-change 1 11))))
```


### S11 {#s11}

Each state that follows is very easy to follow. A simple implementation of the fractran program.

<a id="code-snippet--S11"></a>
```lisp
(defun s11 (reg)
  (incf (r c))
  (cond ((> (r 3) 0)
	 (decf (r 3))
	 (state-change 11 29))
	 (t (state-change 11 13))))
```


### S13 {#s13}

<a id="code-snippet--S13"></a>
```lisp
(defun s13 (reg)
  (incf (r c))
  (cond ((> (r 7) 0)
	 (decf (r 7))
	 (state-change 13 17))
	(t (state-change 13 11))))
```


### S17 {#s17}

<a id="code-snippet--S17"></a>
```lisp
(defun s17 (reg)
  (incf (r c))
  (cond ((> (r 5) 0)
	 (decf (r 5))
	 (incf (r 2))
	 (incf (r 3))
	 (state-change 17 13))
	((> (r 3) 0)
	 (decf (r 3))
	 (state-change 17 19))
	(t (state-change 17 1))))
```


### S19 {#s19}

```lisp
(defun s19 (reg)
  (incf (r c))
  (cond ((> (r 2) 0)
	 (decf (r 2))
	 (state-change 19 23))
	;; otherwise update factors and move to state 11
	(t (incf (r 7))
	   (state-change 19 11))))
```


### S23 S29 {#s23-s29}

```lisp
(defun s23 (reg)
  (incf (r c))
  (incf (r 5))
  (state-change 23 19))

(defun s29 (reg)
  (incf (r c))
  (incf (r 7))
  (state-change 29 11))
```


### Miscellaneous {#miscellaneous}

<a id="code-snippet--iterate"></a>
```lisp
    (defun init-fr ()
    (setf (fr c) 0)
    (setf (fr s) 1)
    (setf (fr p) 0)
    (setf (fr 1) 0)
    (setf (fr 2) 1)
    (setf (fr 3) 0)
    (setf (fr 5) 0)
    (setf (fr 7) 0)
    (setf (fr 11) 0)
    (setf (fr 13) 0)
    (setf (fr 17) 0)
    (setf (fr 19) 0)
    (setf (fr 23) 0)
    (setf (fr 29) 0))

(defun calc-amount ()
   (* (expt 2 (fr 2))
	     (expt 3 (fr 3))
	     (expt 5 (fr 5))
	     (expt 7 (fr 7))
	     (expt 11 (fr 11))
	     (expt 13 (fr 13))
	     (expt 17 (fr 17))
	     (expt 19 (fr 19))
	     (expt 23 (fr 23))
	     (expt 29 (fr 29))))

(defun iterate (i-number)
  (loop repeat i-number
	do (fractran *fractran-registers*)
	collect (list  (fr c) (fr s) (fr 2) (fr 3) (fr 5) (fr 7) (fr 11)
	  (fr 13) (fr 17) (fr 19) (fr 23) (fr 29) (calc-amount))))
(init-fr)
(iterate 20)
```

#+begin_export html
<style>
.my-table-5 tr:nth-child(even){
background-color: #3b3f4a;
}
/*
.my-table-5 thead tr th:nth-child(12) {
  background-color:red;
}
*/
.my-table-5 th{
font-weight:normal;
text-align: center;
background-color: #3b3f4a;
padding:5px;
border: 0;
}
/* Body cells */
.my-table-5 td{
padding:5px;
border: 1px black solid;
}
</style>

<style>.my-table-5 table { text-align: center;  width: 80%;  margin: 0 auto;  }</style>

<div class="ox-hugo-table my-table-5">

| 1  | 1  | 0 | 1 | 1 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 15    |
|----|----|---|---|---|---|---|---|---|---|---|---|-------|
| 2  | 11 | 0 | 1 | 2 | 0 | 1 | 0 | 0 | 0 | 0 | 0 | 825   |
| 3  | 29 | 0 | 0 | 2 | 0 | 0 | 0 | 0 | 0 | 0 | 1 | 725   |
| 4  | 11 | 0 | 0 | 2 | 1 | 1 | 0 | 0 | 0 | 0 | 0 | 1925  |
| 5  | 13 | 0 | 0 | 2 | 1 | 0 | 1 | 0 | 0 | 0 | 0 | 2275  |
| 6  | 17 | 0 | 0 | 2 | 0 | 0 | 0 | 1 | 0 | 0 | 0 | 425   |
| 7  | 13 | 1 | 1 | 1 | 0 | 0 | 1 | 0 | 0 | 0 | 0 | 390   |
| 8  | 11 | 1 | 1 | 1 | 0 | 1 | 0 | 0 | 0 | 0 | 0 | 330   |
| 9  | 29 | 1 | 0 | 1 | 0 | 0 | 0 | 0 | 0 | 0 | 1 | 290   |
| 10 | 11 | 1 | 0 | 1 | 1 | 1 | 0 | 0 | 0 | 0 | 0 | 770   |
| 11 | 13 | 1 | 0 | 1 | 1 | 0 | 1 | 0 | 0 | 0 | 0 | 910   |
| 12 | 17 | 1 | 0 | 1 | 0 | 0 | 0 | 1 | 0 | 0 | 0 | 170   |
| 13 | 13 | 2 | 1 | 0 | 0 | 0 | 1 | 0 | 0 | 0 | 0 | 156   |
| 14 | 11 | 2 | 1 | 0 | 0 | 1 | 0 | 0 | 0 | 0 | 0 | 132   |
| 15 | 29 | 2 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 1 | 116   |
| 16 | 11 | 2 | 0 | 0 | 1 | 1 | 0 | 0 | 0 | 0 | 0 | 308   |
| 17 | 13 | 2 | 0 | 0 | 1 | 0 | 1 | 0 | 0 | 0 | 0 | 364   |
| 18 | 17 | 2 | 0 | 0 | 0 | 0 | 0 | 1 | 0 | 0 | 0 | 68    |
| 19 | 1  | 2 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 4     |
| 20 | 1  | 1 | 1 | 1 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 30    |
| 21 | 1  | 0 | 2 | 2 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 225   |
| 22 | 11 | 0 | 2 | 3 | 0 | 1 | 0 | 0 | 0 | 0 | 0 | 12375 |
| 23 | 29 | 0 | 1 | 3 | 0 | 0 | 0 | 0 | 0 | 0 | 1 | 10875 |
| 24 | 11 | 0 | 1 | 3 | 1 | 1 | 0 | 0 | 0 | 0 | 0 | 28875 |
| 25 | 29 | 0 | 0 | 3 | 1 | 0 | 0 | 0 | 0 | 0 | 1 | 25375 |
| 26 | 11 | 0 | 0 | 3 | 2 | 1 | 0 | 0 | 0 | 0 | 0 | 67375 |
| 27 | 13 | 0 | 0 | 3 | 2 | 0 | 1 | 0 | 0 | 0 | 0 | 79625 |
| 28 | 17 | 0 | 0 | 3 | 1 | 0 | 0 | 1 | 0 | 0 | 0 | 14875 |
| 29 | 13 | 1 | 1 | 2 | 1 | 0 | 1 | 0 | 0 | 0 | 0 | 13650 |
| 30 | 17 | 1 | 1 | 2 | 0 | 0 | 0 | 1 | 0 | 0 | 0 | 2550  |
| 31 | 13 | 2 | 2 | 1 | 0 | 0 | 1 | 0 | 0 | 0 | 0 | 2340  |
| 32 | 11 | 2 | 2 | 1 | 0 | 1 | 0 | 0 | 0 | 0 | 0 | 1980  |
| 33 | 29 | 2 | 1 | 1 | 0 | 0 | 0 | 0 | 0 | 0 | 1 | 1740  |
| 34 | 11 | 2 | 1 | 1 | 1 | 1 | 0 | 0 | 0 | 0 | 0 | 4620  |
| 35 | 29 | 2 | 0 | 1 | 1 | 0 | 0 | 0 | 0 | 0 | 1 | 4060  |
| 36 | 11 | 2 | 0 | 1 | 2 | 1 | 0 | 0 | 0 | 0 | 0 | 10780 |
| 37 | 13 | 2 | 0 | 1 | 2 | 0 | 1 | 0 | 0 | 0 | 0 | 12740 |
| 38 | 17 | 2 | 0 | 1 | 1 | 0 | 0 | 1 | 0 | 0 | 0 | 2380  |
| 39 | 13 | 3 | 1 | 0 | 1 | 0 | 1 | 0 | 0 | 0 | 0 | 2184  |
| 40 | 17 | 3 | 1 | 0 | 0 | 0 | 0 | 1 | 0 | 0 | 0 | 408   |
| 41 | 19 | 3 | 0 | 0 | 0 | 0 | 0 | 0 | 1 | 0 | 0 | 152   |
| 42 | 23 | 2 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 1 | 0 | 92    |
| 43 | 19 | 2 | 0 | 1 | 0 | 0 | 0 | 0 | 1 | 0 | 0 | 380   |
| 44 | 23 | 1 | 0 | 1 | 0 | 0 | 0 | 0 | 0 | 1 | 0 | 230   |
| 45 | 19 | 1 | 0 | 2 | 0 | 0 | 0 | 0 | 1 | 0 | 0 | 950   |
| 46 | 23 | 0 | 0 | 2 | 0 | 0 | 0 | 0 | 0 | 1 | 0 | 575   |
| 47 | 19 | 0 | 0 | 3 | 0 | 0 | 0 | 0 | 1 | 0 | 0 | 2375  |
| 48 | 11 | 0 | 0 | 3 | 1 | 1 | 0 | 0 | 0 | 0 | 0 | 9625  |
| 49 | 13 | 0 | 0 | 3 | 1 | 0 | 1 | 0 | 0 | 0 | 0 | 11375 |
| 50 | 17 | 0 | 0 | 3 | 0 | 0 | 0 | 1 | 0 | 0 | 0 | 2125  |

</div>

<a id="code-snippet--FORMAT-TABLE-for-iterate"></a>
```elisp
(let (value)
  (dolist (p table value)
    (setf (nth 5 p) (if (= (nth 5 p) 0) '-- (nth 5 p)))
    (setf (nth 6 p) (if (= (nth 6 p) 0) '-- (nth 6 p)))
    (setf (nth 7 p) (if (= (nth 7 p) 0) '-- (nth 7 p)))
    (setf (nth 8 p) (if (= (nth 8 p) 0) '-- (nth 8 p)))
    (setf (nth 9 p) (if (= (nth 9 p) 0) '-- (nth 9 p)))
    (setf (nth 10 p) (if (= (nth 10 p) 0) '-- (nth 10 p)))
    (setf (nth 11 p) (if (= (nth 11 p) 0) '-- (nth 11 p)))
    (setf value (cons p value)))
  (setf value (reverse value))
  (setf value (cons 'hline value))
  (setf value (cons '(Iteration State 2 3 5 7 11 13 17 19 23 29 Amount) value)))
```

#+begin_export html
<style>
.my-table-6 tr:nth-child(even){
background-color: #3b3f4a;
}
/*
.my-table-6 thead tr th:nth-child(12) {
  background-color:red;
}
*/
.my-table-6 th{
font-weight:normal;
text-align: center;
background-color: #3b3f4a;
padding:5px;
border: 0;
}
/* Body cells */
.my-table-6 td{
padding:5px;
border: 1px black solid;
}
</style>

<style>.my-table-6 table { text-align: center;  width: 90%;  margin: 0 auto;  }</style>

<div class="ox-hugo-table my-table-6">

| Iteration | State | 2 | 3 | 5 | 7  | 11 | 13 | 17 | 19 | 23 | 29 | Amount |
|-----------|-------|---|---|---|----|----|----|----|----|----|----|--------|
| 1         | 1     | 0 | 1 | 1 | -- | -- | -- | -- | -- | -- | -- | 15     |
| 2         | 11    | 0 | 1 | 2 | -- | 1  | -- | -- | -- | -- | -- | 825    |
| 3         | 29    | 0 | 0 | 2 | -- | -- | -- | -- | -- | -- | 1  | 725    |
| 4         | 11    | 0 | 0 | 2 | 1  | 1  | -- | -- | -- | -- | -- | 1925   |
| 5         | 13    | 0 | 0 | 2 | 1  | -- | 1  | -- | -- | -- | -- | 2275   |
| 6         | 17    | 0 | 0 | 2 | -- | -- | -- | 1  | -- | -- | -- | 425    |
| 7         | 13    | 1 | 1 | 1 | -- | -- | 1  | -- | -- | -- | -- | 390    |
| 8         | 11    | 1 | 1 | 1 | -- | 1  | -- | -- | -- | -- | -- | 330    |
| 9         | 29    | 1 | 0 | 1 | -- | -- | -- | -- | -- | -- | 1  | 290    |
| 10        | 11    | 1 | 0 | 1 | 1  | 1  | -- | -- | -- | -- | -- | 770    |
| 11        | 13    | 1 | 0 | 1 | 1  | -- | 1  | -- | -- | -- | -- | 910    |
| 12        | 17    | 1 | 0 | 1 | -- | -- | -- | 1  | -- | -- | -- | 170    |
| 13        | 13    | 2 | 1 | 0 | -- | -- | 1  | -- | -- | -- | -- | 156    |
| 14        | 11    | 2 | 1 | 0 | -- | 1  | -- | -- | -- | -- | -- | 132    |
| 15        | 29    | 2 | 0 | 0 | -- | -- | -- | -- | -- | -- | 1  | 116    |
| 16        | 11    | 2 | 0 | 0 | 1  | 1  | -- | -- | -- | -- | -- | 308    |
| 17        | 13    | 2 | 0 | 0 | 1  | -- | 1  | -- | -- | -- | -- | 364    |
| 18        | 17    | 2 | 0 | 0 | -- | -- | -- | 1  | -- | -- | -- | 68     |
| 19        | 1     | 2 | 0 | 0 | -- | -- | -- | -- | -- | -- | -- | 4      |
| 20        | 1     | 1 | 1 | 1 | -- | -- | -- | -- | -- | -- | -- | 30     |
| 21        | 1     | 0 | 2 | 2 | -- | -- | -- | -- | -- | -- | -- | 225    |
| 22        | 11    | 0 | 2 | 3 | -- | 1  | -- | -- | -- | -- | -- | 12375  |
| 23        | 29    | 0 | 1 | 3 | -- | -- | -- | -- | -- | -- | 1  | 10875  |
| 24        | 11    | 0 | 1 | 3 | 1  | 1  | -- | -- | -- | -- | -- | 28875  |
| 25        | 29    | 0 | 0 | 3 | 1  | -- | -- | -- | -- | -- | 1  | 25375  |
| 26        | 11    | 0 | 0 | 3 | 2  | 1  | -- | -- | -- | -- | -- | 67375  |
| 27        | 13    | 0 | 0 | 3 | 2  | -- | 1  | -- | -- | -- | -- | 79625  |
| 28        | 17    | 0 | 0 | 3 | 1  | -- | -- | 1  | -- | -- | -- | 14875  |
| 29        | 13    | 1 | 1 | 2 | 1  | -- | 1  | -- | -- | -- | -- | 13650  |
| 30        | 17    | 1 | 1 | 2 | -- | -- | -- | 1  | -- | -- | -- | 2550   |
| 31        | 13    | 2 | 2 | 1 | -- | -- | 1  | -- | -- | -- | -- | 2340   |
| 32        | 11    | 2 | 2 | 1 | -- | 1  | -- | -- | -- | -- | -- | 1980   |
| 33        | 29    | 2 | 1 | 1 | -- | -- | -- | -- | -- | -- | 1  | 1740   |
| 34        | 11    | 2 | 1 | 1 | 1  | 1  | -- | -- | -- | -- | -- | 4620   |
| 35        | 29    | 2 | 0 | 1 | 1  | -- | -- | -- | -- | -- | 1  | 4060   |
| 36        | 11    | 2 | 0 | 1 | 2  | 1  | -- | -- | -- | -- | -- | 10780  |
| 37        | 13    | 2 | 0 | 1 | 2  | -- | 1  | -- | -- | -- | -- | 12740  |
| 38        | 17    | 2 | 0 | 1 | 1  | -- | -- | 1  | -- | -- | -- | 2380   |
| 39        | 13    | 3 | 1 | 0 | 1  | -- | 1  | -- | -- | -- | -- | 2184   |
| 40        | 17    | 3 | 1 | 0 | -- | -- | -- | 1  | -- | -- | -- | 408    |
| 41        | 19    | 3 | 0 | 0 | -- | -- | -- | -- | 1  | -- | -- | 152    |
| 42        | 23    | 2 | 0 | 0 | -- | -- | -- | -- | -- | 1  | -- | 92     |
| 43        | 19    | 2 | 0 | 1 | -- | -- | -- | -- | 1  | -- | -- | 380    |
| 44        | 23    | 1 | 0 | 1 | -- | -- | -- | -- | -- | 1  | -- | 230    |
| 45        | 19    | 1 | 0 | 2 | -- | -- | -- | -- | 1  | -- | -- | 950    |
| 46        | 23    | 0 | 0 | 2 | -- | -- | -- | -- | -- | 1  | -- | 575    |
| 47        | 19    | 0 | 0 | 3 | -- | -- | -- | -- | 1  | -- | -- | 2375   |
| 48        | 11    | 0 | 0 | 3 | 1  | 1  | -- | -- | -- | -- | -- | 9625   |
| 49        | 13    | 0 | 0 | 3 | 1  | -- | 1  | -- | -- | -- | -- | 11375  |
| 50        | 17    | 0 | 0 | 3 | -- | -- | -- | 1  | -- | -- | -- | 2125   |

</div>

So, the FSM works, but it's too slow.

Building this first version was necessary in order to understand the problem.

But now, we need speed.


## FRACTRAN For Real {#fractran-for-real}

I rewrote the FSM using `tagbody`, which is a Common Lisp feature that allows you to `goto`. It is perfect for this project.

I also identified some optimizations. These are easily discernible from the FSM graph. For example, `S11` and `S29`, and `S13` and `S17`. In the code below I have documented where these optimizations have been made, and left behind old code in comments.

The code below is very simple. One thing to note is that there are no loops, only `gotos`. We simply move from node to node (called "states" in the code) until we get our answer.

```lisp

(defparameter *f-count* 0 "iteration count")
(defparameter *f-primes* 0 "count of prime found so far")
(defparameter *f-primes-wanted* 0 "The number of primes to be found")

;; these factors control which transitions can be made from a given
;; state
(defparameter *two* 0 "exponents for 2")
(defparameter *three* 0 "exponents for 3")
(defparameter *five* 0 "exponents for 5")
(defparameter *seven* 0 "exponents for 7")
;; notice that there's no need to track the exponents for primes 11,
;; 13, 17, 19, 23, 29

(defun init-fractran-5 ()
  (setf *f-count* 0) ;; iteration count set to 0
  (setf *f-primes* 0) ;; count of primes set to 0
  (setf *f-primes-wanted* 10001) ;; primes wanted, stop condition
  (setf *two* 1) ;; set the exponents for the start
  (setf *three* 0)
  (setf *five* 0)
  (setf *seven* 0))

(defun fractran-5 ()
  ;;  (declare (optimize (speed 3) (safety 0)))
  (Let ((f-count 0)
	(f-primes *f-primes*)
	(f-primes-wanted *f-primes-wanted*)
	(two *two*)
	(three *three*)
	(five *five*)
	(seven *seven*)
	(min 0))
    (declare (type fixnum f-count f-primes f-primes-wanted two three five seven min))
    (print 'hello)
    (tagbody
     state1
       ;;(print "state1")
       (when (and (eql three 0)
		  (eql five 0)
		  (eql seven 0)
		  (> f-count 1)
		  (incf f-primes)
		  (eql f-primes f-primes-wanted))
	 ;; ending the loop do not update f-count
	 (go end))

       (incf f-count)
       (cond ((> two 0)
	      ;; update factors and stay in state 0
	      (decf two)
	      (incf three)
	      (incf five)
	      (go state1))
	     ((> seven 0)
	      (decf seven)
	      (go state1))
	     (t (incf five)
		(go state11)))
     state11
       (if (> three 0)
	   (progn
	     ;; the FSM diagram says to decrement three and move to
	     ;; S29, but we update factors and more to state 13
	     ;; rational:the move to state 29 increments reg7 and
	     ;; returns to state 11 this is done until reg3 is zero at
	     ;; which point we go to state 13
	     (setf seven (+ seven three))
	     ;; redo the count because of optimization
	     (setf f-count (+ f-count (* 2 three)))
	     ;; zero out reg3
	     (setf three 0)
	     (go state11)))
       ;; else do nothing but go to state13
       (incf f-count)
       (go state13)

     state13
       (if (> seven 0)
	   (if (> five 0)
	       ;; another optimization -- state 17 returns to us if
	       ;; five is greater than 0, so we can just figure out
	       ;; the result; I think of it this way: the cost of a
	       ;; round trip (note, "round trip" ending up back in
	       ;; state13) is a seven and a five. And for every seven
	       ;; and five that match up, I get back a two and a three.
	       ;;
	       ;; As a result, either five, or seven is going to
	       ;; zero. If seven goes to the zero (and I have more
	       ;; fives) then we will go to S11. If five goes to zero
	       ;; (and I have more sevens) then we goto S17, but we
	       ;; aren't coming back (no fives) -- this means that S17
	       ;; can be updated so that it does not have to check for
	       ;; fives. Compiler caught that logic. Amazing. See
	       ;; state17. Code commented out but left for
	       ;; documentation.
	       (progn
		 ;; either we run out of sevens first, or fives, so which is minimum?
		 (setf min (if (< seven five)
			       seven
			       five))
		 (setf f-count (+ f-count (* 2 min)))
		 (setf two (+ two min))
		 (setf three (+ three min))
		 (setf five (- five min))
		 (setf seven (- seven min))
		 (go state13))
	       ;; else
	       ;;reg5 is not greater than 0
	       ;; update factors and move to state 17
	       (progn
		 (incf f-count)
		 (decf seven)
		 (go state17))))
       (incf f-count)
       (go state11)

     state17
       (incf f-count)
       (cond (
	      ;; (> five 0)
	      ;; ;; update factors and move to state 17
	      ;; (decf five)
	      ;; (incf two)
	      ;; (incf three)
	      ;; (go state13))
	      (> three 0)
	      ;; update factors and move to state 19
	      (decf three)
	      (go state19))
	     (t (go state1)))

     state19
       (if (> two 0)
	   (progn
	     ;; optimazation, no real reason to go to state 23
	     (setf f-count (+ f-count (* 2 two)))
	     (setf five (+ five two))
	     (setf two 0)
	     (go state19))
	   ;; otherwise update factors and move to state 11
	   (progn
	     (incf f-count)
	     (incf seven)
	     (go state11)))

     ;; state23
     ;;   (if (= f-count f-count-wanted)
     ;; 	   (go end))
     ;;   (print "state23")
     ;;   ;;((= f-state 23)
     ;;   (incf f-count)
     ;;   (incf five)
     ;;   ;;(setf f-state 19))
     ;;   (go state19)

     ;; state29
     ;;   (if (= f-count f-count-wanted)
     ;; 	   (go end))
     ;;   (print "state29")
     ;;   ;;((= f-state 29)
     ;;   (incf f-count)
     ;;   (incf seven )
     ;;   ;;(setf f-state 11)
     ;;   (go state11)

     end
       (print "end")
       (setf *f-count* f-count)
       (setf *f-primes* f-primes)
       (setf *f-primes-wanted* f-primes-wanted)
       (setf *two* two)
       (setf *three* three)
       (setf *five* five)
       (setf *seven* seven))
    (list two three five seven)))
```

Here is a run without compiler optimizations.

```text

CL-USER> (init-fractran-5)
0
CL-USER> (time (fractran-5))

HELLO
"end"
Evaluation took:
  89.780 seconds of real time
  89.776813 seconds of total run time (89.776812 user, 0.000001 system)
  100.00% CPU
  243,483,348,393 processor cycles
  0 bytes consed

(104743 0 0 0)
CL-USER> (print *f-count*)

1539669807660924
1539669807660924
CL-USER>
```

Interesting results. `0 bytes consed` makes me smile a bit -- clearly not doing much with lists!

But most importantly, we get the correct answer. It takes \\( 1,539,669,807,660,924 \\) iterations to get the \\( 2^{10001^{st}\text{ prime }} \\).

\\( 90 \\) seconds is not too bad, in my view. More speed can be had by setting optimizations:

`(declare (optimize (speed 3) (safety 0)))`

Let's see how we do now.

```text

CL-USER> (init-fractran-5)
0
CL-USER> (time (fractran-5))

HELLO
"end"
Evaluation took:
  74.320 seconds of real time
  74.315707 seconds of total run time (74.315707 user, 0.000000 system)
  99.99% CPU
  201,549,884,404 processor cycles
  0 bytes consed

(104743 0 0 0)
CL-USER> (print *f-count*)

1539669807660924
1539669807660924
CL-USER>
```

Wow, we shaved 16 seconds off the time.


## Conclusion {#conclusion}

In the end, we learn two things:

-   It takes exactly \\( 1,539,669,807,660,924 \\) iterations of Conway’s `FRACTRAN` program to reach the power of two whose exponent is the \\( 10,001^{st} \\) prime.

-   By understanding the `FRACTRAN` program as a finite state machine, and then optimizing the transitions, we can go from a naive simulator to a zero-consing, tagbody-based implementation that solves the problem in just over one minute.

For a "ridiculous" little language, `FRACTRAN` is mind-expanding, and from a programming standpoint, an excellent way to explore number theory, automata, and performance tuning.

`FRACTRAN`: pretty cool.
