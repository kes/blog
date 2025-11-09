---
title: "Project Euler - Number 308"
author: ["Karl Stump"]
date: 2024-08-28
tags: ["euler", "lisp"]
draft: false
math: true
---

Project Euler has a Fractran problem. this will do it

If you don't know about  [John Conway's](https://en.wikipedia.org/wiki/John_Horton_Conway)  Fractran you can  [look here](https://en.wikipedia.org/wiki/FRACTRAN) and [here](https://www.uctv.tv/shows/Fractran-A-Ridiculous-Logical-Language-with-John-Conway-23320).

You can find the problem here: <https://projecteuler.net/problem=308>

I have reproduced it here:

<style>
.verse {
    margin-bottom: 10px;
    padding: 10px;
    ##background-color: #FFF8DC;
    border-left: 2px solid #ffeb8e;
    border-left-color: rgb(255, 228, 102);
    display: block;
    margin-block-start: 1em;
    margin-block-end: 1em;
    margin-inline-start: 40px;
    margin-inline-end: 40px;
}
</style>

<div class="verse">

A program written in the programming language Fractran consists of a list of fractions.<br />
<br />
The internal state of the Fractran Virtual Machine is a positive integer, which is initially set to a seed value. Each iteration of a Fractran program multiplies the state integer by the first fraction in the list which will leave it an integer.<br />
<br />
For example, one of the Fractran programs that John Horton Conway wrote for prime-generation consists of the following 14 fractions:<br />
<br />
<br />
\\[ \dfrac{17}{91}, \dfrac{78}{85}, \dfrac{19}{51}, \dfrac{23}{38}, \dfrac{29}{33}, \dfrac{77}{29}, \dfrac{95}{23}, \dfrac{77}{19}, \dfrac{1}{17}, \dfrac{11}{13}, \dfrac{13}{11}, \dfrac{15}{2}, \dfrac{1}{7}, \dfrac{55}{1} \\]<br />
<br />
<br />
Starting with the seed integer 2, successive iterations of the program produce the sequence:<br />
<br />
\\( 15, 825, 725, 1925, 2275, 425, ..., 68, 4, 30, ..., 136, 8, 60, ..., 544, 32, 240, ... \\)<br />
<br />
The powers of 2 that appear in this sequence are \\( 2^2 \\), \\( 2^3 \\), \\( 2^5 \\), \\( ... \\)<br />
<br />
It can be shown that _all_ the powers of 2 in this sequence have prime exponents and that _all_ the primes appear as exponents of powers of 2, in proper order!<br />
<br />
If someone uses the above Fractran program to solve Project Euler Problem 7 (find the 10001st prime), how many iterations would be needed until the program produces 2^10001st prime?<br />

</div>


## The Idea of a Fractran Simulator {#the-idea-of-a-fractran-simulator}

In Common Lisp, we can define the program as a list

<a id="code-snippet--fractran-program"></a>
{{< highlight lisp >}}
(defparameter *fp*
  '((17 91) (78 85) ( 19 51) ( 23 38) (29 33)
    (77 29) (95 23) (77 19) (1 17) (11 13)
    (13 11) (15 2) (1 7) (55 1)))
{{< /highlight >}}

We can then define a sort of simulator that runs the program.

Initially, of course, any ol' thing will do. What have we been told again?

We've been told that we are to pass in a seed number, also called the state, and that number is to
be multiplied by the first fraction. If we get an integer, we return that integer. If not, we discard
our calculation and move on to the next fraction and follow the same process. Multiple the state by
the fraction, and if we get an integer then return it, otherwise, discard the calculation and move
on. If we exhaust all the fractions having not gotten an integer, the program halts.

So, our simulator looks like this:

<a id="code-snippet--fractran-simulator"></a>
{{< highlight lisp >}}
(defun fractran-simulator (state program)
  (cond ((null program) nil)
	((integerp (/ (* state (caar program)) (cadar program)))
	 (/ (* state (caar program)) (cadar program)))
	(t (fractran-simulator state (cdr program)))))
(fractran-simulator start *fp*)
{{< /highlight >}}

Easy peasy. Let's use an initial seed value of 2 (as the problem description says).

What do we get back from our simulator?

```text
15
```

This is the correct result for the first iteration. So, we're off to a good start. Let's do more iterations:

<a id="code-snippet--simulator-run"></a>
{{< highlight lisp >}}

(loop for i from 1 to 20
      with state = 2
      do (setf state (fractran-simulator state *fp*))
      collect (list i state) into results
      finally (return (cons '(iteration state) results)))
{{< /highlight >}}

Running the above code gives us the following:

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

Huh. What does it mean? I actually don't know. However, I notice:

1.  The state follows the sequence given in the problem description, and so, we can have some confidence
    that we've at least got the right idea.
2.  I also notice that on iteration 19 we get 2^2, which is the first prime exponent of two. And
    the next iteration, 20, we get 30 and that follows the sequence given in the problem
    statement. So, again, we have good confidence we're on the right track.
3.  According to the problem statement, we're looking for the 10001^st prime exponent
    of 2. Theoretically, the simulator is sufficient to find the answer. All we have to do is keep
    calling the simulator, counting each power of two, until we get to the 10001^st. So, can we just
    run it and declare victory? Sadly, no.
4.  It turns out that we'll need something more efficient that this simulator. (Having already
    solved this problem I can tell you that the number of iterations is on the order of 10^15 ---
    in the quadrillions.)

More needs to be done.


## More Analysis {#more-analysis}

Taking the prime factors in the fractran program:

\\( \dfrac{17}{7 \times 13}, \dfrac{2 \times 3 \times 13}{5 \times 17}, \dfrac{19}{3 \times 17}, \dfrac{23}{2 \times 19}, \dfrac{29}{3 \times 11}, \dfrac{7 \times 11}{29}, \dfrac{5 \times 19}{23}, \dfrac{7 \times 11}{19}, \dfrac{1}{17}, \dfrac{11}{13}, \dfrac{13}{11}, \dfrac{3 \times 5}{2}, \dfrac{1}{7}, \dfrac{5 \times 11}{1} \\)

Okay, those are the prime factors. What does that get us?

I'm not sure.

Let's take the prime factors of the result of the fractran simulator.

First we'll need something to get the prime factors from a number. This will do:

<a id="code-snippet--get-prime-factors"></a>
{{< highlight lisp >}}
  (defparameter primes '(2 3 5 7 11 13 17 19 23 29))

  (defun get-prime-factors (num)
    (loop for p in primes
  	if  (eq 0 (mod num p))
  	  collect (loop
  		    with i = num
  		    while (= 0 (mod i p))
  		    do (setf i (/ i p))
  		    collect p into caught
  		    finally (return (list p (length caught))))
	  else
	    collect (list p 0)))

(get-prime-factors 2275)
{{< /highlight >}}

Testing...

```text
((2 0) (3 0) (5 2) (7 1) (11 0) (13 1) (17 0) (19 0) (23 0) (29 0))
```

Perfect. Of course, we're only using the primes that occur in the Fractran program, since by definition
all the results we get from an iteration of the program can only be composed of those factors.

Now let's create something that gives some nice output.

{{< highlight lisp >}}
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
{{< /highlight >}}

```text
(5 0 0 2 1 0 1 0 0 0 0 2275)
```

That's the type of thing we want. Let's get more data (and I'll start displaying it as a table):

<a id="code-snippet--create-table"></a>
{{< highlight lisp >}}
(loop for i from 1 to 20
      with state = 2
      collect (get-status i (setf state (fractran-simulator state *fp*))))
{{< /highlight >}}

Okay, here it is:

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

That looks pretty good. Let's make it more readable, and add some headers. Here's how I do that:

<a id="code-snippet--FORMAT-TABLE"></a>
{{< highlight elisp >}}
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
  (setf value (cons '(Iteration 2 3 5 7 11 13 17 19 23 29 State) value)))
{{< /highlight >}}

And the results look better. What I've done for primes 11 through 29, if the value is 0 (and
remember these are exponents), then just replace it with "--". This removes a lot of clutter and make
the table easier to read.

Examining the table it looks like primes 11 through 29 are mutually exclusive (an `XOR`). Only
one can be on at a time.

| Iteration | 2 | 3 | 5 | 7 | 11 | 13 | 17 | 19 | 23 | 29 | State |
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

If that's true, then primes 11 through 29 function as state, and we can create a state machine.


## FSM {#fsm}

Doing that gets something like this:

{{< highlight dot >}}
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
{{< /highlight >}}

{{< figure src="/ox-hugo/test.png" >}}

Here's how it works. On start, you pass in a 2 to `S1`. At that point, of course, all of the other
primes have an exponent of 0. In `S1` the first option is to decrement the exponent for 2, and increment
the exponents for 3 and 5. This is noted on the digraph as "(1) -2, +3, +5." Just remember that when you
decrement an exponent you can never go negative, because that means you have a fraction. So, `S1` on the
first step the first option is available (and so you must take it) and decrement the exponent for 2, and increment the exponents
for 3 and 5, and your state transition leaves you in `S1`.

Now, that's one iteration. You may want to go back and look at the table.

Now, let's do the second iteration. You have no more 2s, so option (1) is not available. What about
option (2) (in red)? The answer is no, you don't have any 7s. That leaves option (3) in green, and
so we increment 5's exponent (5^1 becomes 5^2) and transition to state `S11`. So, what we have now is
3^1 5^2 and 11^1. And 11^1 gives us opportunity to say that when we enter into a state, that state's
exponent increments. And when we leave a state, that state's exponent decrements. This is true
for all states, except when entering or leaving `S1`. `S1` does not have an exponent.

So that's the second iteration. You may want to check the table.

Okay, next (3^rd) iteration. We're in `S11` and first options is to decrement a 3. And yes, we can do
that, and so, we decrement a 3 (3^1 becomes 3^0), we decrement 11, since we're leaving `S11,` (so,
11^1 becomes 11^0) and we enter `S29` (29^0 becomes 29^1).

And that's the third iteration. You may want to check the table.

And that's the process, moving from state to state, incrementing and decrementing exponents.


## Coding the FSM {#coding-the-fsm}

{{< highlight lisp >}}
(defparameter *fractran-program* '((17 91) (78 85) ( 19 51) ( 23 38) (29 33) (77 29) (95 23) (77 19) (1 17) (11 13) (13 11) (15 2) (1 7) (55 1)))
(defparameter *fractran-registers* '(:rs 1 :rp 0 :rc 0 :r1 0 :r2 1 :r3 0 :r5 0 :r7 0 :r11 0 :r13 0 :r17 0 :r19 0 :r23 0 :r29 0 ))

(defmacro r (x)
  `(getf reg ,(read-from-string (concatenate 'string ":r" (write-to-string x)))))

(defmacro fr (x)
  `(getf *fractran-registers* ,(read-from-string (concatenate 'string ":r" (write-to-string x)))))

(defmacro state-change (os ns)
  (list 'progn `(decf (r ,os)) `(incf (r ,ns)) `(setf (r s) ,ns)))
{{< /highlight >}}

I decided to call the list holding the exponents and the number of primes, etc, the register. and so, I prefixed
the various fields with an "r".

I also defined a few convenience macros that made setting the exponents and changing state much easier.

So, in the following code I simply go to whatever state the register says I'm in.

{{< highlight lisp >}}
(defun fractran (reg)
"Calls the required state with the register. returns a list of state and register."
(cond ((= (r s) 1) (s1 reg))
	((= (r s) 11) (s11 reg))
	((= (r s) 13) (s13 reg))
	((= (r s) 17) (s17 reg))
	((= (r s) 19) (s19 reg))
	((= (r s) 23) (s23 reg))
	((= (r s) 29) (s29 reg))))
{{< /highlight >}}


## S1 {#s1}

Remember what we can do in `S1`. We must follow the specific order:

1.  Can I decrement 2?
    1.  Yes: Decrement 2, increment 3, increment 7, and stay in `S1`
    2.  No. Can I decrement 7?
        1.  Yes. Decrement 7, and stay in `S1`
        2.  No. Increment 5 and move to `S11`

Also, in `S1` a check is made for a power of two.

<a id="code-snippet--S1"></a>
{{< highlight lisp >}}
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
{{< /highlight >}}


## S11 {#s11}

Each state that follows is very easy to follow. A simple implementation of the fractran program.

<a id="code-snippet--S11"></a>
{{< highlight lisp >}}
(defun s11 (reg)
  (incf (r c))
  (cond ((> (r 3) 0)
	 (decf (r 3))
	 (state-change 11 29))
	 (t (state-change 11 13))))
{{< /highlight >}}


## S13 {#s13}

<a id="code-snippet--S13"></a>
{{< highlight lisp >}}
(defun s13 (reg)
  (incf (r c))
  (cond ((> (r 7) 0)
	 (decf (r 7))
	 (state-change 13 17))
	(t (state-change 13 11))))
{{< /highlight >}}


## S17 {#s17}

<a id="code-snippet--S17"></a>
{{< highlight lisp >}}
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
{{< /highlight >}}


## S19 {#s19}

{{< highlight lisp >}}
(defun s19 (reg)
  (incf (r c))
  (cond ((> (r 2) 0)
	 (decf (r 2))
	 (state-change 19 23))
	;; otherwise update factors and move to state 11
	(t (incf (r 7))
	   (state-change 19 11))))
{{< /highlight >}}


## S23 S29 {#s23-s29}

{{< highlight lisp >}}
(defun s23 (reg)
  (incf (r c))
  (incf (r 5))
  (state-change 23 19))

(defun s29 (reg)
  (incf (r c))
  (incf (r 7))
  (state-change 29 11))
{{< /highlight >}}


## Miscellaneous {#miscellaneous}

<a id="code-snippet--iterate"></a>
{{< highlight lisp >}}
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
(iterate 50)
{{< /highlight >}}

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

<a id="code-snippet--FORMAT-TABLE-for-iterate"></a>
{{< highlight elisp >}}
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
  (setf value (cons '(Iteration State 2 3 5 7 11 13 17 19 23 29 Amount) value)))
{{< /highlight >}}

So, now running, we get the same results. The FSM works.

| Iteration | State | 2 | 3 | 5 | 7  | 11 | 13 | 17 | 19 | 23 | 29 | Amount |
|-----------|-------|---|---|---|----|----|----|----|----|----|----|--------|
| 1         | 1     | 0 | 1 | 1 | -- | -- | -- | -- | -- | -- | 0  | 15     |
| 2         | 11    | 0 | 1 | 2 | -- | 1  | -- | -- | -- | -- | 0  | 825    |
| 3         | 29    | 0 | 0 | 2 | -- | -- | -- | -- | -- | -- | 1  | 725    |
| 4         | 11    | 0 | 0 | 2 | 1  | 1  | -- | -- | -- | -- | 0  | 1925   |
| 5         | 13    | 0 | 0 | 2 | 1  | -- | 1  | -- | -- | -- | 0  | 2275   |
| 6         | 17    | 0 | 0 | 2 | -- | -- | -- | 1  | -- | -- | 0  | 425    |
| 7         | 13    | 1 | 1 | 1 | -- | -- | 1  | -- | -- | -- | 0  | 390    |
| 8         | 11    | 1 | 1 | 1 | -- | 1  | -- | -- | -- | -- | 0  | 330    |
| 9         | 29    | 1 | 0 | 1 | -- | -- | -- | -- | -- | -- | 1  | 290    |
| 10        | 11    | 1 | 0 | 1 | 1  | 1  | -- | -- | -- | -- | 0  | 770    |
| 11        | 13    | 1 | 0 | 1 | 1  | -- | 1  | -- | -- | -- | 0  | 910    |
| 12        | 17    | 1 | 0 | 1 | -- | -- | -- | 1  | -- | -- | 0  | 170    |
| 13        | 13    | 2 | 1 | 0 | -- | -- | 1  | -- | -- | -- | 0  | 156    |
| 14        | 11    | 2 | 1 | 0 | -- | 1  | -- | -- | -- | -- | 0  | 132    |
| 15        | 29    | 2 | 0 | 0 | -- | -- | -- | -- | -- | -- | 1  | 116    |
| 16        | 11    | 2 | 0 | 0 | 1  | 1  | -- | -- | -- | -- | 0  | 308    |
| 17        | 13    | 2 | 0 | 0 | 1  | -- | 1  | -- | -- | -- | 0  | 364    |
| 18        | 17    | 2 | 0 | 0 | -- | -- | -- | 1  | -- | -- | 0  | 68     |
| 19        | 1     | 2 | 0 | 0 | -- | -- | -- | -- | -- | -- | 0  | 4      |
| 20        | 1     | 1 | 1 | 1 | -- | -- | -- | -- | -- | -- | 0  | 30     |
| 21        | 1     | 0 | 2 | 2 | -- | -- | -- | -- | -- | -- | 0  | 225    |
| 22        | 11    | 0 | 2 | 3 | -- | 1  | -- | -- | -- | -- | 0  | 12375  |
| 23        | 29    | 0 | 1 | 3 | -- | -- | -- | -- | -- | -- | 1  | 10875  |
| 24        | 11    | 0 | 1 | 3 | 1  | 1  | -- | -- | -- | -- | 0  | 28875  |
| 25        | 29    | 0 | 0 | 3 | 1  | -- | -- | -- | -- | -- | 1  | 25375  |
| 26        | 11    | 0 | 0 | 3 | 2  | 1  | -- | -- | -- | -- | 0  | 67375  |
| 27        | 13    | 0 | 0 | 3 | 2  | -- | 1  | -- | -- | -- | 0  | 79625  |
| 28        | 17    | 0 | 0 | 3 | 1  | -- | -- | 1  | -- | -- | 0  | 14875  |
| 29        | 13    | 1 | 1 | 2 | 1  | -- | 1  | -- | -- | -- | 0  | 13650  |
| 30        | 17    | 1 | 1 | 2 | -- | -- | -- | 1  | -- | -- | 0  | 2550   |
| 31        | 13    | 2 | 2 | 1 | -- | -- | 1  | -- | -- | -- | 0  | 2340   |
| 32        | 11    | 2 | 2 | 1 | -- | 1  | -- | -- | -- | -- | 0  | 1980   |
| 33        | 29    | 2 | 1 | 1 | -- | -- | -- | -- | -- | -- | 1  | 1740   |
| 34        | 11    | 2 | 1 | 1 | 1  | 1  | -- | -- | -- | -- | 0  | 4620   |
| 35        | 29    | 2 | 0 | 1 | 1  | -- | -- | -- | -- | -- | 1  | 4060   |
| 36        | 11    | 2 | 0 | 1 | 2  | 1  | -- | -- | -- | -- | 0  | 10780  |
| 37        | 13    | 2 | 0 | 1 | 2  | -- | 1  | -- | -- | -- | 0  | 12740  |
| 38        | 17    | 2 | 0 | 1 | 1  | -- | -- | 1  | -- | -- | 0  | 2380   |
| 39        | 13    | 3 | 1 | 0 | 1  | -- | 1  | -- | -- | -- | 0  | 2184   |
| 40        | 17    | 3 | 1 | 0 | -- | -- | -- | 1  | -- | -- | 0  | 408    |
| 41        | 19    | 3 | 0 | 0 | -- | -- | -- | -- | 1  | -- | 0  | 152    |
| 42        | 23    | 2 | 0 | 0 | -- | -- | -- | -- | -- | 1  | 0  | 92     |
| 43        | 19    | 2 | 0 | 1 | -- | -- | -- | -- | 1  | -- | 0  | 380    |
| 44        | 23    | 1 | 0 | 1 | -- | -- | -- | -- | -- | 1  | 0  | 230    |
| 45        | 19    | 1 | 0 | 2 | -- | -- | -- | -- | 1  | -- | 0  | 950    |
| 46        | 23    | 0 | 0 | 2 | -- | -- | -- | -- | -- | 1  | 0  | 575    |
| 47        | 19    | 0 | 0 | 3 | -- | -- | -- | -- | 1  | -- | 0  | 2375   |
| 48        | 11    | 0 | 0 | 3 | 1  | 1  | -- | -- | -- | -- | 0  | 9625   |
| 49        | 13    | 0 | 0 | 3 | 1  | -- | 1  | -- | -- | -- | 0  | 11375  |
| 50        | 17    | 0 | 0 | 3 | -- | -- | -- | 1  | -- | -- | 0  | 2125   |

There are some optimizations that can be made. These are easily discernible from the
FSM. (Hint: `S11` and `S29` looks like a good place to start.)
