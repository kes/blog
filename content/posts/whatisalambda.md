---
title: "What's a Lambda? A Brief Walk from Lisp → Closures → Java"
author: ["Karl Stump"]
date: 2025-11-08
tags: ["programming", "lambda", "java"]
draft: false
math: true
---

Note: This began as an explainer for seasoned engineers learning Java 8. It’s not exhaustive, but it is designed to make lambdas intuitive—and maybe even fun.


## TLDR; {#tldr}

A lambda is an anonymous function—a piece of executable code you can create inline, pass around, and call later.

Lisp created them decades ago. Java added them in 2014 (Java 8), built on top of functional interfaces.

Let’s see how they work in Lisp first, then translate that understanding into Java.


## Lambda in Lisp {#lambda-in-lisp}

We start with a simple lambda typed into a Lisp REPL:

{{< figure src="/ox-hugo/lambdaDefined.png" class="center" width="650px" >}}

Lisp returns a function object—displayed with a printed representation like:

```bash
#<FUNCTION (LAMBDA ()) {…}>
```

That printed form isn’t the function itself—it’s a representation of a runtime object created by evaluating the lambda expression.

We can inspect it:

{{< figure src="/ox-hugo/inspectLambda.png" class="center" width="650px" >}}

Now, selecting "inspect"

{{< figure src="/ox-hugo/inspectLambda2.png" class="center" width="650px" >}}

And now, even disassemble it (clicking CODE line):

{{< figure src="/ox-hugo/disassembly.png" class="center" width="650px" >}}

So, let's take a pause and ask the question: what's a lambda?

-   A lambda expression evaluates to a `function object`
-   It may be `anonymous` (no global name)
-   It can be `passed` to other functions
-   It can be `called` (e.g., using funcall)
-   It can `close over` local variables → _closures_

Let’s invoke it. The REPL temporarily names the last result \*:

{{< figure src="/ox-hugo/useAsterisk.png" class="center" width="650px" >}}

A lambda is executable code—created at runtime—whether or not it has a name.


## Using Lambda in Lisp {#using-lambda-in-lisp}

Here's a lambda that adds 1:

```lisp
(lambda (x)
  (+ 1 x))
```

Pass it directly to funcall:

{{< figure src="/ox-hugo/funcallLambda.png" class="center" width="650px" >}}

You can also pass functions to functions—e.g., mapcar:

Let's use the arithmetic `+`

```lisp
(mapcar #'+ '(1 2 3) '(3 3 3) '(2 2 2))
```

The result is:

(6 7 8)

`mapcar` has called the function `#'+` ('plus') passing each list, and collected the results.

But we can use other arithmetic functions too, which in the REPL looks like this:

{{< figure src="/ox-hugo/mapcar.png" class="center" width="650px" >}}

Could a lambda be used? Let's try.

Let's create a lambda that computers the average of three arguments, and pass that to `mapcar`, like this:

```lisp
(mapcar (lambda (x y z)
	  (/ (+ x y z) 3)) '(1 2 3) '(3 3 3) '(2 2 2))
```

And the result is:

(2 7/3 8/3)

In the REPL it looks like this:

{{< figure src="/ox-hugo/average.png" class="center" width="650px" >}}

Lisp encourages using lambdas anywhere you can use data.


## Closure (Briefly) {#closure--briefly}

A closure is a lambda that captures variables from its defining scope.

```lisp
(defun makeClosure ()
(let ((counter 0))
  (lambda ()
    (setq counter (1+ counter)))))
```

`makeClosure` returns a **lambda with memory** -- its own counter.

Each call to `makeClosure` creates a new closure with its own internal state.

{{< figure src="/ox-hugo/closure1.png" class="center" width="650px" >}}

What does this function do?

1.  It creates a local variable called `counter` and initializes it
    to 0.
    -   The Lisp syntax of `let` is used to introduce local lexical
        space.
2.  Then a lambda is returned.
    -   In the lambda the local variable `counter` is
        used. This pattern of a `let` and `lambda` is a closure, and in Lisp
        this is referred to as a `let over lambda`.
    -   Why the word `closure`? Because the lambda "has closed" over
        (or "enclosed") the local variable.

Okay, we have a function that creates and returns functions. Let's invoke it.

{{< figure src="/ox-hugo/closure1.1.png" class="center" width="650px" >}}

Now, let's use `funcall` on the `closure` from `makeClosure`.

{{< figure src="/ox-hugo/closure1.2.png" class="center" width="650px" >}}

Well ... now what?

{{< figure src="/ox-hugo/closure1.3.png" class="center" width="650px" >}}

This doesn't seem to be getting anywhere.

Wait!

Assign the `closure` to a variable!

{{< figure src="/ox-hugo/closure2.png" class="center" width="650px" >}}

Now use `funcall` on it.

{{< figure src="/ox-hugo/closure3.png" class="center" width="650px" >}}

Cool! The lambda really does have a counter.

Closures matter because they let you create tiny bundles of behavior + data--this becomes extremely important when we pivot into Java.


## From Lisp → Java {#from-lisp-java}

> Baseball is the only field of endeavor where a man can succeed three times out of ten and be considered a good performer. - _Ted Williams_

Imagine you’re working with baseball player data:

<style>
.baseball tr:nth-child(even){
background-color: #3b3f4a;
}
/*
.baseball thead tr th:nth-child(12) {
  background-color:red;
}
*/
.baseball th{
font-weight:normal;
text-align: center;
background-color: #3b3f4a;
padding:5px;
border: 0;
}
/* Body cells */
.baseball td{
padding:5px;
border: 1px black solid;
}
</style>

<style>.baseball table { text-align: center;  width: 80%;  margin: 0 auto;  }</style>

<div class="ox-hugo-table baseball">

| Player          | Position | Games | AtBats | Runs | Hits | HR  | RBI  | Avg |
|-----------------|----------|-------|--------|------|------|-----|------|-----|
| PeteRose        | 1B       | 3562  | 14053  | 2165 | 4256 | 160 | 1314 | 303 |
| CarlYastrzemski | LF       | 3308  | 11988  | 1816 | 3419 | 452 | 1844 | 285 |
| HankAaron       | RF       | 3298  | 12364  | 2174 | 3771 | 755 | 2297 | 305 |
| RickeyHenderson | LF       | 3081  | 10961  | 2295 | 3055 | 297 | 1115 | 279 |
| TyCobb          | CF       | 3035  | 11429  | 2246 | 4191 | 117 | 1938 | 367 |
| EddieMurray     | 1B       | 3026  | 11336  | 1627 | 3255 | 504 | 1917 | 287 |
| StanMusial      | OF       | 3026  | 10972  | 1949 | 3630 | 475 | 1951 | 331 |
| CalRipken       | SS       | 3001  | 11551  | 1647 | 3184 | 431 | 1695 | 276 |
| WillieMays      | CF       | 2992  | 10881  | 2062 | 3283 | 660 | 1903 | 302 |
| BarryBonds      | LF       | 2986  | 9847   | 2227 | 2935 | 762 | 1996 | 298 |
| DaveWinfield    | RF       | 2973  | 11003  | 1669 | 3110 | 465 | 1833 | 283 |

</div>

You create a Player class, and a PlayersDB that holds a list and a method to search it:

{{< figure src="/ox-hugo/baseballPlayer.png" class="center" width="650px" >}}

And you create another class to aggregate players into a list and a method to search:

{{< figure src="/ox-hugo/classPlayersDB.png" class="center" width="650px" >}}

But quickly you realize:

> “I need many different kinds of searches… and each one differs only by one little test expression.”

So you start thinking:

How do I pass in that test?

{{< figure src="/ox-hugo/classPlayersDBAnnotated.png" class="center" width="650px" >}}


## Attempt 1: Abstract Classes {#attempt-1-abstract-classes}

And so, how can we pass in that expression?

Let's create a `Tester` that provides a `boolean testPlayer (Player p)` method. And `getPlayersWithTest` would look like this:

{{< figure src="/ox-hugo/classPlayersDB-secondmod.png" class="center" width="650px" >}}

There will be lots of different tests, so `Tester` should be an abstract class.

{{< figure src="/ox-hugo/classPlayersDB-thirdmod.png" class="center" width="650px" >}}

For now, looks good!

You cannot instantiate an abstract class. But you can make a class and extend it with the abstract class. Like this.

{{< figure src="/ox-hugo/moreAtBatsClass.png" class="center" width="650px" >}}

Now, we instantiate `MoreAtBats` (which is a `Tester`) and use it in `getPlayersWithTest`:

{{< figure src="/ox-hugo/testerTest-zero.png" class="center" width="650px" >}}

This works.

But (sadly) it scales poorly. We need new class for every test.


## Attempt 2: Anonymous Classes {#attempt-2-anonymous-classes}

Another technique does exist -- anonymous classes.

```java
  Tester t = new Tester() {
    @Override
    public boolean testPlayer(Player p) {
        return p.getAtBats() > 10000;
    }
};
```

Now, we aren't instantiating the _abstract class_ here. Rather an anonymous class is created that _extends Tester_.

It's a nice technique. Powerful. But it's verbose.


## Lambda In Java {#lambda-in-java}

We should realize that our abstract `Tester` class exists for one reason -- to ensure that all extensions supply a `boolean testPlayer(Player p)` method. One method.

So, we can simplify.

Java has a rule:

> If an interface has exactly one abstract method, it is a functional interface.

```java
@FunctionalInterface
interface Lambda {
    boolean testPlayer(Player p);
}
```

Note: I have picked the name `Lambda` for the interface only to make a point. Admittedly, a better name would be `PlayerTest`

Now, modify `PlayersDB`:

{{< figure src="/ox-hugo/lambda-2.png" class="center" width="650px" >}}

Now, in test, we can "new up" our Lambda interface and IntellJ offers:

{{< figure src="/ox-hugo/lambda-4.png" class="center" width="650px" >}}

Interesting.

Pressing enter ...

{{< figure src="/ox-hugo/lambda-5.png" class="center" width="650px" >}}

Stop! What does `p ->` mean??

Java knows:

-   p must be a Player
-   The body must return a boolean
-   And there must be a class that implements the interface (which will be anonymous in this case)

So

```java
p -> p.getAtBats() > 10000
```

is a lambda expression—a compact representation of:

```java
new Lambda () {
    boolean test(Player p) { return p.getAtBats() > 10000; }
}
```

You can capture variables too:

```java
int limit = 10000;
p -> p.getAtBats() > limit
```

And that's a closure.

{{< figure src="/ox-hugo/lambda-8.png" class="center" width="650px" >}}


## So ...  what's a lambda? {#so-dot-dot-dot-what-s-a-lambda}

Lambdas aren’t magic—they’re mechanics.

`Lisp` shows the idea (perhaps the "raw idea"): an anonymous function created at runtime, tiny bundles of behavior and data that you pass to other functions.

`Java` adopts the concept, wraps it in types, and offers lambda syntax so you don’t have to write an entire class just to supply a small piece of behavior.

If you want to explore lambdas more deeply in Java, start with:

-   Java’s built-in `Predicate`, `Function`, `Supplier`, `Consumer`

-   Streams (filter, map, reduce)

For other points of interest, consider:

-   Lisp
    -   [Portacle](https://portacle.github.io/) (Common Lisp)
    -   [Racket](https://racket-lang.org/) (Scheme)
    -   The historic MIT 6.001 by Sussman and Abelson in Scheme is [here](https://www.youtube.com/watch?v=-J_xL4IGhJA).
-   Oracle
    -   [Anonymous Classes](https://docs.oracle.com/javase/tutorial/java/javaOO/anonymousclasses.html)
    -   [Lambda Expression](https://docs.oracle.com/javase/tutorial/java/javaOO/lambdaexpressions.html)
-   Java REPL can be accessed in IntelliJ! See their page on the [JConsole](https://www.jetbrains.com/help/idea/jshell-console.html).
-   Finally, [Lambda](https://en.wikipedia.org/wiki/Anonymous_function) in many languages.

Happy coding—

λ forever.

:)
