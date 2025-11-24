---
title: "What's a Lambda?"
author: ["Karl Stump"]
date: 2025-11-08
tags: ["programming", "lambda", "java"]
draft: false
math: true
---

NOTE: this document was originally written for seasoned software engineers who were transitioning into Java and beginning to use lambdas. Whether it helps in understanding the concept of lambda, or how lambdas appear in Java, is, I think, very much in the eye of the beholder. I place it here with only slight modification.

This document is a brief investigation into the question:

> What is a lambda?

And, in our context, more specifically, what is lambda in computer
languages, and even more specifically, what is a lambda in Java.

Java added lambda expressions in 2014 (see [Java SE 8](https://en.wikipedia.org/wiki/Java_version_history#Java_SE_8)).

**Caveat**: Nothing in this document is meant to be exhaustive. It's not the final word on anything. And there most likely are errors in it -- in fact, most definitely.  And there are certainly better ways that things could have been said, or demonstrated. But it is hoped that in some way some part of it can prove beneficial in thinking about lambda, and also along the way be fun.

Lambda has been around for a long time in other programming languages, like [Lisp](https://en.wikipedia.org/wiki/Lisp_(programming_language)#Lambda_expressions_and_function_definition).

So, here's the plan:

1.  Let's investigate lambda as it occurs in Common Lisp.
2.  Then with that as a foundation, we will move the investigation into Java.

So, what's a lambda?


## Lambda in Lisp {#lambda-in-lisp}

In demonstrating lambda in Lisp, I'm going to assume that the examples are sufficiently clear that even someone with no experience with Lisp, yet knowledgeable in some other programming language, will have not much trouble following. As such, I will leave a lot of Lisp details unsaid.

Below is a screenshot of a Lisp REPL. A REPL is a read, eval, print loop -- you can think of it as a Lisp interactive window. For details see the [wiki entry](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop).

So, it works like this. In the REPL, we begin to define our lambda:

{{< figure src="/ox-hugo/definingLambda.png" class="center" >}}

And now when we press `return`, and the expression is read, evaluated, and the result is returned.

{{< figure src="/ox-hugo/lambdaDefined.png" class="center" >}}

So, what is a lambda?

You can see that it says it's a `#<Function ...>`. This is called a printed representation. A representation of what? A representation of a function. A function object was returned, and the printed representation signals that fact.

Now, we can inspect the object. Right-clicking on the presentation gives a context menu.

{{< figure src="/ox-hugo/inspectLambda.png" class="center" >}}

Now, selecting "inspect" gives us some details. Specifically, name, arglist, type, and code.

{{< figure src="/ox-hugo/inspectLambda2.png" class="center" >}}

Now, clicking the code line, we get \\( \ldots \\)

{{< figure src="/ox-hugo/disassembly.png" class="center" >}}

An assembler listing, which may be surprising.

So, the lambda expression --- essentially a "hello world" program --- was typed into the REPL, and then enter was pressed. This caused the code to be evaluated, and a function was created, and returned, and we can inspect it, even down to the assembler.

But if it's code, if we have a "function object" returned to us, then we should be able to execute it. So, back in the REPL, we want to invoke, or call, the lambda.

But how? We don't have a name for the function. It's anonymous. The function exists but we can't tell Lisp to evaluate it, because you don't have a name for it.

This raises the question, then, what good is a lambda if it doesn't have a name?

We will see how to deal with that problem in a bit. And, well, it's not exactly true that we can't invoke this function. The fact is that the REPL, behind the scenes, has given a temporary name to the lambda function, which is the symbol `*`. Note, this is only temporary.

Now, how to use it?

We can use `funcall`. `funcall` is a function that takes a function as it first parameter, and optionally takes additional arguments. Like this:

{{< figure src="/ox-hugo/useAsterisk.png" class="center" >}}

What just happened there?

We passed the lambda, named `*` to the function `funcall`, which invoked the lambda. The lambda returned the string, and funcall returned the string, and we see it displayed in the REPL. (And note, now, `*` is no longer the name of the lambda).

So, let's take a pause and ask the question: what's a lambda?

-   In some languages, an expression that begins with "(lambda ...)"
-   In some environments, a printed representation, like `#<FUNCTION (LAMBDA
          ()) {1002F5D59B}>`
-   Code that, in some environments, we can inspect even down to the disassembly.
-   Code that is "anonymous" -- it doesn't have a name, or anything that points to it, leaving us unable to get to it.
-   But also, code that could be executed if we could get to it.
-   And it's a function created "on the fly," at run time, and that will be garbage collected when it's done being used.

That's a good start. Let's see how we might use a lambda.


## Using Lambda in Lisp {#using-lambda-in-lisp}

Let's define a lambda, like this:

```lisp
(lambda (x)
  (+ 1 x))
```

What have we said? Create a function (a lambda) that takes one argument, 'x' and adds one to it.

Excellent, now in the REPL let's put this lambda directly in `funcall`, and also, let's make it so that `funcall` passes this value 100 to the lambda, like this:

{{< figure src="/ox-hugo/funcallLambda.png" class="center" >}}

And you see that we get the value 101 returned in the REPL.

So, what just happened there?

We just created a lambda -- "on the fly" -- it's anonymous, doesn't have a name, but since we're creating it in the `funcall` it doesn't need a name. `funcall` calls the anonymous function passing it the value 100, and the function does what we designed it to do, adds 1, and returns the result, which is displayed.

Pretty neat.

Of course, we could just invoke the `plus` function using the symbol
`+`. Or we could pass `+` to `funcall`, too.

Like this:

{{< figure src="/ox-hugo/funcallPlus.png" class="center" >}}

(Don't be confused by the `#'` prepended to the `+` symbol. This is simply the Lisp way of indicating that the function value of `+` is wanted.)

Sp. this is interesting. And it gets you thinking: might some of this, the lambda, and the passing functions around to other functions, might some of it be useful?

More specifically is it useful to:

1.  Define a function without a name?
2.  To pass functions to other functions?

The answer turns out to be emphatically yes. The myriad of applications and details is far beyond the scope of this brief discussion.

However, in Lisp there are lots of functions that take a function as an argument, not just `funcall`.

For example, `mapcar` is one such function.

```lisp
(mapcar #'+ '(1 2 3) '(3 3 3) '(2 2 2))
```

The result is:

(6 7 8)

`mapcar` has called the function `#'+` ('plus') passing each list, and collected the results.

But we can use other arithmetic functions too, which in the REPL looks like this:

{{< figure src="/ox-hugo/mapcar.png" class="center" >}}

Could a lambda be used? Let's try.

Let's create a lambda that computers the average of three arguments, and pass that to `mapcar`, like this:

```lisp
(mapcar (lambda (x y z)
          (/ (+ x y z) 3)) '(1 2 3) '(3 3 3) '(2 2 2))
```

And the result is:

(2 7/3 8/3)

In the REPL it looks like this:

{{< figure src="/ox-hugo/average.png" class="center" >}}

Here's one that rearranges the numbers in some way. Do you see how?

```lisp
(mapcar (lambda (x y z)
          (list x y z)) '(1 2 3) '(3 3 3) '(2 2 2))
```

Results:

((1 3 2) (2 3 2) (3 3 2))

Or in the REPL:

{{< figure src="/ox-hugo/rearrange.png" class="center" >}}

Here's one that's more complicated, keeping a running total:

```lisp
(let ((running-total 0))
  (mapcar (lambda (x y z)
            (setq running-total (+ x y z running-total)))
          '(1 2 3) '(3 3 3) '(2 2 2)))
```

(6 13 21)

In the REPL

{{< figure src="/ox-hugo/runningTotal.png" class="center" >}}

Oh, wait!

That lambda is a very important case: it's a `closure`.


## Closure {#closure}

> "Sometimes it's called a closure, other times a saved lexical environment. Or, as some of us like to say, let over lambda. Whatever terminology you use, mastering this concept of a closure is the first step to becoming a professional lisp programmer. In fact, this skill is vital for the proper use of many modern programming languages, even ones that don't explicitly contain let or lambda, such as Perl or Javascript."
> -- [Doug Hoyte](https://letoverlambda.com/index.cl/guest/chap2.html)

Let's a define a function.

```lisp
(defun makeClosure ()
  (let ((counter 0))
    (lambda ()
      (setq counter (1+ counter)))))
```

{{< figure src="/ox-hugo/closure1.png" class="center" >}}

What does this function do?

1.  It creates a local variable called `counter` and initializes it
    to 0.
    -   The Lisp syntax of `let` is used to introduce local lexical
        space. How variables function in this space, in terms of scope
        and extent, is exactly what you expect.
2.  It then creates and returns a lambda
    -   Notice that in the lambda the local variable `counter` is
        used. This pattern of a `let` and `lambda` is a closure, and
        as already mentioned in the Hoyte quote above, in the Lisp
        world, is sometimes referred to as a `let over lambda`.
    -   Why the word `closure`? Because the lambda "has closed" over
        (or "enclosed") the local variable. The lambda "has closed"
        (verb), and if you want to label something that "has closed"
        it sounds like "closure" (a noun) would do it -- or maybe we
        should say, "enclosure" --- or perhaps, we should just call it
        an "encapsulation" because it "encapsulates" local variables?

Okay, we have a function, and a named one at that -- a named function
that creates and returns unnamed functions which are `closures`. Let's invoke it.

{{< figure src="/ox-hugo/closure1.1.png" class="center" >}}

Now, let's use `funcall` on the `closure` from `makeClosure`.

{{< figure src="/ox-hugo/closure1.2.png" class="center" >}}

Well ... now what?

{{< figure src="/ox-hugo/closure1.3.png" class="center" >}}

This doesn't seem to be getting anywhere.

Wait!

Assign the `closure` to a variable!

{{< figure src="/ox-hugo/closure2.png" class="center" >}}

Now use `funcall` on it.

{{< figure src="/ox-hugo/closure3.png" class="center" >}}

Cool! Even though `counter` is a local variable in makeClosure, it seems to be hanging around. And that's because the lambda has closed around it.

Is there any way to access `counter`?

No.

`Counter` is encapsulated in the lambda. You have no name, no reference, no nothing. And other than invoking the lambda and causing it to be modified, there is no way to access it. Now, we could have defined an interface, we could have defined various getters and setters and ways to invoke them --- but ... we didn't.

What about getting another instance of a closure from `makeClosure`? Can we? And then call it? What will happen?

{{< figure src="/ox-hugo/closure4.png" class="center" >}}

Clearly, `myClosure2` and `myClosure1` are different instantiations of the lambda, and each lambda has its own counter.

Let's do one more and show how we might initialize an enclosed variable and update.

Assume that we have a sensor that has upper and lower limits of 100 and 0, and we will want to send it a delta value so the sensor can update its state. Like this:

{{< figure src="/ox-hugo/closure5.png" class="center" >}}

That's enough Lisp for now.

We have seen lambda in Lisp. A lambda is an anonymous function. There's also a special type of lambda called a closure, which has enclosed over local variables.

Now, how does this work out in Java?


## Baseball Players {#baseball-players}

> Baseball is the only field of endeavor where a man can succeed
> three times out of ten and be considered a good performer. - Ted
> Williams

Suppose you have data in a file somewhere for baseball players. Like this sample:

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
| OmarVizquel     | SS       | 2968  | 10586  | 1445 | 2877 | 80  | 951  | 272 |
| RustyStaub      | OF       | 2951  | 9720   | 1189 | 2716 | 292 | 1466 | 279 |
| AdrianBeltre    | 3B       | 2933  | 11068  | 1524 | 3166 | 477 | 1707 | 286 |
| BrooksRobinson  | 3B       | 2896  | 10654  | 1232 | 2848 | 268 | 1357 | 267 |
| AlbertPujols    | 1B       | 2872  | 10874  | 1846 | 3244 | 663 | 2104 | 298 |
| RobinYount      | SS       | 2856  | 11008  | 1632 | 3142 | 251 | 1406 | 285 |
| CraigBiggio     | 2B       | 2850  | 10876  | 1844 | 3060 | 291 | 1175 | 281 |
| AlKaline        | RF       | 2834  | 10116  | 1622 | 3007 | 399 | 1583 | 297 |
| RafaelPalmeiro  | 1B       | 2831  | 10472  | 1663 | 3020 | 569 | 1835 | 288 |
| HaroldBaines    | DH       | 2830  | 9908   | 1299 | 2866 | 384 | 1628 | 289 |
| EddieCollins    | 2B       | 2826  | 9949   | 1821 | 3314 | 47  | 1300 | 333 |
| ReggieJackson   | RF       | 2820  | 9864   | 1551 | 2584 | 563 | 1702 | 262 |
| FrankRobinson   | RF       | 2808  | 10006  | 1829 | 2943 | 586 | 1812 | 294 |
| HonusWagner     | SS       | 2792  | 10430  | 1736 | 3430 | 101 | 1732 | 329 |

</div>

And you want to put this into some sort of structure and search it. So, you come up with a baseball player class for this data something like this:

{{< figure src="/ox-hugo/baseballPlayer.png" class="center" >}}

And you create another class to aggregate players into a list of some kind, and you provide a method that will do one of the many searches that you know you'll want to do. Like this:

{{< figure src="/ox-hugo/classPlayersDB.png" class="center" >}}

So it's a start.

But as you look at it you think about all the searches that you  want to do, and you realize that you're going to need a lot of "getPlayers" sort of  methods.

And you realize that the only difference between all these methods is going to be one line, one statement of code, in fact, one expression:

{{< figure src="/ox-hugo/classPlayersDBAnnotated.png" class="center" >}}


## Abstract Classes {#abstract-classes}

And so, how can we pass in that expression?

So, what if we add a new method to the `PlayersDB` class,
`getPlayersWithTest`, and experiment with what the signature could
look like? Like this:

{{< figure src="/ox-hugo/classPlayersDB-firstmod.png" class="center" >}}

And the conditional would then become something like this:

{{< figure src="/ox-hugo/classPlayersDB-secondmod.png" class="center" >}}

So, the idea is to instantiate a `Tester` class with the test method (`testPlayer`) to run -- but really we'll need a subclass of `Tester` for each test, and then we can override the test method -- this sounds like an abstract class.

Like this:

{{< figure src="/ox-hugo/classPlayersDB-thirdmod.png" class="center" >}}

For now, looks good!

And notice that just by adding this abstract class, all the "red underlines" in the editor go away -- this has got to be the right approach!

What now?

Well, you cannot instantiate an abstract class. But you can extend it. Like this.

{{< figure src="/ox-hugo/moreAtBatsClass.png" class="center" >}}

So, how does it work? You add a test for it ...

{{< figure src="/ox-hugo/testerTest-zero.png" class="center" >}}

... and it works.

Cool!

But ...

... there are going to be a lot of extensions of Tester.

Yeah, and so at this point, it feels like a rock-and-a-hard-spot
situation -- either multiply the number of extensions of Tester, or
multiply the number of methods to `PlayersDB`.

If only there was some way ...


### Anonymous Classes {#anonymous-classes}

Another technique does exist -- anonymous classes.

So, adding another test, but this time we'll do this:

{{< figure src="/ox-hugo/testerTest-first.png" class="center" >}}

So, on the left hand side we've specified the type `Tester`. That's perfectly legal in the "type system" because `MoreAtBats` is an `is_a` in relation to `Tester`.

But let's not select `MoreAtBats` --- instead select the `Tester` abstract class itself.

Wait. What?? You **cannot** instantiate an abstract class.

Absolutely correct. You cannot. And we're not.

Selecting `Tester` ...

{{< figure src="/ox-hugo/testAnonymous-create.png" class="center" >}}

... IntelliJ fills in what looks suspiciously like a class definition that extends `Tester`. And in fact, that's exactly what we're doing.

Do not think that we are instantiating `Tester` -- we are not --- this is an anonymous class that extends Tester.

What is the name of this class?

It doesn't have a name. It's anonymous.

So, let's fill it in  ...

{{< figure src="/ox-hugo/testAnonymous-create-2.png" class="center" >}}

... and we run the test, and it works.

And then, it should be apparent that we can do this in the call to `getPlayersWithTest`. Like this:

{{< figure src="/ox-hugo/testAnonymous-create-3.png" class="center" >}}

And you can also use local variables ... a local variable over an anonymous class ...

{{< figure src="/ox-hugo/testAnonymous-create-4.png" class="center" >}}

Pretty cool -- anonymous classes -- yes, they are.

It can't possibly get any better, can it?


## Lambda {#lambda}

We should realize that our abstract `Tester` class exists for one reason -- to ensure that all extensions supply a `boolean testPlayer(Player p)` method. That's it!

So, let's define an interface that does just that (and we'll name it `Lambda` but the name doesn't matter).

{{< figure src="/ox-hugo/lambda-1.png" class="center" >}}

And let's modify our `PlayersDB` and add the following method:

{{< figure src="/ox-hugo/lambda-2.png" class="center" >}}

Now, in test, we can "new up" our Lambda interface just like we did with the abstract class and get an anonymous class.

{{< figure src="/ox-hugo/lambda-3.png" class="center" >}}

Wait. So we're just going to create an anonymous class??

No -- that was just for demo.

We have more expressive possibilities available to us.

Like this:

{{< figure src="/ox-hugo/lambda-4.png" class="center" >}}

Wait. What??

Pressing enter ...

{{< figure src="/ox-hugo/lambda-5.png" class="center" >}}

Stop! What is IntelliJ doing? What does `p ->` mean??

Java gives you the ability to enter a lambda expression, and IntelliJ is just prompting you to do what you obviously must (or at least can) do.

Obvious?

Think about what the interface `Lambda` requires.

The interface `Lambda` requires that any implementation of it must (**must**) have one method that accepts a `Player` parameter and returns a `boolean`.

And, that's **all** the interface requires.

In these cases, Java allows you simply to give the expression that would obviously be inside the obvious method you would obviously create if required to do so. So, you are not required to create the obvious class, the obvious method, not required to do the obvious instantiating of it, etc.  You are only required to give what is not obvious, that is, the `boolean` expression that goes inside the method.

So, `p` (which could be any legal variable name) is of type `Player` (and can only be a type Player). So, let's use `p`  in an expression that results in a `boolean`.

Like this:

{{< figure src="/ox-hugo/lambda-6.png" class="center" >}}

Here, we are saving the lambda in the variable `l` and then using that in the call to `getPlayersWithLambda`. Not that unusual, but of course, we can put the lambda expression in the call to `getPlayersWithLambda`.

{{< figure src="/ox-hugo/lambda-7.png" class="center" >}}

And there's nothing to stop us from using a local variable over the lambda ...

{{< figure src="/ox-hugo/lambda-8.png" class="center" >}}

A local variable over a lambda ... in Java ...  Cool, huh?

So ...  what's a lambda?


## Conclusion {#conclusion}

This document doesn't really have a conclusion.

I hope the lambda examples in Lisp, and then working through lambda in Java, has led to a better understanding and appreciation of this remarkable conception. And that the journey was fun, too.

Other than that ...

So common is the passing of a "tester," as in the examples above, that there's already an interface defined for the purpose: `Predicate`. You'll want to use that rather than define your own.

Lisp is cool, and can be helpful, as well as fun. If interested you'll find many Lisp resources for yourself, but, if you want to get a REPL up and running ASAP, you might start with either of the first two below. A third possibility is also given.


### Portacle {#portacle}

[Portacle](https://portacle.github.io/) (Common Lisp)


### Racket {#racket}

[Racket](https://racket-lang.org/) (Scheme)

The historic MIT 6.001 by Sussman and Abelson in Scheme is [here](https://www.youtube.com/watch?v=-J_xL4IGhJA).

If you use [emacs](https://www.gnu.org/software/emacs/), M-x ielm, will bring up a REPL for emacs-lisp. Quite handy.


### Java {#java}

Java has a REPL!  You can access it in IntelliJ! See their page on the [JConsole](https://www.jetbrains.com/help/idea/jshell-console.html).


### Oracle {#oracle}

Oracle has a few things to say about these topics:

1.  [Anonymous Classes](https://docs.oracle.com/javase/tutorial/java/javaOO/anonymousclasses.html)
2.  [Lambda Expression](https://docs.oracle.com/javase/tutorial/java/javaOO/lambdaexpressions.html)


### Lambda for Ever {#lambda-for-ever}

Finally, [Lambda](https://en.wikipedia.org/wiki/Anonymous_function) in many languages.

  Bye!
:)
