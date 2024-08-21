---
title: "What's a Lambda?"
author: ["Karl Stump"]
date: 2021-09-14
tags: ["programming", "lambda", "java"]
draft: false
---

This document is a brief investigation into the question:

> What is a lambda?

And, in our context, more specifically, what is lambda in computer
languages, and even more specifically, what is lambda in Java.

Java added lambda expressions in 2014 (see [Java SE 8](https://en.wikipedia.org/wiki/Java_version_history#Java_SE_8)).

**Caveat**: Nothing in this document is meant to be exhaustive. It's
not the final word on anything. And there most likely are errors in
it &#x2013; in fact, most definitely.  And there are certainly better ways
that things could have been said, or demonstrated. But it is hoped
that in some way some part of it can prove beneficial in thinking
about lambda, and also along the way be fun.

But lambda has been around for a long time in other languages, like
[Lisp](https://en.wikipedia.org/wiki/Lisp_(programming_language)#Lambda_expressions_and_function_definition).

So, here's the plan:

1.  Let's investigate lambda as it occurs in Common Lisp.
2.  Then with that as a foundation, we will move the investigation
    into Java.

So, what's a lambda?


## Lambda in Lisp {#lambda-in-lisp}

In demonstrating lambda in Lisp, I'm going to assume that the
examples are sufficiently clear that even someone with no experience
with Lisp, yet knowledgeable in some other programming language,
will have not much trouble following. As such, I will leave a lot of
Lisp details  unsaid.

{{< figure src="/ox-hugo/lambdaDefined.png" width="1000px" >}}

In the REPL (Read, Eval, Print Loop; think of it as Lisp's
interactive window &#x2013; okay, if severely interested, you might try
[REPL](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop) on Wiki) defining a lambda &#x2026;

{{< figure src="/ox-hugo/definingLambda.png" width="1000px" >}}

Press `return`, and the REPL evaluates the expression, prints the
"printed representation" of the lambda.

{{< figure src="/ox-hugo/lambdaDefined.png" width="1000px" >}}

What is a lambda? It says it's a function.

Is there more detail available?

In fact, there is.

Right-clicking on the presentation gives a context menu.

Selecting inspect&#x2026;

{{< figure src="/ox-hugo/inspectLambda.png" width="1000px" >}}

&#x2026; gives us some details.

{{< figure src="/ox-hugo/inspectLambda2.png" width="1000px" >}}

The CODE line looks interesting. Click it.

{{< figure src="/ox-hugo/disassembly.png" width="1000px" >}}

Assembler?

Okay, the lambda expression was typed into the REPL, and being
evaluated, a function was created, and of course, functions at some
point must be code on the machine, and yes, that assembler is the
actual code.

But if it's code, then let's execute it.

Okay, back in the REPL, invoke the lambda! Call it, run it.

But how?

Can't be done. It doesn't have a name. It's anonymous. You can't
call it because it doesn't have a name.

Okay, so &#x2026; ?

Here, fortunately, the REPL can help us.

In the environment of the REPL, behind the scenes, a temporary name
has in fact been given to the lambda. But please note that this is
entirely a convenience feature supplied by the REPL.

What is this temporary name? It's the symbol `*`. So, the lambda
does have a name, temporarily. It's `*`

So, let's use it, and invoke the lambda. How?

We can use `funcall` &#x2014; `funcall` is a function that takes a
function as it first parameter, and optionally takes additional
arguments.

{{< figure src="/ox-hugo/useAsterisk.png" width="1000px" >}}

The result of `funcall` invoking the lambda is returned, it's the
string returned by the lambda, "hello world" &#x2014; and note that now
`*` refers to the string "hello world" &#x2013; so, we can't use `*` to
invoke the lambda (and with nothing referring to the lambda, the
garbage collector will dispose of it).

So, let's take a pause and ask the question: what's a lambda?

-   In some languages, an expression that begins with "(lambda &#x2026;"
-   In some environments, a printed representation, like `#<FUNCTION (LAMBDA
          ()) {1002F5D59B}>`
-   Code that, in some environments, we can inspect even down to the
    disassembly.
-   Code that is "anonymous" &#x2013; it doesn't have a name, or anything
    that points to it, leaving us unable to get to it.
-   But also, code that could be executed if we could get to it.
-   And it's a function created "on the fly," at run time, and that
    will be garbage collected when it's done being used.

That's a good start. Let's see how we might use a lambda.

{{< figure src="/ox-hugo/funcallLambda.png" width="1000px" >}}

Wait &#x2013; what?

We just created a lambda &#x2013; "on the fly" &#x2013; it's anonymous, doesn't
have a name, but since we're passing it to `funcall` it doesn't need
a name. And when the evaluation of the lambda is complete it will be
disposed of.

Pretty neat.

Note also that `funcall` is passed not only a lambda but also the
value 100. When `funcall` calls the lambda it will pass it that
value. The lambda code is executed and returns the value 101.

Of course, we could just invoke the `plus` function using the symbol
`+`.

Or we could pass `+` to `funcall`, too.

(Don't be confused by the `#'` prepended to the `+`
symbol. This is simply the Lisp way of indicating that the function
value of `+` is wanted.)

{{< figure src="/ox-hugo/funcallPlus.png" width="1000px" >}}

Interesting. And it gets you thinking: might some of this be useful?

More specifically:

1.  Is defining a function without a name useful?
2.  Is passing around a function useful?

The answer turns out to be emphatically yes. The myriad of
applications and details is far beyond the scope of this brief
discussion.

However,

In Lisp there are lots of functions that take a function as an
argument, not just `funcall`.

For example, `mapcar` is one such function. Rather than tell you
what `mapcar` does, let's just see how it works.

{{< figure src="/ox-hugo/mapcar.png" width="1000px" >}}

How would you describe it?

It looks like `mapcar` passes the first element of each list to the
given function, collecting the results from each iteration.

Could a lambda be used? Let's try.

Here's a lambda that computes averages.

{{< figure src="/ox-hugo/average.png" width="1000px" >}}

Here's one that rearranges the numbers.

{{< figure src="/ox-hugo/rearrange.png" width="1000px" >}}

Here's one that keeps a running total for each iteration.

{{< figure src="/ox-hugo/runningTotal.png" width="1000px" >}}

Oh, wait, that lambda is a very important case: it's a `closure`.


## Closure {#closure}

> "Sometimes it's called a closure, other times a saved lexical
> environment. Or, as some of us like to say, let over
> lambda. Whatever terminology you use, mastering this concept of
> a closure is the first step to becoming a professional lisp
> programmer. In fact, this skill is vital for the proper use of
> many modern programming languages, even ones that don't
> explicitly contain let or lambda, such as Perl or Javascript."
> &#x2013; [Doug Hoyte](https://letoverlambda.com/index.cl/guest/chap2.html)

Among the modern programming languages that Hoyte didn't mention at
the time of his writing, but could now, is Java.

Let's a define a function.

{{< figure src="/ox-hugo/closure1.png" width="1000px" >}}

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
        it sounds like "closure" (a noun) would do it &#x2013; or maybe we
        should say, "enclosure" &#x2014; or perhaps, we should just call it
        an "encapsulation" because it "encapsulates" local variables?

Okay, we have a function, and a named one at that &#x2013; a named function
that creates unnamed functions. Let's invoke it.

{{< figure src="/ox-hugo/closure1.1.png" width="1000px" >}}

Notice that the "printed representation" of the lambda now includes
the word "closure." Fine, let's use `funcall` on the closure from
`makeClosure`.

{{< figure src="/ox-hugo/closure1.2.png" width="1000px" >}}

Well &#x2026; now what?

{{< figure src="/ox-hugo/closure1.3.png" width="1000px" >}}

This doesn't seem to be getting anywhere.

Wait! Assign the closure to a variable!

{{< figure src="/ox-hugo/closure2.png" width="1000px" >}}

Now use `funcall` on it.

{{< figure src="/ox-hugo/closure3.png" width="1000px" >}}

Cool! Even though `counter` is a local variable in makeClosure, it
seems to be hanging around. And that's because the lambda has
closed around it. Is there any way to access `counter`?

No.

It is encapsulated in the lambda. You have no name, no reference,
no nothing. And other than invoking the lambda and causing it to be
modified, there is no way to access it. (Now, we could have defined
an interface, we could have defined various getters and setters and
ways to invoke them &#x2014; but &#x2026; we didn't).

What about getting another instance of a closure from
`makeClosure`? Can we? And then call it? What will happen?

{{< figure src="/ox-hugo/closure4.png" width="1000px" >}}

Clearly, `myClosure2` and `myClosure1` are different instantiations
of the lambda, and each lambda has its own counter.

Let's do one more and show how we might initialize an enclosed
variable and update.

Assume that we have a sensor that has upper and lower limits of 100
and 0, and we will want to send it a delta value so the sensor can
update its state. Like this:

{{< figure src="/ox-hugo/closure5.png" width="1000px" >}}

That's enough Lisp for now.

Now, how does this work out in Java?


## Baseball Players {#baseball-players}

> Baseball is the only field of endeavor where a man can succeed
> three times out of ten and be considered a good performer. - Ted
> Williams

Suppose you have data in a file somewhere for baseball players. Like this sample:

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

And you want to put this into some sort of structure and search
it. So, you come up with a baseball player class for this data
something like this:

{{< figure src="/ox-hugo/baseballPlayer.png" width="1000px" >}}

And you create another class to aggregate players into a list of some
kind, and you provide a method that will do one of the many searches
that you know you'll want to do. Like this:

{{< figure src="/ox-hugo/classPlayersDB.png" width="1000px" >}}

So it's a start.

But as you look at it you think about all the searches that you
want to do, and you realize that you're going to need a lot of
"getPlayers" sort of  methods.

And you realize that the only difference between all these methods
is going to be one line, one statement of code, in fact, one
expression:

{{< figure src="/ox-hugo/classPlayersDBAnnotated.png" width="1000px" >}}


## Abstract Classes {#abstract-classes}

And so, how can we pass in that expression?

So, what if we add a new method to the `PlayersDB` class,
`getPlayersWithTest`, and experiment with what the signature could
look like? Like this:

{{< figure src="/ox-hugo/classPlayersDB-firstmod.png" width="1000px" >}}

And the conditional would then become something like this:

{{< figure src="/ox-hugo/classPlayersDB-secondmod.png" width="1000px" >}}

So, the idea is to instantiate a `Tester` class with the test
method (`testPlayer`) to run &#x2013; but really we'll need a subclass of
`Tester` for each test, and then we can override the test method
&#x2013; this sounds like an abstract class.

Like this:

{{< figure src="/ox-hugo/classPlayersDB-thirdmod.png" width="1000px" >}}

Logging seems like a reasonable idea &#x2026; maybe? Or other methods
will be added later? Maybe these details will be important later,
but for now, looks good!

And notice that just by adding this abstract class, all the "red
underlines" in the editor go away &#x2013; this has got to be the right
approach!

What now?

Well, you cannot instantiate an abstract class. But you can extend
it. Like this.

{{< figure src="/ox-hugo/moreAtBatsClass.png" width="1000px" >}}

So, how does it work? You add a test for it &#x2026;

{{< figure src="/ox-hugo/testerTest-zero.png" width="1000px" >}}

&#x2026; and it works.

Cool!

But &#x2026;

&#x2026; there are going to be a lot of extensions of Tester.

Yeah, and so at this point, it feels like a rock-and-a-hard-spot
situation &#x2013; either multiply the number of extensions of Tester, or
multiply the number of methods to `PlayersDB`.

If only there was some way &#x2026;


### Anonymous Classes {#anonymous-classes}

Another technique does exist &#x2013; anonymous classes.

So, adding another test, but this time we'll do this:

{{< figure src="/ox-hugo/testerTest-first.png" width="1000px" >}}

So, on the left hand side we've specified the type `Tester`. That's
perfectly legal in the "type system" because `MoreAtBats` is an
`is_a` in relation to `Tester`.

But let's not select `MoreAtBats` &#x2014; instead select the `Tester`
abstract class itself.

Wait. What?? You **cannot** instantiate an abstract class.

Absolutely correct. You cannot. And we're not.

Selecting `Tester` &#x2026;

{{< figure src="/ox-hugo/testAnonymous-create.png" width="1000px" >}}

&#x2026; IntelliJ fills in what looks suspiciously like a class
definition that extends `Tester`. And in fact, that's exactly what
we're doing.

Do not think that we are instantiating `Tester` &#x2013; we are not &#x2014;
this is an anonymous class that extends Tester.

What is the name of this class?

It doesn't have a name. It's anonymous.

So, let's fill it in  &#x2026;

{{< figure src="/ox-hugo/testAnonymous-create-2.png" width="1000px" >}}

&#x2026; and we run the test, and it works.

And then, it should be apparent that we can do this in the call to
`getPlayersWithTest`. Like this:

{{< figure src="/ox-hugo/testAnonymous-create-3.png" width="1000px" >}}

And you can also use local variables &#x2026; a local variable over an
anonymous class &#x2026;

{{< figure src="/ox-hugo/testAnonymous-create-4.png" width="1000px" >}}

Pretty cool &#x2013; anonymous classes &#x2013; yes, they are.

It can't possibly get any better, can it?


## Lambda {#lambda}

We should realize that our abstract `Tester` class exists for one
reason &#x2013; to ensure that all extensions supply a `boolean
  testPlayer(Player p)` method. That's it! (And as far as that logging
idea?

So, let's define an interface that does just that (and we'll name it
`Lambda` but the name doesn't matter).

{{< figure src="/ox-hugo/lambda-1.png" width="1000px" >}}

And let's modify our `PlayersDB` and add the following method:

{{< figure src="/ox-hugo/lambda-2.png" width="1000px" >}}

Now, in test, we can "new up" our Lambda interface just like we did
with the abstract class and get an anonymous class.

{{< figure src="/ox-hugo/lambda-3.png" width="1000px" >}}

Wait. So we're just going to create an anonymous class??

No &#x2013; that was just for demo.

We have more expressive possibilities available to us.

Like this:

{{< figure src="/ox-hugo/lambda-4.png" width="1000px" >}}

Wait. What??

Pressing enter &#x2026;

{{< figure src="/ox-hugo/lambda-5.png" width="1000px" >}}

Stop! What is IntelliJ doing? What does `p ->` mean??

Java gives you the ability to enter a lambda expression, and
IntelliJ is just prompting you to do what you obviously must (or at
least can) do.

Obvious?

Think about what the interface `Lambda` requires.

The interface `Lambda` requires that any implementation of it must
(**must**) have one method that accepts a `Player` parameter and returns a
`boolean`.

And, that's **all** the interface requires.

In these cases, Java allows you simply to give the expression that
would obviously be inside the obvious method you would obviously
create if required to do so. So, you are not required to create the
obvious class, the obvious method, not required to do the obvious
instantiating of it, etc.  You are only required to give what is not
obvious, that is, the `boolean` expression that goes inside the
method.

So, `p` (which could be any legal variable name) is of type `Player`
(and can only be a type Player). So, let's use `p`  in an expression
that results in a `boolean`.

Like this:

{{< figure src="/ox-hugo/lambda-6.png" width="1000px" >}}

Here, we are saving the lambda in the variable `l` and then using
that in the call to `getPlayersWithLambda`. Not that unusual, but of
course, we can put the lambda expression in the call to
`getPlayersWithLambda`.

{{< figure src="/ox-hugo/lambda-7.png" width="1000px" >}}

And there's nothing to stop us from using a local variable over the
lambda &#x2026;

{{< figure src="/ox-hugo/lambda-8.png" width="1000px" >}}

A local variable over a lambda &#x2026; in Java &#x2026;  Cool, huh?

So &#x2026;  what's a lambda?


## Conclusion {#conclusion}

This document doesn't really have a conclusion.

I hope the lambda examples in Lisp, and then working through lambda in Java, has
led to a better understanding and appreciation of this remarkable conception.
And that the journey was fun, too.

Other than that &#x2026;

So common is the passing of a "tester," as in the examples above, that there's
already an interface defined for the purpose: `Predicate`. You'll want to use
that rather than define your own.

Lisp is cool, and can be helpful, as well as fun. If interested you'll find many
Lisp resources for yourself, but, if you want to get a REPL up and running ASAP,
you might start with either of the first two below. A third possibility is also
given.


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
