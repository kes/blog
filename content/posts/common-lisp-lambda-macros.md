---
title: "Common Lisp Lambdas and Macros"
author: ["Karl Stump"]
date: 2025-11-12
tags: ["lisp", "java"]
draft: false
math: true
---

I was thinking about lambdas and closures the other day ('cause after all, why not?), and I was looking at Paul Graham's excellent [ANSI Common Lisp](https://www.google.com/search?q=ansi%20common%20lisp%20book&sclient=gws-wiz) -- in chapter one he begins by asking, "why learn Lisp?" And he shows some `C` and `Lisp` code for summing the integers less than `n`, like so:

```c
int sum (int n){
  int i, s = 0;
  for (i=0; i< n; i++)
    s += i;
  return s;
}
```

And, then `Common Lisp`:

```lisp
(defun sum (n)
  (let ((s 0))
    (dotimes (i n s)
      (incf s i))))
```

And he makes the interesting statement:

> If you only need to do such simple things, it doesn't really matter which language you use.

And that's a great observation.

There may be any number of reasons for choosing one language over another. Everything from the number of available developers to code in the language, to the speed of the resulting executable code. Among various reasons, is also the important question, "what can I do with it?"


## Lambda {#lambda}

Graham then demonstrates a lexical closure in `Lisp`:

```lisp
(defun addn (n)
  #'(lambda (x)
    (+ x n)))
```

This type of thing, the lambda, and further a closure, really is the bread-and-butter of `Lisp` programming. The fact that many programmers do know about them, much less, feel a need for them ties into one of Graham's themes: "programming languages teach you not to want what they cannot provide."

Lambdas have been in Lisp since 1959.

Java introduced lambda expressions in version 8 (2014).

And here is Oracle's page on  [Lambda Expressions](https://docs.oracle.com/javase/tutorial/java/javaOO/lambdaexpressions.html). What is clear, at least to me, is that Lambdas were meant to be a convenience for the verbose anonymous class, particularly when that class is very simple, the interface providing only one method. In that case, Java provides a lambda. (But even so it never provided closures, although some might debate that; i.e., what is the definition of a closure?)

At the time, there was a lot of excitement. But, maybe the lambda was not all that it was cracked up to be??

According to some Lambdas have died: [RIP Java Lambdas](https://medium.com/@kotiavula6/rip-java-lambdas-2014-2025-you-were-cool-until-you-werent-4ab9bac644c8), and according to that post the reasons are:

1.  Debugging hell
2.  Unreadable for Junior Devs
3.  Harder to Unit Test
4.  Performance Hit
5.  False Promise of Functional Purity

Definitely worth a look.

The problem is, IMO (you do notice the IMO, right?), the problem is that Lambdas don't really belong in Java. That might be a controversial opinion. But really, lambdas are simply not where the soul of Java is found. This is not to say that the syntactic sugar for an anonymous class in Java is not a nice thing. It totally is. I love it, and I use it.

It should be noted that lambdas and closure are not the same thing. A lambda is an anonymous function. A closure, on the other hand, is an a function that captures and retains access to variable outside of its lexical scope -- even when executed outside that scope. See [Closure vs Lambda](https://calledges.com/computer/closure-vs-lambda)

In Lisp the lambda _is_ a closure. And in Java it's not. And that's okay. In fact, lambda might not even be the strongest paradigm in `Lisp`.

The really key strength of `Lisp` is found in what most coders dislike most on encountering `Lisp` and that is parentheses. Well, not parentheses exactly but something called [homoiconicity](https://en.wikipedia.org/wiki/Homoiconicity). Because of this property, `Lisp` allows the developer to write programs that write programs, using a rather unique feature in `Lisp` called `macros`.  With "run-time typing" thrown in Graham, rhapsodizes:

> With macros, closures, and run-time typing, Lisp transcends object oriented programming.

Graham's book was published in 1996 and he believed that the features of `Lisp` would bring a new way of programming. Indeed, it was one of his goals for his book was to explain this new approach:

> One of the aims of this book is to explain not just the Lisp language, but the new approach to programming that Lisp makes possible. This approach is one that you will see more of in the future.

As noble as his sentiment was, and is, and whatever the benefits of `Lisp` for programming, it is still nevertheless a fact that `Lisp` never caught on.

In Java, if you want to define a Lambda, you'll start with an Interface:

```java
@FunctionalInterface
public interface Operation {
    void operate(int n);
}
```

And somewhere along the line, you will have to implement the interface. (No, you are not instantiating the Interface. That cannot be done. You must implement interfaces.) For example,

```java
public class Lambda {
    public static void main(String args[]) {
        int a = 12;
        int b = 88;

        temp(a, new Operation() {
                // overrides the operate() method
                @Override
                public void operate(int n) {
                    // prints the result
                    System.out.println("Result is: " + (n + b));
                }
            });
    }

    private static void temp(int i, Operation op) {
        op.operate(i);
    }
}
```

Of course, this is all very ugly. I mean it is an anonymous class with the interfaces method, so that's good and useful. But there is better syntactic sugar available.

```java
int a = 12;
int b = 88;

// overrides the operate() method
temp(a, n -> {
        // prints the result
        System.out.println("Result is: " + (n + b));
    });
```

And boom! That strange and scary `->` -- but it's lambda! And that is readable.

So, I agree. It's a lambda. But is it a closure? Typically a closure is define as a lambda enclosing over a local variable in the scope of the defined lambda. In this example, the local `b` is in the scope of the lambda. But there's only so much that you can do with it.

So, in my opinion, Java does have closures. The use of the local variable `b` in the code is very much limited. It cannot be modified. And while there might be any number of technical reasons why this is so, the point is that `b` cannot be modified.

In my view a closure must allow the enclosed local variables to be modified. Perhaps I'm wrong?

In any event, in `Lisp` this is easily done:

```lisp
(defun adjust (meter)
  #'(lambda (x)
      (setq meter (+ x meter))))
```

Here the lambda has closed over the local meter, and meter is fully usable. This would also be true for any variables defined with `let`, and hence a closure in `Lisp` is sometimes called a, "let over a lambda"

Or we could have:

```lisp
(defun meter-maker (meter)
  #'(lambda(x &optional parm)
      (cond ((eq x :incr) (setq meter (+ meter parm)))
            ((eq x :decr) (setq meter (- meter parm)))
            ((eq x :get) meter)
            (t "command unrecognized"))))
```

That's all good. But again, here's a secret: lambda and closures, while very strong, are the not the strongest feature of `Lisp`.


## Macros {#macros}

The real power of `Lisp` is in its macros.

Note: This is simply touching upon the topic of macros, not a deep dive. Someday, I'll ...

Unfortunately, when the word "macro" is used most will think of a sort of "basic" substitution. But in `Lisp` the macro facility has the full power of the `Lisp` language. You are able to control, using the power of `Lisp`, what the compiler sees, and so, the macro, is the defining of a new language, or a DSL, or simply making language additions. These additions can greatly aid in writing code, understanding code, and in abstracting away boilerplate. It is, or should be, a programmer's dream.

And I could be wrong, but the "enlightenment" that is often spoken about when learning `Lisp` is I think found here in the idea of "code that writes code." I could be wrong.

> The most common way to write programs that write programs is by defining macros. Macros are operators that are implemented by transformation. You define a macro by saying how a call to it should be translated. This translation, called macro-expansion, is done automatically by the compiler. So the code generated by your macros becomes an integral part of your program, just as if you had typed it in yourself.

A simple example is given as:

```lisp
(defmacro nil! (x)
  (list 'setf x nil))
```

We can use `macroexpand` to see what the result is:

```lisp
(macroexpand-1 '(nil! a))
```

Which gives the expected `(SETF A NIL)` because that's the code that the macro creates.

You can see that the parameter "x" was assigned the value "a" and so the list generated (as already given) is `(SETF A NIL)`. This list is code that can be (will be) expanded during compilation and executed at runtime.

The significance of this should not be missed. It is of course equivalent to "hello world," and so, it might be easy to say, "so what?" It should be noted that we have just modified the `Lisp` language. This is code that creates code, and that created code will be compiled and executed.

So, a macro is `Lisp` code that produces `Lisp` code. And the macro "language" is not in any way less than any other `Lisp` code. It is `Lisp` code. A `Lisp` macro is not some "lisp lite," but instead, `Lisp` macros have the full power of the `Lisp` compiler.  This macro facility, therefore, is rather unique.  (I am not familiar with all programming languages in the world, so I say, "rather unique.")

My understanding: Java uses annotation processors and code generation tools to simulate some macro-like behavior, but it does not support true macros, and certainly nothing approaching `Lisp` macros. Thus, while annotations can simplify code and reduce boilerplate, they do not offer the same level of flexibility.

Ideally a macro is defined using `backtick` rather than `list` (above) -- and within the `backtick` list a comma for evaluation. Like this:

```lisp
(defmacro nil! (x)
  `(setf ,x nil ))
```

Thus, `backtick` allows for evaluation. `,x` means that x is to be evaluated and the result substituted. Other than this syntax difference, the macros are effectively the same, producing the same code.

However, it can be imagined since ~~creating lists~~ **creating code** is the whole point of a macro, then there would be various kinds of evaluation and substitution.

So, consider the following distinction between `,` and `,@`, that latter of which is called "splicing."

```lisp
CL-USER> (setf lst '(a b c))
(A B C)
CL-USER> `(list is ,lst)
(LIST IS (A B C))
CL-USER> `(list has these elements ,@lst)
(LIST HAS THESE ELEMENTS A B C)
```

That's enough for now. Graham's hope that `Lisp` would bring in a new wave of programming was not fulfilled. Too bad for a generation of programmers.

> Meanwhile, ideas borrowed from Lisp increasingly turn up in the mainstream: interactive programming environments, garbage collection, and run-time typing, to name a few.
