---
title: "Understanding Loop"
author: ["Karl Stump"]
date: 2024-08-23
tags: ["lisp"]
draft: false
---

Loop is a massive [DSL](https://en.wikipedia.org/wiki/Domain-specific_language) buried in Common Lisp. It's possible because of Lisp's macro facility, but that's
for another time.

I'll try to explain it a bit, but nothing exhaustive. Just enough to give a foundation for
additional exploring [loop in the hyperspec](https://www.lispworks.com/documentation/HyperSpec/Body/06_a.htm).


## The Fundamental Idea {#the-fundamental-idea}

From <https://www.lispworks.com/documentation/HyperSpec/Body/06_aaaa.htm>

> A simple loop form is one that has a body containing only compound forms. Each form is evaluated in
> turn from left to right. When the last form has been evaluated, then the first form is evaluated
> again, and so on, in a never-ending cycle.

This provide the fundamental understanding of how loop works. Something like this:

{{< highlight text >}}
(loop body)
body -> compound-forms
compound-forms -> compound-form compound-form ...
compound-form -> form form ...
{{< /highlight >}}

The meaning of the above is that `loop` iterates over a list of lists. Given `(loop (a) (b) (c))` `loop`
first evaluates a, then b, then c. Having finished the forms, it goes back to the beginning of
the list and starts over.

Thus:

{{< highlight lisp >}}
(loop (print "1") (print "2") (print "3"))
{{< /highlight >}}

is an infinite loop and outputs (forever):

```text
"1"
"2"
"3"
"1"
"2"
"3
(etc. .... )
```

Obviously, we need more from `loop` and this takes us to the so-called "extended loop form".

From <https://www.lispworks.com/documentation/HyperSpec/Body/06_aaab.htm>

> An extended loop form is one that has a body containing atomic expressions. When the loop macro
> processes such a form, it invokes a facility that is commonly called \`\`the Loop Facility.''
>
> The Loop Facility provides standardized access to mechanisms commonly used in iterations through
> Loop schemas, which are introduced by loop keywords.
>
> The body of an extended loop form is divided into loop clauses, each which is in turn made up of
> loop keywords and forms.

So,

{{< highlight text >}}
(loop (loop-clause) (loop-clause) (loop-clause)..)
loop-clause -> (loop-keyword (forms))
{{< /highlight >}}


## Loop Keywords {#loop-keywords}

`loop` keywords are not Common Lisp keywords. So, don't be confused. Instead
things like `while,` `collect,` `when,` are loop keywords.

From <https://www.lispworks.com/documentation/HyperSpec/Body/06_aac.htm>

Try this and just remember that `loop` just goes from clause to clause and evaluates  (the indentation helps):

<a id="code-snippet--loop-keywords"></a>
{{< highlight lisp >}}
(defun unacceptable (i)
  (> i 25))
(defun square (x)
  (* x x))

(loop for i from 0 to 100 by 5
      while (not (unacceptable i))
      unless (= i 0)
        collect (list i)
        and collect (square i)
        and collect (+ i 33)
      do
         (format t "Working on ~D now~%" i)
         (format t "...more here ~D now~%" i)
      when (evenp i)
        do (format t "Hey! ~D is a non-odd number~%" i)
      finally
         (format t "About to exit!~%"))
{{< /highlight >}}

The above code prints the following:

{{< highlight text >}}
Working on 0 now
...more here 0 now
Hey! 0 is a non-odd number
Working on 5 now
...more here 5 now
Working on 10 now
...more here 10 now
Hey! 10 is a non-odd number
Working on 15 now
...more here 15 now
Working on 20 now
...more here 20 now
Hey! 20 is a non-odd number
Working on 25 now
...more here 25 now
About to exit!
{{< /highlight >}}

And it returns:

```text
((5) 25 38 (10) 100 43 (15) 225 48 (20) 400 53 (25) 625 58)
```

Okay, now more about keywords:

> Each loop keyword introduces either a compound loop clause or a simple loop clause that can consist
> of a loop keyword followed by a single form. The number of forms in a clause is determined by the
> loop keyword that begins the clause and by the auxiliary keywords in the clause. The keywords do,
> doing, initially, and finally are the only loop keywords that can take any number of forms and group
> them as an implicit progn.

It's interesting in the code example that the first `do` is a clause, while the second `do` is
considered as a part of the `when` clause. How many forms follow this `when ... do...` as many as you want.
The clause will end when the next keyword is encountered, or the `loop` itself.

Now there is this: "The keywords do, doing, initially, and finally are the only loop keywords that
can take any number of forms and group them as an implicit progn."

Note that `do` in `when ... do` is under the `when` and is required.

<a id="code-snippet--loop-1"></a>
{{< highlight lisp >}}
(loop for i from 1 to 3
        initially (format t "do this one time at the start! i = ~D)~%" i)
      do (format t "do keyword: working on ~D~%" i)
         (format t "do keyword: ...still working!~%")
      when (oddp i)
        do
           (format t "     Odd job check! i= ~D~%" i)
           (format t "     bye-bye!!~%" i)
      when (evenp i)
        do
           (format t "     Even job check: yup ~D is even~%" i)
           (format t "     Even check done! have a good day!~%" i)
      when (oddp i)
        do (format t "     wait... what??~%" i)
      finally (format t "finally: ==>I'm outta here~%")
              (format t "finally: ==>really I must go!~%"))
{{< /highlight >}}

You can see by the indentation that there is a `when ... do`
construct. What's the output?

{{< highlight text >}}
do this one time at the start! i = 1)
do keyword: working on 1
do keyword: ...still working!
     Odd job check! i= 1
     bye-bye!!
     wait... what??
do keyword: working on 2
do keyword: ...still working!
     Even job check: yup 2 is even
     Even check done! have a good day!
do keyword: working on 3
do keyword: ...still working!
     Odd job check! i= 3
     bye-bye!!
     wait... what??
finally: ==>I'm outta here
finally: ==>really I must go!
{{< /highlight >}}


## Local Variable Initialization {#local-variable-initialization}

<https://www.lispworks.com/documentation/HyperSpec/Body/06_abb.htm>

`with` is your friend here. Mulitple `with`'s are possible, or `with ... and .. and ...=`

CLHS explains that successive uses of `with` is equivalent to `let*`
where the binding occur successively. But `with ... and` can be used and is the same as `let` where
bindings are done in parallel.

So,

{{< highlight lisp >}}
;; successive, so b can be initialized with a, and c with b
(loop with a = 1
      with b = (+ a 2)
      with c = (+ b 3)
      return (list a b c))
{{< /highlight >}}

If you change the second `with` to an `and` you'll get an error.

But we _can_ change successive uses of `with` to `and` in the above code if we bind a, b globally. But
then of course the global a and b are used in the binding, and not the local.

{{< highlight lisp >}}
;; These bindings occur in parallel.
 (setq a 5 b 10)
=>  10
 (loop with a = 1
       and b = (+ a 2)
       and c = (+ b 3)
       return (list a b c))
{{< /highlight >}}

That's enough to get started with the awesome `loop`!
