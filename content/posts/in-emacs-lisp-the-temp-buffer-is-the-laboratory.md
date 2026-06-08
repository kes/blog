---
title: "In Emacs Lisp, the Temp Buffer Is the Laboratory"
author: ["Karl Stump"]
date: 2026-06-07
draft: false
math: true
---

Coming from other languages, like C, or Python, attempting to learn Emacs Elisp can feel jarring.

Most of us can remember what it was like to learn our first programming language. Here's an imaginary scenario that I think strays not too far from the truth.

Suppose you're a beginning student in programming taking a course on C, and it's the first day of class.  You browsed a bit through the textbook, and you've seen something called `strlen`, and in the textbook you see written: `len=strlen("This is a string");` You don't understand it. Indeed, you don't know anything about programming. But you are eager and want to experiment.

But there is an immediate problem. You want to test a function but the function must run in a program.

In a classroom setting the teacher will find it necessary to give the following code:

```C
#include <stdio.h>
#include <string.h>
int main(void){
  int len = 0;
  /* Uncomment these lines, compile and run! */

  /* len = strlen("How long is this string?");  */
  /* printf("String Length: %d", len); */
  return 0;
  }
```

But that's not all. The teacher will also need to explain how to compile, perhaps how to run the linker, perhaps how to execute the bash command `chmod`, and finally, how to run the program (don't forget `./` at the beginning!).

This is not an exaggeration. In order to test `strlen` all this is required.

However, there might be pushback, "You know, okay, we see your point. Your point is that the student wants to explore the function `strlen` and there's a surrounding environment that he must use, and a somewhat arcane process of compiling and the like. But what else can we do? It might also be observed that you can't run a program without a computer. Are we going to complain about the horrors of that? And really, how difficult is this? The beginning student simply must cope."

I think that's fair, actually. Nevertheless, I believe that we must acknowledge that for the beginning student this is all very difficult, and it creates an impediment. First, the surrounding code will be very distracting. The `#include`, the `int main(void)` is complete noise. The inquisitive student might wonder how all these elements affect `strlen`. Are they required for _strlen_? If not then why are they required? On and on. And then there is the problem of the process, the steps needed to actually produce and run the executable, the compiling and such --- these will not be understood for a rather long time, and as such, one might ask, of what use are they for the beginning student? None, other than learning magic spells, perhaps.

We can shrug our shoulders and conclude that this is the cost of learning to program. After all, at the beginning, even the basics are not known --- everything is confusion --- and so the beginner must be given them, and must simply follow them.

I'm sympathetic to the "shrug your shoulders" camp. However, I still want to recognize and acknowledge that there is a problem, a real and significant problem. A problem that impedes --- even if we can't do much about it.

So, okay, the student is now past the boilerplate and the steps required to produce the executable. It's accepted, even if not totally understood. And the student continues to experiment.

Eventually, the student will try (and I assume all developers will recognize this type of code. We have all written it):

```C
#include <stdio.h>
#include <string.h>
int main(void){
  int len = 0;
  char ary[5];

  strcpy(ary, "hello");
  len = strlen(ary);
  printf("String Length: %d", len);
  return 0;
  }
```

This code blows up. And finally it's understood that strings have a terminating `\0`, and while that requires space in the array, it is not counted by `strlen`

```C
#include <stdio.h>
#include <string.h>
int main(void){
  int len = 0;
  char ary[6];
  ary[0] ='h';
  ary[1] ='e';
  ary[2] ='l';
  ary[3] ='l';
  ary[4] ='o';
  ary[5] ='\0';
  char ary2[6] = "blah";
  strcpy(ary2, ary);
  len = strlen(ary2);
  printf("String ary2: %s\n", ary2);
  printf("String Length: %d", len);
  return 0;
  }
```

Experiments like this will continue on and on, not only through the student's time of study, but into a career. Indeed, I doubt there's a software developer anywhere that doesn't consider hands-on coding and experimentation as the only way of learning to program. It was the method of initially grasping coding concepts and idioms, and it is the way of continuing to advance --- you learn by doing, by writing actual code, by running it, and seeing the results.

And the problem of the beginning student is the same problem all of us face. We want to continue to experiment with code. The question is can we lower the friction enough to do so?

In Elisp, in some ways experimentation is even easier.

Suppose the student is in Emacs and discovers there's a function called `(split-string)`. How does `(split-string)` work? The student types `(split-string "hello world")` and evaluates it with (`C-x C-e`). In the message buffer at the bottom Emacs displays: `("hello" "world")`. Emacs also provide a REPL which makes this sort of experimentation even easier. Type in the expression, press return, and the results display in the REPL. Very convenient.

Whatever the language, continuing this hands-on experimentation approach would seem ideal for more complex program development, too.  The problem is that code multiplies quickly and experimentation becomes more laborious. There is also the problem of setting up test data and various conditions which may have to be done repeatedly.

This breaks down into two problems:

**The Code Itself**
: Code complexity grows and the bits are less well understood.

**The Process**
: Editing, recompiling, setting up the testing structures and constraints becomes more laborious.

In Emacs as we move to writing an Elisp function the process can look like:

1.  Write or edit a skeleton program
2.  Evaluate the `defun` `C-x C-e`
3.  Go to a buffer (or create the buffer)
4.  Insert the test data (copy-and-paste perhaps)
5.  Set other test conditions (like a region)
6.  Ensuring you're in the buffer (and not some other!) run the program
7.  Evaluate the results
8.  Repeat

Clunky.

It should be noted that a massive 5 steps out of 8 (3-7) highlight an inefficient workflow and fall under the topic of managing Emacs screen space effectively. The fact is that switching buffers, populating them, managing regions is the physical labor of testing in Emacs.[^fn:1]

People forget that moving your hands across different buffers just to test five lines of code kills development momentum. It's frustrating, and what is frustrating is eventually only done grudgingly or not at all.

One way developers in other languages address this problem is by using test driven development (TDD) and test harnesses.

But what to do in Elisp?

The standard hands-on learning approach doesn't seem to fit comfortably into Elisp, actually. While simple functions can be tested easily, how are the more complex ones tested? The friction seems too high.


## The Problem Exemplified {#the-problem-exemplified}

Consider `re-search-forward`. The beginning Elisp coder has just discovered it. So, he goes to the documentation.

```text
Signature
(re-search-forward REGEXP &optional BOUND NOERROR COUNT)

Documentation
Search forward from point for regular expression REGEXP.
```

He eventually finds in the manual (`35.7 Search and Replace`):

```text
If you want to find all matches for a regexp in part of the buffer and
replace them, the most flexible way is to write an explicit loop using
‘re-search-forward’ and ‘replace-match’, like this:

     (while (re-search-forward "foo[ \t]+bar" nil t)
       (replace-match "foobar"))
```

This is encouraging. It is already understood that any s-expression can be evaluated. So, in the manual, positioning the cursor and evaluating (`C-x C-e`), now Emacs responds `nil`. But why?

Of course! There is no text that matches! But where would the text be? From previous experience in a C-like language, a simplified program would be created, with necessary structures and data initialized, operated on, and the results printed to the command line.

But this while loop has no place for variables, so \\(\ldots\\) ?

Here the beginning Elisp programming must make a leap. The breadth of the leap will probably not be entire and complete, but it will be sufficient: the code operates on text in a buffer.

Now, the coder creates a buffer (`C-x b asdfasdfasdf`), and the necessary text is typed: `foo       bar`.

All done.

The programmer goes back to the s-expression, which initially was in the help manual, but has been copied into the `*scratch*` buffer. Eagerly the cursor is position and `C-x C-e` is pressed.

Still `nil`

"Oh," the programmer thinks, "I have to be in the buffer `asdfasdfasdf` because that's where the data is." And then, "But, wait, if I'm in the `asdfasdfasdf` buffer where is the s-expression? It's in `*scratch*`." And then comes the conclusion, "So, in order to evaluate the s-expression with `C-x C-e` I have to be in the `*scratch*` buffer, but in order to access the data I have to be in `asdfasdfasdf`. I can't be in both buffers at the same time!"

And the beginner has two options that he knows of. Create a skeleton function that can be called using `M-x`, or copy the expression into the buffer and run it. Creating a skeleton function sounds a lot like working in other languages and is something familiar. However, the programmer, just a beginner, has before tried to write functions and call them using `M-x` and it hasn't worked out. These functions cannot be found for some reason. There is some "magic" that is not understood yet.  Rather than face that problem it's decided to simply copy the expression to the end of the buffer with the test data.

So now the expression and the data are in the same buffer. Pressing `C-x C-e` Emacs responds `nil`.

Frustrating.

Eventually the coder realizes that he has to put the expression above the textual data. This must be remember and applied. Now, evaluating, the experiment works and `foo     bar` is replaced with `foobar`.

Those results are nice. But \\(\ldots\\)

Now, the buffer has to be reset.

Now, it would seem that the code fragments must be developed in this buffer too. But this buffer is only a text buffer and none of the comforts of an Elisp-mode buffer are available. Or the code can be developed in an Elisp buffer but then it must be copied into the data buffer. More steps.

What to do? The beginner, even though experienced in other languages, is experiencing a lot of friction. And it is not surprising that it's observed that the task would be easier in other languages, using known processes.

The above might be an exaggeration, but not by much. And many developers do not even get this far. The standard way of hands-on testing does not seem to be available and so progress is not possible.

Even once the developer learns `(interactive)` there is the problem of having data to work on, and there is the problem of switching buffers.

It is here that the beginner must make another crucial leap in understanding. The fundamental data structure in Emacs is the buffer. And at any given moment some buffer is active, or in Emacs terminology, current. The current buffer (whatever buffer that might be at the moment) is the context of any and all commands.

Now, if there only some way to use this fact.


## The Solution: The Lab is In the Code {#the-solution-the-lab-is-in-the-code}

In other languages, you write a test harness class or pull in a TDD framework. In Elisp, you don't need external infrastructure. You use an elegant, built-in language idiom: `with-temp-buffer`.

Instead of jumping out of your code, opening a scratch file, pasting your test data, selecting a region, and running your function, you bring the entire test environment **inside your scratchpad**.

Let's complete that truncated manual example right here, without ever leaving our cursor position. Put your cursor at the end of the final parenthesis below and press `C-x C-e`:

```emacs-lisp
(with-temp-buffer
  ;; 1. Inject your test data directly into the background RAM
  (insert "Fruit: apple, banana, cherry")

  ;; 2. Instantly run your experimental regex loop safely
  (goto-char (point-min))
  (while (re-search-forward "\\(\\w+\\):" nil t)
    (replace-match "Type:"))

  ;; 3. Pull the resulting string back out to verify it
  (buffer-string))
```

Evaluating and the following is displayed in the message buffer

```text
Type: apple, banana, cherry
```

And if you want to run in an Org Mode code block it would look like this:

```org
#+begin_src emacs-lisp
  (with-temp-buffer
    ;; 1. Inject your test data directly into the background RAM
    (insert "Fruit: apple, banana, cherry")

    ;; 2. Instantly run your experimental regex loop safely
    (goto-char (point-min))
    (while (re-search-forward "\\(\\w+\\):" nil t)
      (replace-match "Type:"))

    ;; 3. Pull the resulting string back out to verify it
    (buffer-string))
#+end_src

#+RESULTS:
: "Type: apple, banana, cherry"
```

Org Mode however is not the point.

Look at what just happened using `with-temp-buffer`. Emacs silently allocated a fresh buffer in RAM, populated it with your string, ran your experimental regex loop, grabbed the final transformed text, and dropped the result right in your Echo area.

This simulates steps 3 through 7 of the clunky process in **less than a millisecond**, without ever shifting  windows, losing cursor focus, or risking corrupting an actual file on your hard drive if the regex accidentally entered an infinite loop.

This isn't string manipulation. This is buffer manipulation. And in Emacs, that makes all the difference.

The work flow has now been reduced to:

1.  Create or modify the code fragment.
2.  Evaluate the fragment.
3.  Evaluate the results.
4.  Repeat.

This is a massive improvement. Quite literally all the friction has been eliminated.


## Conclusion {#conclusion}

Emacs Lisp feels different because the thing being manipulated --- text in a buffer --- seem to not be immediately available.

Granted, it's easy enough to test some functions, like `(split-string "hello world")`

Other functions operate in a buffer, like `re-search-forward`. Experimenting with them, and the inevitably more complicated code, becomes more difficult. This difficulty is friction. And it is sufficiently high to impede progress.

`with-temp-buffer` is a wrapper that effectively creates a live test environment effectively eliminating the friction typically encountered when experimenting with more complex objects and coding fragments.

For an extended demonstration of using `with-temp-buffer` in developing a small utility function see, [Building an Emacs Text-Transformation Command One Step at a Time](https://kes.github.io/posts/building-an-emacs-text-transformation-command-one-step-at-a-time/).

[^fn:1]: The Emacs real-estate problem refers to the challenge of managing screen space effectively within the editor, which can hinder productivity. When users struggle to navigate and organize their workspace efficiently, it can lead to distractions and slow down coding tasks.
