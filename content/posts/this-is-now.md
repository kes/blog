---
title: "The C Programming Language"
author: ["Karl Stump"]
date: 2024-08-10
tags: ["C", "programming"]
draft: false
---

Some time ago, I was on a reddit thread and someone commented that they
knew python and wondered if they should learn C &#x2026;

I thought everybody knew C!   ;)

Back in the day, C was my third or fourth programming language.

With the mention of C I just had to break out my copy of _The C Programming Language_. Unfortunately,
my original copy is long gone but I have the second edition.[^fn:1]

The K&amp;R book is a classic and it's pretty easy to recommend to anyone interested in computer
science, software development, or just plain ol' programming. If you're reading this sentence, then
yes, let me personally recommend it to you &#x2014; and you should read it sooner than later!

As a language, the longer you know C the better you like it. Just it says in K&amp;R,

> In our experience, C has proven to be a pleasant, expressive, and
> versatile language for a wide variety of programs. It is easy to
> learn, and it wears will as one's experience with it grows.

Yup, I agree!

K&amp;R begins with chapter one: "A Tutorial Introduction" &#x2014; and just looking at it, I just thought it would
be cool to skim through it, look at the programs, and just soak-in some cool 70's tech literature. It doesn't get any
better! A language that is still going strong 50 years later has to have something going for it!

A `hello world` program is the canonical first program for anybody learning a new language. Was it back then? Or did K&amp;R
start it? I don't know.

But before we experience the coolness of C we'll need a compiler!

Back in the day, getting access to a compiler was not that easy. Compilers, RDBMS, OSes &#x2026;. believe
me, a closed source world was not fun! Now, it's all free, and a machine can be had for not too
much. May be we do owe a debt of gratitude to Richard Stallman!


## Installing gcc and friends {#installing-gcc-and-friends}

I've got gcc already installed, but if you don't, on Ubuntu,  you can do this:

{{< highlight bash >}}
sudo apt install gcc && sudo apt install build-essential
{{< /highlight >}}

And you should be good to go. With gcc installed you can check the version:

<a id="code-snippet--gcc-version"></a>
{{< highlight bash >}}
gcc --version
{{< /highlight >}}

And you should see something like this:

```text
gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
Copyright (C) 2023 Free Software Foundation, Inc.
This is free software; see the source for copying conditions.  There is NO
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
```

So, if everything looks good, let's write "hello world' program!


## hello world {#hello-world}

K&amp;R gives the "hello world" program on page 6. Like this:

<a id="code-snippet--hello"></a>
{{< highlight C >}}
#include <stdio.h>
main() {
  printf("hello world\n");
}
{{< /highlight >}}

I love how K&amp;R gives such an understated, unassuming, program. Not only does the string not
capitalization or punctuation, not so bad, but hey, nothing, there's no return type in the
signature! Man, this is really bare bones. No need to be too fastidious, I suppose.

I do wonder what they were thinking all those years ago when they chose this as their first example
code for C.

Were either of them sort of chuckling to themselves as they hid the fact that things were going to
get much more complicated? I like to think that they were. "Nothing to see here. Just a little
language put together by us hackers."

Yup, there's definitely some coolness here.

But wait, this may have compiled in the 70's &#x2014; but will it compile today?

<a id="code-snippet--shared-session"></a>
{{< highlight bash >}}
  exec 2>&1
#  alias cc="gcc -x c -w"
  ls
  echo PID: "$$"
{{< /highlight >}}

In fact it will.Just as K&amp;R shows, `cc hello.c`.

However, with today's compile command, `cc`, which on Linux points to `gcc` (which stands for GNU C Compiler), you'll
get warnings. Something like this:

```text
hello.c:2:1: warning: return type defaults to ‘int’ [-Wimplicit-int]
    2 | main() {
      | ^~~~
```

Notice that this is just a warning. (You still get an `a.out` file.) If you don't want to see these
warnings you can alias cc like this: alias cc="gcc -w"/. And for that matter if you want to compile
files without the `.c` extension you could alias like this: alias cc="gcc -x c -w" The `-w` flag says to
ignore warnings, and the `-x` flag says to use the C compiler. So, it's up to you. I think I'll just
leave well-enough alone and put up with seeing the warnings.

Okay, where were we? Oh, yes, compiling creates an executable file called `a.out`. You may have to
`chmod` it (`man chmod`), like this:

{{< highlight bash >}}
chmod 755 a.out
{{< /highlight >}}

And then run it:

{{< highlight bash >}}
./a.out
{{< /highlight >}}

```text
hello world
```

And here's the output:

```text
hello world
```

Everything seems to be working!


## Errors happen {#errors-happen}

Exercise 1.1 tells us to experiment with our program by deleting various parts of it. I'll remove
the `#include` line, and save the modified program to hell-error.c.

<a id="code-snippet--hello-error"></a>
{{< highlight C >}}
main() {
  printf("hello world\n");
}
{{< /highlight >}}

And now, compile it and see what happens.

{{< highlight text >}}
hello-error.c:1:1: warning: return type defaults to ‘int’ [-Wimplicit-int]
    1 | main() {
      | ^~~~
hello-error.c: In function ‘main’:
hello-error.c:2:3: warning: implicit declaration of function ‘printf’ [-Wimplicit-function-declaration]
    2 |   printf("hello world\n");
      |   ^~~~~~
hello-error.c:1:1: note: include ‘<stdio.h>’ or provide a declaration of ‘printf’
  +++ |+#include <stdio.h>
    1 | main() {
hello-error.c:2:3: warning: incompatible implicit declaration of built-in function ‘printf’ [-Wbuiltin-declaration-mismatch]
    2 |   printf("hello world\n");
      |   ^~~~~~
hello-error.c:2:3: note: include ‘<stdio.h>’ or provide a declaration of ‘printf’
{{< /highlight >}}

{{< highlight C >}}
int main() {
  printf("Hello World again\n");
}
{{< /highlight >}}

Now, I'll compile with `cc hello.c` and see what happens.

{{< highlight text >}}
test.c: In function ‘main’:
test.c:3:3: warning: implicit declaration of function ‘printf’ [-Wimplicit-function-declaration]
    3 |   printf("Hello World again\n");
      |   ^~~~~~
test.c:1:1: note: include ‘<stdio.h>’ or provide a declaration of ‘printf’
  +++ |+#include <stdio.h>
    1 |
test.c:3:3: warning: incompatible implicit declaration of built-in function ‘printf’ [-Wbuiltin-declaration-mismatch]
    3 |   printf("Hello World again\n");
      |   ^~~~~~
test.c:3:3: note: include ‘<stdio.h>’ or provide a declaration of ‘printf’
{{< /highlight >}}

Yup. Looks like some problems. But interestingly these are only warnings
and informational messages. Learning to read these messages is an
important part of gaining skill in working with C. The compiler
helpfully tells us that we have an `implicit declaration of function
'printf'`. Looking at the index of `K&R` implicit declaration is
covered on pages 27, 72, and 201.

Also, the compiler tells us that `<stdio.h>` provides a declaration of `printf`, and then has
the line `+++ | +#include <stdio.h>`. It doesn't take too much imagination that this means that we
should include this line in our source code. And, indeed, the last line of output gives us explicit
instructions to do just that.

Nevertheless, `gcc` did create an executable. We'll do an `ls` command.

<a id="code-snippet--ls-output"></a>
{{< highlight bash >}}
ls -l a.out
{{< /highlight >}}

And sure enough, there it is.

```text
-rwxrwxr-x 1 kes kes 15952 Aug 21 19:39 a.out
```

What type of file is it? Let's run the `file` command:

<a id="code-snippet--file-output"></a>
{{< highlight bash >}}
file a.out
{{< /highlight >}}

The `file` command will attempt to classify a file. See `man file` for more details. Here's the output from the command.

```text
a.out: ELF 64-bit LSB pie executable, x86-64, version 1 (SYSV), dynamically linked, interpreter /lib64/ld-linux-x86-64.so.2, BuildID[sha1]=ab17dc2646ecd587c86aa8328ff3821ca535f79f, for GNU/Linux 3.2.0, not stripped
```

Lots to try and understand there, but for now just leave it.

We could continue to remove other lines and see the compiler messages. But lets move onto the next exercise.


## Escape sequences {#escape-sequences}

This exercise draws our attention to the funny backslash-n that is in
the string we're printing out. '' is an escape sequence and it
represents the single character newline. Of course, in the output of a
string we don't see the newline character, but we do see its
results. K&amp;R want us to know that there are other such types of
characters. Namely, , , for backspace, and tab characters, and the
escape sequence \\" that allow you to embed double quotes in a double
quoted string, and even the escape sequence \\\\ that allows you to embed
a backslash in a string. You can find an [ASCII chart here](https://en.wikipedia.org/wiki/ASCII). You can experiment
with these escape sequences in the hello world program.


## Celsius to Fahrenheit Exercise 1-3 {#celsius-to-fahrenheit-exercise-1-3}

The book gives you a program to convert Fahrenheit to Celsius, and asks you to
write one that does Celsius to Fahrenheit.

Here's what I came up with:

<a id="code-snippet--c-f-code"></a>
{{< highlight C "linenos=true, linenostart=1" >}}
#include <stdio.h>

int main(){

  float fahr_correct, fahr_wrong, celsius;
  int lower, upper, step;

  lower = 0;
  upper = 300;
  step = 20;

  celsius = lower;
  printf("Celsius \t Fahrenheit Wrong\tFahrenheit Correct\n");
  while (celsius <= upper){
    fahr_wrong = 9/ 5 * celsius + 32;
    fahr_correct = (float) 9/ 5 * celsius + 32;
    printf ("%3.0f\t\t %6.1f \t\t %6.1f\n", celsius, fahr_wrong, fahr_correct);
    celsius = celsius + step;

  }

}
{{< /highlight >}}

One of the most important points to be made in this section is the difference between integer and
floating point division. In the text, K&amp;R, make a point to force floating point division by using
numeric literals 5.0 and 9.0. "We were unable to use `5/9` &#x2026; because integer division would truncate
it to zero." In fact, only one of the operands has to be floating point to force the conversion of
the remaining operand to floating point.

In my version, I have used 'casting' to force the conversion of the integers to floating
point. `(float) 9` converts the integer 9 to a float, and that forces, by language definition, the 5
to be converted to a float. The entire expression (particularly with my use of cast) raises
questions of precedence and associativity. But these are dealt with later. Also, in my version,
I am using the incorrect `5/9` to demonstrate integer division and incorrect conversion.

For an interesting read on precedence and associativity see: [this discussion](https://skeptics.stackexchange.com/questions/44625/did-the-precedence-of-operations-in-arithmetic-change-since-1917).

Anyway, here is the output.

{{< highlight text >}}
Celsius 	 Fahrenheit Wrong	Fahrenheit Correct
  0		   32.0 		   32.0
 20		   52.0 		   68.0
 40		   72.0 		  104.0
 60		   92.0 		  140.0
 80		  112.0 		  176.0
100		  132.0 		  212.0
120		  152.0 		  248.0
140		  172.0 		  284.0
160		  192.0 		  320.0
180		  212.0 		  356.0
200		  232.0 		  392.0
220		  252.0 		  428.0
240		  272.0 		  464.0
260		  292.0 		  500.0
280		  312.0 		  536.0
300		  332.0 		  572.0
{{< /highlight >}}


## File copying, an idiom and an issue of precedence {#file-copying-an-idiom-and-an-issue-of-precedence}

K&amp;R give an example C program that copies input to output. Let's save this to test.c and compile it.

{{< highlight C "linenos=true, linenostart=1" >}}
#include <stdio.h>
int main()
{
  int c;
  c=getchar();
  while (c != EOF){
    putchar(c)
      c=getchar();
  }
}
{{< /highlight >}}

<a id="code-snippet--cat-password"></a>
{{< highlight bash >}}
cat /etc/passwd | a.out | tail -5
{{< /highlight >}}

```text
jenkins:x:131:140:Jenkins,,,:/var/lib/jenkins:/bin/bash
polkitd:x:995:995:User for polkitd:/:/usr/sbin/nologin
dhcpcd:x:132:65534:DHCP Client Daemon,,,:/usr/lib/dhcpcd:/bin/false
postfix:x:133:143::/var/spool/postfix:/usr/sbin/nologin
gnome-remote-desktop:x:992:992:GNOME Remote Desktop:/var/lib/gnome-remote-desktop:/usr/sbin/nologin
```

And clearly the program works.

The first thing we should ask is what `EOF`? That is what's returned by getchar to indicate that the
end-of-file marker has been hit. Note that `EOF` is not itself the end-of-file marker.

<a id="code-snippet--get-eof"></a>
{{< highlight bash >}}
grep "#define EOF" /usr/include/stdio.h
{{< /highlight >}}

```text
#define EOF (-1)
```

So, it's defined as negative 1 &#x2013; can we see the declaration of getchar?

Of course:

<a id="code-snippet--get-char declare"></a>
{{< highlight bash >}}
grep "getchar (" -A1 -B5  /usr/include/stdio.h
{{< /highlight >}}

```text

/* Read a character from stdin.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern int getchar (void);
```

So, getchar returns an int. Which seems a bit odd. After all the name of the function is
getchar. Why does it not return a `char`. In order to indicate EOF getchar must return a value that
could never occur in a character set. Thus EOF fits the bill, and therefore, the variable c must be
an `int`.

But K&amp;R make another interesting point. The program is a bit verbose. Experienced C programmers would
write it differently, with a different idiom. Like this.

{{< highlight C "linenos=true, linenostart=1" >}}
#include <stdio.h>
int main()
{
  int c;
  c=getchar();
  while ((c = getchar()) != EOF)
    putchar(c);
}
{{< /highlight >}}

K&amp;R remind us that in C an assignment is an expression and has a value. Thus c = getchar() not only
assign a value to c, but expression itself is a value, and in this instance, the value is tested
against EOF. Note that precedence is an issue. The parentheses around c = getchar() are necessary
because the precedence of != is higher than =.


## Word counting {#word-counting}

{{< highlight C "linenos=true, linenostart=1" >}}
#include <stdio.h>
#define IN  1 /* inside a word */
#define OUT 0 /* outside a word */
int main()
{
  int c, nl, nw, nc, state;
  state = OUT;
  nl = nw = nc = 0;
  while ((c = getchar()) != EOF) {
    ++nc;
    if (c == '\n')
      ++nl;
    if (c == ' ' || c == '\n' || c == '\t')
      state = OUT;
    else if (state == OUT) {
      state = IN;
      ++nw;
    }
  }
  printf("%d %d %d\n", nl, nw, nc);
}
{{< /highlight >}}

(&#x2026; to be continued &#x2026;)

[^fn:1]: Brian W. Kernighan and Dennis M. Ritchie, <i>The C Programming Language</i>, 2nd ed (Englewood Cliffs, N.J: Prentice Hall, 1988).
