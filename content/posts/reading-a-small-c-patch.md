---
title: "Reading a Small C Patch: confstr, Buffers, and a Very-Unlikely Leak"
author: ["Karl Stump"]
date: 2026-05-26
draft: false
math: true
---

I was using [Magit](https://magit.vc/) to browse the Emacs git repository.

I came across this interesting commit entitled: "Plug default_PATH memory leak"[^fn:1].

{{< figure src="/ox-hugo/git-emacs-master-log.png" width="700px" >}}

Memory leaks are a huge problem, so I was interested.

Looking at coding changes can be a wall of symbols. Even for small changes, and this one was small. Only one file was changed with 12 insertions and 9 deletions. And the Magit interface was helpfully color coding each.

{{< figure src="/ox-hugo/git-emacs-revision.png" width="700px" >}}

On first impression the obvious change was from a `while` loop to a `for` --- that feels stylistic --- but clearly there's more going on.

But the common code, the code not changed, was also there.

{{< figure src="/ox-hugo/xmalloc-common-code.png" width="700px" >}}

I felt like I wanted to see just the original code.

With git this is definitely possible.

First, find the parent of the commit. In this case, this is easily done by simply going to the previous commit. (Magit also provides a command `C-c C-n` for moving to the \\(n^{th}\\) parent.) You can see that the parent of `59b2` is `1eb2`.

{{< figure src="/ox-hugo/magit-move-to-parent.png" width="700px" >}}

Moving to that commit, then `magit-find-file` for the revision. (Pressing enter here.)

{{< figure src="/ox-hugo/magit-find-file.png" width="700px" >}}

And then we just go to the source file we want to see.

{{< figure src="/ox-hugo/find-file-in-revision.png" width="700px" >}}


## The Original Code {#the-original-code}

The entire function (unmodified) is about 40 lines long.

```c { linenos=true, linenostart=1 }
/* Return the default PATH if it can be determined, NULL otherwise.  */

static char const *
default_PATH (void)
{
  static char const *path;

  /* A static buffer big enough so that confstr is called just once
     in GNU/Linux, where the default PATH is "/bin:/usr/bin".
     If staticbuf[0], path is already initialized.  */
  static char staticbuf[16];

  if (!staticbuf[0])
    {
#ifdef _CS_PATH
      char *buf = staticbuf;
      size_t bufsize = sizeof staticbuf, s;

      /* If necessary call confstr a second time with a bigger buffer.  */
      while (bufsize < (s = confstr (_CS_PATH, buf, bufsize)))
        {
          buf = xmalloc (s);
          bufsize = s;
        }

      if (s == 0)
        {
          staticbuf[0] = 1;
          buf = NULL;
        }

      path = buf;

#elif defined DOS_NT
      /* This is not exactly what Windows does when there's no PATH (see
         documentation of CreateProcessW), but it's a good-enough
         approximation.  */
      path = strcpy (staticbuf, ".");
#endif
    }

  return path;
}
```

The code is interesting. If we look just at the top \\(\ldots\\)

```c { linenos=true, linenostart=1 }
 /* Return the default PATH if it can be determined, NULL otherwise.  */

 static char const *
 default_PATH (void)
 {
   static char const *path;

   /* A static buffer big enough so that confstr is called just once
      in GNU/Linux, where the default PATH is "/bin:/usr/bin".
      If staticbuf[0], path is already initialized.  */
   static char staticbuf[16];

   if (!staticbuf[0])
     {
      /* snip ....*/
  }
   return path;
}
```

It tells us exactly what it's doing, returning the default path.

And the idea that if it's once initialized then it doesn't need to be initialized again, explains why `path` is a static. It persists.

There's also the `static char staticbuff[16]` those 16 bytes are allocated at compile time, and its size cannot change. Static variables are also initialized automatically. This explains why the array `staticbuff` can be declared and then tested immediately. A size of 16 is interesting. It must be thought to be fine.

The first time this function is called the expression `!staticbuf[0]` will be true. Let's see what happens in the body of the code. (I've removed the C preprocessor directive.)

```c { linenos=true, linenostart=1 }
if (!staticbuf[0])
  {
    char *buf = staticbuf;
    size_t bufsize = sizeof staticbuf, s;

    /* If necessary call confstr a second time with a bigger buffer.  */
    while (bufsize < (s = confstr (_CS_PATH, buf, bufsize)))
      {
        buf = xmalloc (s);
        bufsize = s;
      }

    if (s == 0)
      {
        staticbuf[0] = 1;
        buf = NULL;
      }

    path = buf;
  }
return path;
```

Before we even enter the body of the loop, we call `confstr` passing in the address of `staticbuf`, which `buf` has been initialized to. We also pass the size of `staticbuf`, which is 16, which `bufsize` has been initialized to.

The expression `(s = confstr (_CS_PATH, buf, bufsize))` sets the value of `s` to the return value of confstr, and this value is tested against `bufsize`. We conclude that `confstr` is returning a buffer size (which is string length + 1 for the terminator).

So, logically we have:

```c
s = confstr (_CS_PATH, buf, bufsize);
while (bufsize < s){
  /* must need a larger buffer! */
  /* increase buf and bufsize! (somehow) */
  /* ... */
  /* try again! */
  s = confstr (_CS_PATH, buf, bufsize);
 }
```


## What is confstr? {#what-is-confstr}

{{< figure src="/ox-hugo/man-page-confstr-1.png" width="800px" >}}

The man pages says:

```text
confstr() gets the value of configuration-dependent string variables.
```

And it gives a signature:

```text
size_t confstr(int name, char buf[.size], size_t size);
```

And further:

```text
_CS_PATH
   A value for the PATH variable which indicates where all the POSIX.2 standard
   utilities can be found.
```

And also,

```text
If  buf  is  not NULL and size is not zero, confstr() copies the value of the
string to buf truncated to size - 1 bytes if necessary, with a null byte ('\0')
as terminator.  This can be detected by comparing the return value of confstr()
against size.
```

Oh. It copies the path into the buffer. That's easy enough.

Let's test it.

I wrote the following, a simple call to confstr and printing the results.

```c { linenos=true, linenostart=1 }
#include <stdio.h>
#include <unistd.h>
void main (){
  static char buffer[16];
  size_t buffsize = sizeof buffer;
  size_t n;

  n = confstr(_CS_PATH, buffer, (size_t) buffsize);

  printf("buffsize = %ld\nbuffer = %s\n", buffsize, buffer);
  printf("confstr returned: n=%ld\n", n);
  if (buffsize < n)
    printf("uh-oh!! --- buffer too small!\n");
  else
    printf("Yea!! Buffer big enough!\n");
}
```

Nothing complicated here. But I was curious. I really didn't know if 16 bytes would be big enough for the buffer. Only one way to find out.

So, running.

{{< figure src="/ox-hugo/confstr-test-1.png" width="700px" >}}

Okay, 16 bytes is big enough. Let's make the buffer too small.

{{< figure src="/ox-hugo/confstr-test-2.png" width="700px" >}}

This is very helpful. It demonstrates that `confstr` returns the size needed but also returns what it can an truncates the rest.

But that raises the question, what do you do if the static buffer is too small?

The man page has example code that gets the size needed from confstr, and then allocates the memory. Like this:

```c { linenos=true, linenostart=1 }
char *pathbuf;
size_t n;

n = confstr(_CS_PATH, NULL, (size_t) 0);
pathbuf = malloc(n);
if (pathbuf == NULL)
  abort();
confstr(_CS_PATH, pathbuf, n);
```

The man page example doesn't even bother trying a static buffer with a predefined size first. It simply goes straight to `malloc`.

Let's try it.

```c { linenos=true, linenostart=1 }
#include <stdio.h>
#include <unistd.h>
#include<stdlib.h>
#include<string.h>
void main (){

  char *buffer;
  size_t n, m;

  n = confstr(_CS_PATH, NULL, (size_t) 0);
  buffer = malloc(n);
  if (buffer == NULL)
    abort();
  m = confstr(_CS_PATH, buffer, n);

  printf("buffer size = %ld\nbuffer = %s\n", strlen(buffer) + 1, buffer);
  printf("first call confstr returned: n=%ld\n", n);
  printf("second call confstr returned: m=%ld\n", m);
  if (strlen(buffer) + 1 < n)
    printf("uh-oh!! --- buffer too small!\n");
  else
    printf("Yea!! Buffer big enough!\n");
}
```

{{< figure src="/ox-hugo/confstr-man-code-3.png" width="700px" >}}

Okay. So, we know what confstr does.

But wait, we could try a static size first. Just like the source code in Emacs, and then if the size is not big enough, do the malloc.

Sure, why not?

Let's try it!

```c { linenos=true, linenostart=1 }
#include <stdio.h>
#include <unistd.h>
#include<stdlib.h>
#include<string.h>
void main (){

  char *path;
  static char staticbuf [10];
  char *buf = staticbuf;
  size_t bufsize = sizeof staticbuf, s;

  while (bufsize < (s = confstr (_CS_PATH, buf, bufsize)))
    {
      printf("bufsize = %ld too small!\nNeeds to be: %ld\n", bufsize, s);
      buf = malloc(s);
      bufsize = s;
    }
  if (s == 0)
    buf = NULL;

    path = buf;

  printf("buffer size = %ld\nbuffer = %s\n", strlen(path) + 1, path);
  printf("confstr returned: s=%ld\n", s);
  }
```

And when we set the static buffer to 16:

{{< figure src="/ox-hugo/confstr-like-emacs-big-enough.png" width="700px" >}}

And when the static buffer is too small:

{{< figure src="/ox-hugo/confstr-like-emacs-too-small.png" width="700px" >}}

Okay, that's  `confstr`.

Back to the code!


## The Issue {#the-issue}

```c { linenos=true, linenostart=1 }
/* If necessary call confstr a second time with a bigger buffer.  */
while (bufsize < (s = confstr (_CS_PATH, buf, bufsize)))
  {
    buf = xmalloc (s);
    bufsize = s;
  }
/* snip */
path = buf;
```

So, the first time we come into the function it starts by trying to use the static `staticbuf`. Because it is already allocated, there's no additional overhead of allocating memory. For some large percentage of users, the system path probably fits in this initial buffer.

But it **could** be too small.

If `confstr` says, “This system path is too large for your fixed buffer,” the code calls `xmalloc` and grabs a larger buffer from the heap at runtime.

But then comes the question: why is this a loop?

One answer is that the required size might change between calls. `confstr` may tell us how much space is needed, we allocate that much space, and then by the time we call `confstr` again, the required size may no longer be the same. That is probably rare. But the loop exists precisely to handle that possibility.

And this is where the logic gets interesting.

If there were no possibility that the required size could change, we would not need a loop. We would try the fixed buffer, and if that failed, allocate once and be done.

But the code is a loop. That means the code is already admitting the possibility that **one dynamic allocation might not be enough**. It might have to try again.

So a robust version has to reason this way:

1.  Try the fixed buffer.
2.  If it is too small, allocate a heap buffer of the required size.
3.  Call `confstr` again.
4.  If the new buffer is still too small, allocate again.
5.  But before replacing the old heap buffer, **free it**.

That last step is **the whole bug**.


## The Fix {#the-fix}

Focusing in just on the body of the loop in the commit, we see:

```c { linenos=true, linenostart=1 }
/* If necessary call confstr again with a bigger buffer.  */
for (size_t s;
     ! (s = confstr (_CS_PATH, buf, bufsize)) || bufsize < s; )
  {
    if (buf != staticbuf)
      xfree (buf);
    if (!s)
      {
        staticbuf[0] = 1;
        buf = NULL;
        break;
      }
    buf = xmalloc (s);
    bufsize = s;
  }

path = buf;
```

The old code handled the retry logic, but not the ownership logic. It allowed `buf` to be replaced by a new allocation without freeing the previous allocation.

In other words, the **control flow** said, “This may loop more than once.” But the **memory management** said, “This will only allocate once.”

Those two assumptions contradict each other.

That is why the patch adds:

```c
if (buf != staticbuf)
  xfree (buf);
```

The fixed buffer, `staticbuf`, cannot be freed (it's static). But once `buf` points to heap memory from `xmalloc`, that memory has to be freed before `buf` is overwritten or abandoned.

The leak is very unlikely, but the reasoning is not trivial. The bug lives in the gap between “this probably only happens once” and “the code is written as a loop because it might happen more than once.”


## Lessons Learned. {#lessons-learned-dot}

The patch is small, but the reasoning is not.

Serious code review is not just reading syntax.

It means slowing down and thinking through the logic.

This can be difficult when faced with an unfamiliar wall of code.

I was looking at the Emacs log and stumbled upon an interesting commit. It was not immediately obvious what was going on. In these cases you have to make a decision.

Pursue or not?

Navigating commits is easy. Finding the code is easy. Working through the code and understanding the logic is where the challenge lies.

I'm glad I took the time.

[^fn:1]: See, Emacs code base, Git log:  59b2f8f1dc4 \* Plug default_PATH memory leak
