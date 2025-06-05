---
title: "Computational Documents"
author: ["Karl Stump"]
date: 2024-08-04
tags: ["literate", "programming", "org", "babel"]
draft: false
---

Not all the documents that I create are computational &#x2014; but when they are I use
[Emacs](https://www.gnu.org/software/emacs/), [Org Mode](https://orgmode.org/), and [Babel](https://orgmode.org/worg/org-contrib/babel/)

Computational documents are a mixture of text and executable code. And the process of creating them,
bringing together the text and code is called "weaving." But sometimes, you want to separate the
text and code. This is called "tangling." For more see, [extracting source code](https://orgmode.org/manual/Extracting-Source-Code.html).

This turns out that tangling is pretty useful when you want to maintain one source of code for
display and for processing, maintaining the DRY principle


## Tangle {#tangle}

Lots of Emacs users write `init` files in Org Mode, using code blocks. The idea is that in an Org Mode
file initialization code can be organized, along with literate comments. Later, at an appropriate
moment the actual initialization code is extracted and executed.

I suppose it might be asked if code comments aren't already mixed in with code. And, yes, that's true,
but literate code can be executed, and it's ready to be turned into a printable and readable document.

Here's an example from my Org Mode configuration file &#x2013; you can see that text and code is mixed
together.

{{< figure src="/ox-hugo/configuration.png" >}}

And then, when tangled the code is extracted:

{{< figure src="/ox-hugo/untangled-config.png" >}}


## The Tangle Solution {#the-tangle-solution}

I found myself needing to tangle a code block in a computational document that I was creating.

That is, I needed to save the code in the block to file.

Why did I need to do this?

It gets down to **DRY**.

Let me explain.

Suppose I have code that I want to display, and so I make a code block like this:

{{< highlight org >}}
#+NAME: hello-example
#+begin_src C   :main no :eval never :exports code
  #include <stdio.h>
  int main() {
    printf("hello world\n");
  }
#+end_src
{{< /highlight >}}

I don't want to execute this code at this point in my document. And I don't want to capture its
output.

I mean I could evaluate it.  Org Mode and Org Babel are able to do that, and in that case I'd get
the expected output, "hello world" &#x2013; but that's not the point of displaying the code.

By the way, here's what displaying the displayed code will look like.

<a id="code-snippet--hello-example"></a>
{{< highlight C >}}
#include <stdio.h>
int main() {
  printf("hello world\n");
}
{{< /highlight >}}

And I'll have text, lots of text, before the code, after the code &#x2014; sorta like now, in fact.

So, the point of the code block is simply to display the code.

Later, I'll want to compile the code. And, then, I'll show my readers how to do that with another
code block. And then later, I'll want to execute the executable by invoking the `./a.out` file.

So, pretty clear, I want to show the code, then compile the code, then execute the code.

That sounds like three code blocks to me. The first has already been shown, while the third is just
a bash code block that invokes the `./a.out`. So, really, with the first and third code blocks out of
the way, I just need to create the second one.

And, yeah, I mean it sounds easily enough, and it is easy&#x2026;. except &#x2026;.

Well, I need to _compile_ the _code_.

And that means something like `cc <filename>`. And that can easily be done in a bash code
block. What's the problem?

The problem is, where's `<filename>` and what's in it?

Well, `<filename>` must be on the disk somewhere! Pick someplace, create the file, and put the code
into it. Problem solved!

But no, it's not solved.

Oh, sure, I _can_ create `<filename>` outside the document, and yes, I _can_ create a
code block that runs `cc <filename>` &#x2013; all of that can clearly be done. But what happens when the
code in the first code block changes. You know, when someone decides to update it, you know, to
something like, `Howdy World! How ya doin?'` And then they'd have to remember to change the file on
the disk, which they'll either forget about, or won't know about at all &#x2013; and even if you do know,
and if you do remember, it's just a pain to have to make code changes in two place &#x2014; and oh, yeah,
by the way, we've just lost DRY.

Okay, okay, let's think this through.

My first idea, was to put the file creation in a code block! And while, I'm thinking how brilliant this is, and that
I'm clearly in the presence of genius it begins to occur to me that I have to have a way to create
the file and its contents. No problem I think. I'll just echo string with file redirection. It
doesn't take long to realize that this is a stupid idea. Not only is painful to echo those strings
correctly, it does nothing to address the problem of having code in two places. Now, if you change
the code displayed, you also have to change the echoed text. We are still not DRY.

We're stuck!

Nah! Tangling makes this problem trivial. Look at this code block. It's still both the code that we
want to display in the document, and that we want saved for later processing. But, I've added a
`tangle` header argument.

{{< highlight org >}}
#+NAME: hello-example
#+begin_src C   :main no :tangle computational-documents/hello.c  :eval never :exports code
  #include <stdio.h>
  int main() {
    printf("hello world\n");
  }
#+end_src
{{< /highlight >}}

The tangle header says that this code can be extracted.

All we need to do is run `(org-babel-tangle)` on the code block. The documentation explains
very well what this does:

> Write code blocks to source-specific files.  Extract the bodies of all source code blocks from the
> current file into their own source-specific files.

And where will the code be written? The file is show in the tangle header. `:tangle computational-documents/hello.c`

Given that the block has now been tangled, I can now run the code block that compiles the file, and the code block that executes it.

Here are the code blocks:

{{< highlight org >}}
#+NAME: compile-example
#+begin_src bash :results none :exports both :dir ./computational-documents
  cc hello.c
#+end_src

#+NAME: run-example
#+begin_src bash :results output verbatim :exports both :dir ./computational-documents
  ./a.out
#+end_src

#+RESULTS: run-example
: hello world
{{< /highlight >}}

Which will display, with output, like this:

<a id="code-snippet--compile-example"></a>
{{< highlight bash >}}
cc hello.c
{{< /highlight >}}

<a id="code-snippet--run-example"></a>
{{< highlight bash >}}
./a.out
{{< /highlight >}}

```text
what test Now? hello world -- now, test the tangle!No, really, now!
```

Easy-peasy!
