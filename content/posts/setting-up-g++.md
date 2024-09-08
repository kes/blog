---
title: "Setting up g++"
author: ["Karl Stump"]
date: 2024-09-07
tags: ["compiler", "cpp"]
draft: false
---

I want to use `std::format` in C++. If I just compile with g++ without any command line options, I get
an error &#x2013; format is not found. I'm using GCC version 13, and C++20 is supported. What gives? (GCC
supports different C++ standards, for details see: <https://gcc.gnu.org/projects/cxx-status.html>.)

(Oh, and don't be confused by the GCC version and the C++ standard &#x2013; these are different
things. You can use the GCC version 13 compiling to C++ Standard 17 (for example; or 11, or 20,
or&#x2026;). So, if someone asks, "What version of the compiler are you using," it's conceivable that
they might be asking what version of GCC, or what Standard of C++.)

By default GCC 13 compiles C++ source code according to the C++17 standard (C++17 mode is the default since
GCC 11) . And C++17 does not have support for `std::format`.

Unfortunately, the C++17 standard is set at compile time &#x2013; that is, the compiling of the GCC
compiler! That makes this standard the  builtin option.

So, if you want to use C++11 or C++20?

The typical way to switch among them is by using the `-std` flag, something like:

{{< highlight text >}}
g++ -std=c++11 foo.cpp -o foo
{{< /highlight >}}

So, of course, I can just use `-std=c+20` and it's all good. (Of course, GCC takes lots of options. See: <https://gcc.gnu.org/onlinedocs/gcc/Option-Summary.html>)

But what if you want the default to be C++11? Or, C++20? Is there nothing to be done? Do you just have to use the `-std` option?

Well, there might be a few ways to overcome this, and depending on what you're trying to do, some
will be better than others. But it turns our there's a way to control this using the GCC -`specs`
option. Spec <https://gcc.gnu.org/onlinedocs/gcc-13.3.0/gcc/Spec-Files.html>

> The spec strings built into GCC can be overridden by using the -specs= command-line switch to specify a spec file.

So, this is definitely one option. And it sounds pretty neat. So, we need a spec file, and then we
just tell GCC to use it. Cool.

Might be good to get like a starter spec file, but how?

Turns out it's not that hard!

{{< highlight bash >}}
g++ -dumpspecs
{{< /highlight >}}

Okay how do we we set the `-std` option in this file?

Well, let's grep.

{{< highlight bash >}}
g++ -dumpspecs | grep -n cc1plus
{{< /highlight >}}

```text
43:*cc1plus:
```

That's the line where we need to make our modification, so redirect to a file

{{< highlight bash >}}
g++ -dumpspecs > specs
{{< /highlight >}}

&#x2013; now, after the `*cc1plus:` line you can specify the standard, so it will look like this:

{{< highlight text >}}
*cc1plus:
%{!std*:-std=c++20}
{{< /highlight >}}

Make sure you don't have any extra blank lines. In my first go at this, I had an extra blank line
after the `-std` line, and g++ complained.

Once you've got it set, you can do this:

{{< highlight text >}}
g++ -specs=./<myspecs file> foo.cpp -o foo
{{< /highlight >}}

And you're compiling with a the C++20 standard without having to put any switches on the command
line. Very cool. Play around with this and prove it to yourself. But, yes, it works.

But wait. This gets us nowhere &#x2013; we may as well just specify `-std`.

If only we could put the specs file somwhere g++ would look for it!

Turns out, GCC does look for a spec file in a few places.

Where?

To find out, do this:

{{< highlight bash >}}
strace g++ 2>&1 | grep specs
{{< /highlight >}}

```text
access("/usr/lib/gcc/x86_64-linux-gnu/13/specs", R_OK) = -1 ENOENT (No such file or directory)
access("/usr/lib/gcc/x86_64-linux-gnu/13/../../../../x86_64-linux-gnu/lib/x86_64-linux-gnu/13/specs", R_OK) = -1 ENOENT (No such file or directory)
access("/usr/lib/gcc/x86_64-linux-gnu/13/../../../../x86_64-linux-gnu/lib/specs", R_OK) = -1 ENOENT (No such file or directory)
access("/usr/lib/gcc/x86_64-linux-gnu/specs", R_OK) = -1 ENOENT (No such file or directory)
```

And we see that GCC looks in a couple of places for a specs file. Copy your specs file to your preferred location.

And then you'll be using your preferred C++ standard by default.

Before setting the spec file:

{{< highlight bash >}}
g++ foo.cpp
{{< /highlight >}}

```text
foo.cpp:7:12: error: ‘format’ has not been declared in ‘std’
    7 | using std::format;
      |            ^~~~~~
foo.cpp: In function ‘int main()’:
foo.cpp:92:13: error: ‘format’ was not declared in this scope
   92 |     cout << format("a is: {} = {}\n", "hello", "world");
      |             ^~~~~~
```

And after setting the spec file, it just works, and with no command line options! (Look, ma! No hands!)

Excellent!

I was inspired to pursue this by this:  <https://stackoverflow.com/questions/41648978/change-default-c-standard-in-g>
