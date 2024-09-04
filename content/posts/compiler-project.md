---
title: "Compiler Project"
author: ["Karl Stump"]
date: 2024-09-03
tags: ["compiler", "cpp"]
draft: false
---

I've started writing a C compiler, and I'm using Nora Sandler's book, Writing a C
Compiler.[^fn:1] Recommended. Created github repository for the project:
<https://github.com/kes/c-compiler>.

Back in the day, I was taught compilers using the "Dragon book." (Compilers[^fn:2])

After a brief examination of Sandler's book, what I like is that while it's a "detailed guide," and
(so far at least) with plenty of technical insight, nevertheless the reader is left to his own devices.

For example, Sandler tells the reader to use whatever language he prefers for the
implementation. Granted, she offers advice &#x2014; some languages are better than others after all &#x2014;
nevertheless, the choice is the reader's. Second, when it comes to writing the code she again offers
advice, and in some cases offers pseudocode. But the burden is the reader's to actually implement,
to make choices on data representation, to decide on program organization, and to just solve
problems along the way.

This is really a great approach. In some books in a similar vein, the program code is given,
data-structures given, program organization given, and in the extreme, the reader becomes a
typist. Quite to the contrary, with Sandler's approach, the reader must make choices, even choices
with limited information. As a result the reader must be willing to make progress, to move forward,
under a "cloud of unknowing," and be willing to back-track when greater understanding comes.

The result is pretty much exactly what it feels like to be a professional software developer.

Okay, enough talk, let's do it!


## Chapter 1: A minimal compiler {#chapter-1-a-minimal-compiler}

The first goal is to write a minimal compiler. Generally, there are three steps.

1.  Preprocess the source code
2.  Compile the source code into assembler
3.  Assemble and link into an executable.

For this chapter, the compiler will have four passes. (1) It will break the source code up into _tokens_. Then
(2) the tokens will be converted into an abstract syntax tree, otherwise known as an AST. Then, (3) the
AST will be converted into assembly language (still in a data structure). Finally, (4) the actual assembly
code will be written to a file for processing by an assembler and linker.

One of the things that I like most about Sandler's book is that there's also available a test
suite. So, as various milestones are completed, these can be tested.

So, the first step is to write a lexer.


## Language, Project Structure, etc {#language-project-structure-etc}

So, actually getting started I've had to make some choices.

First, I'm definitely using C++.

Second, the directory lay out is going to be something like this:

{{< highlight text >}}
.
├── build
├── CMakeLists.txt
├── external
├── src
└── tests
{{< /highlight >}}

You can see the CMakeLists.txt file, so, third I'm using CMake for build and test.  I'm using CTest
and Google Test, and of course, Sandler's own tests.

What else? Oh, editors. Sigh. For editing, I'm splitting time between emacs, and VS Code. The reason
is that I don't have LSP (Language Server Protocol) setup on emacs &#x2013; well, that's not true. I don't
have it completely setup. It starts the LSP server, connects, and gives advice, intellisense &#x2014;
which is all great, but it doesn't work and look like I want. I declared emacs init file bankruptcy
a while back and have been slowly building my configuration, and the last thing I want to do is just
dump stuff into my .init. The fact is that I've got too much invested into emacs to just forgo it
altogether. I create my blog posts (like this one) using emacs and ox-hugo, I use org-roam daily,
and I like programming in Lisp. So, emacs will be there. But VS Code just pretty much works out of
the box, very little configuration, and C++ and CMake intelli-sense just works, and git too. So,
wanting to focus on the compiler project, focusing on C++, and CMake, and Google test, I decided not
spend time focusing on my editor. Alright, let's not get hung up on the editor.


## Lexer {#lexer}

The first task is to create a lexer that creates tokens from a simple C program. Here's the simple C program:

{{< highlight C >}}
int main(void){
  return 2;
}
{{< /highlight >}}

And the lexer that I've written outputs the following.

{{< highlight text >}}
KEYWORD:INT
IDENTIFIER:main
OPENPAREN
KEYWORD:VOID
CLOSEPAREN
OPENBRACE
KEYWORD:RETURN
CONSTANT:2
SEMICOLON
CLOSEBRACE
{{< /highlight >}}

The idea of the lexer is to search from the start of the string for a token. Tokens are identified using
regular expressions. I've got ten (or so)  defined, and here are three of them, for an example.

{{< highlight text >}}
regex identifierRegex("^[a-zA-Z_]\\w*\\b");
regex constantRegex("^[0-9]+\\b");
regex intKeywordRegex("^int\\b");
{{< /highlight >}}

So, here is the lexer testing the string against a regular expression. And if success, then a token is returned.

{{< highlight text >}}
regex_search(raw, m, voidKeywordRegex);
if (!m.empty()){
  t.returnCode = SUCCESS;
  t.name = "KEYWORD:VOID";
  t.value = "";
  t.remaining = m.suffix();
  return t;
 }
{{< /highlight >}}

I've broken up the lexer into two essential functions. The first is the lexer. The lexer is the
entry point for the compiler to get the tokens from the program string. The lexer's job is to call
the tokenizer (which, actually, is the code seen above), and then to package up all the tokens and
return them to the compiler. The tokenizer returns only one token at a time from the program
string. So, the lexer keeps calling the tokenizer with smaller and smaller strings.


## Running Sandler's Test {#running-sandler-s-test}

On the first run of Sandler's tests for the lexer, I scored 20/24 &#x2013; not too bad.

{{< highlight text >}}
Ran 24 tests in 0.100s

FAILED (failures=4)
{{< /highlight >}}

I was missing handling comments in the tokenizer &#x2013; so I added two regular expressions for comments (two types of comments).

You might think it difficult to handle multi-line comments with the tokenizer returning only one token at a time. But
no that's not the case. The tokenizer gets the full program string. Finding a multi-line comment becomes is quite easy. In
fact the tokenizer returns a comment token. But the lexer throws away comment tokens.

Rerunning tests and 100% pass.

Created github repository: <https://github.com/kes/c-compiler> and have pushed the lexer code.

Fun!

[^fn:1]: Nora Sandler, <i>Writing a C Compiler: Build a Real Programming Language from Scratch</i> (San Francisco, CA: No Starch Press, 2024).
[^fn:2]: Alfred V. Aho et al., eds., <i>Compilers: Principles, Techniques, &#38; Tools</i>, 2. ed., Pearson internat. ed (Boston Munich: Pearson Addison-Wesley, 2007).
