---
title: "Making TOML Obvious by Turning It Into JSON"
author: ["Karl Stump"]
date: 2026-05-01
draft: false
math: true
---

## The Obvious {#the-obvious}

I was looking at some `TOML` --- [Tom's Obvious Minimum Language](https://toml.io/en/) --- in a [Hugo](https://gohugo.io/) config file, pondering the configuration of a static site.

{{< figure src="/ox-hugo/toml-config.png" class="my-screenshot" >}}

In Hugo, you have the choice between `YAML` --- [YAML Ain't Markup Language](https://yaml.org/) --- and TOML.

YAML is used heavily in DevOps[^fn:1] --- while it's claimed that TOML is easier to understand for smaller configs.[^fn:2]

I say "claimed," but nothing about it seemed obvious to me. I really wanted to see some equivalent `JSON`  --- [JSON](https://www.json.org/json-en.html), now, that I understand, because as they say:

> It is easy for humans to read and write.[^fn:3]

Indeed, hashes and arrays, nested however and wherever, and it even uses square brackets and curly braces, to the delight of any [perl](https://www.perl.org/) programmer, the reference to which should inform you as to how I spent my tender youth.

{{< figure src="https://imgs.xkcd.com/comics/lisp.jpg" class="my-screenshot" >}}

Where was I?

Oh, yes, what I wanted was a translator. Something that would take the obviously not so obvious TOML and convert it into JSON --- just so I could get a basic sense of structure.

Admittedly, I could have read the documentation, but what fun is there in that? I wanted to code something and experiment!

Fortunately,  I have spent a good deal of time in `Emacs` --- pondering the meaning of [lambda](https://web.archive.org/web/20171010235747/http://repository.readscheme.org/ftp/papers/ai-lab-pubs/AIM-353.pdf), wondering if it really is [turtles all the way down](https://www.youtube.com/watch?v=k_4rLyqQeAA) --- 🐢 🐢 🐢 --- and so, a REPL[^fn:4] is always nearby (`M-x ielm`).

But that aside, there must be a library.


## Parsing TOML {#parsing-toml}

And, yes, there is: [tomlparse.el](https://github.com/johannes-mueller/tomlparse.el).

If you read a bit about it, you'll find that as of `Emacs 29+`, `tomlparse` uses `tree-siter`.

[Tree-sitter](https://tree-sitter.github.io/tree-sitter/)[^fn:5] is an incremental parsing library that gives Emacs[^fn:6] a deep, structural understanding of the code[^fn:7]. Instead of relying on rigid, error-prone regular expressions, it instantly builds a complete Abstract Syntax Tree (AST), even as you type.

Very cool.

So, `emacs 29+`, eh?

At the time of writing, the [latest release](https://www.gnu.org/software/emacs/) of Emacs is 30.2.

`M-x emacs-version` tells me:

```text { class="my-example-10" }
GNU Emacs 32.0.50 (build 4, x86_64-pc-linux-gnu,
  GTK+ Version 3.24.41, cairo version 1.18.0) of 2026-05-14.
```

Built it a few days ago, so, I'm good to go. Time to update my init file.

This is good enough:

```emacs-lisp { class="my-code" }
(use-package tomlparse
  :ensure t
  :init
  (add-to-list 'treesit-language-source-alist
               '(toml "https://github.com/tree-sitter/tree-sitter-toml")))
```

Now, I didn't want to do much. Just see some JSON equivalence for a bit of TOML. Even the TOML documention does that. For example in the official documentation, discussing an [array of tables](https://toml.io/en/v1.1.0#array-of-tables) we read:

> The last syntax that has not yet been described allows writing arrays of tables. These can be expressed by using a header with a name in double brackets. The first instance of that header defines the array and its first table element, and each subsequent instance creates and defines a new table element in that array. The tables are inserted into the array in the order encountered.

And here is the example given,

```TOML { class="my-code" }
[[product]]
name = "Hammer"
sku = 738594937

[[product]]  # empty table within the array

[[product]]
name = "Nail"
sku = 284758393

color = "gray"
```

And to explain it, "JSON land" makes all clear:

```json { class="my-code" }
{
  "product": [
    { "name": "Hammer", "sku": 738594937 },
    {},
    { "name": "Nail", "sku": 284758393, "color": "gray" }
  ]
}
```

And from the same documentation we learn:

> Any reference to an array of tables points to the most recently defined table element of the array. This allows you to define sub-tables, and even sub-arrays of tables, inside the most recent table.

And, we are shown this:

```TOML { class="my-code" }
[[fruits]]
name = "apple"

[fruits.physical]  # subtable
color = "red"
shape = "round"

[[fruits.varieties]]  # nested array of tables
name = "red delicious"

[[fruits.varieties]]
name = "granny smith"

[[fruits]]
name = "banana"

[[fruits.varieties]]
name = "plantain"
```

But it's explained using JSON:

```json { class="my-code" }
{
  "fruits": [
    {
      "name": "apple",
      "physical": {
        "color": "red",
        "shape": "round"
      },
      "varieties": [{ "name": "red delicious" }, { "name": "granny smith" }]
    },
    {
      "name": "banana",
      "varieties": [{ "name": "plantain" }]
    }
  ]
}
```

So, if the documentation uses JSON clarity to show TOML meaning, then why can't I? Perfectly reasonable, then, to want to translate TOML into JSON

Back to `tomlparse.el`. It provides `tomlparse-string`. The docstring reveals:

```text { class="my-example-10" }
Signature
(tomlparse-string STRING &rest ARGS)

Documentation

Return a hash table with the contents of the toml data STRING.
```

Of course, the source code is exposed if we want to investigate:

{{< figure src="/ox-hugo/tomlparse-string-source.png" class="my-screenshot" >}}

That's great. We pass in a TOML string and we get back a hash object. But now that needs to be converted it to JSON --- for that `json-serialize` is the answer.

{{< figure src="/ox-hugo/json-serialize.png" class="my-screenshot" >}}

Interestingly, this is defined in `json.c` so it's native in emacs and not a call into an `.elc`.

Here's a bit of the code:

```c { class="my-code" }
static void
json_serialize (json_out_t *jo, Lisp_Object object,
                ptrdiff_t nargs, Lisp_Object *args)
{
  jo->maxdepth = 50;
  jo->size = 0;
  jo->capacity = 0;
  jo->chars_delta = 0;
  jo->buf = NULL;
  jo->ss_table = NULL;
  jo->conf.object_type = json_object_hashtable;
  jo->conf.array_type = json_array_array;
  jo->conf.null_object = QCnull;
  jo->conf.false_object = QCfalse;

  json_parse_args (nargs, args, &jo->conf, false);
  record_unwind_protect_ptr (cleanup_json_out, jo);

  /* Make float conversion independent of float-output-format.  */
  if (!NILP (Vfloat_output_format))
    specbind (Qfloat_output_format, Qnil);

  json_out_something (jo, object);
}
```

I guess it's not turtles all the down! 🐢 🐢 🐢


## First Pass {#first-pass}

So, the little bit of Lisp I'm thinking about looks something like this:

```elisp { class="my-code" }
(defun kes/toml2json ()
  "Convert TOML from clipboard to pretty JSON and put result back in clipboard."
  (interactive)
  (let* ((toml-data (tomlparse-string (current-kill 0)))
         (json-content (json-serialize toml-data))
         ....
         ....
         ....more turtles....)))
```

It's simple.

Get the TOML in the kill buffer, then call `kes/toml2json`, which itself only has to make two calls:

tomlparse-string
: this returns a hash object based on the TOML string

json-serialize
: this returns a string of JSON

The only problem is we want our JSON to look pretty.

There is a shell utility --- `jq` --- that does that, and lots more.

{{< figure src="/ox-hugo/jq-man.png" class="my-screenshot" >}}

It would be a shame not to use it.

So, we just make a call to the shell from Lisp using `shell-command-to-string` --- looking at the docstring, it warns us to prefer alternatives, like `call-process` and friends --- this is basic security --- however, in this case, I'm not worried --- the docstring helpfully warns us to use `shell-quote-argument` "to quote dangerous characters" --- and I think that's a good idea too!

So, boldly,

```elisp { class="my-code" }
(defun kes/toml2json ()
  "Convert toml from clipboard to json put result back in clipboard."
  (interactive)
  (let* ((toml (tomlparse-string (current-kill 0)))
         (json-content (json-serialize toml))
         (pretty-json
          (shell-command-to-string
           (format "echo %s | jq ." (shell-quote-argument json-content)))))
    (kill-new pretty-json)
    (message "✅ TOML  → JSON converted and copied to clipboard!")))
```

And it works!

Putting the TOML into the kill-ring --- `Ctrl-c` for many, in Emacs it's `kill-ring-save` which is bound to `M-w`.

Then, invoking the function `M-x kes/toml2json`, and then paste, and you've got the JSON in the buffer. Cool!

This works because `shell-quote-argument` keeps the serialized JSON as one safe shell argument. Still, `echo` is not ideal for arbitrary data.

This version is fine as a first pass.


## A Second Pass {#a-second-pass}

However, we can do a bit better.

Like this:

```elisp { class="my-code" }
(defun kes/toml2json ()
  "Convert TOML from clipboard to pretty JSON and put result back in clipboard."
  (interactive)
  (let* ((toml-data (tomlparse-string (current-kill 0)))
         (json-content (json-serialize toml-data))
         (pretty-json
          (with-temp-buffer
            (insert json-content)
            (call-process-region (point-min) (point-max)
                                 "jq"
                                 t t nil
                                 ".")
            (buffer-string))))
    (kill-new pretty-json)
    (message "✅ TOML → JSON converted and copied to clipboard!")))
```

Slightly more advanced, but only very slightly. (But the Emacs Lisp idiom? This is gold. Read on!)

Now, using the wrapper `with-temp-buffer` and inserting the JSON, opens up a lot of flexibility and plays to strength of buffers in Emacs.

It's a fundamental technique in Emacs Lisp programming:

1.  Get the data you want to work on
2.  Then using a `with-temp-buffer` wrapper, insert the data into the buffer
3.  Now, process to your needs
4.  When finished, just collect the temp-buffer contents, and be on your way

This is the part worth remembering even if you never convert TOML to JSON.

We also use `call-process-region` and eliminated `shell-quote-argument`, too.

Wins all around.[^fn:8]


## Demo {#demo}

Here's the buffer --- I've copied the buffer text and it's in the clipboard. At the bottom you can see that I'm calling `kes/toml2json`.

{{< figure src="/ox-hugo/demo-1.png" class="my-screenshot" >}}

Pressing return has shown the message I was expecting.

{{< figure src="/ox-hugo/demo-2.png" class="my-screenshot" >}}

Now, I paste the contents of the clipboard into the buffer --- and it's the JSON representation of the TOML.

{{< figure src="/ox-hugo/demo-3.png" class="my-screenshot" >}}

Neat!


## What I Learned {#what-i-learned}

This little function clarified a few things for me.

-   `tomlparse-string` turns TOML into an Emacs hash table.
-   `json-serialize` turns that Lisp object into JSON.
-   `jq` is a wonderful little tool for pretty-printing JSON.
-   `with-temp-buffer` plus `call-process-region` is cleaner, and more idiomatic Emacs Lisp, than going straight through the shell.

Converting one format into another is sometimes the quickest way to see the structure hiding underneath the syntax.

🐢 🐢 🐢

[^fn:1]: For tools like Kubernetes, Docker Compose, and Terraform. See, <https://yaml.org/about/>
[^fn:2]: The truth is that they all, TOML, YAML, and JSON, all claim to be easy to read. Me? I keep wondering what was wrong with the s-expression.
[^fn:3]: See,[JSON is easy](https://www.json.org/json-en.html)
[^fn:4]: Fun fact: L. Peter Deutsch, at age 17, implemented the first truly interactive REPL on the PDP-1, allowing real-time interaction via a Teletype. See, [The LISP Implementation for the PDP-1 Computer](https://s3data.computerhistory.org/pdp-1/DEC.pdp_1.1964.102650371.pdf)
[^fn:5]: Max Brunsfeld's presentation at Strange Loop: [Strange Loop](https://www.thestrangeloop.com/2018/tree-sitter---a-new-parsing-system-for-programming-tools.html).
[^fn:6]: Just to be clear, Tree-sitter is not exclusive to emacs, rather emacs now has built-in support for it.
[^fn:7]: About which, see, [Explore ASTs in Emacs](https://dev.to/rajasegar/exploring-asts-in-emacs-with-tree-sitter-fg1).
[^fn:8]: This assumes that `jq` is installed. But what if it's not? I'll leave it as an exercise for the reader. Hint: we can test for an executable with `(executible-find <some-execuitble>)`, and perhaps minimally wrap in an `(unless ...)`. Easy peasy! Go for it!
