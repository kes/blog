---
title: "Building an Emacs Text-Transformation Command One Step at a Time"
author: ["Karl Stump"]
date: 2026-06-01
draft: false
math: true
---

Search and replace is a typical function when editing text documents, and regular expressions are usually involved.

Emacs provides `query-replace-regexp` out of the box (`(C-M %)`)[^fn:1].

Though `query-replace-regexp` is excellent, there are times when a more sophisticated operation is wanted.

The source code for `query-replace-regexp` and what it ultimately calls, `perform-replace`, clocks in at around 600 lines of Elisp. Looking at the doc-string, we are warned-off using `perform-replace` in our own program, and rather advised to use a simple paradigm, given as:

```elisp
(while (re-search-forward "foo[ \\t]+bar" nil t)
  (replace-match "foobar" nil nil))
```

`re-search-forward` does what you think it does: it searches forward from `point` for a match to a regular expression.

<div class="pro-tip">

**Pro Tip:** Regular Expressions are one of the most useful tools in any programmer's kit. And yet, many have never learned how they work, or the meaning of the symbols, or even worse, confuse them with shell globbing. Websites to experiment with REs are numerous. These look reasonable, among many others: [RegexPlanet.com](https://www.regexplanet.com/), [Regex101.com](https://regex101.com/), [RegExr.com](https://regexr.com/). Searching will reveal more.

</div>

The last two parameters of `re-search-forward`, `nil` and `t`, are optional, but when used in a while construct they are necessary.

-   The first sets the maximum bound, and `nil`, means no bounds, go until the end of the buffer.
-   The second controls what happens on error --- and, note,  not finding a match will generate an error ---  `t` means simply return `nil`. That works well for controlling the while loop --- when no more matches are found, it exits.

There are a number of functions that work with `re-search-forward`. These are documented as:

**(match-beginning SUBEXP)**
: Return position of start of text matched by last search.

**(match-end SUBEXP)**
: Return position of end of text matched by last search.

**(match-string NUM &amp;optional STRING)**
: Return the string of text matched by the previous search or regexp operation.

**(replace-match NEWTEXT &amp;optional FIXEDCASE LITERAL STRING SUBEXP)**
: Replace text matched by last search with NEWTEXT.

However, we might need only some of them.

Still, all of this looks a bit daunting. Fortunately, Emacs and Elisp allow an incremental approach for development of functions. We don't have know everything to just get started.


## Markdown for Footnotes {#markdown-for-footnotes}

In Org Mode[^fn:2] footnotes can be defined with the syntax `[fn:1]` which is then matched somewhere else in the document with `[fn:1][Citation]`[^fn:3]. Multiple footnotes can be specified as `[fn:1][fn:2][fn:3]`.

However, sometimes footnotes in non-Org documents are given as  `[1]` or as `[1, 2, 3]`

Bringing this text into an Org document can be frustrating since it requires making laborious changes.


## Transforming Footnotes {#transforming-footnotes}

Transforming `[1, 2, 3]` to `[fn:1][fn:2][fn:3]` should not be difficult. But the answer will not spring from the brow of Zeus fully formed. We must proceed step-by-step.

Simplifying the problem can be a good start.

If we imagine that we can strip away the square brackets, we can work on the task of transforming string: `"1, 2, 3"` --- not much a change, but a bit easier.


### split-string {#split-string}

Splitting a delimited string is easy.

```elisp
(split-string "1, 2, 3" ",")
```

Evaluating this we get a list:

```elisp
("1" " 2" " 3")
```

We have made a first step. We can generate a list from a comma delimited string.

Since we have a list, Lisp **ought** to be the right tool, for sure.


### mapconcat {#mapconcat}

mapconcat applies a  function to each element of a list and then concatenates the results as strings.

So, we need a function that takes as its input a string, and outputs a string in the correct footnote syntax.

We'll use a lambda, and in the lambda put `format` to work.

```elisp
(mapconcat (lambda (num)
             (format "[fn:%s]" num)) '("1" "2" "3"))
```

Such coding is the bread-and-butter of Lisp programming. As useful an idiom as can be found anywhere.

```elisp
"[fn:1][fn:2][fn:3]"
```

That's perfect, actually.

What's next?


### Square Brackets {#square-brackets}

We need to address the square brackets. Given "[1, 2, 3]" how do we get rid of those pesky square brackets?

We can use a regular expression to match the beginning and ending bracket, and then capture what's inside using parentheses. We know we can do this from previous experience. We have done it before. Still, we will need to experiment to get the syntax correct.

How to best experiment?

We can experiment using the `replace-regexp-in-string` function.[^fn:4]

The documentation tells us:

```text
(replace-regexp-in-string REGEXP REP STRING &optional FIXEDCASE LITERAL SUBEXP START)

Documentation
Replace all matches for REGEXP with REP in STRING.

Return a new string containing the replacements.
```

Okay. We need

1.  A regular expression.
2.  A replacement string.
3.  A string to operate on, and for that "[1, 2, 3]" will do.

In Emacs Elisp the regular expression syntax requires the use of double backslashes. This makes the syntax messy. But for a simple expressions, it's not horrible.[^fn:5]

Our regex is actually in the category of not horrible: `\\[\\([[:digit:], ]+\\)\\]`

If not familiar with regular expressions:

-   The beginning `\\[` and ending `\\]` is matching the literal opening and closing bracket of our target string.
-   The parentheses, `\\(` and `\\)`  capture the sub-expression of the matched text. An expression so captured can be accessed later.
-   `[:digit:]` means a digit and it is part of the POSIX standard. Another way to specify a digit is `0-9`. Some prefer that.[^fn:6]
-   Opening bracket `[` and closing bracket `]` specify what is called a character class, a list of characters we are seeking to match on. Within this character class we have specified a digit, a comma, and a space.
-   The plus `+` means match 1 or more of the preceding expression (in this case, the character class we just specified).

So, we now have the REGEXP filled in:

```elisp
(replace-regexp-in-string "\\[\\([[:digit:], ]+\\)\\]" REP STRING)
```

And for the second and third arguments:

-   "\\\\\\\\1" is a reference to the inner sub-expression that we captured in the regular expression with the parentheses. Thus, our replacement text will be "1, 2, 3" without the brackets.
-   "[1, 2, 3]" --- the text we want to operate on.

Now, we have:

```elisp
(replace-regexp-in-string "\\[\\([[:digit:], ]+\\)\\]" "\\1" "[1, 2, 3]")
```

Evaluating gives us:

```elisp
"1, 2, 3"
```

Looks good. We've gotten rid of the square brackets and we know we can send that string to `split-string` and get a list, and then send that list on to `mapconcat`. Things are coming together.


## Build It {#build-it}

Now that we have all the pieces we need to build our little function.

First we'll focus on the main body.

We want to run `re-search-forward` for one pass.

But how?

We want to run a program fragment against a buffer with text to be transformed. But we can't call a program fragment. So, it looks like we need a skeleton program we can at least call.

And that is one approach. But, importantly, we don't have to follow that approach either. We can, but we're not forced to do so.

Instead, just as we've been testing with various bits, we can continue to do so as we bring the pieces together and grow the program.

We want to develop our code and have it run in the context of a buffer we've filled with targets to test against. Emacs makes this very convenient with the `with-temp-buffer` wrapper.

The documentation reads:

> The ‘with-temp-buffer’ macro evaluates the BODY forms with a
> temporary buffer as the current buffer.  It saves the identity of
> the current buffer, creates a temporary buffer and makes it
> current, evaluates the BODY forms, and finally restores the
> previous current buffer while killing the temporary buffer.

So, even with our fragmentary code, let's create a temporary buffer and insert some strings to target.


## Testing the Capture Groups {#testing-the-capture-groups}


### First Test {#first-test}

```elisp
(with-temp-buffer
  (insert "Here is the first target: [1, 2, 3], second, [4, 5] and third [1, 2, 3, 4, 5, 6]")
  (goto-char (point-min))

  ;; Run the search exactly once
  (re-search-forward "\\[\\([[:digit:], ]+\\)\\]" nil t)

  ;; Return a list showing where the point is and what was caught
  (list :current-point (point)
        :matched-text  (match-string 0)
        :captured-group (match-string 1)))
```

`with-temp-buffer` creates a temp buffer, and makes it current. Now, our code is running in that buffer's context. So, we insert some data (and note, we don't specify a buffer name).

Now, we `goto-char` to the `(point-min)` of the temp buffer, run `re-search-forward` one time. Then collect some data into a list. This list is then returned.

So, evaluating we get:

```elisp
(:current-point 36 :matched-text "[1, 2, 3]" :captured-group "1, 2, 3")
```

We have assurance that we're on the right track because we can verify the results.


### Second Test {#second-test}

Let's see the match beginning and end, too. These numbers locate the match in the buffer.

```elisp
(with-temp-buffer
  (insert "Here is the first target: [1, 2, 3], second, [4, 5] and third [1, 2, 3, 4, 5, 6]")
  (goto-char (point-min))

  (re-search-forward "\\[\\([[:digit:], ]+\\)\\]" nil t)

  ;; Capture the structural boundaries of the match
  (let ((beg (match-beginning 0))
        (end (match-end 0)))
    (list :beg-index beg
          :end-index end
          :verified-text (buffer-substring beg end))))
```

Now, evaluating, the above returns:

```elisp
(:beg-index 27 :end-index 36 :verified-text "[1, 2, 3]")
```

This is what we want.

Next, we simulate a loop. Suppose we want to catch the second iteration. We can just call `re-search-forward` twice. (Not to worry. We'll do an actual loop in a bit.)


### Third Test {#third-test}

```elisp
(with-temp-buffer
  (insert  "Here is the first target: [1, 2, 3], second, [4, 5] and third [1, 2, 3, 4, 5, 6]")
  (goto-char (point-min))

  ;; First loop iteration (skipped inline here)
  (re-search-forward "\\[\\([[:digit:], ]+\\)\\]" nil t)

  ;; Second loop iteration
  (re-search-forward "\\[\\([[:digit:], ]+\\)\\]" nil t)

  (list :second-match-string (match-string 0)
        :second-capture-group (match-string 1)))
```

Evaluating returns the following:

```elisp
(:second-match-string "[4, 5]" :second-capture-group "4, 5")
```

That is what we expected.


## Testing the Transformation {#testing-the-transformation}

Now let's do the transformation.

```elisp
(with-temp-buffer
  (insert  "Here is the first target: [1, 2, 3], second, [4, 5] and third [1, 2, 3, 4, 5, 6]")
  (goto-char (point-min))

  (re-search-forward "\\[\\([[:digit:], ]+\\)\\]" nil t)
  (let*
      ((raw-numbers (match-string-no-properties 1))
       ;; Split "1, 2, 3" into a list of strings
       (num-list (split-string raw-numbers ","))
       ;; Map over the list, trim spaces, and wrap in [fn:X]
       ;; Note: string-trim overwrites match-data here!
       (new-footnotes
        (mapconcat (lambda (num)
                     (format "[fn:%s]" (string-trim num)))
                   num-list
                   "")))
    new-footnotes))
```

After `re-search-forward` we collect the inner string using `match-string-no-properties` The number 1 indicates that we want the first capture sub-expression. (You might have multiple captured expressions, all of which would be numbered. We have only one sub-expression, so we want number 1.)

Evaluating:

```elisp
"[fn:1][fn:2][fn:3]"
```

We have taken this step-by-step, and it's really paying off.

Using `with-temp-buffer` makes testing our growing code very easy.[^fn:7]


## Adding the While Loop {#adding-the-while-loop}

Now, we'll add in a while loop.

Notice that we are saving the match beginning and end. A frequent source of headaches is having your match data overwritten by another function. So, saving the match data is necessary.[^fn:8]

With the match beginning and end we can delete the matched string and then insert our transformed text.

```elisp
(with-temp-buffer
  (insert  "Here is the first target: [1, 2, 3],\nsecond, [4, 5]\nand third [1, 2, 3, 4, 5, 6]")
  (goto-char (point-min))

  (while (re-search-forward "\\[\\([[:digit:], ]+\\)\\]" nil t)
    (let* ((beg (match-beginning 0)) ;; Cache start of [1, 2, 3]
           (end (match-end 0))       ;; Cache end of [1, 2, 3]
           (raw-numbers (match-string-no-properties 1))
           ;; Split "1, 2, 3" into a list of strings
           (num-list (split-string raw-numbers ","))
           ;; Map over the list, trim spaces, and wrap in [fn:X]
           ;; Note: string-trim overwrites match-data here!
           (new-footnotes
            (mapconcat (lambda (num)
                         (format "[fn:%s]" (string-trim num)))
                       num-list
                       "")))
      ;; Safely delete the old text and insert the new text using cached positions
      (delete-region beg end)
      (insert new-footnotes)))
  (buffer-string))
```

Notice here that we are making all the modifications in temp-buffer, and then we return `(buffer-string)` which as the name implies is contents of the buffer as a string.[^fn:9]

```text
Here is the first target: [fn:1][fn:2][fn:3],
second, [fn:4][fn:5]
and third [fn:1][fn:2][fn:3][fn:4][fn:5][fn:6]
```


## Final {#final}

Now, we can finish up.[^fn:10]

```elisp
(defun my/reformat-footnotes ()
  "Transform [1, 2, 3] style footnotes into [fn:1][fn:2][fn:3]."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    ;; Search for brackets containing digits, commas, and spaces
    (while (re-search-forward "\\[\\([[:digit:], ]+\\)\\]" nil t)
      (let* ((beg (match-beginning 0)) ;; Cache start of [1, 2, 3]
             (end (match-end 0))       ;; Cache end of [1, 2, 3]
             (raw-numbers (match-string-no-properties 1))
             ;; Split "1, 2, 3" into a list of strings
             (num-list (split-string raw-numbers ","))
             ;; Map over the list, trim spaces, and wrap in [fn:X]
             ;; Note: string-trim overwrites match-data here!
             (new-footnotes
              (mapconcat (lambda (num)
                           (format "[fn:%s]" (string-trim num)))
                         num-list
                         "")))
        ;; Safely delete the old text and insert the new text using cached positions
        (delete-region beg end)
        (insert new-footnotes)))))
```


## Conclusion {#conclusion}

Textual modification is a constant necessity.

For me, I get footnotes that are not supported by Org Mode. I must modify them for them to be useful.

There might be many acceptable approaches for doing these modifications: doing it manually, writing macros, writing a shell script,  or, as demonstrated in this document, writing an Elisp function.

Developing `my/refromat-footnote` did not require us to know a 600-line general purpose replacement engine. We started with one function and set to work on a simplified problem. From there we gradually added complexity and checked the results along the way at each step. Using `with-temp-buffer` was an essential element of being able to test and grow the program.[^fn:11]

Emacs, Elisp and incremental development is a powerful and satisfying approach for building up useful and moderately complex utility functions.

[^fn:1]: A keybinding not too bad for occasional use, but in my view one that should be changed if used heavily.
[^fn:2]: See, [Org Mode](https://orgmode.org/)
[^fn:3]: In Orgmode, footnotes can also be inlined, but that is not the point here.
[^fn:4]: Is this the best way? There is also `re-builder`.
[^fn:5]: And there are other ways to specify a regex in Elisp.
[^fn:6]: I'm one that prefers it. I would rather write the character class as `[0-9, ]` but thought I would use the POSIX `[:digit:]`. Initially, this can feel more confusing because the square brackets are a part of the POSIX name, and can easily be misinterpreted. To each his own.
[^fn:7]: And I could have executed this code in a REPL, or in a buffer (using `C-x e`). For this document, I am using code-blocks in Orgmode.
[^fn:8]: There's another way to do this, using the `(save-match-data ...)` wrapper. I very much like this, too, and arguably it's more readable and lispy. My use of mapconcat and string-trim, while lispy, might be challenged. This will cause more consing and garbage collection. But for small buffers, it's completely fine.
[^fn:9]: Notice, too, that I have inserted newlines into the initial temp-buffer string to increase readability
[^fn:10]: There is more we could do, of course. However, this is fine for our immediate purposes.
[^fn:11]: This command assumes that bracketed numeric lists in the buffer are footnote references. In a document containing other bracketed numeric lists, I would narrow to a region or continue development and make the replacement interactive.
