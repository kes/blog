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

The source code for `query-replace-regexp`, which ultimately calls `perform-replace` clocks in at around 600 lines of Elisp. Looking at the doc-string, we are warned off using `perform-replace` in our own program, and are rather advised to use a simple paradigm:

```elisp
(while (re-search-forward "foo[ \\t]+bar" nil t)
  (replace-match "foobar" nil nil))
```

The last two parameters of `re-search-forward`, `nil` and `t`, are optional, but when used in a while construct necessary.

-   The first of which sets the maximum bound, and `nil`, means no bounds, go until the end of the buffer.
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

However, we might only need some of them.

Still, all of this looks a bit daunting. Fortunately, Emacs and Elisp allow an incremental approach for development of commands. We don't have know everything to just get started.


## Markdown for Footnotes {#markdown-for-footnotes}

In Orgmode footnotes can be defined with the Org syntax of `[fn:1]` which is then matched somewhere else in the document with `[fn:1][Citation]`[^fn:2]. Multiple footnotes can be specified as `[fn:1][fn:2][fn:3]`.

However, sometimes footnotes in non-Org documents is given as  `[1]` or as `[1, 2, 3]`

Bring such text into an Org document can be frustrating for the Org user since it requires making laborious changes.


## Transforming Footnotes {#transforming-footnotes}

Certainly, transforming `[1, 2, 3]` to `[fn:1][fn:2][fn:3]` should not be difficult. But the answer will not spring from the brow of Zeus fully formed.

We must start at the beginning. And simplifying the problem can be a good start.

If we imagine we can strip away the square brackets, we can work on the task of transforming string: `"1, 2, 3"` --- not much, but a bit easier.


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

For the function, we'll specify a lambda, and in the lambda put `format` to work.

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

So, now we need to address the square brackets. Given "[1, 2, 3]" how do we get rid of those pesky square brackets?

We can use a regular expression to match the beginning and ending bracket, and then capture what is inside using parentheses. We know we can do this from previous experience. We have done it before. Still, we will need to experiment to get the syntax correct.

How to best experiment?

We can experiment with the `replace-regexp-in-string` function.[^fn:3]

The documentation tells us:

```text
(replace-regexp-in-string REGEXP REP STRING &optional FIXEDCASE LITERAL SUBEXP START)

Documentation
Replace all matches for REGEXP with REP in STRING.

Return a new string containing the replacements.
```

Okay. We need

1.  A regular expression
2.  A replacement string (of some type)
3.  A string to operate on, and for that "[1, 2, 3]" will do.

In Emacs Elisp the regular expression syntax requires the use of double backslashes. This makes the syntax messy. But for a simple expressions, it's not horrible.[^fn:4]

Our regex is actually in the category of not horrible: `\\[\\([[:digit:], ]+\\)\\]`

If not familiar:

-   the beginning `\\[` and ending `\\]` is matching the literal opening and closing bracket of our target string.
-   the parans, `\\(` and `\\)`  capture the sub-expression of the matched text. An expression so captured can be accessed later.
-   `[:digit:]` means a digit. Another way to specify a digit is `0-9`. Some prefer that.
-   Opening bracket `[` and closing bracket `]` specify what is called a character class, a list of characters we are seeking to match on. In this case either a digit, a comma, or space.
-   the plus =+=means match 1 or more of the preceding expression (in this case, the character class we just specified).

So, we now have this, the REGEXP is filled in:

```elisp
(replace-regexp-in-string "\\[\\([[:digit:], ]+\\)\\]" REP STRING)
```

And for the second and third argument:

-   "\\\\\\\\1" is a reference to the inner sub-expression we captured in the regular expression with the parentheses. This is our replacement text, the "1, 2, 3" without the brackets.
-   "[1, 2, 3]" --- the text we want to operate on.

This gives:

```elisp
(replace-regexp-in-string "\\[\\([[:digit:], ]+\\)\\]" "\\1" "[1, 2, 3]")
```

So, evaluating this expression gives us:

```elisp
"1, 2, 3"
```

Looks good. We've gotten rid of the square brackets and we know we can send that string to `split-string` and get a list, and then send that list on to `mapconcat`. Things are coming together.


## Build It {#build-it}

Now that we have all the pieces we need to build our little function.

First we'll focus on the main body.

Let's run `re-search-forward` for one pass.

But how? It feels like we have to have everything working before we can test and  see the results.

But no, and this is important: just as we've been testing with various bits above, we can continue to do so.

We want develop our code and have it run in a buffer. Emacs makes this very convenient with the `with-temp-buffer` wrapper.

So, let's create a temporary buffer and insert some strings to target.


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

`with-temp-buffer` creates a temp buffer and our code is now running in it's context. So, we insert some data and run function and see what happens.

After we insert the string we go to the top of the buffer and run `re-search-forward` one time. Then collect some data into a list. This list is then returned.

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

Now, we can even simulate a loop. Suppose we want to catch the second iteration. We can just call `re-search-forward` twice. (Not to worry. We'll do an actual loop in a bit.)


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

Now that we know that we are matching and extracting the sub-expression, let's do the transformation.

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

Now, after `re-search-forward` we collect the inner string using `match-string-no-properties` The number 1 indicates that we want the capture sub-expression. (You might have multiple captured expressions and these are numbered. We have only one sub-expression, so we want number 1.)

Evaluating:

```elisp
"[fn:1][fn:2][fn:3]"
```

We have taken this step-by-step, and it's really paying off.

Using `with-temp-buffer` makes testing in this way very easy.[^fn:5]


## Adding the While Loop {#adding-the-while-loop}

Now, we'll add in a while loop.

Notice that we are saving the match beginning and end. A frequent source of headaches is having your match data overwritten by another function. So, saving the match data is necessary.[^fn:6]

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

Notice here that we make all the modifications in temp-buffer, and then we return `(buffer-string)` which as the name implies is the string of the temp-buffer.[^fn:7]

```text
Here is the first target: [fn:1][fn:2][fn:3],
second, [fn:4][fn:5]
and third [fn:1][fn:2][fn:3][fn:4][fn:5][fn:6]
```


## Final {#final}

Now, we can finish up.

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

For me, I get footnotes that are not supported by Orgmode. I must modify them for them to be useful.

There might be many acceptable approaches for doing these modifications: doing it manually, writing macros, or, as demonstrated in this document, writing an elisp function.

Developing `my/refromat-footnote` did not require us to know a 600-line general purpose replacement engine. We started with one function and set to work on a simplified problem. From there we gradually add complexity and checked the results along the way at each step. Using `with-temp-buffer` was an essential element of being able to test the development.[^fn:8]

Emacs, elisp and incremental development is a powerful and satisfying approach for building up useful and moderately complex utility functions.

[^fn:1]: A keybind not too bad for occasional use, but must be changed if used heavily.
[^fn:2]: In Orgmode, footnotes can also be inlined, but that is not the point here.
[^fn:3]: Is this the best way? There is also `re-builder`.
[^fn:4]: And there are other ways to specify a regex in Elisp.
[^fn:5]: And I could have executed this code in a REPL, or in a buffer (using `C-x e`). For this document, I am using code-blocks in Orgmode.
[^fn:6]: There's another way to do this, using the `(save-match-data ...)` wrapper. I very much like this, too, and arguably it's more readable and lispy. My use of mapconcat and string-trim, while lispy, might be challenged. This will cause more consing and garbage collection. But for small buffers, it's completely fine.
[^fn:7]: Notice, too, that I have inserted newlines into the initial temp-buffer string to increase readability
[^fn:8]: This command assumes that bracketed numeric lists in the buffer are footnote references. In a document containing other bracketed numeric lists, I would narrow to a region or continue development and make the replacement interactive.
