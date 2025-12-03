---
title: "Sorting in Emacs with Elisp"
author: ["Karl Stump"]
date: 2025-11-08
tags: ["lisp", "emacs"]
draft: false
math: true
---

How would you sort lines in a text file by length?

I came across a post on X with about thirty full names, mostly well-known authors. The OP asked which one was everyone’s favorite. The interesting part: the names weren’t sorted alphabetically, but by length. When asked how they did it, the OP said it was done by hand.

Manual is fine for short lists. It becomes tedious very quickly.

So: how do we sort a list of names by length?

<style>
.table-0 table th {
    text-align: left;
    padding: 15px;
    text-transform: capitalize;
}
.table-0 table td {
    text-align: left;
    padding: 15px;
    text-transform: capitalize;
}
.table-number {
  display: none;
}
.table-caption{
    font-style: italic;
    font-weight: lighter;
    text-align: center;
}
</style>

<style>.table-0 table { text-align: center;  width: 25%;  margin: 0 auto;  }</style>

<div class="ox-hugo-table table-0">
<a id="table--short-table"></a>
<div class="table-caption">
  <span class="table-number"><a href="#table--short-table">Table 1</a>:</span>
  How to sort these names by length?
</div>

| Amelia Clarkson  |
|------------------|
| Nicola Gill      |
| Heather Anderson |
| Amy Burgess      |
| Trevor Hughes    |

</div>


## The Classic Problem: which tool? {#the-classic-problem-which-tool}

When you first learn to program, there’s a real sense of magic: you move from “I have no idea how to do this” to “I can bend the machine to my will.”

With editors and the shell, the progression often looks like this:

Stage 1
: "How do I do this?"

Stage 2
: “Oh wow, I can do this in awk/sed/paste/sort.”

Stage 3
: “Oh wow, I can do this in Vim.”

Stage 4
: “Oh wow, I can do this in Emacs.”

Stage 5
: “Wait… now **which** one should I use?”

Stage 5 is where the unease begins. There must be one **correct** way, right?

As an Emacs user I’m probably supposed to say, “Of course the right way is in Emacs!”

<style>
figure.my-figure{
    margin: 0 auto;  /* Centers the figure */
    text-align: center;  /* Centers the caption */
    width: 65%;  /* Set a width for the figure */
}
.my-figure figcaption{
    font-style: italic;
    font-weight: lighter;
}
.figure-number{
    display: none;
}
</style>

{{< figure src="https://imgs.xkcd.com/comics/real_programmers.png" caption="<span class=\"figure-number\">Figure 1: </span>Real programmers use Emacs" class="my-figure" >}}

But in practice there isn’t one right way for everything, even when you live inside Emacs.

Sometimes I reach for Elisp. Sometimes for the shell and Unix tools. The question is: how do you decide?

Here are the loose guidelines I use:

1.  Don’t re-invent the wheel → use existing Elisp functions if they exist.
2.  If the transformation is purely textual → use Unix tools via `M-x shell-command-on-region`.
3.  If the transformation is just repeatable keystrokes → use Emacs keyboard macros.
4.  If the operation is complex and will be repeated or shared → write Emacs Lisp.
5.  When in doubt → prototype with a shell pipeline first.

These aren’t laws; they’re more like habits that keep me from over-engineering.

One more point that I think is important: `M-x shell-command-on-region` plus the Unix toolchain should **not** be viewed with disdain.

Yes, Emacs is:

-   a powerful Lisp environment
-   easy to extend, explore, and debug

All true.

But Unix tools are also excellent and battle-tested. They’re highly optimized for text manipulation. Taking advantage of them from inside Emacs is a **good** thing, not a betrayal of Emacs purity.

So, back to our example: we have a list of names, and we want to sort by line length.

Emacs doesn’t ship (as far as I can tell) with a built-in “sort lines by length” command. So: can we let the shell do the heavy lifting?

Yes. With some basic Unix tools, the job is trivial:

```bash
awk '{print length($0), $0}' | sort -n | cut -d ' ' -f2-
```

-   `awk` prints the line length followed by the line itself.
-   `sort -n` sorts numerically by that length.
-   `cut -d ' ' -f2-` strips the length prefix off again.

<style>
.table-1 table th {
    text-align: left;
    padding: 15px;
    text-transform: capitalize;
}
.table-1 table td {
    text-align: left;
    padding: 15px;
    text-transform: capitalize;
}
.table-number {
  display: none;
}
.table-caption{
    font-style: italic;
    font-weight: lighter;
    text-align: center;
}
</style>

<style>.table-1 table { text-align: center;  width: 25%;  margin: 0 auto;  }</style>

<div class="ox-hugo-table table-1">
<a id="table--sorted-short-table"></a>
<div class="table-caption">
  <span class="table-number"><a href="#table--sorted-short-table">Table 2</a>:</span>
  A list sorted by length
</div>

| Amy Burgess      |
|------------------|
| Nicola Gill      |
| Trevor Hughes    |
| Amelia Clarkson  |
| Heather Anderson |

</div>

`M-x shell-command-on-region` is a clean, efficient way to get this behavior. If you need it frequently, you can always wrap the pipeline in a small Elisp function — but the **core** remains a Unix one-liner.

So far, so good.

Now let’s make the problem more interesting.


## Sorting names in a table {#sorting-names-in-a-table}

Consider this table:

<style>
.table-2 table th {
    text-align: left;
    padding: 10px;
    text-transform: capitalize;
}
.table-2 table td {
    text-align: left;
    padding: 10px;
    text-transform: capitalize;
}
.table-number {
  display: none;
}
.table-caption{
    font-style: italic;
    font-weight: lighter;
    text-align: center;
}
</style>

<style>.table-2 table { text-align: center;  width: 100%;  margin: 0 auto;  }</style>

<div class="ox-hugo-table table-2">
<div class="table-caption">
  <span class="table-number">Table 3:</span>
  How to sort these names by length?
</div>

| Patty O’Furniture | P. Ann O’Recital | Maureen Biologist | Anne Teak        |
|-------------------|------------------|-------------------|------------------|
| Ann Chovey        | Ray O’Sun        | Teri Dactyl       | U.R. Nice        |
| Hazel Nutt        | Ray Sin          | Peg Legge         | Anita Bath       |
| Chris P. Bacon    | Isabelle Ringing | Allie Grater      | Harriet Upp      |
| Marsha Mellow     | Eileen Sideways  | Liz Erd           | I.M. Tired       |
| Olive Yew         | Rita Book        | A. Mused          | I. Missy Ewe     |
| Barb Akew         | Paige Turner     | Constance Noring  | Ivana B. Withew  |
| Aida Bugg         | Rhoda Report     | Lois Di Nominator | Anita Letterback |
| Chris Anthemum    | Augusta Wind     | Minnie Van Ryder  | Lynn O’Leeum     |

</div>

The goal: sort these **cells** by name length, left-to-right, top-to-bottom.

That’s quite different from a simple vertical list. Now we’re dealing with:

-   an Org table
-   multiple cells per line
-   a desired **reading order** (by rows) that we want to preserve

There’s no obvious Unix one-liner for “flatten the table, sort cells by length, and write them back into the grid shape.” It’s possible, but it’s not simple — and it’s certainly not “just one more =sort | awk=”.

At this point, the problem edges into “worth writing some Elisp.”

Fortunately, Emacs does ship with a general text-sorting workhorse that fits perfectly here: `sort-subr`.

Let’s read the docstring:

```text
sort-subr is an autoloaded native-comp-function in ‘sort.el’.

(sort-subr REVERSE NEXTRECFUN ENDRECFUN &optional STARTKEYFUN
ENDKEYFUN PREDICATE)

General text sorting routine to divide buffer into records and sort them.
```

That opening is already promising: “divide buffer into records and sort them.”

Clicking through to the source (in my case, `~/.local/share/emacs/31.0.50/lisp/sort.el`) reveals how it works, but the docstring alone gives a decent mental model.[^fn:1]

We define:

-   what counts as a **record** (a line, a cell, etc.)
-   what part of that record is the **key**
-   how keys are compared

`sort-subr` does the rearranging.

From the docstring:

```text
We divide the accessible portion of the buffer into disjoint pieces
called sort records.  A portion of each sort record (perhaps all of
it) is designated as the sort key.  The records are rearranged in the
buffer in order by their sort keys.  The records may or may not be
contiguous.
```

“Disjoint pieces” just means records don’t overlap. The “accessible portion” is whatever region is visible after narrowing.

Now the arguments:

REVERSE
: non-nil means sort in reverse order

NEXTRECFUN, ENDRECFUN
: functions that describe where each record starts and ends

STARTKEYFUN, ENDKEYFUN
: functions that mark or compute the key within a record

PREDICATE
: an optional comparison function

Next, from the docstring:

```text
NEXTRECFUN is called with point at the end of the previous record.
It moves point to the start of the next record.
...
The first record is assumed to start at the position of point when
‘sort-subr’ is called.

ENDRECFUN is called with point within the record.
It should move point to the end of the record.
```

Behind the scenes, `sort-subr` calls these functions to walk the buffer, capturing record boundaries.

Then key selection:

```text
STARTKEYFUN moves from the start of the record to the start of the key.
It may return either a non-nil value to be used as the key, or
else the key is the substring between the values of point after
STARTKEYFUN and ENDKEYFUN are called.  If STARTKEYFUN is nil, the key
starts at the beginning of the record.

ENDKEYFUN moves from the start of the sort key to the end of the sort key.
ENDKEYFUN may be nil if STARTKEYFUN returns a value or if it would be the
same as ENDRECFUN.
```

This is subtle, but powerful. There are two main modes:

1.  ****Key by value****
    -   STARTKEYFUN **returns** a value → that value **is** the key.
    -   In this case, ENDKEYFUN is unnecessary.

2.  ****Key by position****
    -   STARTKEYFUN moves point, returns nil.
    -   ENDKEYFUN moves point again.
    -   `sort-subr` treats the region between those two positions as the key.

Finally, the predicate:

```text
PREDICATE, if non-nil, is the predicate function for comparing
keys; it is called with two arguments, the keys to compare, and
should return non-nil if the first key should sort before the
second key.  If PREDICATE is nil, comparison is done with ‘<’ if
the keys are numbers, with ‘compare-buffer-substrings’ if the
keys are cons cells (the car and cdr of each cons cell are taken
as start and end positions), and with ‘string<’ otherwise.
```

So PREDICATE receives two keys — either values or cons cells locating substrings — and decides their order.

Let’s put this to use.


## First pass: sorting lines by length {#first-pass-sorting-lines-by-length}

We’ll start with the simple case: a plain list of lines (names) and we want to sort them by length.

Here’s a first pass at an interactive function:

```emacs-lisp
  (defun kes/sort-lines-by-length (reverse beg end)
    "Sort lines by length in region from BEG to END.
With prefix argument REVERSE, sort longest first."
    (interactive "P\nr")
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
        (goto-char (point-min))
        (sort-subr
         reverse
         #'forward-line       ; NEXTRECFUN: move to next line start
         #'end-of-line        ; ENDRECFUN: move to end of line
         nil                  ; STARTKEYFUN: whole line is key
         nil                  ; ENDKEYFUN: not needed
         (lambda (k1 k2)      ; PREDICATE: compare line lengths
           (< (- (cdr k1) (car k1))
              (- (cdr k2) (car k2)))))))
```

A few details worth noting:

-   `forward-line` is our NEXTRECFUN: it moves to the start of the next line.
-   `end-of-line` is ENDRECFUN: it moves to the end of the current line.
-   STARTKEYFUN and ENDKEYFUN are nil → the **entire line** is the key.
-   In that case, `sort-subr` passes each key to the predicate as a cons cell: `(BEG . END)`.
-   Our PREDICATE just computes the length `END - BEG` and compares lengths.

This works nicely for a list of names.[^fn:2] But our original goal was to sort **cells** in an Org table, left-to-right, top-to-bottom.

Time to get a bit fancier.


## Second pass: sorting an Org table by cell length {#second-pass-sorting-an-org-table-by-cell-length}

We’ll now assume the table is an [Org table](https://orgmode.org/manual/Tables.html). That lets us use a couple of handy Org functions to find table boundaries.

Here’s what the table looks like in the buffer:

```text
| Patty O’Furniture | P. Ann O’Recital | Maureen Biologist | Anne Teak        |
| Ann Chovey        | Ray O’Sun        | Teri Dactyl       | U.R. Nice        |
| Hazel Nutt        | Ray Sin          | Peg Legge         | Anita Bath       |
...
```

We want to treat each **cell** as a record, and the length of that cell as the key.

Let’s start with the outer function:

```emacs-lisp
  (defun kes/sort-org-table-by-field-length (reverse)
    "Sort current Org table by cell length.
Invoke anywhere inside the table (no active region needed).
With prefix argument REVERSE, sort longest cells first."
    (interactive "P")
    (save-excursion
      (save-restriction
        (let ((beg (org-table-begin))
              (end (org-table-end)))
          (narrow-to-region beg end)
          (goto-char (point-min))
          (kes/record-start)
          (sort-subr
           reverse
           #'kes/record-start
           #'kes/record-end
           (lambda ()
             ;; STARTKEYFUN: compute width of current cell as key value.
             (save-excursion
               (let ((start (point)))
                 (kes/record-end)
                 (- (point) start))))))))
```

Key points:

-   We don’t pass BEG/END as arguments. Instead we ask Org:
    -   `org-table-begin`
    -   `org-table-end`
-   We narrow to the table and move to the beginning.
-   Before calling `sort-subr` we **must** position point at the start of the first record, so we call `kes/record-start` once.
-   We pass:
    -   NEXTRECFUN  → `kes/record-start`
    -   ENDRECFUN   → `kes/record-end`
    -   STARTKEYFUN → a lambda that **returns** the cell length as a value
    -   ENDKEYFUN   → omitted (because STARTKEYFUN returns the key value directly)
    -   PREDICATE   → omitted (default numeric comparison is fine for lengths)

All that remains is to define what a “record” is in this context — one cell — and how to jump to its start and end.

```emacs-lisp
  (defun kes/record-start ()
    "Move point to the start of the next cell’s content.
If no further cell is found, move to `point-max'."
    (if (re-search-forward "| \\([a-zA-Z.']\\)" nil t)
        (goto-char (match-beginning 1))
      (goto-char (point-max)))
    nil)

  (defun kes/record-end ()
    "Move point to the end of the current cell’s content."
    (re-search-forward "[a-zA-Z.']+\\( \\) *|" nil t)
    (goto-char (match-beginning 1))
    nil)
```

These are straight regex walkers:

-   `kes/record-start`:
    -   Searches forward for a vertical bar, a space, then the first character of the cell.
    -   On success, moves point to the first character of the cell content.
    -   On failure, moves to `point-max` (as required by `sort-subr`).

-   `kes/record-end`:
    -   Searches for the end of the cell content, just before the trailing space and `|`.
    -   Moves point to that final space (the end of the content).

Since STARTKEYFUN returns a numeric key (the length), `sort-subr` uses the default number comparison and we don't need a custom predicate.

Here is the result of calling `kes/sort-org-table-by-field-length` on the table:

<style>
.table-3 table th {
    text-align: left;
    padding: 15px;
    text-transform: capitalize;
}
.table-3 table td {
    text-align: left;
    padding: 15px;
    text-transform: capitalize;
}
.table-number {
  display: none;
}
.table-caption{
    font-style: italic;
    font-weight: lighter;
    text-align: center;
}
</style>

<style>.table-3 table { text-align: center;  width: 100%;  margin: 0 auto;  }</style>

<div class="ox-hugo-table table-3">
<div class="table-caption">
  <span class="table-number">Table 4:</span>
  Sorted by length, left to right, top to bottom
</div>

| Ray Sin          | Liz Erd           | A. Mused          | Anne Teak         |
|------------------|-------------------|-------------------|-------------------|
| Ray O’Sun        | U.R. Nice         | Peg Legge         | Olive Yew         |
| Rita Book        | Barb Akew         | Aida Bugg         | Ann Chovey        |
| Hazel Nutt       | Anita Bath        | I.M. Tired        | Teri Dactyl       |
| Harriet Upp      | Allie Grater      | I. Missy Ewe      | Paige Turner      |
| Rhoda Report     | Augusta Wind      | Lynn O’Leeum      | Marsha Mellow     |
| Chris P. Bacon   | Chris Anthemum    | Eileen Sideways   | Ivana B. Withew   |
| P. Ann O’Recital | Isabelle Ringing  | Constance Noring  | Anita Letterback  |
| Minnie Van Ryder | Patty O’Furniture | Maureen Biologist | Lois Di Nominator |

</div>


## Conclusion {#conclusion}

We started with a common little problem: sorting a list of names by length.

For a simple vertical list, Unix tools plus `M-x shell-command-on-region` gave us an elegant one-liner. That solved the first problem without touching Elisp.

Then we changed the shape of the data. Sorting names by length **inside an Org table**, left-to-right and top-to-bottom, was no longer a straightforward shell pipeline. At that point, Emacs Lisp — and specifically `sort-subr` — became the right tool.

Along the way, we:

-   used Unix tools where they shine (simple line transformations)
-   dropped into Elisp where structure and context mattered (Org tables, cell boundaries)
-   relied on Emacs’s own sorting primitive rather than re-implementing a sort from scratch

The pattern is useful beyond this example:

-   Let Unix do what Unix is good at.
-   Let Emacs do what Emacs is good at.
-   And when the problem sits at the intersection — like sorting structured text inside your editor — it’s nice to know what tools Emacs has waiting under the hood.

[^fn:1]: In sort.el is found the function sort-lines which sorts alphabetically. Instrumenting and stepping through it with the debugger is another excellent path forward.
[^fn:2]: An improvement would be to have STARTKEFUN simply compute the length and return it. In that case, ENDKEYFUN and PREDICATE can be nil. We will see this in the next version, along with other changes.
