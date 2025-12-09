---
title: "Cyclical Thinking and the Zigzag Conversion Problem"
author: ["Karl Stump"]
date: 2025-12-07
tags: ["programming", "lisp", "leetcode"]
draft: false
math: true
---

LeetCode has a well-known challenge, Zigzag Conversion (rated medium), introduced like this:

Leetcode has a coding challenge [zigzag Conversion](https://leetcode.com/problems/zigzag-conversion/description/) (rated at medium), that is presented like this:

```text
The string "PAYPALISHIRING" is written in a zigzag pattern on a given number of rows like this: (you may want to display this pattern in a fixed font for better legibility)

P   A   H   N
A P L S I I G
Y   I   R

And then read line by line: "PAHNAPLSIIGYIR"
```

Interesting! How to solve it?


## TL;DR {#tl-dr}

I assumed I needed a 2D array, diagonal coordinates, sparse matrix handling, etc.

That assumption sent me straight into a conceptual ditch.

Eventually I stepped back and realized: This problem isn’t geometric at all.

The entire behavior is governed by a simple cyclic pattern.


## Insight 1 — The Zigzag Is Cyclical, Not Geometric {#insight-1-the-zigzag-is-cyclical-not-geometric}

Zigzag is not a 2D grid problem.

Consider the row movement for rows equal to 3:

```text
0, 1, 2, 1, 0, 1, 2, 1, ...
```

That's a cycle.

For row equal to 4:

```text
0, 1, 2, 3, 2, 1, 0, 1, ...
```

Again a cycle.

So, what's the period (or what's the length)?

For n rows, the pattern is:

-   Go down n rows
-   Go up n-2 rows (skipping the top and bottom)

Like this:

```text
down:   n rows
up:     n-2 rows  (skipping top and bottom)
total:  n + (n-2) = 2n - 2 = 2*(n-1)
```

Total cycle length (or, period): \\[ 2(n-1)\\]

So, this is a periodic function problem.

Now that we know the length of the cycle, we can calculate where we are in the cycle for any \\( i \\) using modulo. Like this:

<style>.my-table-1 table { text-align: center;  width: 30%;  margin: 0 auto;  }</style>

<div class="ox-hugo-table my-table-1">

| i | k = (modulo i p=4) |
|---|--------------------|
| 0 | 0                  |
| 1 | 1                  |
| 2 | 2                  |
| 3 | 3                  |
| 4 | 0                  |
| 5 | 1                  |
| 6 | 2                  |
| 7 | 3                  |
| 8 | 0                  |

</div>

So the idea then would be something like the following where we assume that rows = 3 and the old string is "PAYPALISHIRING"

```text
period = 2 * (3 - 1)
loop for i = 0 to size(old_string)
    push(new_rows[(modulo i, period)], old_string[i])
```

But there's a problem:

You can see it easily in this table:

<style>.my-table-3 table { text-align: center;  width: 70%;  margin: 0 auto;  }</style>

<div class="ox-hugo-table my-table-3">

| i | k = (modulo i 4) | The Row We Want |                                                                   |
|---|------------------|-----------------|-------------------------------------------------------------------|
| 0 | 0                | 0               |                                                                   |
| 1 | 1                | 1               |                                                                   |
| 2 | 2                | 2               |                                                                   |
| 3 | 3                | 1               | \\( \bbox[5px, border:2px solid green]{\unicode{x274c}} \\)       |
| 4 | 0                | 0               |                                                                   |
| 5 | 1                | 1               |                                                                   |
| 6 | 2                | 2               |                                                                   |
| 7 | 3                | 1               | \\( \quad \bbox[5px, border:2px solid green]{\unicode{x274c}} \\) |

</div>

So, we must normalize our results. Like this

<style>.my-table-4 table { text-align: center;  width: 90%;  margin: 0 auto;  }</style>

<div class="ox-hugo-table my-table-4">

| i | k = (modulo i 4) | The Row We Want | row = k&lt;3 ? k : 4-k |                                                                         |
|---|------------------|-----------------|------------------------|-------------------------------------------------------------------------|
| 0 | 0                | 0               | 0                      |                                                                         |
| 1 | 1                | 1               | 1                      |                                                                         |
| 2 | 2                | 2               | 2                      |                                                                         |
| 3 | 3                | 1               | 1                      | \\( \bbox[5px, border:2px solid green;color:green]{\unicode{x2714}} \\) |
| 4 | 0                | 0               | 0                      |                                                                         |
| 5 | 1                | 1               | 1                      |                                                                         |
| 6 | 2                | 2               | 2                      |                                                                         |
| 7 | 3                | 1               | 1                      | \\( \bbox[5px, border:2px solid green;color:green]{\unicode{x2714}} \\) |

</div>

Modulo gives us the full cycle; normalization maps it to actual rows.

```text
period = 2 * (n-1)
k = mod i period
row = k < n ? k : period - k
```

This analysis greatly clarifies matters.


## Second Insight: Columns Don't Matter {#second-insight-columns-don-t-matter}

The problem statement encourages you to picture:

-   a 2D grid
-   diagonal movement
-   sparse character placement
-   column-by-column mapping

This mental model is deeply misleading.

**Columns play no role in the final output.**

The zigzag is only about row order.

Once you know the row for each character, the solution becomes trivially simple:

1.  Initialize rows, a list/vector of empty strings (one per row).
2.  For each character at index i, compute its row using the formula above.
3.  Append the character to that row.
4.  Concatenate all rows.

Done.


## Coding Zigzag: First Pass {#coding-zigzag-first-pass}

First we need some way to convert a position in the string (i)  to a row based on the number of rows. This will do. This mapping is just easy math.

```lisp { linenos=true, linenostart=1 }
;; map index i to a row number based on cyclic pattern
(define (index->row i num-rows)
    (let* ((cycle (* 2 (- num-rows 1)))
           (k (modulo i cycle)))
      (if (< k num-rows)
          k
          (- cycle k))))
```

Now, we need some way put the rows together. Each string in the ordered `los` represents a row. So, just loop over them and `string-append`.

```lisp { linenos=true, linenostart=1 }
  ;; join-strings: list-of-strings -> string
;; concatenate all the strings in LOS
(define (join-strings los)
    (let loop ((ls los) (acc ""))
      (if (null? ls)
          acc
          (loop (cdr ls) (string-append acc (car ls))))))
```

Given a string and the number of rows, we build strings. The vector is just a list of strings, each string represents a row.

```lisp { linenos=true, linenostart=1 }
(define (zigzag-convert-numeric s num-rows)
    (let ((n (string-length s)))
      (if (or (<= num-rows 1)
              (<= n num-rows))
          s
          (let ((rows (make-vector num-rows "")))
            (let loop ((i 0))
              (if (= i n)
                  (join-strings (vector->list rows))
                  (let* ((ch       (string-ref s i))
                         (k (index->row i num-rows))
                         (old-row  (vector-ref rows k))
                         (new-row  (string-append old-row (make-string 1 ch))))
                    (vector-set! rows k new-row)
                    (loop (+ i 1)))))))))
```

This solution works well.

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

{{< figure src="/ox-hugo/zig-zag-first-pass.png" caption="<span class=\"figure-number\">Figure 1: </span>...pretty good..." class="my-figure" >}}

There is room to optimize.


## Coding Zigzag: Pass 2 "I feel the need for speed" {#coding-zigzag-pass-2-i-feel-the-need-for-speed}

In the first pass, every append creates a new string.
This is expensive.

A faster strategy:

-   Accumulate _characters_ in lists
-   Reverse them _once_ per row
-   Write them into a _preallocated_ result string

Will these changes make any difference?

<style>
.my-code-1{
/* margin-right:-100px;
margin-left: -100px; */
}
</style>

```lisp { class="my-code-1", linenos=true, linenostart=1 }
;; zigzag-convert-numeric-fast: string x integer -> string
;; More efficient version using lists of chars + one big final string.
(define (zigzag-convert-numeric-fast s num-rows)
    (let ((n (string-length s)))
      (if (or (<= num-rows 1)
              (<= n num-rows))
          s
          (let ((rows (make-vector num-rows '())))
            (let build ((i 0))
              (if (= i n)
                  (let ((result (make-string n)))
                    (let fill-rows ((r 0) (pos 0))
                      (if (= r num-rows)
                          result
                          (let ((row-list (reverse (vector-ref rows r))))
                            (let fill-row ((lst row-list) (p pos))
                              (if (null? lst)
                                  (fill-rows (+ r 1) p)
                                  (begin
                                   (string-set! result p (car lst))
                                   (fill-row (cdr lst) (+ p 1)))))))))
                  (let* ((ch       (string-ref s i))
                         (k        (index->row i num-rows))
                         (old-row  (vector-ref rows k)))
                    (vector-set! rows k (cons ch old-row))
                    (build (+ i 1)))))))))
```

And the results are:

<style>
figure.my-figure-2{
    margin: 0 auto;  /* Centers the figure */
    text-align: center;  /* Centers the caption */
    width: 65%;  /* Set a width for the figure */
}
.my-figure-2 figcaption{
    font-style: italic;
    font-weight: lighter;
}
.figure-number{
    display: none;
    }
</style>

{{< figure src="/ox-hugo/zig-zag-speed.png" caption="<span class=\"figure-number\">Figure 2: </span>That's fast!" class="my-figure-2" >}}

Down from 66ms to 4ms -- that's fast! It's a huge improvement over the previous version. Excellent!


## Conclusion {#conclusion}

The Zigzag Conversion problem looks geometric, but it is purely cyclic.

From this exercise we learn:


### Technical {#technical}

1.  Problem descriptions may intentionally (or unintentionally) mislead. And this can be true in coding interviews too!
2.  Cycles and modulo arithmetic are powerful tools for pattern analysis.
3.  The simplest correct solution becomes obvious once the cycle is understood.
4.  Performance often comes from reducing allocations, not changing algorithms.


### Language-specific (Scheme) {#language-specific--scheme}

1.  Named let, tail recursion, along with vector/list hybrids led to an elegant solution. Of those the most important is probably tail call.
2.  Functional accumulation \\(\rightarrow\\) and final flattening is a common in Scheme.
3.  The reference and optimized versions demonstrate a striking differential in speed, but a small-looking difference in code.

In the end, Zigzag Conversion was much less about coding and much more about analyzing and seeing the real shape of the problem.

And it was an excellent problem for me to learn more about Scheme!
