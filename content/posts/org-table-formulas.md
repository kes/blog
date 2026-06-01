---
title: "Org Mode Table Formulas"
author: ["Karl Stump"]
date: 2026-05-23
draft: false
math: true
---

I have used Org tables for various reasons, but not as spreadsheets and not specifying calculations.

In this post I want to examine table formulas for calculations. In other words, using Org tables as spreadsheets.

The topics covered will be references, cell, column, and row, both absolute and relative. From there ranges will be introduced.

That doesn't sound like much. But it's a lot to cover in one post.

So let's get going.


## Org Spreadsheet {#org-spreadsheet}

Org Mode can treat tables like spreadsheets. That's pretty amazing, but there are some key differences between traditional spreadsheets and Org-Mode tables used as spreadsheets.

<div class="ox-hugo-table my-table-1">

| Traditional Spreadsheets (Excel)                 | Org-Mode Tables                                             |
|--------------------------------------------------|-------------------------------------------------------------|
| Data and logic are mixed.                        | Data and logic are separated.                               |
| A cell contains either raw data or a formula.    | Cells only contain raw data or calculated outputs.          |
| You must click a cell to see the hidden formula. | All logic is completely visible at the bottom of the table. |
| Formulas are stored per cell.                    | Formulas are stored as global rules for columns or rows.    |

</div>

In Org, formulas for how to calculate the value of a cell are specified in a `#+TBLFM:` line beneath the table. The formulas can never be written directly inside the table cells. They must always live in the `#+TBLFM:` line just below the table.

As an example, here is a table in which the contents of the first and third column are added and placed in the fifth. Notice that the formula is in the `#+TBLFM` line.

```text
| 111 | + | 222 | = | 333 |
#+TBLFM: @1$5=@1$1+@1$3
```

The details of cell references, and the `@` and the `$` prefixes, will be explained in the next section. For now, it's enough to know that "@" specifies the row, while "$" specifies the column.

While it is possible to edit the `#+TBLFM` line directly, Emacs has a formula editor. So, one way to edit the formula is to position the cursor on the `#+TBLFM` line, and then press `C-c '`.

This will open a buffer where the formula can be edited.

{{< figure src="/ox-hugo/edit-formulas.png" >}}

Org's standard notation for cell references is to use the prefixes "@" and "$." However, you can use letters for columns and numbers for rows. So, row three, column two, can be referenced as B3.

However, this feature is only available in the formula editor. If you choose to enter a formula in this way, Org will convert it to its preferred notation in the `#+TBLFM` line when you exit the buffer. In no case will Org recognize letters and numbers in the `#+TBLFM`.

{{< figure src="/ox-hugo/editing-formulas-letter-number-syntax.png" >}}

This feature works only if you have set the variable `org-table-use-standard-references` to either 'from or 't. See the variable's docstring for details.[^fn:1]

Org tables do not automatically run the calculations. Position the cursor on the `#+TBLFM:` line and press `C-c C-c` to run the calculations.

Note, if editing the `#+TBLFM` line directly, the space after `#+TBLFM:` is required.

```text
| 5 | + | 8 | = | 13 |
#+TBLFM: @1$5=@1$1 + @1$3
```

Multiple formulas in the `#+TBLFM:` line are separated by a double colon, `::`.

```text
|  5 | + | 8 | = | 13 |
| 10 | + | 8 | = | 18 |
| 20 | + | 8 | = | 28 |
| 25 | + | 8 | = | 33 |
#+TBLFM: @1$5=@1$1 + @1$3::@2$5=@2$1 + @2$3::@3$5=@3$1 + @3$3::@4$5=@4$1 + @4$3
```

Long formulas can be tiresome to edit directly under the table. Use the table formula editor `C-c '` and Emacs will separate the formulas each on its own line. When done, `C-c C-c` to exit, and Emacs puts the `#+TBLFM:` line back together nicely separated with the required `::`.

{{< figure src="/ox-hugo/edit-long-formulas.png" >}}

At this point you've seen enough to enter basic formulas. But there is much more! Let's move on and discuss how to reference a cell in detail.


## Absolute Cell References {#absolute-cell-references}

We've already seen something like this, `@3$2` and you probably know this is a way to reference row 3, column 2. But there's much more to explain.

`@3$2` is called an  "absolute cell reference" and it is the most explicit and basic reference to a cell.

-   Org uses prefixes to indicate the row number and column number.
    -   **For the row:** the prefix is `@`
    -   **For column:** the prefix is `$`

So, an absolute cell reference looks like this: `@row$column`

Example: to reference row three and column two you would write:  `@3$2`

<div class="pro-tip">

**Pro Tip:** To find the reference for any cell in an Org table, put your cursor in the cell and press `C-c ?`.

</div>

Here is a table. You can see how the absolute references are working. There are exactly nine formulas[^fn:2] separated by `::`.

```text
| 1 | 2 | 3 |
| 4 | 5 | 6 |
| 7 | 8 | 9 |
#+TBLFM: @1$1=1::@1$2=2::@1$3=3::@2$1=4::@2$2=5::@2$3=6::@3$1=7::@3$2=8::@3$3=9
```

We aren't limited, of course, to just assigning values to cells. We can calculate values using the values from other cells, too.

```text
| Breakfast   | Cost |
|-------------+------|
| Eggs        | 2.99 |
| Grits       | 1.50 |
| Biscuits    | 1.25 |
| Coffee      | 0.25 |
|-------------+------|
| Total       | 5.99 |
| Tip  (15%)  | 0.90 |
| Grand Total | 6.89 |
#+TBLFM: @6$2=@2$2+@3$2+@4$2+@5$2::@7$2=@6$2*0.15;%.2f::@8$2=@7$2+@6$2;%.2f
```

Or in the formula editor:

{{< figure src="/ox-hugo/editing-breakfast.png" >}}

Notice that we have added a `;%.2f` to the end of the second and third formulas to nicely format our output.

<div class="pro-tip">

**Please note:** the above formulas are very verbose for pedagogical purposes: to demonstrate absolute cell references. There are much more concise ways of expressing these formulas.

</div>

We now move to absolute column references.


## Absolute Column References {#absolute-column-references}

In Org, you can make an absolute reference to a column, for every applicable row.

That's sort of a funny statement. It's easy to run past it, or not quite get it.

The manual says: "if you omit either the column or the row part of the reference, the current row/column is implied."[^fn:3]

Okay, let's look at some examples.

So, here's an example of an absolute column reference: `$5`. This is an absolute reference to the fifth column.

But for which row? The answer is, for every applicable row. (More about "applicable" in a moment.)

So, given:

```text
|   | foo    |
|   | bar    |
|   | baz    |
|   | foobar |
#+TBLFM: $1=99
```

And positioning our cursor on the `#+TBLFM` line and pressing `C-c C-c` we get:

```text
| 99 | foo    |
| 99 | bar    |
| 99 | baz    |
| 99 | foobar |
#+TBLFM: $1=99
```

How did every row of the first column get set to 99?

Simple, we used an absolute column reference in the formula.

Let's state the meaning of the formula in words: "For every applicable row in the table, set column 1 to the value 99."

That's it!

Now, about that word, "applicable?" Here is another example demonstrating that the header (row 1) and the hline (not a row) are automatically skipped --- they are not applicable.

<div class="pro-tip">

To verify that the header is row 1, and the hline is not its own row, put your cursor on each, and press `C-c ?`.

</div>

Consider this table.

```text
| qux   | quux   |
|-------+--------|
| alpha | foo    |
| beta  | bar    |
| gamma | baz    |
| delta | foobar |
#+TBLFM: $1=101;
```

Here, when we run the calculations, we expect the first column will get overwritten. But will the header (row 1) in column 1 get overwritten?

Let's try it.

`C-c C-c` yields \\(\ldots\\)

```text
| qux | quux   |
|-----+--------|
| 101 | foo    |
| 101 | bar    |
| 101 | baz    |
| 101 | foobar |
#+TBLFM: $1=101;
```

You see, "qux" did not get overwritten but everything else did. This is because the first row is regarded as the header and skipped (and the hline, isn't really a row at all --- but we will make use of it later). So, to state the formula in words, I would say something like, "For every **applicable row**, set the first column to the value 101."

Of course, we're not limited to just assigning constant values. Here, we introduce the use of a function for calculation.

```text
| Base | Power | Base^Power |
|------+-------+------------|
|    2 |     0 |          1 |
|    2 |     1 |          2 |
|    2 |     2 |          4 |
|    2 |     3 |          8 |
|    2 |     4 |         16 |
|    2 |     5 |         32 |
#+TBLFM: $3=pow($1,$2)
```

If we state the formula as explicitly as possible, it means:

> For each applicable row, set column 3 to the value of column 1 in that same row, raised to the power of the value in column 2 in that same row.

Or, more compactly:

> For each row, compute: Base^Power.

Here, \\$3 is the target column. On the right-hand side, \\$1 and \\$2 refer to columns 1 and 2 in the row currently being processed. So as Org recalculates the table, it effectively does this:

```text
row 2: column 3 = pow(column 1, column 2) = pow(2, 0) = 1
row 3: column 3 = pow(column 1, column 2) = pow(2, 1) = 2
row 4: column 3 = pow(column 1, column 2) = pow(2, 2) = 4
...
```

The good news is that we do not have to say all of that every time. Once the notation becomes familiar, the formula reads naturally:

```text
#+TBLFM: $3=pow($1,$2)

Column 3 is calculated from columns 1 and 2 in the same row.
```

Imagine trying to write this table using only absolute field references such as @2\\$3, @3\\$3, @4\\$3, and so on. You would need a separate formula for every row. By using column references like \\$1, \\$2, and \\$3, we let Org apply the same pattern across the table. The result is shorter, clearer, and much easier to maintain.

This concludes absolute column references. Now, let's turn our attention to absolute row references.


## Absolute Row References {#absolute-row-references}

So, `@2` is an absolute row reference. It means, if we were to write it out explicitly, `@2$1, @2$2, @2$3, ... etc.`

And a plain English statement we might begin with, "for each column in row 2, \\(\ldots\\)"

For example:

```text
| foo | bar | baz | foobar |
|  99 |  99 |  99 |     99 |
| 100 | 100 | 100 |    100 |
| 101 | 101 | 101 |    101 |
#+TBLFM: @2=99::@3=100::@4=101
```

I would read the formula as, "For each column, for row two, set the value to 99, for row three set the value to 100, and for row four set the value to 101."

Let's return to our power table.

Looks a bit different now.

```text
| Base       | 2 | 2 | 2 | 2 |  2 |  2 |
| Power      | 0 | 1 | 2 | 3 |  4 |  5 |
| Base^Power | 1 | 2 | 4 | 8 | 16 | 32 |
#+TBLFM: @3=pow(@1, @2)
```

I would state this as, "For ever column in row three set it to the base in row one, raised to the power in row two."


## Summarizing Absolute Cell, Column, and Row References {#summarizing-absolute-cell-column-and-row-references}

Absolute cell references are easy to understand. Not much more to say.

However, Org has both column formulas and row formulas.

-   A column formula such as `$3=` applies the formula down a column (that is, for every applicable row), usually skipping header rows separated by hlines. This means Org has a strong idea of "header columns" and knows to skip them.
-   A row formula such as `@3=` applies the formula across a row (that is, for every column). However, Org does not have an equally strong idea of “header columns,” so if the first column contains labels, either these will get over written, or, you will have to restrict the target range of the columns explicitly. We will see how to do this shortly.

A few more notes:

1.  Always start your absolute column reference formula with the words, "for each row," or "across each row" and you should be fine:

    > \\$3=\\$1+\\$2
    > For each row: col 3 = col 1 + col 2.

2.  Always start your absolute row reference formula with the words, "for each column," or "down each column"  and you're good to go:

    > @3=@1+@2
    > For each column: row 3 = row 1 + row 2.

3.  Here's a list that includes absolute column, row, and cell references:

    <div class="ox-hugo-table my-table-1">

    | Formula              | Plain English                                         |
    |----------------------|-------------------------------------------------------|
    | \\$3=\\$1+\\$2       | For each row, col 3 = col 1 + col 2.                  |
    | @3=@1+@2             | For each column, row 3 = row 1 + row 2.               |
    | @2\\$3=\\$1+\\$2     | In row 2 only, col 3 = col 1 + col 2.                 |
    | @3\\$2=@1\\$2+@2\\$2 | In cell row 3 col 2, add row 1 col 2 and row 2 col 2. |

    </div>

In order to master Org formulas, it is essential to master absolute column and row references.

Now we move on to relative column and row references.


## Relative Column References {#relative-column-references}

We've already seen that column references can be absolute, like, `$1`, now, we introduce relative column references. These column references are relative to the column of the field which is being computed.

So, if we wanted to references one column to the left of the column of the table being computed, we could write: `$-1`. An example will help.

<div class="pro-tip">

**Note:** In the table below notice the **header line** ( `|A | B | C | D |`) and the **hline** (`|-----+-----+-----+-----|`). Org knows to skip the header and the hline. However, if we put our cursor in the headline and press `C-c ?` we will see that the header is row one. And if we did the same for the first data line, we would see that it is row two.

</div>

In the following table we have not yet run the calculations. What is going on here? What will happen?

```text
|   A |   B | C | D |
|-----+-----+---+---|
| 100 | 200 |   |   |
| 300 | 400 |   |   |
| 500 | 600 |   |   |
#+TBLFM: $3=$-2 + 1::$4=$-2 + 5
```

In this example we have two formulas:

```text
$3=$-2 + 1
$4=$-2 + 5
```

Each formula uses an absolute column reference on the left-hand side and a relative column reference on the right-hand side.

In the first formula, `$3` means: “for each applicable row, calculate column 3.” In the second formula, `$4` means: “for each applicable row, calculate column 4.”

Now what does `$-2` mean?

`$-2` is a relative column reference. It means “the value two columns to the left of the field currently being calculated.”

That last phrase is important: relative to the field currently being calculated.

So in the first formula:

```text
$3=$-2 + 1
```

the field being calculated is in column 3. Therefore `$-2` refers to the column two places to the left of column 3, which is column 1. So column 3 gets the value from column 1 plus 1.

In the second formula:

```text
$4=$-2 + 5
```

the field being calculated is in column 4. Therefore `$-2` refers to the column two places to the left of column 4, which is column 2. So column 4 gets the value from column 2 plus 5.

After running the calculations, the table becomes:

```text
|   A |   B |   C |   D |
|-----+-----+-----+-----|
| 100 | 200 | 101 | 205 |
| 300 | 400 | 301 | 405 |
| 500 | 600 | 501 | 605 |
#+TBLFM: $3=$-2 + 1::$4=$-2 + 5
```

The question with relative references is: "relative to what?" Remember: relative references are interpreted relative to the field currently being calculated.

We now proceed to relative row references.


## Relative Row References {#relative-row-references}

We proceed as we have for relative column references.

If you've understood relative column references, then relative row references should be easy.

The row references are relative to the row of the field which is being computed.

```text
| A | 990 | 300 | 500 |
| B | 200 | 400 | 600 |
| C | 101 | 301 | 501 |
| D | 205 | 405 | 605 |
|---+-----+-----+-----|
|   |     |     |     |
#+TBLFM: @5=@-4 + @-3 +@-2 + @-1
```

`@5=` is read as, "set every column of row 5 to the following." And then follows, relative row references, `@-4 + @-3 +@-2 + @-1`

We ask relative to what? And the answer is, relative to row 5. So, `@-4` must mean 4 rows above row five, and so on, for the other relative row references.

We run the calculation and get:

```text
| A             |  990 |  300 |  500 |
| B             |  200 |  400 |  600 |
| C             |  101 |  301 |  501 |
| D             |  205 |  405 |  605 |
|---------------+------+------+------|
| A + B + C + D | 1496 | 1406 | 2206 |
#+TBLFM: @5=@-4 + @-3 + @-2 + @-1
```

The formula has worked as expected. It calculated row 5 by adding the four rows above it for each column.

But notice the first column. Why does row 5, column 1 contain `A + B + C + D`?

The reason is that `@5` is a row formula. It applies across the row, column by column. Org does not automatically skip the first column just because we are using it as a label column.

So for column 2, Org calculates:

```text
990 + 200 + 101 + 205
```

For column 3, it calculates:

```text
300 + 400 + 301 + 405
```

For column 4, it calculates:

```text
500 + 600 + 501 + 605
```

And for column 1, it calculates:

```text
A + B + C + D
```

Since column 1 contains text rather than numbers, the result is not a numeric sum. Org leaves the expression as text.

This is a useful lesson: row formulas apply across columns, and Org does not have the same automatic idea of a “header column” that it has for header rows. If the first column is a label column, we usually need to avoid calculating it explicitly.

Org gives a way to do this easily. Like this:

```text
| A     | 990 | 300 | 500 |
| B     | 200 | 400 | 600 |
| C     | 101 | 301 | 501 |
| D     | 205 | 405 | 605 |
|-------+-----+-----+-----|
| Total |     |     |     |
#+TBLFM: @5$2..@5$4=@-4 + @-3 + @-2 + @-1
```

Notice that the target `@5$2..@5$4=` is what is called a range (notice the two dots). We can state its meaning as, "for row five, and the columns two through four, ..."

We will cover ranges in more detail shortly. For now, let's run the calculations.

```text
| A     |  990 |  300 |  500 |
| B     |  200 |  400 |  600 |
| C     |  101 |  301 |  501 |
| D     |  205 |  405 |  605 |
|-------+------+------+------|
| Total | 1496 | 1406 | 2206 |
#+TBLFM: @5$2..@5$4=@-4 + @-3 + @-2 + @-1
```

Perfect! Column one of row five was not overwritten.

Let's look at ranges in more detail.


## Ranges {#ranges}

From the previous section we've already seen the use of a range with the "dot dot" notation.

Ranges are vectors and can be used in various ways. For example, we can pass a range to a function. Let's take the range example from the previous section and change it a bit.

First let's remember it:

```text
| A     |  990 |  300 |  500 |
| B     |  200 |  400 |  600 |
| C     |  101 |  301 |  501 |
| D     |  205 |  405 |  605 |
|-------+------+------+------|
| Total | 1496 | 1406 | 2206 |
#+TBLFM: @5$2..@5$4=@-4 + @-3 + @-2 + @-1
```

We are happy with the formula, basically. But the right-hand side does seem a bit verbose.  Let's call a function to do the summation, and not only that, but a function that accepts a range. Like this:

```text
| A     | 990 | 300 | 500 |
| B     | 200 | 400 | 600 |
| C     | 101 | 301 | 501 |
| D     | 205 | 405 | 605 |
|-------+-----+-----+-----|
| Total |     |     |     |
#+TBLFM: @5$2..@5$4=vsum(@-4..@-1)
```

Let's run the calculation:

```text
| A     |  990 |  300 |  500 |
| B     |  200 |  400 |  600 |
| C     |  101 |  301 |  501 |
| D     |  205 |  405 |  605 |
|-------+------+------+------|
| Total | 1496 | 1406 | 2206 |
#+TBLFM: @5$2..@5$4=vsum(@-4..@-1)
```

And it works! Not only is the formula more concise, it's also more clear what our intention is.

Now, what happens if we shorten our range on the left-hand side to `@5$2..@5$3`, like this (note, I've removed the calculations and add the text "foo") --- this is before calculations:

```text
| A     | 990 | 300 | 500 |
| B     | 200 | 400 | 600 |
| C     | 101 | 301 | 501 |
| D     | 205 | 405 | 605 |
|-------+-----+-----+-----|
| Total |     |     | foo |
#+TBLFM: @5$2..@5$3=vsum(@-4..@-1)
```

Running the calculation:

```text
| A     |  990 |  300 | 500 |
| B     |  200 |  400 | 600 |
| C     |  101 |  301 | 501 |
| D     |  205 |  405 | 605 |
|-------+------+------+-----|
| Total | 1496 | 1406 | foo |
#+TBLFM: @5$2..@5$3=vsum(@-4..@-1)
```

That was certainly expected. The left-hand side specified a range of only two cells, and so the fourth column was not touched.

But what happens if the range sent to function vsum is too large and goes outside the table?

```text
| A     |  990 |  300 |  500 |
| B     |  200 |  400 |  600 |
| C     |  101 |  301 |  501 |
| D     |  205 |  405 |  605 |
|-------+------+------+------|
| Total | 1496 | 1406 | 2206 |
#+TBLFM: @5$2..@5$4=vsum(@-9..@-1)
```

Running the calculation, and we get a helpful message displayed:

```text
org-table--row-type: Row descriptor -9 leads outside table
```

It would be interesting if we could see exactly what's passed to `vsum`.

Here's a hack we can do. Let's call a function that doesn't exist. We'll call it `foobar`.

```text
| A     |  990 |  300 |  500 |
| B     |  200 |  400 |  600 |
| C     |  101 |  301 |  501 |
| D     |  205 |  405 |  605 |
|-------+------+------+------|
| Total | 1496 | 1406 | 2206 |
#+TBLFM: @5$2..@5$4=foobar(@-4..@-1)
```

Running the calculation using a non-existent function gives us this:

```text
| A     |                          990 |                          300 |                          500 |
| B     |                          200 |                          400 |                          600 |
| C     |                          101 |                          301 |                          501 |
| D     |                          205 |                          405 |                          605 |
|-------+------------------------------+------------------------------+------------------------------|
| Total | foobar([990, 200, 101, 205]) | foobar([300, 400, 301, 405]) | foobar([500, 600, 501, 605]) |
#+TBLFM: @5$2..@5$4=foobar(@-4..@-1)
```

That's interesting. We can see that a vector of values is created and used in the function call.

Now, we will look at others ways of specifying ranges.


## Ranges (2) {#ranges--2}

A very important way of defining a range is using `@I` and friends.

-   @I refers to the first hline.
-   @II refers to the second hline.
-   @III refers to the third hline.

And so on. I don't know if there's an upper limit.

Let's go back to a previous example --- the table holding our cost for breakfast --- it had a long formula.

```text
| Breakfast   | Cost |
|-------------+------|
| Eggs        | 2.99 |
| Grits       | 1.50 |
| Biscuits    | 1.25 |
| Coffee      | 0.25 |
|-------------+------|
| Total       | 5.99 |
| Tip  (15%)  | 0.90 |
| Grand Total | 6.89 |
#+TBLFM: @6$2=@2$2+@3$2+@4$2+@5$2::@7$2=@6$2*0.15;%.2f::@8$2=@7$2+@6$2;%.2f
```

Suppose now, we're buy breakfast for two?

```text
| Breakfast    | Cost |
|--------------+------|
| Eggs         | 2.99 |
| Eggs         | 2.99 |
| Pancakes     | 2.75 |
| Sausage      | 3.25 |
| Orange Juice | 1.00 |
| Grits        | 1.50 |
| Biscuits     | 1.25 |
| Coffee       | 0.25 |
| Tea          | 0.19 |
|--------------+------|
| Total        | 5.99 |
| Tip  (15%)   | 0.90 |
| Grand Total  | 6.89 |
#+TBLFM: @6$2=@2$2+@3$2+@4$2+@5$2::@7$2=@6$2*0.15;%.2f::@8$2=@7$2+@6$2;%.2f
```

It looks like it will take some work to update that formula. Can't we tighten this up a bit?

Yes, we can tighten the formula by using a function and a range.

```text
| Breakfast    |  Cost |
|--------------+-------|
| Eggs         |  2.99 |
| Eggs         |  2.99 |
| Pancakes     |  2.75 |
| Sausage      |  3.25 |
| Orange Juice |  1.00 |
| Grits        |  1.50 |
| Biscuits     |  1.25 |
| Coffee       |  0.25 |
| Tea          |  0.19 |
|--------------+-------|
| Total        | 16.17 |
| Tip  (15%)   |  2.43 |
| Grand Total  | 18.60 |
#+TBLFM: @11$2=vsum(@I..@II)::@12$2=@-1*0.15;%.2f::@13$2=@-1+@-2;%.2f
```

@I is the first hline, and @II is the second hline. So in this formula:

```text
@11$2=vsum(@I..@II)
```

we are saying:

In row 11, column 2, put the sum of the values between the first and second hlines.

Because the target field is in column 2, the range @I..@II is interpreted in the current column, which is column 2. We could write this more explicitly as:

```text
@11$2=vsum(@I$2..@II$2)
```

That version says the same thing, but it makes the column explicit.

Notice:

-   `vsum` takes a range of values and adds them.
-   `@I` and `@II` refer to the first and second hlines.
-   In `vsum(@I..@II)`, the column is omitted, so Org uses the current column.
-   Since the target field is `@11$2`, the current column is column 2.
-   The range being summed will still work if breakfast items are added or removed between the hlines.
-   However, the target `@11$2` still depends on the `Total` row being row 11.

The following version avoids that problem by putting the calculated values in fixed columns on the first data row. Now the formulas do not need to move when more breakfast items are added between the hlines.

```text
| Breakfast    | Cost | Total | Tip (15%) | Grand Total |
|--------------+------+-------+-----------+-------------|
| Eggs         | 2.99 | 19.67 |      2.95 |       22.62 |
| Eggs         | 2.99 |       |           |             |
| Pancakes     | 2.75 |       |           |             |
| Sausage      | 3.25 |       |           |             |
| Orange Juice | 1.00 |       |           |             |
| Grits        | 1.50 |       |           |             |
| Biscuits     | 1.25 |       |           |             |
| Coffee       | 0.25 |       |           |             |
| Tea          | 0.19 |       |           |             |
| Omelet       | 3.50 |       |           |             |
|--------------+------+-------+-----------+-------------|
#+TBLFM: @2$3=vsum(@I$2..@II$2)::@2$4=$-1*0.15;%.2f::@2$5=$-1+$-2;%.2f
```

This layout is a bit artificial, but it is useful because it keeps the formula targets fixed while the list of ordered items grows or shrinks.

Notice:

-   In `@2$3=vsum(@I$2..@II$2)`, we explicitly sum column 2 between the first and second hlines.
    -   The target is `@2$3`, so the total is placed in row 2, column 3.
-   In `@2$4=$-1*0.15;%.2f`, `$-1` means “the value one column to the left of the field currently being calculated.”
    -   Since the field currently being calculated is `@2$4`, `$-1` refers to `@2$3`, the total.
-   In `@2$5=$-1+$-2;%.2f`, `$-1` refers to the tip and `$-2` refers to the total.
    -   There is no need to specify the row on the right-hand side because relative column references like \\$-1 stay in the current row. Since the target is @2\\$4, the current row is row 2.

If we wanted something a bit more natural, we could do this:

```text
|--------------+-------|
| Breakfast    |       |
|--------------+-------|
| Total        | 19.67 |
| Tip          |  2.95 |
| Grand Total  | 22.62 |
|--------------+-------|
| Items        |  Cost |
|--------------+-------|
| Eggs         |  2.99 |
| Eggs         |  2.99 |
| Pancakes     |  2.75 |
| Sausage      |  3.25 |
| Orange Juice |  1.00 |
| Grits        |  1.50 |
| Biscuits     |  1.25 |
| Coffee       |  0.25 |
| Tea          |  0.19 |
| Omelet       |  3.50 |
|--------------+-------|
#+TBLFM: @2$2=vsum(@IIII..@IIIII)::@3$2=@-1*0.15;%.2f::@4$2=@-1+@-2;%.2f
```

The only difficulty might be all those "I's," but unfortunately I don't know what to do about that, other than remove some of the hlines.


## Conclusion {#conclusion}

Org table formulas look strange at first because there are several things happening at once.

A formula has a target, usually on the left-hand side, and an expression, usually on the right-hand side. The target tells Org which field or fields to calculate. The expression tells Org how to calculate them.

The most important idea is context.

References like `$1`, `$-2`, `@-1`, and `@I..@II` are interpreted relative to the field currently being calculated. Once that idea clicks, many of the examples become much less mysterious.

A column formula such as:

```text
#+TBLFM: $3=$1+$2
```

means: for each applicable row, calculate column 3 from columns 1 and 2 in that same row.

A row formula such as:

```text
#+TBLFM: @5=@-4+@-3+@-2+@-1
```

means: for each applicable column, calculate row 5 from the rows above it.

A range such as:

```text
@I$2..@II$2
```

selects a group of fields, and functions like `vsum` can operate on that group.

That is the basic model:

**target**
: where the result goes

**right-hand side**
: how the result is computed

**current field**
: the field Org is calculating right now

**range**
: a group of fields passed to a function

There is much more available: Calc functions, formatting options, Lisp formulas, named fields, remote references, and more. But the examples above are enough to make ordinary Org tables useful.

Once you understand targets, current fields, relative references, and ranges, the notation stops looking like magic and starts looking like a compact way to express the pattern of calculations you want.


## Addendum {#addendum}

In order to whet your appetite for more, here's a table that's a bit more rich:

```text
|       |    T1 |   T2 |    T3 | Total |  Mean | Max |
|-------+-------+------+-------+-------+-------+-----|
| A     |   990 |   20 |   500 |  1510 | 503.3 | 990 |
| B     |   201 |   40 |   600 |   841 | 280.3 | 600 |
| C     |   101 |    3 |   501 |   605 | 201.7 | 501 |
| D     |    12 |    5 |   605 |   622 | 207.3 | 605 |
|-------+-------+------+-------+-------+-------+-----|
| Total |  1304 |   68 |  2206 |       |       |     |
| Mean  | 326.0 | 17.0 | 551.5 |       |       |     |
| Max   |   990 |   40 |   605 |       |       |     |
#+TBLFM: $5=if(@# < 6, vsum($2..$4), string(""))::$6=if(@# < 6, vmean($2..$4), string(""));f1::$7=if(@# < 6, vmax($2..$4), string(""))::@6$2..@6$4=vsum(@I..@II)::@7$2..@7$4=vmean(@I..@II);%.1f::@8$2..@8$4=vmax(@I..@II)
```

The row formulas are:

```text
$5=if(@# < 6, vsum($2..$4), string(""))
$6=if(@# < 6, vmean($2..$4), string(""));f1
$7=if(@# < 6, vmax($2..$4), string(""))
```

These fill in the Total, Mean, and Max columns for the data rows only.

There is something new here that we have not covered. The test `@# < 6` means "only do this for rows before row 6." For the summary rows, the formula returns an empty string.

Notice the formatting on the mean formula:

```text
;f1
```

This displays the mean with one digit after the decimal point while still allowing the empty-string result to remain blank.

<div class="pro-tip">

**Pro Tip:** Org table formulas support different formatting styles. A printf-style format like `;%.1f` works well for ordinary numeric results, but in a conditional formula that sometimes returns an empty string, Calc-style formatting such as `;f1` may preserve the blank cell better.

-   ;%.1f  → stricter numeric formatting, may turn blank into 0.0
-   ;f1    → fixed-point display with one decimal, but friendlier to blank strings

</div>

The summary formulas are:

```text
@6$2..@6$4=vsum(@I..@II)
@7$2..@7$4=vmean(@I..@II);%.1f
@8$2..@8$4=vmax(@I..@II)
```

These calculate totals, means, and maximums for each test column.

When formulas begin to get long and complex, the formula editor becomes necessary:

{{< figure src="/ox-hugo/6formula.png" >}}

Notice that it helpfully divides the formulas according to type, "column formulas" and "field and range formulas." (Not shown, but as you move your cursor on the formula, Emacs will also highlight the fields in the main buffer.)

You do not need to absorb every detail of this formula at once. The point is that the same basic pieces --- targets, ranges, relative references, and functions --- can be combined into useful tables.

[^fn:1]: 'c-h v' and then enter the variable name.
[^fn:2]: I'm tempted to use "formulae," but no. Well...? No.
[^fn:3]: See, [References](https://orgmode.org/manual/References.html) in the Org Manual
