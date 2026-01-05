---
title: "Stock Market Problems"
author: ["Karl Stump"]
date: 2026-01-04
tags: ["programming", "scheme"]
draft: false
math: true
---

Coding challenges themed around the "stock market" are extremely common. They appear frequently in interview settings and on sites like LeetCode, and they range from trivial to genuinely subtle.

In this post, I’ll focus on the **first** and **easiest** variant in this family of problems.

The solution is trivial — but only once you understand what the problem is **actually** asking.


## Problem Description {#problem-description}

```text
You are given an integer array prices where prices[i] is the price of a given stock on the ith day. On each day, you may decide to buy and/or sell the stock. You can only hold at most one share of the stock at any time. However, you can sell and buy the stock multiple times on the same day, ensuring you never hold more than one share of the stock.

Find and return the maximum profit you can achieve.

You’re allowed to:
- buy and sell as many times as you want
- never hold more than 1 share
- even sell and buy again on the same day
```


## The Narrative Is the Problem {#the-narrative-is-the-problem}

The problem description is distracting — and arguably deceptive.

It has almost nothing to do with stocks. It’s not trading, it’s not finance, and it’s not a simulation of any real market behavior. Even the ideas of “buy” and “sell” are largely fake. They exist purely as narrative scaffolding for a family of algorithmic problems.

That would be fine — **if the narrative didn’t actively push you in the wrong direction**.

Given the wording, a conscientious programmer is likely to think in terms of:

-   searching for valleys and peaks
-   simulating decisions over time
-   tracking state
-   optimizing when to buy and sell
-   reasoning about future versus present

You might even start thinking in terms of search problems -- something closer to a knight’s tour than a simple greedy algorithm.

The narrative encourages you to:

-   overthink the problem
-   invent unnecessary algorithms
-   assume stock-like constraints
-   reason about holding periods
-   treat the problem as decision optimization

And because money and markets carry emotional weight, it’s easy to mentally frame this as something "real":

-   "I must learn to trade"
-   "I must optimize decisions"
-   "I must maximize profit"
-   "I must be a smart investor"

It **feels** like a real-world problem that deserves careful modeling.

None of that is helpful here.


## The Actual Problem {#the-actual-problem}

Here is the real requirement, stripped of its story:

```text
Examine a sequence of numbers.
If the slope between adjacent values is positive, accumulate it.
```

That’s it.

Once you see this, the entire problem collapses into something almost embarrassingly simple.

The key fact is:

```text
The maximum profit is just the sum of all positive day-to-day price increases.
```

In other words, for every adjacent pair where the price goes up, take the difference and add it to the total.

Pseudocode:

```text
profit = 0
for i from 0 to n-2:
  if prices[i+1] > prices[i]:
    profit += prices[i+1] - prices[i]
return profit
```

There is no strategy.
There is no foresight.
There is no optimization beyond summing local gains.

For this variant of the problem, you don’t need to be clever — you simply harvest every upward slope.


## Identifying This Class of Problems {#identifying-this-class-of-problems}

This is the first and simplest category in the family of stock-themed problems.

The **only** question you need to ask is:

```text
Does the problem allow unlimited transactions?
```

Look for phrases like:

-   "You may buy and sell as many times as you want."
-   "Multiple transactions are allowed."
-   "You must sell before buying again."
-   "You can buy and sell on the same day."

If the answer is **yes**, then this is a ****Category 1 problem****:

```text
Sum all positive price deltas.
```

No dynamic programming.
No state machine.
No search.
Just a greedy pass through the data.


## A Scheme Solution {#a-scheme-solution}

Below is a Scheme implementation. A few comments are included for readers unfamiliar with the language.

```lisp
(define (max-profit prices)
  (if (< (length prices) 2)
      0                                  ; fewer than two prices → no profit
      (let ((rev (reverse prices)))      ; process adjacent pairs
        (let loop ((lst rev)
                   (profit 0))
          (if (= (length lst) 1)
              profit                     ; done
              (let* ((current (car lst))
                     (next (cadr lst))
                     (delta (- current next)))
                (if (> delta 0)
                    (loop (cdr lst) (+ profit delta))
                    (loop (cdr lst) profit))))))))
```

---


## Closing Thought {#closing-thought}

Once you remove the narrative, this problem stops being about "stocks" and starts being about **slopes**.

That’s the pattern to recognize -- and once you do, an entire class of interview problems becomes trivial.

In future posts, I’ll look at the harder variants, where the narrative **still** lies to you — but the underlying structure genuinely changes.
