---
title: "Stock Market Problems (Category 2: At Most One Transaction)"
author: ["Karl Stump"]
date: 2026-01-06
draft: false
math: true
---

In the previous post, I covered the simplest member of the so-called “stock market” interview problems: the version that allows unlimited transactions. Once you strip away the narrative, the solution reduces to summing all positive price deltas.

This post moves to the **second** category. The problem is slightly more interesting, while the algorithm is still easy.


## Problem Description {#problem-description}

```text
You are given an integer array prices where prices[i] is the price of a given stock on the ith day.

You may complete at most one transaction.
You must buy before you sell.

Return the maximum profit you can achieve.
```

The primary constraint is "one transaction" -- which clearly means a buy and eventual sell.


## The Narrative Trap (Again) {#the-narrative-trap--again}

Once again, the story encourages you to think in the wrong terms.

The wording suggests: timing the market, choosing the “best” buy day, choosing the “best” sell day, reasoning about decisions and foresight. And all of this leads to over-thinking and over-analyzing.

But — just as in Category 1 — the stock narrative is mostly irrelevant.


## The Actual Problem {#the-actual-problem}

Stripped of its story, the real requirement is:

```text
Given a sequence of numbers,
find the maximum difference v[j] − v[i]
such that j > i.
```

That’s it.

You are not simulating trades.
You are not modeling decisions.
You are not predicting the future.

You are simply looking for the ****largest upward gap**** where the smaller value appears before the larger one.

This is a classic prefix problem.


## Why Greedy Works Here (But Differently) {#why-greedy-works-here--but-differently}

In Category 1, greediness meant “take every positive slope.”

Here, greediness takes a different form:

-   Track the smallest value seen so far.
-   At each step, compute the profit you **could** make by selling today.
-   Keep the best such profit.

You never need to look ahead.
You never need to reconsider earlier choices.
You only carry two pieces of information forward.


## Pseudocode {#pseudocode}

```text
min_price = infinity
max_profit = 0

for price in prices:
  max_profit = max(max_profit, price - min_price)
  min_price  = min(min_price, price)

return max_profit
```

This is a single-pass, constant-space algorithm.

And it’s worth being very clear: this does **not** model a real trade.

Instead, it performs continuous revision of a _hypothetical_ trade:

-   “If I had bought at the lowest price so far…”
-   “If I sold right now…”
-   “Is this better than any previous hypothetical trade?”

No buy is ever committed.
No sell is ever committed.
No position is ever held.

The algorithm is not executing a trade — it is **evaluating all possible trades** in a single pass.


## Why This Is Not Category 1 {#why-this-is-not-category-1}

It’s tempting to think this is just a restricted version of Category 1 — but that intuition fails.

Consider:

```text
prices = [1, 3, 2, 4]
```

Category 1 (unlimited transactions):

-   (1 → 3) = 2
-   (2 → 4) = 2
-   total = 4

Category 2 (at most one transaction):

-   best is (1 → 4) = 3

The best single transaction **crosses a dip**.

This is the key difference:
with only one allowed interval, it may be worth tolerating temporary losses to reach a better endpoint.

That tradeoff is exactly what Category 1 never needs to consider.


## Scheme Solution {#scheme-solution}

Here is a Scheme implementation:

```lisp
(define (max-profit-once prices)
  (let loop ((ps prices)
             (min-price +inf.0)
             (best 0))
    (if (null? ps)
        best
        (let* ((p (car ps))
               (profit (- p min-price)))
          (loop (cdr ps)
                (min min-price p)
                (max best profit))))))
```

This function carries only:

-   the minimum price seen so far
-   the best profit achievable so far

No state machine.
No search.
No backtracking.


## Recognizing Category 2 Instantly {#recognizing-category-2-instantly}

Ask yourself one question:

```text
Are you allowed at most one transaction?
```

If yes:

-   you are looking for a single maximum rise
-   not the sum of rises
-   not multiple intervals
-   not a state machine

This problem is closer to “maximum difference with order” than anything involving trading.


## A Word of Encouragement {#a-word-of-encouragement}

A large number of competent programmers get stuck on Category 1 and 2 stock problems, even though the algorithms are easy. They don’t fail because they can’t code. They fail because they trust the narrative.

There is a real mismatch between what the problem **pretends** to be about (decision-making over time) and what it **actually** tests (recognizing a numerical pattern).

From the interviewer’s point of view, this problem is a filter: can you strip away irrelevant detail and reason abstractly — or at least recognize a known pattern?

Once you see what’s really going on, it becomes a kind of _cultural shibboleth_. People who have studied algorithms before recognize the structure immediately and produce the (rather trivial) solution.


## Closing Thought {#closing-thought}

Category 1 problems are about **accumulating slopes**.

Category 2 problems are about **finding a single dominant rise**.

The algorithms are easy.

The stock market story doesn’t help — but once you see through it, you can't unsee it.

In the next post, I’ll move on to Category 3, where greediness alone fails and dynamic programming becomes unavoidable.
