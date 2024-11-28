---
title: "Leetcode Algorithms 7"
author: ["Karl Stump"]
date: 2024-11-24
tags: ["CPP", "Leetcode"]
draft: false
---

Okay, this is the seventh question from the "Top Interview 150."

If you're new here:

Working on [leetcode.com](https:leetcode.com) there's a "Top Interview 150" list of problems. I'm not sure where the list
comes from, but no matter. Doing a quick count it looks like there are around 20 hard problems,about
40 easy, and about 90 medium.

So that's the goal. Do all 150 problems.


## Best Time to Buy and Sell Stock {#best-time-to-buy-and-sell-stock}

Level: easy

Read the [problem description](https://leetcode.com/problems/best-time-to-buy-and-sell-stock/description/?envType=study-plan-v2&envId=top-interview-150).

Suppose we have a list of daily stock prices: `(7, 1, 5, 3, 6, 4)`.

And we want to find what "buy" and "sell" days would give the largest profits. For example, suppose
we buy when the price is 7, and then sell the next day, at 1, giving a profit of -6, which is a
loss. Can we do better? What's the highest profit we can derive from this list of daily stock prices?

Here's a table showing profits for the given day purchased and sold. So, regarding the row labeled 7,
imagine that you purchase on the day the price is 7, and then working across that row, each column
shows the profit for the given sale price, which prices are occurring on successive days.

|   | 7 | 1  | 5  | 3  | 6  | 4  |
|---|---|----|----|----|----|----|
| 7 | x | -6 | -2 | -4 | -1 | -3 |
| 1 |   | x  | 4  | 2  | 5  | 3  |
| 5 |   |    | x  | -2 | 1  | -1 |
| 3 |   |    |    | x  | 3  | 1  |
| 6 |   |    |    |    | x  | -2 |
| 4 |   |    |    |    |    | x  |

It's easy to see that purchasing on the first day at a price of 7 does not lead to any
profit. The most you can do is limit your losses by selling at price 6 for a mere -1 loss.

But iterating over the list, that cannot possibly be known up-front.

And that idea, that "we cannot know," leads to the idea that there is nothing to do but
"brute-force," calculate all the profit-loss values in the table, something like this pseudo-code:

{{< highlight C >}}
int prices[] = {7, 1, 5, 3, 6, 4};
int max_profit = 0;
int profit = 0;
int buy = 0;
for (int i = 0; < prices.size(); i++){
  buy = prices[i];
  for (int j = i + 1; j < prices.size() ; j++){
    profit = prices[j] - buy ;
    if (profit > max_profit){
      max_profit = profit;
    }
  }
 }
{{< /highlight >}}

We can make an interesting chart, pretty self-explanatory:

| Outside Iteration Step | Inside Iterations | Running Total | Potential Max Profit Found |
|------------------------|-------------------|---------------|----------------------------|
| 1                      | 5                 | 5             | -1                         |
| 2                      | 4                 | 9             | 5                          |
| 3                      | 3                 | 12            | 1                          |
| 4                      | 2                 | 15            | 3                          |
| 5                      | 1                 | 16            | -2                         |

And you can see, at step 5, we already are at 16 iterations &#x2013; and that's O(n<sup>2</sup>) &#x2013; as we expected
from such nested loops. In other words, the search space for finding the maximum profit is going to
grow enormous very fast as the list size grows.

Is there a better way?

In fact, there is. And we can find it easily, if we are able to grasp one simple concept.

On successive days, if we encounter a price that is lower than our current buy price, then it must
be the case that any future prices (future sell prices) **must** create a greater delta (the profit)
using this lower buy price, than with the higher buy price. Now, note that this future potential
profit will not necessarily be greater than what has been found previously. And I suppose missing
this point is a bit of a hiccup. It is not being said that all future profits using this lower price
will be greater than a previously found profit. Only that future calculated profits must be greater using
this lower price than the higher price.

This above point is obvious, after all, but easy to overlook.

But there's an even greater significance, also easy to overlook, which is that using this observation, we can reduce our
search space from O(n<sup>2</sup>) to O(n). Amazing.

Here's the code and tests.

Notice, there is only one loop. So, O(n) nirvana.

{{< highlight C >}}
#include <vector>
#include <gtest/gtest.h>

class Solution {
public:
  static int maxProfit(std::vector<int>& prices) {
    int profit = 0;
    int buy = prices[0];
    for (auto sell = prices.begin() + 1 ; sell != prices.end(); sell++){
      if (*sell > buy)
        profit = std::max(profit, *sell - buy);
      else
        buy = *sell;
    }
    return profit;
  }
};

TEST(MaxProfit, test_1) {

  // Arrange
  std::vector<int> prices = {7,1,5,3,6,4};
  int returns;
  int expectedReturns = 5 ;
  // Act
  returns = Solution::maxProfit(prices);

  // Assert
  EXPECT_TRUE(expectedReturns == returns);
}
TEST(MaxProfit, test_2) {

  // Arrange
  std::vector<int> prices = {7,6,4,3,1};
  int returns;
  int expectedReturns = 0 ;
  // Act
  returns = Solution::maxProfit(prices);

  // Assert
  EXPECT_TRUE(expectedReturns == returns);
}



int main(int argc, char **argv) {
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
{{< /highlight >}}

I have a simple Makefile:

{{< highlight makefile >}}
OBJS = max-profit.o

remove-element: $(OBJS)
        g++ -Wall -g -o max-profit $(OBJS) -lgtest -lgtest_main

remove-element.o: max-profit.cpp
        g++ -Wall -g -c max-profit.cpp

clean:
        rm max-profit $(OBJS)
{{< /highlight >}}

Now, when I run, the tests are passing:

{{< highlight bash >}}
[==========] Running 2 tests from 1 test suite.
[----------] Global test environment set-up.
[----------] 2 tests from MaxProfit
[ RUN      ] MaxProfit.test_1
[       OK ] MaxProfit.test_1 (0 ms)
[ RUN      ] MaxProfit.test_2
[       OK ] MaxProfit.test_2 (0 ms)
[----------] 2 tests from MaxProfit (0 ms total)

[----------] Global test environment tear-down
[==========] 2 tests from 1 test suite ran. (0 ms total)
[  PASSED  ] 2 tests.
{{< /highlight >}}

{{< figure src="/ox-hugo/complexity-buy-sell.png" >}}

Heck, yeah!
