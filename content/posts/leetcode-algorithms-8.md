---
title: "Leetcode Algorithms 8"
author: ["Karl Stump"]
date: 2024-11-24
draft: false
---

Okay, this is the eighth question from the "Top Interview 150."

If you're new here:

Working on [leetcode.com](https:leetcode.com) there's a "Top Interview 150" list of problems. I'm not sure where the list
comes from, but no matter. Doing a quick count it looks like there are around 20 hard problems,about
40 easy, and about 90 medium.

So that's the goal. Do all 150 problems.


## Best Time to Sell Stock II {#best-time-to-sell-stock-ii}

Level: easy.

Read the [description](https://leetcode.com/problems/best-time-to-buy-and-sell-stock-ii/description/?envType=study-plan-v2&envId=top-interview-150) here.

I think the description makes the problem sound more difficult than it is.

What is needed is summing up all the profits along the way.

{{< highlight C >}}
#include <vector>
#include <gtest/gtest.h>

class Solution {
public:
  static int maxProfit2(std::vector<int>& prices) {
    int profit = 0;
    int right = 1;
    int left = 0;
    while (right < prices.size()){
      if (prices[left] < prices[right]){
        profit += prices[right] - prices[left];
        left = right++;
      }else{
        ++left;
        ++right;
      }
    }
    return profit;
  }
};

TEST(MaxProfit2, test_1) {

  // Arrange
  std::vector<int> prices = {7,1,5,3,6,4};
  int returns;
  int expectedReturns = 7 ;
  // Act
  returns = Solution::maxProfit2(prices);

  // Assert
  EXPECT_TRUE(expectedReturns == returns);
}
TEST(MaxProfit2, test_2) {

  // Arrange
  std::vector<int> prices = {1, 2, 3, 4, 5};
  int returns;
  int expectedReturns = 4;
  // Act
  returns = Solution::maxProfit2(prices);

  // Assert
  EXPECT_TRUE(expectedReturns == returns);
}

TEST(MaxProfit2, test_3) {

  // Arrange
  std::vector<int> prices = {7,6,4,3,1};
  int returns;
  int expectedReturns = 0 ;
  // Act
  returns = Solution::maxProfit2(prices);

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
OBJS = max-profit-2.o

max-profit-2: $(OBJS)
        g++ -Wall -g -o max-profit-2 $(OBJS) -lgtest -lgtest_main

max-profit-2.o: max-profit-2.cpp
        g++ -Wall -g -c max-profit-2.cpp

clean:
        rm max-profit-2 $(OBJS)
{{< /highlight >}}

Now, when I run, the tests are passing:

{{< highlight bash >}}
[==========] Running 3 tests from 1 test suite.
[----------] Global test environment set-up.
[----------] 3 tests from MaxProfit2
[ RUN      ] MaxProfit2.test_1
[       OK ] MaxProfit2.test_1 (0 ms)
[ RUN      ] MaxProfit2.test_2
[       OK ] MaxProfit2.test_2 (0 ms)
[ RUN      ] MaxProfit2.test_3
[       OK ] MaxProfit2.test_3 (0 ms)
[----------] 3 tests from MaxProfit2 (0 ms total)

[----------] Global test environment tear-down
[==========] 3 tests from 1 test suite ran. (0 ms total)
[  PASSED  ] 3 tests.
{{< /highlight >}}

{{< figure src="/ox-hugo/complexity-best-time-to-buy-2.png" >}}

Excellent!
