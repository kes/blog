---
title: "Leetcode Algorithms 14"
author: ["Karl Stump"]
date: 2024-11-28
tags: ["CPP", "Leetcode"]
draft: false
---

Okay, this is the fourteenth question from the "Top Interview 150."

If you're new here:

Working on [leetcode.com](https:leetcode.com) there's a "Top Interview 150" list of problems. I'm not sure where the list
comes from, but no matter. Doing a quick count it looks like there are around 20 hard problems,about
40 easy, and about 90 medium.

So that's the goal. Do all 150 problems.


## Gas Station {#gas-station}

Level: medium.

Read the [description](https://leetcode.com/problems/h-index/description/?envType=study-plan-v2&envId=top-interview-150) here.


## First Attempt {#first-attempt}

Here's the brute-force method. Unfortunately, worst case, it's O(n<sup>2</sup>).

{{< highlight C >}}
#include <vector>
#include <gtest/gtest.h>

class Solution {
public:
  static int canCompleteCircuit(std::vector<int> &gas, std::vector<int> &cost) {
    int totalGas = 0;
    int totalCost = 0;
    for (int i = 0; i < gas.size(); i++) {
      totalCost += cost[i];
      totalGas += gas[i];
    }
    if (totalCost > totalGas)
      return -1;

    for (int i = 0; i < gas.size(); i++){
      if (gas[i] >= cost[i]) {
        int Jmod = i ;
        int myTank = gas[i];
        bool cycle = false;
        while (cycle != true) {
          int travelNet = myTank - cost[Jmod];
          if (travelNet < 0) {
            // if we cannot make it from i to Jmod, then we cannot
            // make it from anything in-between but Jmod may have
            // wrapped around. In which case we're done.
            if (Jmod < i) return -1;

            // Here, because of the preceeding test, Jmod must be
            // equal to or greater than i; skip all the in-between.
            i = Jmod;
            break;
          }
          myTank = travelNet + gas[(Jmod + 1) % gas.size()];
          // We might wrap
          Jmod = ++Jmod % gas.size();
          // have we cycled
          if (i == Jmod) {
            cycle = true;
            if (myTank >= 0) {
              // description says there is only one solution
              return i;
            }
          }
        }
      }
    }
    return -1;
  }
};

TEST(CanCompeleteCircuit, test_1) {

  // Arrange
  std::vector<int> gas = {1, 2, 3, 4, 5};
  std::vector<int> cost = {3, 4, 5, 1, 2};
  int returns;
  int expectedReturns = 3 ;
  // Act
  returns = Solution::canCompleteCircuit(gas, cost);

  // Assert
  EXPECT_TRUE(expectedReturns == returns) << expectedReturns << " not equal" << returns;
}

TEST(CanCompeleteCircuit, test_2) {

  // Arrange
  std::vector<int> gas = {2,3,4};
  std::vector<int> cost = {3, 4, 3};
  int returns;
  int expectedReturns = -1 ;
  // Act
  returns = Solution::canCompleteCircuit(gas, cost);

  // Assert
  EXPECT_TRUE(expectedReturns == returns) << expectedReturns << " not equal" << returns;
}
TEST(CanCompeleteCircuit, test_3) {

  // Arrange
  std::vector<int> gas = {5,0,0,1,1,50};
  std::vector<int> cost = {1,1,1,1,4,1};
  int returns;
  int expectedReturns = 5;
  // Act
  returns = Solution::canCompleteCircuit(gas, cost);

  // Assert
  EXPECT_TRUE(expectedReturns == returns) << expectedReturns << " not equal" << returns;
}

TEST(CanCompeleteCircuit, test_4) {

  // Arrange
  std::vector<int> gas = {1,1,0,1,1,1};
  std::vector<int> cost = {1,1,1,1,1,1};
  int returns;
  int expectedReturns = -1;
  // Act
  returns = Solution::canCompleteCircuit(gas, cost);

  // Assert
  EXPECT_TRUE(expectedReturns == returns) << expectedReturns << " not equal" << returns;
}

int main(int argc, char **argv) {
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
{{< /highlight >}}


## Second Pass {#second-pass}

The brute-force method isn't that far off.  But we don't have to wrap around. Instead, we'll keep the total gas as a running total.

So, the goals: is there enough gas total, and what is the starting point? Assuming that there is
enough gas, there must be a starting point that will take us to the end of the vector.

{{< highlight c >}}
#include <vector>
#include <gtest/gtest.h>

class Solution {
public:
  static int canCompleteCircuit(std::vector<int> &gas, std::vector<int> &cost) {
    int totalGas = 0;
    int myTank = 0;

    for (int i = 0; i < gas.size(); i++){
      totalGas += gas[i] - cost[i];
      myTank += gas[i] - cost[i];
      if (myTank < 0){
        myTank = 0;
        start = i + 1;

      }
    }
    return (totalGas < 0) ? -1 : start;
  }
};

TEST(CanCompeleteCircuit, test_1) {

  // Arrange
  std::vector<int> gas = {1, 2, 3, 4, 5};
  std::vector<int> cost = {3, 4, 5, 1, 2};
  int returns;
  int expectedReturns = 3 ;
  // Act
  returns = Solution::canCompleteCircuit(gas, cost);

  // Assert
  EXPECT_TRUE(expectedReturns == returns) << expectedReturns << " not equal" << returns;
}

TEST(CanCompeleteCircuit, test_2) {

  // Arrange
  std::vector<int> gas = {2,3,4};
  std::vector<int> cost = {3, 4, 3};
  int returns;
  int expectedReturns = -1 ;
  // Act
  returns = Solution::canCompleteCircuit(gas, cost);

  // Assert
  EXPECT_TRUE(expectedReturns == returns) << expectedReturns << " not equal" << returns;
}
TEST(CanCompeleteCircuit, test_3) {

  // Arrange
  std::vector<int> gas = {5,0,0,1,1,50};
  std::vector<int> cost = {1,1,1,1,4,1};
  int returns;
  int expectedReturns = 5;
  // Act
  returns = Solution::canCompleteCircuit(gas, cost);

  // Assert
  EXPECT_TRUE(expectedReturns == returns) << expectedReturns << " not equal" << returns;
}

TEST(CanCompeleteCircuit, test_4) {

  // Arrange
  std::vector<int> gas = {1,1,0,1,1,1};
  std::vector<int> cost = {1,1,1,1,1,1};
  int returns;
  int expectedReturns = -1;
  // Act
  returns = Solution::canCompleteCircuit(gas, cost);

  // Assert
  EXPECT_TRUE(expectedReturns == returns) << expectedReturns << " not equal" << returns;
}

int main(int argc, char **argv) {
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
{{< /highlight >}}

I have a simple Makefile:

{{< highlight makefile >}}
OBJS = gasStation.o

gasStation: $(OBJS)
        g++ -Wall -g -o gasStation $(OBJS) -lgtest -lgtest_main

gasStation.o: gasStation.cpp
        g++ -Wall -g -c gasStation.cpp

clean:
        rm gasStation $(OBJS)
{{< /highlight >}}

And here are my google test results:

{{< highlight bash >}}
[==========] Running 4 tests from 1 test suite.
[----------] Global test environment set-up.
[----------] 4 tests from CanCompeleteCircuit
[ RUN      ] CanCompeleteCircuit.test_1
[       OK ] CanCompeleteCircuit.test_1 (0 ms)
[ RUN      ] CanCompeleteCircuit.test_2
[       OK ] CanCompeleteCircuit.test_2 (0 ms)
[ RUN      ] CanCompeleteCircuit.test_3
[       OK ] CanCompeleteCircuit.test_3 (0 ms)
[ RUN      ] CanCompeleteCircuit.test_4
[       OK ] CanCompeleteCircuit.test_4 (0 ms)
[----------] 4 tests from CanCompeleteCircuit (0 ms total)

[----------] Global test environment tear-down
[==========] 4 tests from 1 test suite ran. (0 ms total)
[  PASSED  ] 4 tests.
{{< /highlight >}}

{{< figure src="/ox-hugo/complexity-gas-station.png" >}}

Slick!
