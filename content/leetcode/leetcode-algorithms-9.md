---
title: "Leetcode Algorithms 9"
author: ["Karl Stump"]
date: 2024-11-24
tags: ["CPP", "Leetcode"]
draft: false
---

Okay, this is the ninth question from the "Top Interview 150."

If you're new here:

Working on [leetcode.com](https:leetcode.com) there's a "Top Interview 150" list of problems. I'm not sure where the list
comes from, but no matter. Doing a quick count it looks like there are around 20 hard problems,about
40 easy, and about 90 medium.

So that's the goal. Do all 150 problems.


## Jump Game {#jump-game}

Level: medium.

Read the [description](https://leetcode.com/problems/jump-game/description/?envType=study-plan-v2&envId=top-interview-150) here.


### First Try {#first-try}

The problem has an easy solution, but even though it runs in O(n), I was surprised the code below is
not by comparison a great runtime. It only gets:

{{< figure src="/ox-hugo/complexity-canjump-1.png" >}}

This version works by solving increasingly smaller versions of the problem. I have to get to `n`, so,
can I get to `n` from `n-1`. If so, then work on getting to `n-1`. The problem is that you're guaranteed
to touch all items in the vector.

{{< highlight C >}}
#include <vector>
#include <gtest/gtest.h>

class Solution {
public:
  static bool canJump(std::vector<int>& nums) {
    int target = nums.size() - 1;
    int index = target - 1;
    bool makeIt = true;


    while (index >= 0){
      if (index + nums[index] >= target){
        target = index;
        makeIt = true;
      }else{
        makeIt = false;
      }
      --index;
    }
    return makeIt;
  }
};

TEST(CanJump, test_1) {

  // Arrange
  std::vector<int> nums = {2,3,1,1,4};
  int returns;
  int expectedReturns = true ;
  // Act
  returns = Solution::canJump(nums);

  // Assert
  EXPECT_TRUE(expectedReturns == returns);
}
TEST(CanJump, test_2) {

  // Arrange
  std::vector<int> nums = {3,2,1,0,4};
  int returns;
  int expectedReturns = false;
  // Act
  returns = Solution::canJump(nums);

  // Assert
  EXPECT_TRUE(expectedReturns == returns);
}

int main(int argc, char **argv) {
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
{{< /highlight >}}

Here's my makefile:
I have a simple Makefile:

{{< highlight makefile >}}
OBJS = canJump.o

canJump: $(OBJS)
        g++ -Wall -g -o canJump $(OBJS) -lgtest -lgtest_main

canJump.o: canJump.cpp
        g++ -Wall -g -c canJump.cpp

clean:
        rm canJump $(OBJS)
{{< /highlight >}}

And here are my google test results:

{{< highlight bash >}}
[==========] Running 2 tests from 1 test suite.
[----------] Global test environment set-up.
[----------] 2 tests from CanJump
[ RUN      ] CanJump.test_1
[       OK ] CanJump.test_1 (0 ms)
[ RUN      ] CanJump.test_2
[       OK ] CanJump.test_2 (0 ms)
[----------] 2 tests from CanJump (0 ms total)

[----------] Global test environment tear-down
[==========] 2 tests from 1 test suite ran. (0 ms total)
[  PASSED  ] 2 tests.
{{< /highlight >}}


### Second Try {#second-try}

Turns out it's just better to start at the beginning of the array and calculate a maximum reach, where starting at the beginning, you always test for the greatest jump. And if that jump gets to the goal then you're done. An improvement. The key to understanding this approach is that if the index `i` is every greater than the `maxReach` attained then the goal cannot be reached so return false.

{{< highlight C >}}
class Solution {
public:
  bool canJump(vector<int>& nums) {
    int maxReach = 0;
    for (int i = 0; i < nums.size(); i++) {
      if (i > maxReach) return false; // Cannot proceed further
      maxReach = max(maxReach, i + nums[i]);
      if (maxReach >= nums.size() - 1) return true; // Reached or surpassed last index
    }
    return false;
  }
};
{{< /highlight >}}

And that gets the runtime down.

{{< figure src="/ox-hugo/complexity-max-reach.png" >}}

Cool!
