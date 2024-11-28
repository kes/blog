---
title: "Leetcode Algorithms 4"
author: ["Karl Stump"]
date: 2024-11-23
tags: ["CPP", "Leetcode"]
draft: false
---

Okay, this is the fourth question from the "Top Interview 150."

If you're new here:

Working on [leetcode.com](https:leetcode.com) there's a "Top Interview 150" list of problems. I'm not sure where the list comes from, but no matter. Doing a quick count it looks like there are around 20 hard problems,about 40 easy, and about 90 medium.

So that's the goal. Do all 150 problems.


## Remove Duplicates from Sorted Array II {#remove-duplicates-from-sorted-array-ii}

Level: medium

Read the [problem description](https://leetcode.com/problems/remove-duplicates-from-sorted-array-ii/description/?envType=study-plan-v2&envId=top-interview-150).

The key difference between this problem and the previous remove duplicates problem is that you're
not permitted to use an auxiliary array.

> Do not allocate extra space for another array. You must do this by modifying the input array
> in-place with O(1) extra memory.

Yike! Gulp!

That's okay. Where there's a will, there's a way.

Here's the code and tests.

{{< highlight C >}}
#include <vector>
#include <gtest/gtest.h>

class Solution {
public:
  static int removeDuplicates(std::vector<int>& nums) {
    int k = 0;
    int previous = 0;
    bool discarding = false;

    for (int i = 0; i < nums.size(); i++) {
      if (i == 0){
        previous = nums[i];
        ++k;
      }else{
        if (nums[i] == previous){
          if (! discarding){
            nums[k++] = nums[i];
            discarding = true;
          }
        }else{
          nums[k++] = nums[i];
          previous = nums[i];
          discarding = false;
        }
      }
    }
    return k ;
  }
};

TEST(RemoveDuplicates, test_1) {

  // Arrange
  std::vector<int> nums = {1,1,1,2,2,3};
  std::vector<int> expectedNums = {1,1,2,2,3,-9};
  int returns;
  int expectedReturns = 5 ;
  // Act
  returns = Solution::removeDuplicates(nums);

  // Assert
  ASSERT_TRUE(returns == expectedReturns) << "returns: " << returns << "expected: " << expectedReturns << "\n";
  for (int i = 0; i < returns; i++) {
    ASSERT_TRUE(nums[i] == expectedNums[i]) << "at i = " << i << ": " << nums[i] << " != " << expectedNums[i] << "\n";
  }
}

TEST(RemoveDuplicates, test_2) {

  // Arrange
  std::vector<int> nums = {0,0,1,1,1,1,2,3,3};
  std::vector<int> expectedNums = {0,0,1,1,2,3,3,-9,-9};
  int returns;
  int expectedReturns = 7;
  // Act
  returns = Solution::removeDuplicates(nums);

  // Assert
  ASSERT_TRUE(returns == expectedReturns);
  for (int i = 0; i < returns; i++) {
    ASSERT_TRUE(nums[i] == expectedNums[i]) << "at i = " << i << ": " << nums[i] << " != " << expectedNums[i] << "\n";
  }
}


int main(int argc, char **argv) {
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
{{< /highlight >}}

I have a simple Makefile:

{{< highlight makefile >}}
OBJS = remove-duplicates-from-sorted-2.o

remove-duplicates-from-sorted-2: $(OBJS)
        g++ -Wall -g -o remove-duplicates-from-sorted-2 $(OBJS) -lgtest -lgtest_main

remove-duplicates-from-sorted-2.o: remove-duplicates-from-sorted-2.cpp
        g++ -Wall -g -c remove-duplicates-from-sorted-2.cpp

clean:
        rm remove-duplicates-from-sorted-2 $(OBJS)
{{< /highlight >}}

Now, when I run, the tests are passing:

{{< highlight bash >}}
[==========] Running 2 tests from 1 test suite.
[----------] Global test environment set-up.
[----------] 2 tests from RemoveDuplicates
[ RUN      ] RemoveDuplicates.test_1
[       OK ] RemoveDuplicates.test_1 (0 ms)
[ RUN      ] RemoveDuplicates.test_2
[       OK ] RemoveDuplicates.test_2 (0 ms)
[----------] 2 tests from RemoveDuplicates (0 ms total)

[----------] Global test environment tear-down
[==========] 2 tests from 1 test suite ran. (0 ms total)
[  PASSED  ] 2 tests.
{{< /highlight >}}

![](/ox-hugo/complexity.png)]]

0 ms is good.
