---
title: "Leetcode Algorithms 3"
author: ["Karl Stump"]
date: 2024-11-23
tags: ["CPP", "Leetcode"]
draft: false
---

Okay, this is the third question from the "Top Interview 150."

If you're new here:

Working on [leetcode.com](https:leetcode.com) there's a "Top Interview 150" list of problems. I'm not sure where the list comes from, but no matter. Doing a quick count it looks like there are around 20 hard problems,about 40 easy, and about 90 medium.

So that's the goal. Do all 150 problems.


## Remove Duplicates from Sorted Array {#remove-duplicates-from-sorted-array}

Level: easy

Read the [problem description](https://leetcode.com/problems/remove-duplicates-from-sorted-array/description/?envType=study-plan-v2&envId=top-interview-150).

Here's the code and tests.

{{< highlight C >}}
#include <vector>
#include <gtest/gtest.h>

class Solution {
public:
  static int removeDuplicates(std::vector<int>& nums) {
    int index[nums.size()];
    int indexSize = nums.size();
    int k = 0;

    for (int i = 0; i < nums.size(); i++) {
      if (i == 0) {
        index[k++] = i;
      } else {
        if (nums[i] != nums[i - 1]) {
          index[k++] = i;
        }
      }
    }
    for (int i = 0; i < k; i++) {
      nums[i] = nums[index[i]];
    }
    return k;
  }
};

TEST(RemoveDuplicates, test_1) {

  // Arrange
  std::vector<int> nums = {1, 1, 2};
  std::vector<int> expectedNums = {1, 2, -1};
  int returns;
  int expectedReturns = 2 ;
  // Act
  returns = Solution::removeDuplicates(nums);

  // Assert
  ASSERT_TRUE(returns == expectedReturns);
  for (int i = 0; i < returns; i++) {
    ASSERT_TRUE(nums[i] == expectedNums[i]);
  }
}

TEST(RemoveDuplicates, test_2) {

  // Arrange
  std::vector<int> nums = {0,0,1,1,1,2,2,3,3,4};
  std::vector<int> expectedNums = {0,1,2,3,4,9,9,9,9,9};
  int returns;
  int expectedReturns = 5;
  // Act
  returns = Solution::removeDuplicates(nums);

  // Assert
  ASSERT_TRUE(returns == expectedReturns);
  for (int i = 0; i < returns; i++) {
    ASSERT_TRUE(nums[i] == expectedNums[i]);
  }
}


int main(int argc, char **argv) {
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
{{< /highlight >}}

And, I have a simple Makefile:

{{< highlight makefile >}}
OBJS = remove-duplicates-from-sorted.o

remove-element: $(OBJS)
        g++ -Wall -g -o remove-duplicates-from-sorted $(OBJS) -lgtest -lgtest_main

remove-element.o: remove-duplicates-from-sorted.cpp
        g++ -Wall -g -c remove-duplicates-from-sorted.cpp

clean:
        rm remove-duplicates-from-sorted $(OBJS)
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

{{< figure src="/ox-hugo/remove-duplicates.png" >}}

Cool!
