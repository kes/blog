---
title: "Leetcode Algorithms 6"
author: ["Karl Stump"]
date: 2024-11-23
tags: ["CPP", "Leetcode"]
draft: false
---

Okay, this is the sixth question from the "Top Interview 150."

If you're new here:

Working on [leetcode.com](https:leetcode.com) there's a "Top Interview 150" list of problems. I'm not sure where the list comes from, but no matter. Doing a quick count it looks like there are around 20 hard problems,about 40 easy, and about 90 medium.

So that's the goal. Do all 150 problems.


## Rotate Array {#rotate-array}

Level: medium

Read the [problem description](https://leetcode.com/problems/rotate-array/description/?envType=study-plan-v2&envId=top-interview-150).

This is a rotation problem.

I break the array into left and right sides. This might be called a slice. I then concatenate them back together, only reversed.

I make sure to use modulo on the rotation `k`. This "normalizes" `k` into an acceptable value which can
then be used for the split.

Here's the code and tests.

{{< highlight C >}}
#include <vector>
#include <gtest/gtest.h>

class Solution {
public:
  static void rotate(std::vector<int> &nums, int k) {
    int i = k % nums.size();
    if (i == 0)
      return;
    std::vector<int> left(nums.begin(), nums.end() - i);
    std::vector<int> right(nums.end() - i , nums.end());
    right.insert(right.end(), left.begin(), left.end());
    nums.assign(right.begin(), right.end());
  }
};
TEST(Rotate, test_1) {

  // Arrange
  std::vector<int> nums = {1, 2, 3, 4, 5, 6, 7};
  int k = 3;
  std::vector<int> numsExpected = {5, 6, 7, 1, 2, 3, 4};

  // Act
  Solution::rotate(nums, k);

  // Assert
  for (int i = 0; i < nums.size(); i++) {
    ASSERT_TRUE(nums[i] == numsExpected[i]) << "at i = " << i << ": " << nums[i] << " != " << numsExpected[i] << "\n";
  }

};

TEST(Rotate, test_2) {
  // Arrange
  std::vector<int> nums = {-1, -100, 3, 99};
  int k = 2;
  std::vector<int> numsExpected = {3, 99, -1, -100};

  // Act
  Solution::rotate(nums, k);

  // Assert
  for (int i = 0; i < nums.size(); i++) {
    ASSERT_TRUE(nums[i] == numsExpected[i]) << "at i = " << i << ": " << nums[i] << " != " << numsExpected[i] << "\n";
  }

}

TEST(Rotate, test_3) {
  // Arrange
  std::vector<int> nums = {-1};
  int k = 2;
  std::vector<int> numsExpected = {-1};

  // Act
  Solution::rotate(nums, k);

  // Assert
  for (int i = 0; i < nums.size(); i++) {
    ASSERT_TRUE(nums[i] == numsExpected[i]) << "at i = " << i << ": " << nums[i] << " != " << numsExpected[i] << "\n";
  }

}

int main(int argc, char **argv) {
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
{{< /highlight >}}

I have a simple Makefile:

{{< highlight makefile >}}
OBJS = rotate-array.o

rotate-array: $(OBJS)
        g++ -Wall -g -o rotate-array $(OBJS) -lgtest -lgtest_main

rotate-array.o: rotate-array.cpp
        g++ -Wall -g -c rotate-array.cpp
clean:
        rm rotate-array $(OBJS)
{{< /highlight >}}

Now, when I run, the tests are passing:

{{< highlight bash >}}
[==========] Running 3 tests from 1 test suite.
[----------] Global test environment set-up.
[----------] 3 tests from Rotate
[ RUN      ] Rotate.test_1
[       OK ] Rotate.test_1 (0 ms)
[ RUN      ] Rotate.test_2
[       OK ] Rotate.test_2 (0 ms)
[ RUN      ] Rotate.test_3
[       OK ] Rotate.test_3 (0 ms)
[----------] 3 tests from Rotate (0 ms total)

[----------] Global test environment tear-down
[==========] 3 tests from 1 test suite ran. (0 ms total)
[  PASSED  ] 3 tests.
{{< /highlight >}}

{{< figure src="/ox-hugo/complexity-rotate-array.png" >}}

Sweet!
