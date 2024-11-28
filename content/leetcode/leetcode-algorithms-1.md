---
title: "Leetcode Algorithms 1"
author: ["Karl Stump"]
date: 2024-11-22
tags: ["CPP", "Leetcode"]
draft: false
---

Working on [leetcode.com](https:leetcode.com) there's a "Top Interview 150" list of problems. I'm not sure where the list comes from, but no matter. Doing a quick count it looks like there are around 20 hard problems,about 40 easy, and about 90 medium.

So that's the goal. Do all 150 problems.


## Merge Sorted Array {#merge-sorted-array}

Level: easy

Read the [problem description](https://leetcode.com/problems/merge-sorted-array/description/?envType=study-plan-v2&envId=top-interview-150).

Here's the code:

{{< highlight C >}}
#include <vector>
#include <gtest/gtest.h>

class Solution {
public:
  static void merge(std::vector<int> &nums1, int m, std::vector<int> &nums2,
                    int n) {

    int i = m - 1; // keeping track of where we are in nums1
    int j = n - 1; // keeping track of where we are in nums2
    int k = m + n - 1; // keeping track of where to insert the next
                       // digit in nums1

    /* The loop is controlled by j because once we've moved all of
       num2 into num1, we're done no matter what i is */
    while (j >= 0) {
      /* Either move the end of nums1[i] or the end of nums2[j] into
         nums1[k] -- and notice, that if i == 0 then just keep moving
         digits from nums2[j] */
      nums1[k--] = i >= 0 && nums1[i] > nums2[j] ?
        nums1[i--] : nums2[j--];
    }
  }
};

TEST(Merge, test_1) {

  // Arrange
  int v1N = 3;
  int v2N = 3;
  std::vector<int> v1 = {1,2,3,0,0,0};
  std::vector<int> v2 = {2, 5, 6};
  std::vector<int> expected = {1,2,2,3,5,6};

  // Act
  Solution::merge(v1, v1N, v2, v2N);

  // Assert
  EXPECT_TRUE(v1 == expected);
}

TEST(Merge, test_2) {

  // Arrange
  int v1N = 1;
  int v2N = 0;
  std::vector<int> v1 = {1};
  std::vector<int> v2 = {};
  std::vector<int> expected = {1};

  // Act
  Solution::merge(v1, v1N, v2, v2N);

  // Assert
  EXPECT_TRUE(v1 == expected);
}

TEST(Merge, test_3) {

  // Arrange
  int v1N = 0;
  int v2N = 1;
  std::vector<int> v1 = {0};
  std::vector<int> v2 = {1};
  std::vector<int> expected = {1};

  // Act
  Solution::merge(v1, v1N, v2, v2N);

  // Assert
  EXPECT_TRUE(v1 == expected);
}

int main(int argc, char **argv) {
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
{{< /highlight >}}

Compile with:

{{< highlight text >}}
g++ merge-sorted-array.cpp -lgtest -lgtest_main
{{< /highlight >}}

Results:

{{< highlight text >}}
[==========] Running 3 tests from 1 test suite.
[----------] Global test environment set-up.
[----------] 3 tests from Merge
[ RUN      ] Merge.test_1
[       OK ] Merge.test_1 (0 ms)
[ RUN      ] Merge.test_2
[       OK ] Merge.test_2 (0 ms)
[ RUN      ] Merge.test_3
[       OK ] Merge.test_3 (0 ms)
[----------] 3 tests from Merge (0 ms total)

[----------] Global test environment tear-down
[==========] 3 tests from 1 test suite ran. (0 ms total)
[  PASSED  ] 3 tests.
{{< /highlight >}}

Time complexity: O(m + n)
