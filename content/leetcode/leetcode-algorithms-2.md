---
title: "Leetcode Algorithms 2"
author: ["Karl Stump"]
date: 2024-11-23
tags: ["CPP", "Leetcode"]
draft: false
---

Okay, this is the second question from the "Top Interview 150."

If you're new here:

Working on [leetcode.com](https:leetcode.com) there's a "Top Interview 150" list of problems. I'm not sure where the list comes from, but no matter. Doing a quick count it looks like there are around 20 hard problems,about 40 easy, and about 90 medium.

So that's the goal. Do all 150 problems.


## Remove Element {#remove-element}

Level: easy

Read the [problem description](https://leetcode.com/problems/remove-element/?envType=study-plan-v2&envId=top-interview-150).


## Starting {#starting}

Here's how I start:

{{< highlight C >}}
#include <vector>
#include <gtest/gtest.h>

class Solution {
public:
  static int removeElement(std::vector<int>& nums, int val) {

      return 0;
    }
};
TEST(Remove, test_1) {

  // Arrange
  std::vector<int> nums = {1, 2, 3, 4};
  int val = 1;
  int results;
  int expected = 0;
  // Act
  results = Solution::removeElement(nums, val);

  // Assert
  EXPECT_TRUE(expected == results);
}

int main(int argc, char **argv) {
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
{{< /highlight >}}

And, I have a simple Makefile:

{{< highlight makefile >}}
OBJS = remove-element.o

remove-element: $(OBJS)
        g++ -o remove-element $(OBJS) -lgtest -lgtest_main

remove-element.o: remove-element.cpp

clean:
        rm remove-element $(OBJS)
{{< /highlight >}}

Now, I can add tests (that is, the examples that are given in the description), and code to the
tests. TDD.

The first of which is:

> Input: nums = [3,2,2,3], val = 3
> Output: 2, nums = [2,2,_,_]
> Explanation: Your function should return k = 2, with the first two elements of nums being 2.
> It does not matter what you leave beyond the returned k (hence they are underscores).

So, the first test case becomes:

{{< highlight C >}}
TEST(Remove, test_1) {

  // Arrange
  std::vector<int> nums = {3, 2, 2, 3};
  int val = 3;
  int returns;
  int expectedReturns = 2;
  std::vector<int> expectedNums = {2, 2, -1, -1};
  // Act
  returns = Solution::removeElement(nums, val);

  // Assert
  EXPECT_TRUE(expectedReturns == returns);
  EXPECT_TRUE(expectedNums == nums);
}
{{< /highlight >}}

Now, when I run, I get the expected failures:

{{< highlight bash >}}
[==========] Running 1 test from 1 test suite.
[----------] Global test environment set-up.
[----------] 1 test from Remove
[ RUN      ] Remove.test_1
remove-element.cpp:24: Failure
Value of: expectedReturns == returns
  Actual: false
Expected: true

remove-element.cpp:25: Failure
Value of: expectedNums == nums
  Actual: false
Expected: true

[  FAILED  ] Remove.test_1 (0 ms)
[----------] 1 test from Remove (0 ms total)

[----------] Global test environment tear-down
[==========] 1 test from 1 test suite ran. (0 ms total)
[  PASSED  ] 0 tests.
[  FAILED  ] 1 test, listed below:
[  FAILED  ] Remove.test_1

 1 FAILED TEST
{{< /highlight >}}


## Final {#final}

{{< highlight C >}}
#include <vector>
#include <gtest/gtest.h>

class Solution {
public:
  static int removeElement(std::vector<int> &nums, int val) {
    /* Scan the array nums for the value val beginning at i = 0.  Let
     k be the location of the last element in the array. When the
     value val is found at the ith location, move the kth value into
     the ith, and then replace the kth value with -1 and decrement k */
    int i = 0;
    int k = nums.size() - 1;

    for (i = 0; i <= k && i < nums.size(); ++i) {
      if (nums[i] == val) {

        while (k > -1 && nums[k] == val) {
          nums[k--] = -1;
        }

        if (k >= i){
          nums[i] = nums[k];
          nums[k--] = -1;
        }
      }
    }
    return k + 1;
  }
};

TEST(Remove, test_1) {

  // Arrange
  std::vector<int> nums = {3, 2, 2, 3};
  int val = 3;
  int returns;
  int expectedReturns = 2;
  std::vector<int> expectedNums = {2, 2, -1, -1};
  // Act
  returns = Solution::removeElement(nums, val);

  // Assert
  EXPECT_TRUE(expectedReturns == returns);
  EXPECT_TRUE(expectedNums == nums);
}

TEST(Remove, test_2) {

  // Arrange
  std::vector<int> nums = {0, 1,2, 2, 3,0,4,2};
  int val = 2;
  int returns;
  int expectedReturns = 5;
  std::vector<int> expectedNums = {0, 1, 4, 0, 3,-1,-1,-1};
  // Act
  returns = Solution::removeElement(nums, val);

  // Assert
  EXPECT_TRUE(expectedReturns == returns);
  EXPECT_TRUE(expectedNums == nums);
}
TEST(Remove, test_3) {

  // Arrange
  std::vector<int> nums = {1};
  int val = 1;
  int returns;
  int expectedReturns = 0;
  std::vector<int> expectedNums = {-1};
  // Act
  returns = Solution::removeElement(nums, val);

  // Assert
  EXPECT_TRUE(expectedReturns == returns);
  EXPECT_TRUE(expectedNums == nums);
}

int main(int argc, char **argv) {
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
{{< /highlight >}}

{{< highlight bash >}}
[==========] Running 3 tests from 1 test suite.
[----------] Global test environment set-up.
[----------] 3 tests from Remove
[ RUN      ] Remove.test_1
[       OK ] Remove.test_1 (0 ms)
[ RUN      ] Remove.test_2
[       OK ] Remove.test_2 (0 ms)
[ RUN      ] Remove.test_3
[       OK ] Remove.test_3 (0 ms)
[----------] 3 tests from Remove (0 ms total)

[----------] Global test environment tear-down
[==========] 3 tests from 1 test suite ran. (0 ms total)
[  PASSED  ] 3 tests.
{{< /highlight >}}


## Use GDB {#use-gdb}

Now's a good time to practice with GDB.

Update the makefile to include debugging information (`-g`). I've also added in `-Wall` &#x2013; not required
but probably a good idea.

{{< highlight makefile >}}
OBJS = remove-element.o

remove-element: $(OBJS)
        g++ -Wall -g -o remove-element $(OBJS) -lgtest -lgtest_main

remove-element.o: remove-element.cpp
        g++ -Wall -g -c remove-element.cpp

clean:
        rm remove-element $(OBJS)
{{< /highlight >}}

And here's GDB in emacs:

{{< figure src="/ox-hugo/use-gdb.png" >}}
