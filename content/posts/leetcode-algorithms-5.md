---
title: "Leetcode Algorithms 5"
author: ["Karl Stump"]
date: 2024-11-23
tags: ["CPP", "Leetcode"]
draft: false
---

Okay, this is the fifth question from the "Top Interview 150."

If you're new here:

Working on [leetcode.com](https:leetcode.com) there's a "Top Interview 150" list of problems. I'm not sure where the list comes from, but no matter. Doing a quick count it looks like there are around 20 hard problems,about 40 easy, and about 90 medium.

So that's the goal. Do all 150 problems.


## Majority Element {#majority-element}

The majority element is the one that appears more than n/2. The problem description doesn't say that
there's only one, but perhaps that's implied by "return the majority element."  And it says that you
can assume that one always exists. Doesn't that mean that I can count the number of times each
integer occurs and just returns the largest?

Level: easy

Read the [problem description](https://leetcode.com/problems/majority-element/description/?envType=study-plan-v2&envId=top-interview-150).

So, it says "easy" &#x2014; and using a hashmap it is easy &#x2013; nothing difficult. But as we'll see there's a better way.


### First Try {#first-try}

Here's the code and tests.

{{< highlight C >}}
#include <vector>
#include <map>
#include <gtest/gtest.h>

class Solution {
public:
  static int majorityElement(std::vector<int>& nums) {
    std::unordered_map<int, int> m;
    int count = 0;
    int maxCount = 0;
    int maxCountsNumber = 0;
    for (int i = 0; i < nums.size(); i++){
        count = (++m[nums[i]]);
          if (count > maxCount){
            maxCount = count;
            maxCountsNumber = nums[i];
          }
    }

    return maxCountsNumber;
  }
};

TEST(MajorityElement, test_1) {

  // Arrange
  std::vector<int> nums = {3, 2, 3};
  int returns;
  int expectedReturns = 3 ;
  // Act
  returns = Solution::majorityElement(nums);

  // Assert
  ASSERT_TRUE(returns == expectedReturns) << "returns: " << returns << "expected: " << expectedReturns << "\n";
}

TEST(MajorityElement, test_2) {

  // Arrange
  std::vector<int> nums = {2, 2, 1, 1, 1, 2, 2};
  int returns;
  int expectedReturns = 2;
  // Act
  returns = Solution::majorityElement(nums);

  // Assert
  ASSERT_TRUE(returns == expectedReturns) << "returns: " << returns << "expected: " << expectedReturns << "\n";
}

int main(int argc, char **argv) {
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
{{< /highlight >}}

I have a simple Makefile:

{{< highlight makefile >}}
OBJS = majority-element.o

majority-element: $(OBJS)
        g++ -Wall -g -o majority-element $(OBJS) -lgtest -lgtest_main

majority-element.o: majority-element.cpp
        g++ -Wall -g -c majority-element.cpp

clean:
        rm majority-element $(OBJS)
{{< /highlight >}}

Now, when I run, the tests are passing:

{{< highlight bash >}}
[==========] Running 2 tests from 1 test suite.
[----------] Global test environment set-up.
[----------] 2 tests from MojorityElement
[ RUN      ] MojorityElement.test_1
[       OK ] MojorityElement.test_1 (0 ms)
[ RUN      ] MojorityElement.test_2
[       OK ] MojorityElement.test_2 (0 ms)
[----------] 2 tests from MojorityElement (0 ms total)

[----------] Global test environment tear-down
[==========] 2 tests from 1 test suite ran. (0 ms total)
[  PASSED  ] 2 tests.
{{< /highlight >}}

So, that's fine. But it's much slower by comparison a proper solution.


### Second Try {#second-try}

The key point to understand is that this is a counting problem. A majority element occurs more than n/2 times. And the problem description says that
we can assume it. And that means essentially that we can count up, and count down on every element, and if we do it right
we'll have the element in O(n).

So, it's called Moore's Voting Algorithm.

It's simple. But it's also neat.

{{< highlight C >}}
#include <vector>
#include <gtest/gtest.h>

class Solution {
public:
  static int majorityElement(std::vector<int>& nums) {
  int candidate = 0, count = 0;
  for (int num : nums) {
    if (count == 0) {
      // The count is the key to setting (or switching) the
      // candidate. Again, we are relying on the fact that there are
      // more, (n/2) more, of one element than any other.
      candidate = num;
    }
    count += (num == candidate) ? 1 : -1;
  }
  return candidate;
  }

};

TEST(MajorityElement, test_1) {

  // Arrange
  std::vector<int> nums = {3, 2, 3};
  int returns;
  int expectedReturns = 3 ;
  // Act
  returns = Solution::majorityElement(nums);

  // Assert
  ASSERT_TRUE(returns == expectedReturns) << "returns: " << returns << "expected: " << expectedReturns << "\n";
}

TEST(MajorityElement, test_2) {

  // Arrange
  std::vector<int> nums = {2, 2, 1, 1, 1, 2, 2};
  int returns;
  int expectedReturns = 2;
  // Act
  returns = Solution::majorityElement(nums);

  // Assert
  ASSERT_TRUE(returns == expectedReturns) << "returns: " << returns << "expected: " << expectedReturns << "\n";
}

int main(int argc, char **argv) {
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
{{< /highlight >}}

So, that works, and it's O(n).

{{< figure src="/ox-hugo/complexity-majority-element.png" >}}

Yeah, that's fast.

Cool!
