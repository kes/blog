---
title: "Leetcode Algorithms 10"
author: ["Karl Stump"]
date: 2024-11-25
tags: ["CPP", "Leetcode"]
draft: false
---

Okay, this is the tenth question from the "Top Interview 150."

If you're new here:

Working on [leetcode.com](https:leetcode.com) there's a "Top Interview 150" list of problems. I'm not sure where the list
comes from, but no matter. Doing a quick count it looks like there are around 20 hard problems,about
40 easy, and about 90 medium.

So that's the goal. Do all 150 problems.


## Jump Game II {#jump-game-ii}

Level: medium.

Read the [description](https://leetcode.com/problems/jump-game-ii/description/?envType=study-plan-v2&envId=top-interview-150) here.

The solution requires creating "windows," which I prefer to call islands. The key is to jump from island to island. The size of any next island is defined by the greatest distance you can jump from your current island.

You always start on your the first island, so with `i ​= 0` and you're on the island `nums[i]`, with its
left and right boundaries equal to zero. The next island is defined by the boundaries `left ​= right +
1` and `right ​=` nums[i]=. On the next island, you'll have to search that island now for the largest
jump, and set the boundaries of the next island appropriately, as in `left ​= right + 1` and `right ​=
largest jump`

Yeah, islands are better than windows.

{{< highlight C >}}
#include <vector>
#include <gtest/gtest.h>
class Solution {
public:
  static int jump(std::vector<int>& nums) {
    int leftIndex = 0;
    int rightIndex = 0;
    int mostDistantIndex = 0;
    int jumpCount = 0;
    while (rightIndex < nums.size() - 1){
      mostDistantIndex = 0;

      for (int i = leftIndex; i < rightIndex + 1; i++)
        mostDistantIndex = std::max(mostDistantIndex, i + nums[i]);

      leftIndex = rightIndex + 1;
      rightIndex = mostDistantIndex;
      ++jumpCount;
    }

    return jumpCount;
  }
};

TEST(Jump, test_1) {

  // Arrange
  std::vector<int> nums = {2,3,1,1,4};
  int returns;
  int expectedReturns = 2 ;
  // Act
  returns = Solution::jump(nums);

  // Assert
  EXPECT_TRUE(expectedReturns == returns);
}
TEST(Jump, test_2) {

  // Arrange
  std::vector<int> nums = {2, 3, 0, 1, 4};
  int returns;
  int expectedReturns = 2;
  // Act
  returns = Solution::jump(nums);

  // Assert
  EXPECT_TRUE(expectedReturns == returns);
}
TEST(Jump, test_3) {

  // Arrange
  std::vector<int> nums = {1,1,1,1,1,1};
  int returns;
  int expectedReturns = 5 ;
  // Act
  returns = Solution::jump(nums);

  // Assert
  EXPECT_TRUE(expectedReturns == returns);
}
TEST(Jump, test_4) {

  // Arrange
  std::vector<int> nums = {0};
  int returns;
  int expectedReturns = 0;
  // Act
  returns = Solution::jump(nums);

  // Assert
  EXPECT_TRUE(expectedReturns == returns);
}
TEST(Jump, test_5) {

  // Arrange
  std::vector<int> nums = {1};
  int returns;
  int expectedReturns = 0;
  // Act
  returns = Solution::jump(nums);

  // Assert
  EXPECT_TRUE(expectedReturns == returns);
}

TEST(Jump, test_6) {
  // Arrange
  std::vector<int> nums = {7,0,9,6,9,6,1,7,9,0,1,2,9,0,3};
  int returns;
  int expectedReturns = 2;
  // Act
  returns = Solution::jump(nums);

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
OBJS = canJump2.o

canJump2: $(OBJS)
        g++ -Wall -g -o canJump2 $(OBJS) -lgtest -lgtest_main

canJump2.o: canJump2.cpp
        g++ -Wall -g -c canJump2.cpp

clean:
        rm canJump2 $(OBJS)
{{< /highlight >}}

And here are my google test results:

{{< highlight bash >}}
[==========] Running 6 tests from 1 test suite.
[----------] Global test environment set-up.
[----------] 6 tests from Jump
[ RUN      ] Jump.test_1
[       OK ] Jump.test_1 (0 ms)
[ RUN      ] Jump.test_2
[       OK ] Jump.test_2 (0 ms)
[ RUN      ] Jump.test_3
[       OK ] Jump.test_3 (0 ms)
[ RUN      ] Jump.test_4
[       OK ] Jump.test_4 (0 ms)
[ RUN      ] Jump.test_5
[       OK ] Jump.test_5 (0 ms)
[ RUN      ] Jump.test_6
[       OK ] Jump.test_6 (0 ms)
[----------] 6 tests from Jump (0 ms total)

[----------] Global test environment tear-down
[==========] 6 tests from 1 test suite ran. (0 ms total)
[  PASSED  ] 6 tests.
{{< /highlight >}}

{{< figure src="/ox-hugo/complexity-fewest-jumps.png" >}}

Island-jumping good!
