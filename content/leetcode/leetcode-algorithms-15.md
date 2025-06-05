---
title: "Leetcode Algorithms 15"
author: ["Karl Stump"]
date: 2024-11-29
tags: ["CPP", "Leetcode"]
draft: false
---

Okay, this is the fifteenth question from the "Top Interview 150."

If you're new here:

Working on [leetcode.com](https:leetcode.com) there's a "Top Interview 150" list of problems. I'm not sure where the list
comes from, but no matter. Doing a quick count it looks like there are around 20 hard problems,about
40 easy, and about 90 medium.

So that's the goal. Do all 150 problems.


## Candy {#candy}

Level: Hard.

Read the [description](https://leetcode.com/problems/candy/description/?envType=study-plan-v2&envId=top-interview-150) here.

Example: In the table the rating of each child is given, and then the initial candy. Next calculate
the candy for each child moving left to right. Finally, calculate the candy for each child, right to
left. In whichever direction, you are always comparing with the child 'behind.' So when moving
left-to-right, always compare with the child to the left (and the left-most child has no child to
compare against). The same is true for moving right-to-left, where the comparison is made to the
child on the right, and the rightmost child has no child to the right to compare against.

| Rating        | 1 | 0 | 2 |
|---------------|---|---|---|
| Initial Candy | 1 | 1 | 1 |
| Left to Right | 1 | 1 | 2 |
| Right to Left | 2 | 1 | 2 |

Total = 5.

Let's do another:

| Rating        | 1 | 2 | 87 | 87 | 87 | 2 | 1 |
|---------------|---|---|----|----|----|---|---|
| Initial Candy | 1 | 1 | 1  | 1  | 1  | 1 | 1 |
| Left to Right | 1 | 2 | 3  | 1  | 1  | 1 | 1 |
| Right to Left | 1 | 2 | 3  | 1  | 3  | 2 | 1 |

Total = 13.

{{< highlight C >}}
#include <vector>
#include <gtest/gtest.h>

class Solution {
public:
  static int candy(std::vector<int>& ratings) {
    int totalCandy = 0;
    // every child starts with one candy
    std::vector<int> candyPerChild(ratings.size(), 1);

    // from left to right, check the left; notice that the left-most
    // child has nothing to check
    for (int i = 1; i < ratings.size(); i++) {
      if (ratings[i] > ratings[i - 1]) {
        // rating is higher, so
        if (candyPerChild[i] <= candyPerChild[i - 1]){
          candyPerChild[i] = candyPerChild[i-1] + 1;
        }
      }
    }
    // from right to left, check the right; notice that the right-most
    // child has nothing to check
    for (int i = ratings.size() - 2; i >= 0; i--) {
      if (ratings[i] > ratings[i + 1]){
        if (candyPerChild[i] <= candyPerChild[i + 1]){
          candyPerChild[i] = candyPerChild[i + 1] + 1;
        }
      }
    }
    for (int i = 0; i < candyPerChild.size(); i++) {
      totalCandy += candyPerChild[i];
    }
    return totalCandy;
  }
};

TEST(Candy, test_1) {
  // Arrange
  std::vector<int> ratings = {1, 0, 2};
  int returns;
  int expectedReturns = 5 ;
  // Act
  returns = Solution::candy(ratings);

  // Assert
  EXPECT_TRUE(expectedReturns == returns);
}

TEST(Candy, test_2) {
  // Arrange
  std::vector<int> ratings = {1,2,2};
  int returns;
  int expectedReturns = 4 ;
  // Act
  returns = Solution::candy(ratings);

  // Assert
  EXPECT_TRUE(expectedReturns == returns);
}

TEST(Candy, test_3) {
  // Arrange
  std::vector<int> ratings = {1,3,2,2,1};
  int returns;
  int expectedReturns = 7 ;
  // Act
  returns = Solution::candy(ratings);

  // Assert
  EXPECT_TRUE(expectedReturns == returns);
}

TEST(Candy, test_4) {
  // Arrange
  std::vector<int> ratings = {1,2,87,87,87,2,1};
  int returns;
  int expectedReturns = 13 ;
  // Act
  returns = Solution::candy(ratings);

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
OBJS = candy.o

candy: $(OBJS)
        g++ -Wall -g -o candy $(OBJS) -lgtest -lgtest_main

candy.o: candy.cpp
        g++ -Wall -g -c candy.cpp

clean:
        rm candy $(OBJS)
{{< /highlight >}}

And here are my google test results:

{{< highlight bash >}}
[==========] Running 4 tests from 1 test suite.
[----------] Global test environment set-up.
[----------] 4 tests from Candy
[ RUN      ] Candy.test_1
[       OK ] Candy.test_1 (0 ms)
[ RUN      ] Candy.test_2
[       OK ] Candy.test_2 (0 ms)
[ RUN      ] Candy.test_3
[       OK ] Candy.test_3 (0 ms)
[ RUN      ] Candy.test_4
[       OK ] Candy.test_4 (0 ms)
[----------] 4 tests from Candy (0 ms total)

[----------] Global test environment tear-down
[==========] 4 tests from 1 test suite ran. (0 ms total)
[  PASSED  ] 4 tests.
{{< /highlight >}}

{{< figure src="/ox-hugo/complexity-candy.png" >}}

Yum!
