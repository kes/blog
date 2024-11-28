---
title: "Leetcode Algorithms 11"
author: ["Karl Stump"]
date: 2024-11-26
tags: ["CPP", "Leetcode"]
draft: false
---

Okay, this is the tenth question from the "Top Interview 150."

If you're new here:

Working on [leetcode.com](https:leetcode.com) there's a "Top Interview 150" list of problems. I'm not sure where the list
comes from, but no matter. Doing a quick count it looks like there are around 20 hard problems,about
40 easy, and about 90 medium.

So that's the goal. Do all 150 problems.


## H-Index {#h-index}

Level: medium.

Read the [description](https://leetcode.com/problems/h-index/description/?envType=study-plan-v2&envId=top-interview-150) here.

The problem requires determining an "h-index" from a list of numbers. From the description: a
maximum value of h such that the given researcher has published at least h papers that have each
been cited at least h times.

So, given: citations list of `[3, 0, 6, 1, 5]` we can sort this like this: `[6, 5, 3, 1, 0]`

|           |   |   |   |   |   |
|-----------|---|---|---|---|---|
| Citations | 6 | 5 | 3 | 1 | 0 |
| Index     | 1 | 2 | 3 | 4 | 5 |
| H-index   | 1 | 2 | 3 | 1 | 0 |

How do we calculate the h-index?

So, we have 1 paper with 6 citations. The h-index at this point is can only be 1.  At index 2, we
have two papers with at least 5 citations. Thus, the h-index at this point is 2. At index 3 we have
3 papers with at least 3 citations, thus the h-index is 3. At index 4 we have at 4 papers with at
least 1 citation, thus the h-index at this point is 1. Finally at index 5 we have 5 papers with at
least 0 citations and an h-index of 0. And the h-index we want is the greatest, that is, 3.

At every index, we can calculate an h-index.

If index `<` citations then the h-index is the index number. Otherwise, the h-index is the citation
number.

Let's try another.

So, given: citations list of `[1, 3, 1]` we can sort this like this: `[3, 1, 1]`

|           |   |   |   |
|-----------|---|---|---|
| Citations | 3 | 1 | 1 |
| Index     | 1 | 2 | 3 |
| H-index   | 1 | 1 | 1 |

At index 1, we have 3 citations, so, h-index 1 (1 is less than 3). At index 2 we have 2 papers with at least 1
citation, so h-index of 1. At index 3 we have 3 papers with at least 1 citation, and an h-index
of 1. In the first instance, index 1 is less than citations, so take the index number. In the last two
cases, the index is greater than the citation, so take the citation number.

But what about this case? Given six papers: `[6, 5, 4, 4, 4, 3]`

|           |   |   |   |   |   |   |
|-----------|---|---|---|---|---|---|
| Citations | 6 | 5 | 4 | 4 | 4 | 3 |
| Index     | 1 | 2 | 3 | 4 | 5 | 6 |
| H-index   | 1 | 2 | 3 | 4 | 4 | 3 |

Here the h-index tracks the index number until index 4. When we get to index 5, since 5 is less than
4 we take citation count. And the same is true for index 6. 6 is less than 3, take the citation
number. The greatest h-index is 4, so, that's the h-index we want.

Now what about this:

But what about this case? Given four papers: `[6, 5, 4, 0]`

|           |   |   |   |   |
|-----------|---|---|---|---|
| Citations | 6 | 5 | 4 | 0 |
| Index     | 1 | 2 | 3 | 4 |
| H-index   | 1 | 2 | 3 | 0 |

This is the same as the previous case.

Maybe more interesting would be this:

|           |   |   |   |   |
|-----------|---|---|---|---|
| Citations | 6 | 5 | 4 | 4 |
| Index     | 1 | 2 | 3 | 4 |
| H-index   | 1 | 2 | 3 | 4 |

Where in the last element, we're actually taking the citation number.

Finally, another without comment.

|           |   |   |   |   |   |   |   |   |
|-----------|---|---|---|---|---|---|---|---|
| Citations | 4 | 4 | 4 | 4 | 4 | 3 | 3 | 3 |
| Index     | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 |
| H-index   | 1 | 2 | 3 | 4 | 4 | 3 | 3 | 3 |

So, the basic idea is to sort the list of nums, and then iterate over the list, saving the highest
h-number. And whenever the h-number is less than we can return.

{{< highlight C >}}
#include <algorithm>
#include <vector>
#include <gtest/gtest.h>

class Solution {
public:
  static int hIndex(std::vector<int>& citations) {
    int maxHIndex = -1;
    int hIndex = 0;
    std::sort(citations.begin(), citations.end(), std::greater<int>());
    for (int i = 0; i < citations.size(); i++) {
      if (i + 1 < citations[i])
        hIndex = i + 1;
      else
        hIndex = citations[i];

      if (hIndex <= maxHIndex)
        return maxHIndex;
      else
        maxHIndex = hIndex;
    }

    return maxHIndex;
  }
};

TEST(hIndex, test_1) {

  // Arrange
  std::vector<int> citations = {3,0,6,1,5};
  int returns;
  int expectedReturns = 3 ;
  // Act
  returns = Solution::hIndex(citations);

  // Assert
  EXPECT_TRUE(expectedReturns == returns) << "expected returns = " << expectedReturns << " but got " << returns << "\n";
}

TEST(hIndex, test_2) {

  // Arrange
  std::vector<int> citations = {1,3,1};
  int returns;
  int expectedReturns = 1 ;
  // Act
  returns = Solution::hIndex(citations);

  // Assert
  EXPECT_TRUE(expectedReturns == returns) << "expected returns = " << expectedReturns << "but got " << returns << "\n";
}

TEST(hIndex, test_3) {

  // Arrange
  std::vector<int> citations = {6, 5, 4, 4, 4, 3};
  int returns;
  int expectedReturns = 4 ;
  // Act
  returns = Solution::hIndex(citations);

  // Assert
  EXPECT_TRUE(expectedReturns == returns) << "expected returns = " << expectedReturns << "but got " << returns << "\n";
}

TEST(hIndex, test_4) {

  // Arrange
  std::vector<int> citations = {6, 5, 4, 0};
  int returns;
  int expectedReturns = 3 ;
  // Act
  returns = Solution::hIndex(citations);

  // Assert
  EXPECT_TRUE(expectedReturns == returns) << "expected returns = " << expectedReturns << "but got " << returns << "\n";
}
TEST(hIndex, test_5) {

  // Arrange
  std::vector<int> citations = {6, 5, 4, 4};
  int returns;
  int expectedReturns = 4 ;
  // Act
  returns = Solution::hIndex(citations);

  // Assert
  EXPECT_TRUE(expectedReturns == returns) << "expected returns = " << expectedReturns << "but got " << returns << "\n";
}

TEST(hIndex, test_6) {

  // Arrange
  std::vector<int> citations = {4, 4, 4, 4, 3, 4, 3, 3};
  int returns;
  int expectedReturns = 4 ;
  // Act
  returns = Solution::hIndex(citations);

  // Assert
  EXPECT_TRUE(expectedReturns == returns) << "expected returns = " << expectedReturns << "but got " << returns << "\n";
}

int main(int argc, char **argv) {
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
{{< /highlight >}}

I have a simple Makefile:

{{< highlight makefile >}}
OBJS = hIndex.o

hIndex: $(OBJS)
        g++ -Wall -g -o hIndex $(OBJS) -lgtest -lgtest_main

hIndex.o: hIndex.cpp
        g++ -Wall -g -c hIndex.cpp

clean:
        rm hIndex $(OBJS)
{{< /highlight >}}

And here are my google test results:

{{< highlight bash >}}
[==========] Running 6 tests from 1 test suite.
[----------] Global test environment set-up.
[----------] 6 tests from hIndex
[ RUN      ] hIndex.test_1
[       OK ] hIndex.test_1 (0 ms)
[ RUN      ] hIndex.test_2
[       OK ] hIndex.test_2 (0 ms)
[ RUN      ] hIndex.test_3
[       OK ] hIndex.test_3 (0 ms)
[ RUN      ] hIndex.test_4
[       OK ] hIndex.test_4 (0 ms)
[ RUN      ] hIndex.test_5
[       OK ] hIndex.test_5 (0 ms)
[ RUN      ] hIndex.test_6
[       OK ] hIndex.test_6 (0 ms)
[----------] 6 tests from hIndex (0 ms total)

[----------] Global test environment tear-down
[==========] 6 tests from 1 test suite ran. (0 ms total)
[  PASSED  ] 6 tests.
{{< /highlight >}}

{{< figure src="/ox-hugo/complexity-hindex.png" >}}

Zowie!
