---
title: "Leetcode Algorithms 13"
author: ["Karl Stump"]
date: 2024-11-26
tags: ["CPP", "Leetcode"]
draft: false
math: true
---

Okay, this is the thirteenth question from the "Top Interview 150."

If you're new here:

Working on [leetcode.com](https:leetcode.com) there's a "Top Interview 150" list of problems. I'm not sure where the list
comes from, but no matter. Doing a quick count it looks like there are around 20 hard problems,about
40 easy, and about 90 medium.

So that's the goal. Do all 150 problems.


## Product of Array, 'cept Self {#product-of-array-cept-self}

Level: medium.

Read the [description](https://leetcode.com/problems/h-index/description/?envType=study-plan-v2&envId=top-interview-150) here.

Given: `[1,2,3,4]` we return, `[24,12,8,6]`. How does this work?

<style>.table-1 table { text-align: center;  width: 75%;  margin: 0 auto;  }</style>

<div class="ox-hugo-table table-1">

| Index | Calculation | Return         |
|-------|-------------|----------------|
| 0     | 2 \* 3 \* 4 | [24]           |
| 1     | 1 \* 3 \* 4 | [24, 12]       |
| 2     | 1 \* 2 \* 4 | [24, 12, 8]    |
| 3     | 1 \* 2 \* 3 | [24, 12, 8, 6] |

</div>

If we could take the product of the entire list, and then divide by the excluded number, that would
be best.

But the description **forbids** the use of division.

Let's try another: `[-1, 1, 0, -3, 3]`

<style>.table-2 table { text-align: center;  width: 75%;  margin: 0 auto;  }</style>

<div class="ox-hugo-table table-2">

| Index | Calculation        | Return          |
|-------|--------------------|-----------------|
| 0     | 1 \* 0 \* -3 \* 3  | [0]             |
| 1     | -1 \* 0 \* -3 \* 3 | [0, 0]          |
| 2     | -1 \* 1 \* -3 \* 3 | [0, 0, 9]       |
| 3     | -1 \* 1 \* 0 \* 3  | [0, 0, 9, 0]    |
| 4     | -1 \* 1 \* 0 \* -3 | [0, 0, 9, 0, 0] |

</div>

That last is an interesting one. If there's a zero in the list, everything will be zero
except where the zero is excluded.

So, maybe this is a left and right problem. Calculate the product for all numbers in the vector up
to but not including the `ith`. Then calculate the product for all numbers from (but not including)
the `ith` to the end. Finally, calculate the produce of the right and left.

<style>.table-3 table { text-align: center;  width: 100%;  margin: 0 auto;  }</style>

<div class="ox-hugo-table table-3">

| Index | Left               | Right             | Calculation | Return           |
|-------|--------------------|-------------------|-------------|------------------|
| 0     | 1 (Default)        | 1 \* 0 \* -3 \* 3 | 1 \* 0      | [0]              |
| 1     | -1                 | 0 \* -3 \* 3      | -1 \* 0     | [0, 0]           |
| 2     | -1 \* 1            | -3 \* 3           | -1 \* -9    | [0, 0, -9]       |
| 3     | -1 \* 1 \* 0       | 3                 | 0 \* 3      | [0, 0, -9, 0]    |
| 4     | -1 \* 1 \* 0 \* -3 | 1 (Default)       | 0 \* 1      | [0, 0, -9, 0, 0] |

</div>

Let's try `[1, 2, 3, 4, 5]`

<style>.table-4 table { text-align: center;  width: 100%;  margin: 0 auto;  }</style>

<div class="ox-hugo-table table-4">

| Index | Left             | Right            | Calculation | Return                |
|-------|------------------|------------------|-------------|-----------------------|
| 0     | 1 (Default)      | 2 \* 3 \* 4 \* 5 | 1 \* 120    | [120]                 |
| 1     | 1                | 3 \* 4 \* 5      | 1 \* 60     | [120, 60]             |
| 2     | 1 \* 2           | 4 \* 5           | 2 \* 20     | [120, 60, 40]         |
| 3     | 1 \* 2 \* 3      | 5                | 6 \* 5      | [120, 60, 40, 30]     |
| 4     | 1 \* 2 \* 3 \* 4 | 1 (Default)      | 24 \* 1     | [120, 60, 40, 30, 24] |

</div>

But try as you might, there's no way to avoid O(n^2).

The no-division requirement is artificial. Is there some way around that?

There is the identity: \\( a/b = a \* b^{-1} \\)

So, first calculate the total and then for each `i`, push onto a list \\( total \* v[i]^{-1} \\) There will be
the case of having a zero. But that won't be hard to catch.

```C
#include <vector>
#include <cmath>
#include <gtest/gtest.h>

class Solution {
public:
  static std::vector<int> productExceptSelf(std::vector<int>& nums) {

    int total = 1;
    int zerosFound = 0;
    for(std::size_t i = 0; i < nums.size(); i++) {
      if (nums[i] == 0){
	zerosFound++;
      } else {
	total *= nums[i];
      }
    }
    // we expect only one zero, but it's not in the constraints, so if
    // more than one zero, everything is zero
    if (zerosFound > 1) {
      for(std::size_t i=0; i<nums.size(); i++) {
	nums[i] = 0;
      }
      return nums;
    }
    // from here on out we have no more than one zero
    for(std::size_t i=0; i<nums.size(); i++) {
      if (nums[i] == 0) {
        // found a zero
	nums[i] = total;
      }else{
	if (zerosFound == 1){
	  nums[i] = 0;
	}else{
	  nums[i] = total * pow(nums[i], -1);
	}
      }
    }
    return nums;
  }
};

TEST(ProductExceptSelf, test_1) {

  // Arrange
  std::vector<int> nums = {1, 2, 3, 4};
  std::vector<int> returns;
  std::vector<int> expectedReturns = {24, 12, 8, 6};
  // Act
  returns = Solution::productExceptSelf(nums);

  // Assert
  EXPECT_TRUE(expectedReturns == returns);
}

TEST(ProductExceptSelf, test_2) {

  // Arrange
  std::vector<int> nums = {-1, 1, 0, -3, 3};
  std::vector<int> returns;
  std::vector<int> expectedReturns = {0, 0, 9, 0, 0};
  // Act
  returns = Solution::productExceptSelf(nums);

  // Assert
  EXPECT_TRUE(expectedReturns == returns);
}


int main(int argc, char **argv) {
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
```

I have a simple Makefile:

```makefile
OBJS = productExceptSelf.o

productExceptSelf: $(OBJS)
	g++ -Wall -g -o productExceptSelf $(OBJS) -lgtest -lgtest_main

productExceptSelf.o: productExceptSelf.cpp
	g++ -Wall -g -c productExceptSelf.cpp

clean:
	rm productExceptSelf $(OBJS)
```

And here are my google test results:

```bash
[==========] Running 2 tests from 1 test suite.
[----------] Global test environment set-up.
[----------] 2 tests from ProductExceptSelf
[ RUN      ] ProductExceptSelf.test_1
[       OK ] ProductExceptSelf.test_1 (0 ms)
[ RUN      ] ProductExceptSelf.test_2
[       OK ] ProductExceptSelf.test_2 (0 ms)
[----------] 2 tests from ProductExceptSelf (0 ms total)

[----------] Global test environment tear-down
[==========] 2 tests from 1 test suite ran. (0 ms total)
[  PASSED  ] 2 tests.
```

Runtime?

{{< figure src="/ox-hugo/complexity-product-except-self.png" >}}

Yowsers!
