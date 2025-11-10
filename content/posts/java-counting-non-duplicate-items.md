---
title: "Java: Counting non-Duplicate Items"
author: ["Karl Stump"]
date: 2024-09-12
tags: ["java"]
draft: false
math: true
---

This problem came up at [codewars.com](https://www.codewars.com/kata/635fc0497dadea0030cb7936).

Given the goal to count items in a list, and with the caveat that some of the items will be
duplicated, and these additional duplicates are not to be counted. So, a count of unique items is
what is wanted.

Such problems always make me think of a hash table, or an "associative array."

But there are more details: the items to count are in an array, and the items themselves
are also an array -- so a two dimensional array, something like:

```text
[[1,2,3,4], ...]
```

To be intrepreted as:

\\[\Large \begin{pmatrix} 1 & 2 \\\ 3 & 4 \end{pmatrix}  \\]

So, then the question becomes, in what way are matrices to be regarded as the same?

Matrices are to be regarded as the same, if they match through any "rotations."

What's a rotation?

If we consider that this matrix is at 0 degrees:

\\[ \Large \begin{pmatrix}
1 & 2 \\\\
3 & 4
\end{pmatrix}
\\]

Then, rotated 90 degress (clockwise) would be:

\\[
\Large \begin{pmatrix}
3 & 1 \\\\
4 & 2
\end{pmatrix}
\\]

And, rotated 90 more degrees (180 degrees in total) would be:

\\[
\Large \begin{pmatrix}
4 & 3 \\\\
2 & 1
\end{pmatrix}
\\]

And, again rotated 90 more degrees (270 degrees in total) would be:

\\[
\Large \begin{pmatrix}
2 & 4 \\\\
1 & 3
\end{pmatrix}
\\]

All of these matrices are to be regarded as the same, and counted only once.


## Rotation {#rotation}

My first thought was that I needed to actually rotate an array, moving values around. But a moments reflection
disabuses one of that notion. So, I came up with four groups of comparisons, to determine if two
arrays were the same.

```java
public static boolean same(final int []m1, final int[] m2){
    if (( // 0 degrees
	  m1[0] == m2[0] && m1[1] == m2[1] &&
	  m1[2] == m2[2] && m1[3] == m2[3]
	  ) ||
        ( // 90 degrees
	  m1[0] == m2[1] && m1[1] == m2[3] &&
	  m1[2] == m2[0] && m1[3] == m2[2]
	  ) ||
        ( // 180 degrees
	  m1[0] == m2[3] && m1[1] == m2[2] &&
	  m1[2] == m2[1] && m1[3] == m2[0]
	  ) ||
        ( // 270 degrees
	  m1[0] == m2[2] && m1[1] == m2[0] &&
	  m1[2] == m2[3] && m1[3] == m2[1]
	  ))
        {
            return true;
        }else{
	return false;
    }
}
```

That seems to work quite well.


## Counting non duplicates {#counting-non-duplicates}

Java provides us with a nice way to filter a stream. (Well, Java provides a nice way to stream an
array and then filter it... it all gets down to List Processing, I suppose.) For the predicate of
the filter the `same` method will do nicely. Having filtered the stream, it will be turned back
into an array, and one with all those items deleted.

```java
// let m be: int [][] passed into be counted
int counter = 0;
for( ; 0 < m.length ;){
    counter++;
    int [] testThis = m[0];
    m = Arrays.stream(m)
	.filter(s-> !same(testThis,s))
	.toArray(int[][]::new);
}
return counter;
```

Nice!

In one pass not only do we count (that's just incrementing the counter), but we remove **all** the
duplicates, including the current matrix (the `testThis` one), so none of them will cause any
bother. And so, the array `m` will continue to grow smaller and smaller, until there's nothing in it.

The more duplicates we have the better this approach works. Of course, worst-case ....

Initially, I was doing a loop with a control variable (your typical, `i` or `j)` but then it became
clear that it was not needed. I decided to leave the `for` anyway, although a `while` might be
preferable. The `for` with the first and third expressions omitted does focus one's attention. (And,
as we learned many years ago, the choice between `for` and `while` is arbitrary and a matter of personal
preference.[^fn:1])


## Redoing Same {#redoing-same}

We can implement `same` method as a lambda using a functional interface. `BiPredicate` is a functional
interface (`public interface BiPredicate<T, U>`) that requires a method, `test,` to be implemented. The
`test` method is declared in the BiPredicate interface just as you expect:

`boolean test(T var1, U var2);`

So, defining the lambda means supplying the body of `test`. Of course, you could define your own
interface, but why? These generic functional interfaces are available for just such cases, and so
there's no reason to reinvent the wheel. Here's what the definition looks like:

```java
BiPredicate<int [], int[]> p = (m1,m2) ->
    ( // 0 degrees
      m1[0] == m2[0] && m1[1] == m2[1] &&
      m1[2] == m2[2] && m1[3] == m2[3]
      ) ||
    ( // 90 degrees
      m1[0] == m2[1] && m1[1] == m2[3] &&
      m1[2] == m2[0] && m1[3] == m2[2]
      ) ||
    ( // 180 degrees
      m1[0] == m2[3] && m1[1] == m2[2] &&
      m1[2] == m2[1] && m1[3] == m2[0]
      ) ||
    ( // 270 degrees
      m1[0] == m2[2] && m1[1] == m2[0] &&
      m1[2] == m2[3] && m1[3] == m2[1]
      );
```

And then, the `for` looks like this:

```java
for( ; 0 < matrices.length ;){
    counter++;
    int [] testThis = matrices[0];
    matrices = Arrays.stream(matrices)
	.filter(s-> !p.test(testThis,s))
	.toArray(int[][]::new);
}
```

Of course, the results are the same. But there is some gained clarity and readability.


## Other Possibilities {#other-possibilities}

But we've go some nested loops here don't we?

Filter, as optimized as it may be, will be going through the entire list on each iteration. For
shorter arrays this will not be a problem. And for arrays with a high degree of duplication, and a
greatly shrinking list on each iteration, this will not be a problem. Even so, it's hard to get away
from the fear of \\(\mathcal{O}(n^2)\\). And, well, if that's the best we can do, that's the best we
can do. But it's not.

There is a better way. And it gets back to a hash table. If we could
form a hash from the four rotations of a matrix, then we'd only touch
each matrix one.

So the idea is, get a matrix, form a hash value from the four rotational positions, and save the
hash. For the next matrix, we do the same, get a hash value -- if the hash value matches any previously
saved hash values, then we don't count it. Continue like that through the array.

How to form the hash? We can form a unique hash by considering each of the four places
a power of ten. So, `m[0]` could be 10^0. `m[1]` 10^1, `m[2]` 10^2, and finally, `m[3]` 10^3.

So,

\\[ \Large \begin{pmatrix}
1 & 2 \\\\
3 & 4
\end{pmatrix}
\\]

At zero degree rotation this matrix has a hash value of:

\\( 1 x 10^0 + 2 x 10^1 + 3 x 10^2 + 4 \* 10^3 = 4321 \\)

And, as we rotate (90 degrees) the matrix we get:

\\( 3 x 10^0 + 1 x 10^1 + 4 x 10^2 + 2 \* 10^3 = 2413 \\)

And, again rotating (180 degrees) we get:

\\( 4 x 10^0 + 3 x 10^1 + 2 x 10^2 + 1 \* 10^3 = 1234 \\)

And, finally rotate again (270 degrees) we get:

\\( 2 x 10^0 + 4 x 10^1 + 1 x 10^2 + 3 \* 10^3 = 3142 \\)

So, we can simply take the maximum. All matrices that have the same maximum hash value are the duplicates.

So, like this:

```java
  public static int calculateHash (int[] m){
  // zero degrees
  int hash0 = m[0] + (m[1] * 10) + (m[2] * 100) + (m[3] * 1000);
  // 90 degrees
  int hash90 = m[2] + (m[0] * 10) + (m[3] * 100) + (m[1] * 1000);
  // 180 degrees
  int hash180 = m[3] + (m[2] * 10) + (m[1] * 100) + (m[0] * 1000);
  // 270 degrees
  int hash270 = m[1] + (m[3] * 10) + (m[0] * 100) + (m[2] * 1000);
  int maxHash = Math.max(Math.max(Math.max(hash0, hash90),hash180), hash270);
  return maxHash;
}
```

And then the body of the main function becomes simply:

```java
Set<Integer> set = new HashSet<>();
for(int [] x : matrices){
    set.add(calculateHash(x));
}
System.out.println("Set Count: " + set.size());
```

And of course, set.add does not add duplicates.

Using 25000 randomly generate matrices I compared time. For the filter method:

```text
real	0m0.310s
user	0m0.317s
sys	0m0.087s
```

And for the hash method:

```text
real	0m0.122s
user	0m0.135s
sys	0m0.050s
```

The user row is what you want to look at. And that's a pretty significant difference.

UPDATE: I went ahead and implemented the hash approach in C. The main loop is:

```c
for (int i = 0; i < MSIZE; i++){
  hashValue = calculateHash(matrices[i]);
  if (hashStore[hashValue] == 0){
    counter++;
    hashStore[hashValue] = 1;
  }
 }
```

and timing 25000 randomly generated matrices yields:

```text
real	0m0.008s
user	0m0.008s
sys	0m0.001s
```

... and that's more than "pretty significant." Wow!

Okay, that's (really) enough.

That was fun!

[^fn:1]: , Brian W. Kernighan and Dennis M. Ritchie, <i>The C Programming Language</i>, 2nd ed (Englewood Cliffs, N.J: Prentice Hall, 1988), 14, 60.
