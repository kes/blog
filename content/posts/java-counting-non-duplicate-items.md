---
title: "Java: Counting non-Duplicate Items"
author: ["Karl Stump"]
date: 2024-09-12
tags: ["java"]
draft: false
---

This problem came up at [codewars.com](https://www.codewars.com/kata/635fc0497dadea0030cb7936).

Given the goal to count items in a list, and with the caveat that some of the items will be
duplicated, and these additional duplicates are not to be counted. So, a count of unique items is
what is wanted.

Such problems always make me think of a hash table, or an "associative array."

But there are more details: the items to count are in an array, and the items themselves
are also an array &#x2013; so a two dimensional array, something like:

```text
[[1,2,3,4], ...]
```

To be intrepreted as:

<img src="/ltximg/whatisalambda_d1e5692a115d7e2848cbdb27a0786fa743b5fdea.png" alt="\(\LARGE\begin{pmatrix}
1 &amp;amp; 2 \\
3 &amp;amp; 4
\end{pmatrix}
\)" />

So, then the question becomes, in what way are matrices to be regarded as the same?

Matrices are to be regarded as the same, if they match through any "rotations."

What's a rotation?

If we consider that this matrix is at 0 degrees:

<img src="/ltximg/whatisalambda_5cad1313cdd8087f556b46806908dbdf2490ee6e.png" alt="\(\LARGE\begin{pmatrix}
1 &amp;amp; 2 \\
3 &amp;amp; 4
\end{pmatrix}
\)" />

Then, rotated 90 degress (clockwise) would be:

<img src="/ltximg/whatisalambda_d126427ad023c839a760bfe1c92da76c9026347a.png" alt="\(
\LARGE\begin{pmatrix}
3 &amp;amp; 1 \\
4 &amp;amp; 2
\end{pmatrix}
\)" />

And, rotated 90 more degrees (180 degrees in total) would be:

<img src="/ltximg/whatisalambda_6eca06b3f1d714024f610b0696f467c760039cd8.png" alt="\(
\LARGE\begin{pmatrix}
4 &amp;amp; 3 \\
2 &amp;amp; 1
\end{pmatrix}
\)" />

And, again rotated 90 more degrees (270 degrees in total) would be:

<img src="/ltximg/whatisalambda_33c366941654885450c502307deec958524de0de.png" alt="\(
\LARGE\begin{pmatrix}
2 &amp;amp; 4 \\
1 &amp;amp; 3
\end{pmatrix}
\)" />

All of these matrices are to be regarded as the same, and counted only once.


## Rotation {#rotation}

My first thought was that I needed to actually rotate an array, moving values around. But a moments reflection
disabuses one of that notion. So, I came up with four groups of comparisons, to determine if two
arrays were the same.

{{< highlight java >}}
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
{{< /highlight >}}

That seems to work quite well.


## Counting non duplicates {#counting-non-duplicates}

Java provides us with a nice way to filter a stream. (Well, Java provides a nice way to stream an
array and then filter it&#x2026; It all gets down to LISt Processing, I suppose.) For the predicate of
the filter the `same` method will do nicely. Having filtered the stream, it will be turned back
into an array, and one with all those items deleted.

{{< highlight java >}}
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
{{< /highlight >}}

Nice!

In one pass not only do we count (that's just incrementing the counter), but we remove **all** the
duplicates, including the current matrix (the `testThis` one), so none of them will cause any
bother. And so, the array `m` will continue to grow smaller and smaller, until there's nothing in it.

Initially, I was doing a loop with a control variable (your typical, `i` or `j)` but then it became
clear that it was not needed. I decided to leave the `for` anyway, although a `while` might be
preferable. The `for` with the first and third expressions omitted does focus one's attention.

As we learned many years ago, the choice between `for` and `while` is arbitrary and a matter of personal
preference.[^fn:1]

That was fun!

[^fn:1]: , Brian W. Kernighan and Dennis M. Ritchie, <i>The C Programming Language</i>, 2nd ed (Englewood Cliffs, N.J: Prentice Hall, 1988), 14, 60.
