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

The more duplicates we have the better this approach works. Of course, worst-case &#x2026;.

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

{{< highlight java >}}
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
{{< /highlight >}}

And then, the `for` looks like this:

{{< highlight java >}}
for( ; 0 < matrices.length ;){
    counter++;
    int [] testThis = matrices[0];
    matrices = Arrays.stream(matrices)
        .filter(s-> !p.test(testThis,s))
        .toArray(int[][]::new);
}
{{< /highlight >}}

Of course, the results are the same. But there is some gained clarity and readability.


## Other Possibilities {#other-possibilities}

But we've go some nested loops here don't we?

Filter, as optimized as it may be, will be going through the entire list on each iteration. For
shorter arrays this will not be a problem. And for arrays with a high degree of duplication, and a
greatly shrinking list on each iteration, this will not be a problem. Even so, it's hard to get away
from the fear of <img src="/ltximg/whatisalambda_c0079969b7cdf0d140bd75815ecca9f116ee8fa5.png" alt="\(\mathcal{O}(n^2)\)" />. And, well, if that's the best we can do, that's the best we
can do. But it's not.

There is a better way. And it gets back to a hash table. If we could
form a hash from the four rotations of a matrix, then we'd only touch
each matrix one.

So the idea is, get a matrix, form a hash value from the four rotational positions, and save the
hash. For the next matrix, we do the same, get a hash value &#x2013; if the hash value matches any previously
saved hash values, then we don't count it. Continue like that through the array.

How to form the hash? We can form a unique hash by considering each of the four places
a power of ten. So, `m[0]` could be 10<sup>0</sup>. `m[1]` 10<sup>1</sup>, `m[2]` 10<sup>2</sup>, and finally, `m[3]` 10<sup>3</sup>.

So,

<img src="/ltximg/whatisalambda_d1e5692a115d7e2848cbdb27a0786fa743b5fdea.png" alt="\(\LARGE\begin{pmatrix}
1 &amp;amp; 2 \\
3 &amp;amp; 4
\end{pmatrix}
\)" />

At zero degree rotation this matrix has a hash value of:

1 x 10<sup>0</sup> + 2 x 10<sup>1</sup> + 3 x 10<sup>2</sup> + 4 \* 10<sup>3</sup> = 4321

And, as we rotate (90 degrees) the matrix we get:

3 x 10<sup>0</sup> + 1 x 10<sup>1</sup> + 4 x 10<sup>2</sup> + 2 \* 10<sup>3</sup> = 2413

And, again rotating (180 degrees) we get:

4 x 10<sup>0</sup> + 3 x 10<sup>1</sup> + 2 x 10<sup>2</sup> + 1 \* 10<sup>3</sup> = 1234

And, finally rotate again (270 degrees) we get:

2 x 10<sup>0</sup> + 4 x 10<sup>1</sup> + 1 x 10<sup>2</sup> + 3 \* 10<sup>3</sup> = 3142

So, we can simply take the maximum. All matrices
that have the same maximum hash value are the duplicates.

So, like this:

{{< highlight java >}}
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
{{< /highlight >}}

And then the body of the main function becomes simply:

{{< highlight java >}}
Set<Integer> set = new HashSet<>();
for(int [] x : matrices){
    set.add(calculateHash(x));
}
System.out.println("Set Count: " + set.size());
{{< /highlight >}}

And of course, set.add does not add duplicates.

Using 25000 randomly generate matrices I compared time. For the filter method:

{{< highlight text >}}
real	0m0.310s
user	0m0.317s
sys	0m0.087s
{{< /highlight >}}

And for the hash method:

{{< highlight text >}}
real	0m0.122s
user	0m0.135s
sys	0m0.050s
{{< /highlight >}}

The user row is what you want to look at. And that's a pretty significant difference.

Okay, that's enough.

That was fun!

[^fn:1]: , Brian W. Kernighan and Dennis M. Ritchie, <i>The C Programming Language</i>, 2nd ed (Englewood Cliffs, N.J: Prentice Hall, 1988), 14, 60.
