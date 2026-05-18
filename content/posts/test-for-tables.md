---
title: "Test Tables"
author: ["Karl Stump"]
date: 2026-01-06
draft: false
math: true
---

Do I get nothing?


## A Table {#a-table}

<style>

.my-table-2 table {
  /* background-color: #132d16;*/   /* chalkboard */
  border-collapse: collapse;
  margin: 0 auto;
  font-size: 0.95rem;
  border: 0;
  width: 90%;


}

.my-table-2 td,
.my-table-2 th {
  /* color: #f7f7e5;              chalk */
  padding: 6px 10px;
  text-align: center;
  border: 0;
}
.my-table-2 .table-caption {
  caption-side: bottom;
  text-align: center;
}
</style>

<style>.my-table-2 table { width: 50%;  }</style>

<div class="ox-hugo-table my-table-2">
<div class="table-caption">
  <span class="table-number">Table 1:</span>
  Table with verbatim CSS
</div>

| \\(2 \times \cdots \\)                           |                        | Result                                                      |                                                                         |
|--------------------------------------------------|------------------------|-------------------------------------------------------------|-------------------------------------------------------------------------|
| \\( \dfrac{17}{7 \times 13} \\)                  | \\(\longrightarrow \\) | \\( \dfrac{2 \times 17}{7 \times 13} \\)                    | \\( \bbox[5px, border:2px solid green]{\unicode{x274c}} \\)             |
| \\( \dfrac{2 \times 3 \times 13}{5 \times 17}\\) | \\(\longrightarrow \\) | \\( \dfrac{2^2 \times 3 \times 13}{5 \times 17} \\)         | \\( \bbox[5px, border:2px solid green]{\unicode{x274c}} \\)             |
| \\( \dfrac{19}{3 \times 17} \\)                  | \\(\longrightarrow \\) | \\(  \dfrac{ 2 \times 19}{3 \times 17}\\)                   | \\( \bbox[5px, border:2px solid green]{\unicode{x274c}} \\)             |
| \\( \dfrac{23}{2 \times 19} \\)                  | \\(\longrightarrow \\) | \\( \dfrac{ \cancel{2} \times 23}{\cancel{2} \times 19} \\) | \\( \bbox[5px, border:2px solid green]{\unicode{x274c}} \\)             |
| \\( \dfrac{29}{3 \times 11} \\)                  | \\(\longrightarrow \\) | \\( \dfrac{ 2 \times 29}{3 \times 11} \\)                   | \\( \bbox[5px, border:2px solid green]{\unicode{x274c}} \\)             |
| \\( \dfrac{7 \times 11}{29} \\)                  | \\(\longrightarrow \\) | \\( \dfrac{ 2 \times 7 \times 11}{29} \\)                   | \\( \bbox[5px, border:2px solid green]{\unicode{x274c}} \\)             |
| \\( \dfrac{5 \times 19}{23} \\)                  | \\(\longrightarrow \\) | \\( \dfrac{ 2 \times 5 \times 19}{23} \\)                   | \\( \bbox[5px, border:2px solid green]{\unicode{x274c}} \\)             |
| \\( \dfrac{7 \times 11}{19} \\)                  | \\(\longrightarrow \\) | \\( \dfrac{ 2 \times 7 \times 11}{19} \\)                   | \\( \bbox[5px, border:2px solid green]{\unicode{x274c}} \\)             |
| \\( \dfrac{1}{17}           \\)                  | \\(\longrightarrow \\) | \\( \dfrac{ 2 \times 1}{17} \\)                             | \\( \bbox[5px, border:2px solid green]{\unicode{x274c}} \\)             |
| \\( \dfrac{11}{13}          \\)                  | \\(\longrightarrow \\) | \\( \dfrac{ 2 \times 11}{13} \\)                            | \\( \bbox[5px, border:2px solid green]{\unicode{x274c}} \\)             |
| \\( \dfrac{13}{11}          \\)                  | \\(\longrightarrow \\) | \\( \dfrac{ 2 \times 13}{11} \\)                            | \\( \bbox[5px, border:2px solid green]{\unicode{x274c}} \\)             |
| \\( \dfrac{3 \times 5}{2}   \\)                  | \\(\longrightarrow \\) | \\( \dfrac{ \cancel{2} \times 3 \times 5}{\cancel{2}} \\)   | \\( \bbox[5px, border:2px solid green;color:green]{\unicode{x2714}} \\) |

</div>

how is that?

<style>.table-nocaption table { width: 50%;  }</style>

<div class="ox-hugo-table table-nocaption">

| 1 |
|---|
| 2 |
| 3 |

</div>
