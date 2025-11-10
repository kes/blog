---
title: "C Programming with ChatGPT"
author: ["Karl Stump"]
date: 2025-11-08
tags: ["C", "programming", "ChatGPT"]
draft: false
math: true
---

I wanted to experiment with `ChatGPT` and programming. So, I thought I would just start with an exercise from Kernighan and Ritchie's _The C Programming Language_.

I prompted `ChatGPT`  with the following:

> In C write a program called detab that replaces tabs in the input with the proper number of spaces to reach the next tabstop. Assume tab stops every eight columns. This is exercise 1.20 from The C Programming Language by Kernighan and Ritchie. I would also like some sort of tests (not sure what suites or frameworks are available).

And I got the following reply:

> Love this exercise. Here’s a clean, portable detab implementation (C17), plus a lightweight test harness you can run with plain Bash. Tab stops default to every 8 columns, and you can optionally override with -t

In the few weeks that I've been using `ChatGPT` I have been surprised by responses a number of times. It's difficult not to anthropomorphize many of them. I have shared some of the responses with non-technical friends who make an apt observation: "It's like talking to a friend, only they're really, really, smart!" I have to laugh at that, and yes, I agree.  The "Love this exercise," seems to emphasize familiarity with the exercise, perhaps the entire book -- I will use this familiarity later.

What code came back? The following:

```C
// detab.c — replace tabs with spaces to the next tab stop (default width 8)
// Usage: ./detab            (tab width = 8)
//        ./detab -t 4       (tab width = 4)
//
// Reads from stdin, writes to stdout.

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

static int parse_tabwidth(int argc, char **argv) {
    int tabw = 8;
    if (argc == 3 && strcmp(argv[1], "-t") == 0) {
        char *end = NULL;
        long v = strtol(argv[2], &end, 10);
        if (!end || *end != '\0' || v <= 0 || v > 256) {
            fprintf(stderr, "Invalid tab width: %s\n", argv[2]);
            exit(EXIT_FAILURE);
        }
        tabw = (int)v;
    } else if (argc != 1) {
        fprintf(stderr, "Usage: %s [-t width]\n", argv[0]);
        exit(EXIT_FAILURE);
    }
    return tabw;
}

int main(int argc, char **argv) {
    int tabw = parse_tabwidth(argc, argv);
    int c;
    int col = 0; // current column, 0-based

    while ((c = getchar()) != EOF) {
        if (c == '\t') {
            int spaces = tabw - (col % tabw);
            for (int i = 0; i < spaces; ++i) {
                if (putchar(' ') == EOF) {
                    perror("putchar");
                    return EXIT_FAILURE;
                }
            }
            col += spaces;
        } else {
            if (putchar(c) == EOF) {
                perror("putchar");
                return EXIT_FAILURE;
            }
            if (c == '\n') {
                col = 0;
            } else if (c == '\r') {
                // Conservative: treat CR as carriage return (column reset)
                col = 0;
            } else {
                col += 1;
            }
        }
    }

    if (ferror(stdin)) {
        perror("getchar");
        return EXIT_FAILURE;
    }
    return EXIT_SUCCESS;
}
```

Utterly fantastic. What else? A `Makefile`, of course.

```makefile
CC      := gcc
CFLAGS  := -std=c17 -O2 -Wall -Wextra -Wpedantic

.PHONY: all clean test
all: detab

detab: detab.c
	$(CC) $(CFLAGS) -o $@ $<

clean:
	rm -f detab

test: detab
	./tests/run.sh
```

Perfect! What about testing? The following `bash` test harness was created.

```bash
  #!/usr/bin/env bash
set -euo pipefail

pass=0
fail=0

run_case() {
  local name="$1"
  local args="${2:-}"
  local input="$3"
  local expect="$4"

  local out
  out="$(printf "%b" "$input" | ./detab $args)"
  if [[ "$out" == "$(printf "%b" "$expect")" ]]; then
    printf "✅ %s\n" "$name"
    ((pass++))
  else
    printf "❌ %s\n" "$name"
    printf "   expected: %q\n" "$expect"
    printf "   got     : %q\n" "$out"
    ((fail++))
  fi
}

# 1) Basic: single tab expands to 8-space boundary
run_case "basic: 'a\\tb'" "" \
  "a\tb\n" \
  "a       b\n"   # 'a' at col 0 -> tab adds 7 spaces

# 2) Tab at column boundary (col 8) -> expands to full width
# "abcdefgh" = 8 chars; next stop is 16, so 8 spaces.
run_case "tab at boundary -> 8 spaces" "" \
  "abcdefgh\tZ\n" \
  "abcdefgh        Z\n"

# 3) Multiple tabs in a row
run_case "multiple tabs" "" \
  "\t\tX\n" \
  "                X\n"  # two tabs from col 0 -> 16 spaces

# 4) Mixed text and tabs
run_case "mixed" "" \
  "ab\tcd\te\n" \
  "ab      cd      e\n"  # 'ab' col 2 -> 6 spaces, then 'cd' col 10 -> 6 spaces

# 5) Custom width: -t 4
run_case "custom width -t 4" "-t 4" \
  "a\tb\tc\n" \
  "a   b   c\n"

# 6) Newline resets column count
run_case "newline reset" "" \
  "1234\t\nX\tY\n" \
  "1234    \nX       Y\n"

# 7) Carriage return (optional handling)
run_case "CR resets column" "" \
  "ABC\r\tZ\n" \
  "ABC        Z\n"

printf "\nSummary: %d passed, %d failed\n" "$pass" "$fail"
[[ $fail -eq 0 ]]
```

And helpfully, `ChatGPT` also gave me a, "How to Run:"

```bash
mkdir -p tests
# save detab.c, Makefile, tests/run.sh as shown
chmod +x tests/run.sh
make
make test
```

`ChatGPT` also gave me a "Notes and Gotchas" section:

> Notes &amp; gotchas
>
> Column math: spaces = tabWidth - (col % tabWidth). Reset col on \n (and optionally \r).
>
> Non-printing/backspace: The K&amp;R exercise doesn’t require special handling for backspace or multibyte characters. The implementation treats every non-newline byte (including UTF-8 bytes) as one column; that’s fine for the exercise.
>
> Custom tab width: Optional -t N flag if you want flexibility; default is the classic 8.

And,

> Testing frameworks (optional)
>
> If you want a C unit test framework:
>
> Unity or cmocka are lightweight and easy to vend into a repo.
>
> For this specific tool, the Bash golden tests above are usually the simplest and clearest: pipe input → compare expected output.
>
> If you’d like, I can convert the Bash tests into a CUnit/cmocka test suite, or wrap the tests with ctest (CMake) — but the current Makefile + Bash keeps dependencies minimal.

While I like the bash test harness, I did not feel that it was best to go in that direction. So, after some discussion with `ChatGPT`, I opted to go with `cmocka` (and admittedly, it was a bit of a "flip o' the coin"). This required some changes to the Makefile and the source, which, if interested you can see my repository. Here are the resulting tests:

```C
#include <stdarg.h>
#include <stddef.h>
#include <setjmp.h>
#include <cmocka.h>
#include <string.h>
#include "detab.h"

static void test_basic_single_tab(void **state) {
    (void)state;
    char out[128];
    detab_line("a\tb\n", out, 8);
    assert_string_equal("a       b\n", out);
}

static void test_tab_at_boundary(void **state) {
    (void)state;
    char out[128];
    detab_line("abcdefgh\tZ\n", out, 8);
    assert_string_equal("abcdefgh        Z\n", out);
}

static void test_multiple_tabs(void **state) {
    (void)state;
    char out[128];
    detab_line("\t\tX\n", out, 8);
    assert_string_equal("                X\n", out);
}

static void test_mixed(void **state) {
    (void)state;
    char out[128];
    detab_line("ab\tcd\te\n", out, 8);
    assert_string_equal("ab      cd      e\n", out);
}

static void test_custom_width_4(void **state) {
    (void)state;
    char out[128];
    detab_line("a\tb\tc\n", out, 4);
    assert_string_equal("a   b   c\n", out);
}

static void test_cr_resets_column(void **state) {
    (void)state;
    char out[128];
    detab_line("ABC\r\tZ\n", out, 8);
    assert_string_equal("ABC\r        Z\n", out);
}

int main(void) {
    const struct CMUnitTest tests[] = {
        cmocka_unit_test(test_basic_single_tab),
        cmocka_unit_test(test_tab_at_boundary),
        cmocka_unit_test(test_multiple_tabs),
        cmocka_unit_test(test_mixed),
        cmocka_unit_test(test_custom_width_4),
        cmocka_unit_test(test_cr_resets_column),
    };
    return cmocka_run_group_tests(tests, NULL, NULL);
}
```

This was so easy, the code, the makefile, the tests, that I decide to do the next problem in the `K&R` book. Counting on `ChaptGPT`'s familiarity with the problems in the `K&R` book, here's the prompt I used:

> Using a similiar code architecture and testing with cmocka, let's do the next exercise in the book, The C Programming language. Exercise 1.21. Do you know this exercise, or should I paste it in?

And here's the response:

> Great prompt—yes, Exercise 1.21 is entab: replace runs of blanks with the minimum number of tabs and spaces to get the same alignment (assume tab stops every 8 columns). Below is a drop-in implementation that mirrors the detab architecture you just built: a pure function (entab_line) + a tiny CLI wrapper + cmocka tests + Makefile targets.

I take it that with the "Great prompt" that `ChatGPT` is indicating that it knows all the exercises in the book. Amazing.

From here on, I won't go into a lot of details, just things that stood aout to me. If interested you can see all the details in the repository.

Additional property tests were requested, and this resulted in the following response:

> Love it—property-style tests are perfect here. We’ll add a cmocka test module that generates randomized lines and checks invariants that should always hold for any string under `detab_line` and `entab_line`.

"Love it" -- amazing. What a neat response!

And then the following details from `ChatGPT`:

> **Chosen invariants**
>
> 1.  Round-trip visual equivalence via detab:
>     For any input s, expanding tabs to spaces once gives a concrete, canonical layout:
>
>     \\( D(s) = detab\_line(s) \\)
>
>     If we entab and then detab again, the fully expanded view should be identical:
>
>     \\( detab\_line(entab\_line(D(s))) == D(s) \\)
>
>     (Equivalently: \\( detab(entab(detab(s))) == detab(s) \\) . )
>     This asserts that entab_line preserves alignment and detab_line is idempotent on already-detabbed strings.
>
> 2.  No tabs after detab:
>     Output of `detab_line` must not contain `'\t'`.
>
> 3.  Optional sanity: bounded growth
>     `entab_line` never increases the count of spaces for runs that cross tab stops when using same tab width and no literal \t present (not strictly required, so we won’t assert it globally—just FYI).
>
> Below is a new `cmocka` test file plus `Makefile` additions. It reuses your existing detab_lib.c and entab_lib.c (no CLI/main linked).

And more details from `ChatGPT`:

> **Why these properties are strong**
>
> 1.  \\( D(E(D(s))) == D(s) \\) is a robust alignment invariant: anything entab does must be lossless with respect to the “visual layout” that detab defines. If this ever fails, either `entab` emitted an incorrect mix of tabs/spaces, or `detab` miscomputed column movement.
>
> 2.  No tabs after `detab` confirms that detab truly expands.
>
> 3.  `Idempotence` ensures `detab` is stable on tab-free input.

And to be sure, `idempotence` is: for some function \\( f(x) \\), then   \\[ f(f(x))=f(x) \\]

This is totally fantastic. Again, if you want the details see the repository.

After all is said and done, here is a run of the tests:

```text
make test
./test_cmocka_detab
[==========] tests: Running 6 test(s).
[ RUN      ] test_basic_single_tab
[       OK ] test_basic_single_tab
[ RUN      ] test_tab_at_boundary
[       OK ] test_tab_at_boundary
[ RUN      ] test_multiple_tabs
[       OK ] test_multiple_tabs
[ RUN      ] test_mixed
[       OK ] test_mixed
[ RUN      ] test_custom_width_4
[       OK ] test_custom_width_4
[ RUN      ] test_cr_resets_column
[       OK ] test_cr_resets_column
[==========] tests: 6 test(s) run.
[  PASSED  ] 6 test(s).
./test_cmocka_entab
[==========] tests: Running 7 test(s).
[ RUN      ] test_exact_8_spaces_to_tab
[       OK ] test_exact_8_spaces_to_tab
[ RUN      ] test_after_char_to_tab
[       OK ] test_after_char_to_tab
[ RUN      ] test_two_runs
[       OK ] test_two_runs
[ RUN      ] test_short_spaces_no_tab
[       OK ] test_short_spaces_no_tab
[ RUN      ] test_custom_width_4
[       OK ] test_custom_width_4
[ RUN      ] test_cr_resets
[       OK ] test_cr_resets
[ RUN      ] test_literal_tabs_passthrough
[       OK ] test_literal_tabs_passthrough
[==========] tests: 7 test(s) run.
[  PASSED  ] 7 test(s).
./test_cmocka_props
[==========] tests: Running 3 test(s).
[ RUN      ] prop_roundtrip_equivalence
[       OK ] prop_roundtrip_equivalence
[ RUN      ] prop_detab_no_tabs
[       OK ] prop_detab_no_tabs
[ RUN      ] prop_detab_idempotent
[       OK ] prop_detab_idempotent
[==========] tests: 3 test(s) run.
[  PASSED  ] 3 test(s).
```

Quite productive!
