---
layout: post
title: Comparing Erlang, Python, and C
---

Comparing languages just on the basis of their aesthetics and simple
performance aspects is a fun pastime. Erlang, Python, and C can form an
interesting triad when you try to focus them around the same general problems.

I've been slowly but surely working my way through Coursera's [Data Structures
and Algorithms](https://www.coursera.org/specializations/data-structures-algorithms)
specialization track. It's been a good time, serving as a refresher for topics
I haven't touched since Uni and letting me jog my memory on the finer points of
priority queues, various graph traversal methods, and etc. Sadly, I don't
usually need these skills on a regular basis in the real world, but it's a
healthy distraction.

Because my current work has me paying more attention to systems rather than
software, I probably spend more time than I should thinking about the runtime
of my programming solutions, rather than the theory. When I'm solving a problem
with a language, I like thinking about and observing how it writes and
comparing it to how it performs.

Coursera lets you choose from a list of languages for each problem, and my
common choices are C or Python (Erlang, sadly, is not an option). My decision
process is usually this:

1.  Should I try this problem in C?
2.  Try it in C, anyway.
3.  Work out a performance bottleneck or edge case (usually involving types).
4.  If time elapsed > 10 hours or I have to make some weird data structure:
    1.  Well, this is stressful.
    2.  Try it in Python.
5.  Goto 3, as necessary.
6.  Write it in Erlang because writing Python after C makes me feel like a
    hack-fraud.

## Lessons in the trenches

Unlike my Algorithms and Data Structures courses in college, platform
considerations *are* part of the Coursera agenda. Coursera does a good job of
actually making you think about performance in a real, tangible way when
submitting solutions. You have to make sure your code runs within certain,
pre-determined time and memory bounds. At first I was opposed to this -- isn't
it the case that computational analysis should try to divorce us from the
subjective performance of particular systems? After a while I started to
appreciate the lessons it was trying to teach me.

The Coursera time/memory boxing is mostly consistent and implemented well,
providing a real-world window into what it really means, for example, to be
`O(n)` over `O(n^2)`. It makes you recognize the value in things like proper
recursion implementation, the difficulty in managing memory, and the basic
speed characteristics of languages (Coursera time bounds seem to indicate you
can expect C++ to run about 1.5x slower than pure C, which runs about 10x
faster than Ruby, Python, and Javascript).

## C: no batteries included

Here is a very simple routine in C which finds the greatest common whole-number
divisor between two integers using Euler's formula.

```C
/**
 *  gcd
 *
 *  Compute the greatest common divisor of two integers.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
 * Read a multi-entry problem input.
 */
unsigned long long *read_multi_input() {
  unsigned long long *input =
    (unsigned long long *)malloc(sizeof(unsigned long long) * 10);

  // Read initial input.
  char buffer[100];
  fgets(buffer, sizeof(buffer), stdin);

  // Tokenize the input and parse into numbers.
  int i = 0;
  unsigned long long *r_input;
  char *token = strtok(buffer, " ");
  while(token != NULL) {
    // expand input, if needed
    if(i % 10 == 0) {
      r_input = (unsigned long long *)realloc(
         input, sizeof(unsigned long long) * 10);
      input = r_input;
    }

    input[i] = atoi(token);

    token = strtok(NULL, " ");
    i++;
  }

  return input;
}

/*
 * Compute the GCD of two integers.
 */
unsigned long long gcd(unsigned long long n1, unsigned long long n2) {
  unsigned long long result;

  if(n2 == 0) {
    result = n1;
  } else if(n1 == 0) {
    result = n2;
  } else if(n1 < n2) {
    result = gcd(n2 % n1, n1);
  } else {
    result = gcd(n1 % n2, n2);
  }

  return result;
}

int main(int argc, char *argv[]) {
  unsigned long long result;

  // Read input.
  unsigned long long *n = read_multi_input();

  // Compute the answer.
  result = gcd(n[0], n[1]);  

  // Write the output.
   printf("%ld", result);
}
```

There is so much more to consider when writing C that I had completely
forgotten about after years without it. I remembered about pointers and memory,
what I didn't recall were things like:

- Print formatting.
- Number types and large number manipulation.
- Complexity with reading input.
- Constructing proper strings.
- Finding array sizes for looping.
- Losing lots of array manipulation when your arrays *aren't* made up of
  `char`. 

In the end, what really takes up my time is the added overhead of figuring out
how to do so many simple things that C just doesn't provide you out of the box.

### Erlang: the functional approach

Below is the same algorithm in Erlang. In the words of Joe Armstrong:

> "Programs in [functional] languages are considerably shorter than equivalent
programs in imperative languages." [^2]

```erlang
-module(gcd).
-export([
  gcd/2
]).

%%
%% GCD
%%

gcd(A, B) when is_integer(a), is_integer(B), B > A ->
  gcd(B, A);
gcd(A, 0) -> A;
gcd(A, B) -> gcd(A rem B, B).
```

I think Joe was right. Notice how this obfuscates a lot of general mess I had
to do in C (input, typing, etc.).

It's amazing to me that Erlang (and, I'm assuming, most of its syntax) were
implemented in the 1980's and that such a succinct way to express recursive
algorithms was available to programmers, even then. This is a bit of an
over-simplification, however: at the time, Erlang was considered too slow to be
useful until it was re-implemented from Prolog to C [^1], and it's widely known
to be a bad choice for number crunching, in general.

### Python: a nice compromise

Finally, here's Python. This program avoids recursion, which in my experience
performs particularly poorly in the language.

```python
import sys


def gcd(A, B):
    """
    Compute the common divisor of two integers.
    """
    while A > 0 and B > 0:
      if A > B:
        A = A % B
      else:
        B = B % A

    return A if A > 0 else B


gcd(sys.argv[1], sys,argv[2])
```

Here we see the conciseness that Python is known for. Until you want to attempt
recursion (Python does not tail-call optimize), this seems to me like the
perfect harmony of iterative functionality and readability. I've seen quite a
few coworkers pick up Python as a first or subsequent language, and almost
always they are productive with it in a realtively short amount of time.

[^1]: Joe Armstrong. "History of Erlang," in HOPL III: Proceedings of the third ACM SIGPLAN conference on History of programming languages.
[^2]: "Erlang: The Movie" <https://www.youtube.com/watch?v=xrIjfIjssLE>.
