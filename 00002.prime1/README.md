# [PRIME1 - Prime Generator](https://www.spoj.com/problems/PRIME1/)

[#number-theory](https://www.spoj.com/problems/tag/number-theory)

Peter wants to generate some prime numbers for his cryptosystem.  Help him!  Your task is to generate all prime numbers between two given numbers!

## Input

The input begins with the number $t$ of test cases in a single line $(t \le 10)$.  In each of the next $t$ lines there are two numbers $m$ and $n$ $(1 \le m \le n \le 1000000000, n-m \le 100000)$ separated by a space.

## Output

For every test case print all prime numbers $p$ such that $m \le p \le n$, one number per line, test cases separated by an empty line.

## Example

Input:
```
2
1 10
3 5
```

Output:
```
2
3
5
7

3
5
```

**Warning: large Input/Output data, be careful with certain languages (though most should be OK if the algorithm is well designed).**

## Information

After cluster change, please consider [PRINT](https://www.spoj.com/problems/PRINT/) as a more challenging problem.

## Solution

[main.ml](main.ml)
