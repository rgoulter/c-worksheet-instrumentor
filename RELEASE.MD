# Release Notes

## v0.2.4

Fix a couple of corner cases:

* `"abc";` now visualised as `> abc` (rather than `> 0xabc124 = a`).  If you'd
  still like to see pointer addresses of string literals, you can make use of
  statements like: `char *p = "lit"; p;` instead.

* `char ca[] = "Hello"; ca;` now visualised as `> Hello`. (Previously, would
  try to output char by char, including `NUL`, so resulted in `> [H, e, l, l,
  o, `).

* Array Subscripting now works for pointers, and for when subscriping an array
  onto an integer. e.g. `5[arr];`.
