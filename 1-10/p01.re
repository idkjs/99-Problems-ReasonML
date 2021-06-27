/* Find the last element of a list. */

let rec last =
  fun
  | [] => None
  | [x] => Some(x)
  | [_, ...t] => last(t);

assert(last([`a, `b, `c, `d]) == Some(`d));
assert(last([]) == None);
