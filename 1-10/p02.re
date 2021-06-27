/* Find the last but one element of a list */

let rec last_two =
  fun
  | []
  | [_] => None
  | [x, y] => Some((x, y))
  | [_, ...t] => last_two(t);

assert(last_two([`a, `b, `c, `d]) == Some((`c, `d)));
assert(last_two([`a]) == None);
