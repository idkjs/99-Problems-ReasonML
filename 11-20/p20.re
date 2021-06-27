/* Remove the K'th element from a list */

let rec remove_at = n =>
  fun
  | [] => []
  | [h, ...t] =>
    if (n == 1) {
      t;
    } else {
      [h, ...remove_at(n - 1, t)];
    };

assert(remove_at(2, [`a, `b, `c, `d]) == [`a, `c, `d]);
