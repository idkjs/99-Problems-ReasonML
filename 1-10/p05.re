/* Reverse a list. */

let rev = list => {
  let rec aux = acc =>
    fun
    | [] => acc
    | [h, ...t] => aux([h, ...acc], t);
  aux([], list);
};

assert(rev([`a, `b, `c]) == [`c, `b, `a]);
