/* Drop every N'th element from a list */

let drop = (list, n) => {
  let rec aux = i =>
    fun
    | [] => []
    | [h, ...t] =>
      if (i == n) {
        aux(1, t);
      } else {
        [h, ...aux(i + 1, t)];
      };

  aux(1, list);
};

assert(
  drop([`a, `b, `c, `d, `e, `f, `g, `h, `i, `j], 3)
  == [`a, `b, `d, `e, `g, `h, `j],
);
