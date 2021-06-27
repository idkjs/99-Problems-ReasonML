/* Extract a slice from a list */

let slice = (list, b, e) => {
  let rec aux = (drop, take) =>
    fun
    | [] => []
    | [h, ...t] =>
      if (take == 0) {
        [];
      } else if (drop == 0) {
        [h, ...aux(0, take - 1, t)];
      } else {
        aux(drop - 1, take, t);
      };

  aux(b - 1, e - b + 1, list);
};

assert(
  slice([`a, `b, `c, `d, `e, `f, `g, `h, `i, `j], 3, 7)
  == [`c, `d, `e, `f, `g],
);
