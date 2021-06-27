/* Duplicate the elements of a list a given number of times */

let replicate = (list, n) => {
  let rec prepend = (n, acc, x) =>
    if (n == 0) {
      acc;
    } else {
      prepend(n - 1, [x, ...acc], x);
    };

  let rec aux = acc =>
    fun
    | [] => acc
    | [h, ...t] => aux(prepend(n, acc, h), t);

  /* This could also be written as: List.fold_left (prepend n) [] (List.rev list) */
  aux([], List.rev(list));
};

assert(replicate([`a, `b, `c], 3) == [`a, `a, `a, `b, `b, `b, `c, `c, `c]);
