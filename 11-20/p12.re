/* Decode a run-length encoded list */

type rle('a) =
  | One('a)
  | Many((int, 'a));

let decode = list => {
  let rec many = (acc, n, x) =>
    if (n == 0) {
      acc;
    } else {
      many([x, ...acc], n - 1, x);
    };

  let rec aux = acc =>
    fun
    | [] => acc
    | [One(x), ...t] => aux([x, ...acc], t)
    | [[@implicit_arity] Many(n, x), ...t] => aux(many(acc, n, x), t);

  aux([], List.rev(list));
};

let example_from = [
  [@implicit_arity] Many(4, `a),
  One(`b),
  [@implicit_arity] Many(2, `c),
  [@implicit_arity] Many(2, `a),
  One(`d),
  [@implicit_arity] Many(4, `e),
];

let example_to = [`a, `a, `a, `a, `b, `c, `c, `a, `a, `d, `e, `e, `e, `e];

assert(decode(example_from) == example_to);
