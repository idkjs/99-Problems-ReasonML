/* Pack consecutive duplicates of list elements into sublists. */

/* Since Objective Caml cannot represent a list of both normal values and N,X values, one needs to
   define a type for that. */

type rle('a) =
  | One('a)
  | Many((int, 'a));

let pack = list => {
  let rec aux = (current, acc) =>
    fun
    | [] => [] /* Can only be reached if original list is empty */
    | [x] => [[x, ...current], ...acc]
    | [a, ...[b, ..._] as t] =>
      if (a == b) {
        aux([a, ...current], acc, t);
      } else {
        aux([], [[a, ...current], ...acc], t);
      };

  List.rev(aux([], [], list));
};

let encode = list => {
  let rec aux =
    fun
    | [] => []
    | [[], ...t] => aux(t)
    | [[x], ...t] => [One(x), ...aux(t)]
    | [[x, ...l], ...t] => [
        [@implicit_arity] Many(1 + List.length(l), x),
        ...aux(t),
      ];

  aux(pack(list));
};

assert(
  encode([`a, `a, `a, `a, `b, `c, `c, `a, `a, `d, `e, `e, `e, `e])
  == [
       [@implicit_arity] Many(4, `a),
       One(`b),
       [@implicit_arity] Many(2, `c),
       [@implicit_arity] Many(2, `a),
       One(`d),
       [@implicit_arity] Many(4, `e),
     ],
);
