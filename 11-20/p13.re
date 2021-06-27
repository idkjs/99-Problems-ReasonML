/* Run-length encoding of a list (direct solution) */

/* Since Objective Caml cannot represent a list of both normal values and N,X values, one needs to
   define a type for that. */

type rle('a) =
  | One('a)
  | Many((int, 'a));

let encode = list => {
  let rle = (count, x) =>
    if (count == 0) {
      One(x);
    } else {
      [@implicit_arity] Many(count + 1, x);
    };

  let rec aux = (count, acc) =>
    fun
    | [] => [] /* Can only be reached if original list is empty */
    | [x] => [rle(count, x), ...acc]
    | [a, ...[b, ..._] as t] =>
      if (a == b) {
        aux(count + 1, acc, t);
      } else {
        aux(0, [rle(count, a), ...acc], t);
      };

  List.rev(aux(0, [], list));
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
