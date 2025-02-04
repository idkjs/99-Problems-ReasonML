/* Pack consecutive duplicates of list elements into sublists. */

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

assert(
  pack([`a, `a, `a, `a, `b, `c, `c, `a, `a, `d, `d, `e, `e, `e, `e])
  == [
       [`a, `a, `a, `a],
       [`b],
       [`c, `c],
       [`a, `a],
       [`d, `d],
       [`e, `e, `e, `e],
     ],
);
