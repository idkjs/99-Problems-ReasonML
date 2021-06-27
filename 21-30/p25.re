/* Generate a random permutation of the elements of a list. */

let random = n => 1337 mod n;

let rec permutation = list => {
  let rec extract = (acc, n) =>
    fun
    | [] => raise(Not_found)
    | [h, ...t] =>
      if (n == 0) {
        (h, acc @ t);
      } else {
        extract([h, ...acc], n - 1, t);
      };

  let extract_rand = (list, len) => extract([], random(len), list);

  let rec aux = (acc, list, len) =>
    if (len == 0) {
      acc;
    } else {
      let (picked, rest) = extract_rand(list, len);
      aux([picked, ...acc], rest, len - 1);
    };

  let len = List.length(list);

  aux([], list, len);
};

assert(permutation([`a, `b, `c, `d, `e, `f]) == [`b, `d, `a, `e, `c, `f]);
