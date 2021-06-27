/* Extract a given number of randomly selected elements from a list */

/* Deterministic, for testing. Replace with Random.int to get true random results */
let random = n => 1337 mod n;

let rec rand_select = (list, n) => {
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

  let rec aux = (n, acc, list, len) =>
    if (n == 0) {
      acc;
    } else {
      let (picked, rest) = extract_rand(list, len);
      aux(n - 1, [picked, ...acc], rest, len - 1);
    };

  let len = List.length(list);

  aux(min(n, len), [], list, len);
};

assert(rand_select([`a, `b, `c, `d, `e, `f, `g, `h], 3) == [`h, `a, `b]);
