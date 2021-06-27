/* Determine whether a given integer number is prime. */

let divides = (d, n) => n mod d == 0;

let is_prime = n => {
  let n = max(n, - n);
  let rec aux = d => d * d > n || !divides(d, n) && aux(d + 1);

  aux(2);
};

assert(is_prime(7));
assert(!is_prime(12));
