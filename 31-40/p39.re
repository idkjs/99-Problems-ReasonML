/* Determine whether a given integer number is prime. */

let divides = (d, n) => n mod d == 0;

let is_prime = n => {
  let n = max(n, - n);
  let rec aux = d => d * d > n || !divides(d, n) && aux(d + 1);

  aux(2);
};

let rec all_primes = (a, b) =>
  if (a > b) {
    [];
  } else {
    let rest = all_primes(a + 1, b);
    if (is_prime(a)) {
      [a, ...rest];
    } else {
      rest;
    };
  };

assert(List.length(all_primes(2, 7920)) == 1000);
