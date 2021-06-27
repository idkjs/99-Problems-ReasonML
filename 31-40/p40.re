/* Goldbach's conjecture */

let divides = (d, n) => n mod d == 0;

let is_prime = n => {
  let n = max(n, - n);
  let rec aux = d => d * d > n || !divides(d, n) && aux(d + 1);

  aux(2);
};

let goldbach = n => {
  let rec aux = d =>
    if (is_prime(d) && is_prime(n - d)) {
      (d, n - d);
    } else {
      aux(d + 1);
    };
  aux(2);
};

assert(goldbach(28) == (5, 23));
