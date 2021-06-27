/* Determine the prime factors of of a given positive integer. */

let divides = (d, n) => n mod d == 0;

let factors = n => {
  let rec aux = (d, n) =>
    if (n == 1) {
      [];
    } else if (divides(d, n)) {
      [d, ...aux(d, n / d)];
    } else {
      aux(d + 1, n);
    };

  aux(2, n);
};

assert(factors(315) == [3, 3, 5, 7]);
