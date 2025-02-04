/* Calculate Euler's totient function phi(m) (improved) */

let divides = (d, n) => n mod d == 0;

let factors = n => {
  let rec aux = (d, n) =>
    if (n == 1) {
      [];
    } else if (divides(d, n)) {
      switch (aux(d, n / d)) {
      | [(h, n), ...t] when h == d => [(h, n + 1), ...t]
      | l => [(d, 1), ...l]
      };
    } else {
      aux(d + 1, n);
    };

  aux(2, n);
};

let rec pow = (n, p) =>
  if (p < 1) {
    1;
  } else {
    n * pow(n, p - 1);
  };

/* Note : the formula provided by the exercise is incorrect
   (you need "* acc" instead of "+ acc" below for this to work). */

let phi = n => {
  let rec aux = acc =>
    fun
    | [] => acc
    | [(p, m), ...t] => aux((p - 1) * pow(p, m - 1) * acc, t);
  aux(1, factors(n));
};

assert(phi(10) == 4);
assert(phi(13) == 12);
