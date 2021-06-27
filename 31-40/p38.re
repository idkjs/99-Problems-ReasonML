/* Compare the two euler computation methods. */

let mod_count = ref(0);

let divides = (d, n) => {
  incr(mod_count);
  n mod d == 0;
};
let rec gcd = (a, b) =>
  if (b == 0) {
    a;
  } else {
    incr(mod_count);
    gcd(b, a mod b);
  };

/* Naive method */

let coprime = (a, b) => gcd(a, b) == 1;

let naive_phi = n => {
  mod_count := 0;
  let rec aux = (acc, d) =>
    if (d < n) {
      aux(
        if (coprime(n, d)) {
          acc + 1;
        } else {
          acc;
        },
        d + 1,
      );
    } else {
      acc;
    };

  let result =
    if (n == 1) {
      1;
    } else {
      aux(0, 1);
    };
  Printf.printf("Naive: %d divides\n", mod_count^);
  result;
};

/* Improved method */

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

let improved_phi = n => {
  mod_count := 0;
  let rec aux = acc =>
    fun
    | [] => acc
    | [(p, m), ...t] => aux((p - 1) * pow(p, m - 1) * acc, t);

  let result = aux(1, factors(n));
  Printf.printf("Improved: %d divides\n", mod_count^);
  result;
};

/* Comparison */

let _ = naive_phi(10090); /* 77393 divides */
let _ = improved_phi(10090); /*  1010 divides */
