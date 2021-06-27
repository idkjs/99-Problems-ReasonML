/* Determine the GCD of two positive integer numbers. */

let rec gcd = (a, b) =>
  if (a < b) {
    gcd(b, a);
  } else if (b == 0) {
    a;
  } else {
    gcd(b, a mod b);
  };

assert(gcd(13, 27) == 1);
assert(gcd(20536, 7826) == 2);
