Solving the 99 Lisp Problems, which can be found here :

http://www.ic.unicamp.br/~meidanis/courses/mc336/2006s2/funcional/L-99_Ninety-Nine_Lisp_Problems.html

These are fairly simple problems with clean, idiomatic solutions in most functional languages, including Objective Caml. If you believe you have a simpler approach for one of the problems (or that my code could be cleaner), feel free to propose a commit.

Solutions are found in files numbered based on the problem, so the solution to problem 13 can be found in 11-20/p13.ml

# Usages with `rtop`

If you need to install `rtop`, it comes with the `reason` package on `opam`. Run `opam install reason` to get it.

```sh
> rtop
rtop
─────────────────────────────────────
utop version 2.7.0 (using OCaml versi
─────────────────────────────────────

                   ___  _______   ________  _  __
                  / _ \/ __/ _ | / __/ __ \/ |/ /
                 / , _/ _// __ |_\ \/ /_/ /    /
                /_/|_/___/_/ |_/___/\____/_/|_/

  Execute statements/let bindings. Hit <enter> after the semicolon. Ctrl-d to quit.

        >   let myVar = "Hello Reason!";
        >   let myList: list(string) = ["first", "second"];
        >   #use "./src/myFile.re"; /* loads the file into here */

Type #utop_help for help about using utop.

Reason #
Reason # #use "./1-10/p01.re";
let last: list('a) => option('a) =
  <fun>;
- : unit = ()
- : unit = ()
Reason #
```
