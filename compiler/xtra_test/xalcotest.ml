open Alcotest

let quick n f = (n, [ test_case n `Quick f ])

let quicks n fs =
  (n, List.mapi (fun i f -> test_case (string_of_int i) `Quick f) fs)

let slow n f = (n, [ test_case n `Slow f ])

let slows n fs =
  (n, List.mapi (fun i f -> test_case (string_of_int i) `Slow f) fs)
