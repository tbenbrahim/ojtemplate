let _ =
	let rec fib = function
		| 0 -> 0
		| 1 -> 1
		| n -> fib (n - 1) + fib(n - 2)
	in print_int (fib 32); print_newline ()