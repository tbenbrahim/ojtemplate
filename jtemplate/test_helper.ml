open Unix

let assertEqual a b = a = b

let assertNotEqual a b = a <> b

let assertRaises e expr =
	try
		expr(); false
	with
		ex -> e = ex

let run_test utest =
	let (name, func) = utest in
	let result = (
			try
				func() 
			with
			| e -> print_string ("\tuncaught exception " ^ (Printexc.to_string e)^" thrown in "^name^"\n"); false
		)
	in match result with
		true -> print_string ("\t" ^ name ^ ": PASS\n"); 1
	| false -> print_string ("\t" ^ name ^ ": FAIL\n"); 0

let run_test_suite testsuite =
	let (description, testlist) = testsuite
	in
	print_string("Running test suite "^description^"\n");
	let (pass, total) = List.fold_left (fun acc el ->
						let (pass, total) = acc in
						(pass + run_test el, total + 1)
			) (0, 0) testlist
	in
	print_string (description^": "^ (string_of_int pass) ^"/"^(string_of_int total) ^" passed: ");
	if pass = total then print_string "PASS\n\n"
	else print_string "FAIL\n\n";
	(pass, total)

let run_test_suites testsuites =
	let (pass, total) = List.fold_left
			(fun acc el ->
						let (gpass, gtotal) = acc
						in
						let (pass, total) = run_test_suite el
						in
						(gpass + pass, gtotal + total))
			(0, 0) testsuites
	in
	print_string ("ALL TESTS: "^ (string_of_int pass) ^"/"^(string_of_int total) ^" passed: ");
	if pass = total then print_string "PASS\n"
	else print_string "FAIL\n"