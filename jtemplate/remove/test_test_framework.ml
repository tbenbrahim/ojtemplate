module TestTestFramework =
struct
	open Test_helper
	
	exception TestException1
	exception TestException2 of string
	
	let test_suite = ("Test Framework",[
			("test assertEqual success", fun () -> assertEqual 1 1);
			("test assertEqual failure", fun () -> not(assertEqual 1 2));
			("test assertNotEqual success", fun () -> assertNotEqual 1 2);
			("test assertNotEqual failure", fun () -> not(assertNotEqual 1 1));
			("test raises exception success", fun () ->
						assertRaises TestException1
							(fun () -> () = raise (TestException1))
			);
			("test raises exception failure", fun () ->
						not (assertRaises TestException1
									(fun () -> () = raise (TestException2 "foo")))
			);
			("test failure with exception", fun () ->
						0 = run_test ("foo", fun () -> raise TestException1));
			])
	
end