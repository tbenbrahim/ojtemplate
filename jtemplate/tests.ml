open Test_helper
open Test_test_framework
open Test_symbol_table
open Test_interpreter


let _= run_test_suites  [
	TestTestFramework.test_suite;
	TestSymbolTable.test_suite;
	TestInterpreter.test_suite;
]


