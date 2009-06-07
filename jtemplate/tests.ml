open Test_helper
open Test_test_framework
open Test_symbol_table


let _= run_test_suites  [
	TestTestFramework.test_suite;
	TestSymbolTable.test_suite;
]


