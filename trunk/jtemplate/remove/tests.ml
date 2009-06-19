open Test_helper
open Test_test_framework
open Test_symbol_table
open Test_interpreter
open Test_library_string
open Test_library_io
open Test_library_builtin

let _= run_test_suites  [
	TestTestFramework.test_suite;
	TestSymbolTable.test_suite;
	TestInterpreter.test_suite;
	TestStringLibrary.test_suite;
	TestIOLibrary.test_suite;
	TestBuiltinLibrary.test_suite;
]


