all:
	@echo targets:
	@echo '    native       - makes a native executable and copies it to subdirectory bin'
	@echo '    byte         - makes a byte code executable in subdirectory _build'
	@echo '    instrumented - makes an instrumented version for bisect code coverage'
	@echo '    coverage     - instrumented + run the test suite and produce report in coverage directory'
	@echo '    doc          - produces module documentation in subdirectory docs'

native:
	@rm -Rf _build
	@ocamlbuild -lib unix -cflags '-ccopt -O3' jtemplate.native
	@mkdir -p bin
	@mv _build/jtemplate.native bin/jtemplate.native

byte:
	@rm -Rf _build
	@ocamlbuild -lib unix jtemplate.byte
	
doc:
	@rm -Rf _build
	@rm -Rf docs
	@mkdir -p docs
	@ocamlbuild -lib unix jtemplate.byte
	@ocamldoc -I _build -html -d docs _build/*.ml
	rm -Rf _build
	
instrumented: build/jtemplate-i

build/jtemplate-i:
	rm -Rf build	
	make -f make_instrumented
	
coverage: build/jtemplate-i coverage-report 

coverage-report: 
	rm -f bisect*.out
	build/jtemplate-i -parsetree tests/tests.jtp
	bisect-report bisect0001.out -html coverage -title "Code coverage for the Jtemplate interpreter" bisect*.out
	
	