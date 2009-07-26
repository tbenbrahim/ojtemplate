instrumented: build/jtemplate-i

build/jtemplate-i:
	make -f make_instrumented
	
coverage: build/jtemplate-i coverage-report 

coverage-report: 
	rm -f bisect*.out
	build/jtemplate-i tests/tests.jtp
	bisect-report bisect0001.out -html coverage -title "Code coverage for the Jtemplate interpreter" bisect*.out
	
clean:
	rm -Rf build	