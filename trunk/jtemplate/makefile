instrumented: build/jtemplate-i.exe

build/jtemplate-i.exe:
	make -f make_instrumented
	
coverage: build/jtemplate-i.exe coverage-report 

coverage-report: 
	rm -f bisect*.out
	build/jtemplate-i.exe tests/tests.jtp
	bisect-report bisect0001.out -html coverage -title "Code coverage for the Jtemplate interpreter" bisect*.out
	
clean:
	rm -Rf build	