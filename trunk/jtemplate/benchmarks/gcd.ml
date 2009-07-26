let _=
	let rec gcd x y=
		if x=y then x
		else
			if x<y then
				gcd x (y-x)
			else
				gcd (x-y) x
	in let rec loop=function
		| 0 -> ()
		| n -> let _=gcd 28388383 100101 in loop (n-1)
	in loop 10000
