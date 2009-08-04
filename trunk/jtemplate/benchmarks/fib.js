var fib = function(n) {
	return n >1?  fib(n - 1) + fib(n - 2): n;
};
print(fib(32));
