var fib = function(n) {
	return n >1?  fib(n - 1) + fib(n - 2): n==1 ? 1: 0;
};
print(fib(32));
