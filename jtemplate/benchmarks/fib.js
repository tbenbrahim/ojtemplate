var fib = function(n) {
	return n == 0 ? 0 : n == 1 ? 1 : fib(n - 1) + fib(n - 2);
};
print(fib(32));