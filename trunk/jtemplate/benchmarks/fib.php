<?php
function fib($n)
{
	return $n >1 ?  fib($n - 1) + fib($n - 2) : $n==1 ? 1: 0;  
}

print_r (fib(32));
?>
