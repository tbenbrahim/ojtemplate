<?php
function fib($n)
{
	return $n >1 ?  fib($n - 1) + fib($n - 2) : $n;  
}

print_r (fib(32));
?>
