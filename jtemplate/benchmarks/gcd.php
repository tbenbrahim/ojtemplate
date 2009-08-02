<?php
function gcd($x,$y){
	return $x<$y ? gcd($x,$y-$x) :( $x>$y ? gcd($x-$y,$x) : $x);
}

$a=0;
for ($i=0;$i<10000;$i+=1){
	$a=gcd(28388383,100101);
}
print_r ($a);
?>
