<?php
function gcd($x,$y){
	if ($x==$y)
		return $x;
	if ($x<$y)
		return gcd($x,$y-$x);
	else
		return gcd($x-$y,$x);
}

$a=0;
for ($i=0;$i<10000;$i+=1){
	$a=gcd(28388383,100101);
}
print_r ($a);
?>
