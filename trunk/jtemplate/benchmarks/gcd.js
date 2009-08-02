var gcd=function(x,y){
	return x<y ? gcd(x,y-x) : x>y ? gcd(x-y,x) : x; 
};

var a=0;
for (i=0;i<10000;++i)
	a=gcd(28388383,100101);
print(a);
