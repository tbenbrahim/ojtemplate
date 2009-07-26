public class Gcd{

public static int gcd(int x,int y){
	if (x==y)
		return x;
	if (x<y)
		return gcd(x,y-x);
	else
		return gcd(x-y,x);
}

public static void main(String[] args){
	int a=0;
	for (int i=0;i<10000;++i)
		a=gcd(28388383,100101);
	System.out.println(a);
}

}
