def gcd(a,b):
	if a > b:
		return gcd(a-b,b)
	else:
		if a<b :
			return gcd(a,b-a)
		else :
			return a

a=0

for i in range(1,10000):
	a=gcd(28388383,100101)
	
print(a)	