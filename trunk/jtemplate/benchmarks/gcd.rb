def gcd(a,b)
	return a<b ? gcd(a,b-a) : a > b ? gcd(a-b,b) : a
end


a=0
for i in 1..10000 do
	a=gcd(28388383,100101)
end

print(a)
