doubleMe :: Int->Int
doubleMe x=x+x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x > 100 
						then x
						else x*2
triangels=[(a,b,c)|c <-[1..10],b <-[1..10],a <- [1..10]]			
rightTriangels=[(a,b,c)|c <-[1..10],b <-[1..10],a <- [1..10],a^2+b^2==c^2,a+b+c==24]	--right triangels with sides 1..10, perimeter=24	

factorial :: Integer -> Integer
factorial n = product [1..n]
	
circumference :: Float -> Float
circumference r = 2*pi*r