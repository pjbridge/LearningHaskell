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

addThree :: Int -> Int -> Int ->Int
addThree x y z = x + y + z  

lucky :: Int -> String  
lucky 7 = "LUCKY NUMBER SEVEN!"  
lucky x = "Sorry, you're out of luck, pal!"  
 
 
factorialB :: Integer -> Integer  --recursively implemented
factorialB 0 = 1  
factorialB n = n * factorial (n - 1)  

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)  
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)  


