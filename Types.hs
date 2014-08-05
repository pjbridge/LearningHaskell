import qualified Data.Map as Map 

data LockerState = Taken | Free deriving (Show, Eq)
type Code = String 
type LockerMap = Map.Map Int (LockerState, Code)

data Person = Person { firstName :: String --record syntax
					, lastName :: String 
					, age :: Int
					, phoneNumber :: String
					} deriving (Show, Eq)

data Car = Car {company :: String 
				, model :: String
				, year :: Int
				} deriving (Show)  

data Maybe' a = Nothing' | Just' a  deriving (Show) --a = type parameter

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday 
		   deriving (Eq, Ord, Show, Read, Bounded, Enum)

phoneBook :: [(String,String)]  
phoneBook =      
    [("betty","555-2938")     
    ,("bonnie","452-2928")     
    ,("patsy","493-2928")     
    ,("lucille","205-2928")     
    ,("wendy","939-8282")     
    ,("penny","853-2492")     
    ] 		  

lockerLookup :: Int -> LockerMap -> Either String Code  
lockerLookup lockerNumber map =   
    case Map.lookup lockerNumber map of   
        Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"  
        Just (state, code) -> if state /= Taken   
                                then Right code  
                                else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

lockers :: LockerMap  
lockers = Map.fromList   
    [(100,(Taken,"ZD39I"))  
    ,(101,(Free,"JAH3I"))  
    ,(103,(Free,"IQSA9"))  
    ,(105,(Free,"QOTSA"))  
    ,(109,(Taken,"893JJ"))  
    ,(110,(Taken,"99292"))  
    ] 

  --useage: ghci> lockerLookup 101 lockers  
