{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
module Main where
import Data.Data
import Data.List
import Data.Eq
type Name =String
type LastName = String
type ParentList = [Int]
type ChildList = [Int]

type PersonName = (String,String)
data Siblings = Siblings [PersonName]



-- male 1 female 0 
data Person = Person
 {name :: Name
 ,lastname :: LastName
 ,personId :: Int
 ,sex :: Int
 ,parentList ::  [Int]
 ,childList :: [Int]
 ,siblingList :: [Int]
 }
 deriving (Show,Typeable,Data,Eq)  


--baba
getParentListx :: [Person]-> Person -> [Person]
getParentListx ps p = filter (\px -> personId px == head(parentList p)) ps

getParentList :: Person -> [Int]
getParentList = parentList
-- Parent listten cinsiyeti male olanları getir
{-getParentListM :: Person ->Int
getParentListM p = head(filter (\p -> sex p == 1) (parentList p))

getParentListF :: Person ->Int
getParentListF p = head(filter (\p -> sex p == 0) (parentList p))

getSiblingM :: Person -> [Int]
getSiblingM p = (filter (\p -> sex p == 1) (siblingList p))

getSiblingListF :: Person -> [Int]
getSiblingListF p = (filter (\p -> sex p == 0) (siblingList p))-}

getChildList :: Person -> [Int]
getChildList p = childList p

getPersonById :: [Person] ->Int-> Person
getPersonById p x = head(filter (\p -> personId p == x) p)

getParentListPerson :: Person-> [Person] -> [Person]
getParentListPerson p ps = filter((\px -> elem (personId p) (childList px))) ps
      
--getPersonById everybody head(parentList p) 

isParent :: Person -> Person ->Bool
isParent p1 p2 = elem (personId p1) (parentList p2)

isChild :: Person -> Person ->Bool
isChild p1 p2 = elem (personId p1) (childList p2)

--isAmca :: Person -> Person -> Bool
--isAmca p1 p2 =  elem True (map (\x -> elem x (siblingList p1)) (getParentListM p2))

isAmca2:: Person ->Person ->Bool
isAmca2 p1 p2 = (sex p1)==1 && any f (siblingList p1)
        where f = (\x -> (elem x (parentList p2)))

isKardes :: Person -> Person -> Bool
isKardes p1 p2 = elem (personId p2)(siblingList p1)

isDayi :: Person -> Person -> Bool
isDayi p1 p2 = (sex p1)==1 && any f (siblingList p1) 
       where f= (\x -> (elem x (parentList p2)))
--todo any f (siblingList p1) değil elem
isTeyze :: Person -> Person -> Bool
isTeyze p1 p2 = (sex p1)==0 && any f (siblingList p1)
        where f= (\x -> (elem x (parentList p2)))

isYegen :: Person ->Person ->Bool
isYegen p1 p2 = any f (parentList p1)
       where f= (\x -> (elem x (siblingList p2)))

isCousen :: Person -> Person -> Bool
isCousen p1 p2 =any f (parentList p1)
        where f= (\x -> elem x <$> concatMap func2 <$> func3 p2 everybody) ::([Int]->Bool)
              func2 = (\y -> (siblingList y)) ::(Person->[Int])
              func3 = (\z k-> getParentListPerson z k)::(Person->[Person]->[Person])


--where f= (\x -> (elem x (siblingList head((getPersonById everybody head(parentList p2))))))
-- p2 'nin parent'nın sibling list 
--önce parent'nı get le
-- parent ının sibling listini get let

everybody :: [Person]
everybody = [mahmut,ismail,gulsen,ilhami,ramazan,gulcan,enes]

-- name surname parentlist childlist siblinglist
mahmut :: Person
mahmut = Person {name ="mahmut"
                  ,lastname = "salman"
                  ,personId = 1
                  ,sex = 1
                  ,parentList = [2,3]
                  ,childList = []
                  ,siblingList = [5]

}
ismail ::Person
ismail =Person{name ="İsmail"
                  ,lastname = "Salman"
                  ,personId = 2
                  ,sex = 1
                  ,parentList = []
                  ,childList = [1,5]
                  ,siblingList = [4]

}
gulsen ::Person
gulsen =Person{name ="Gülşen"
                  ,lastname = "Salman"
                  ,personId = 3
                  ,sex = 0
                  ,parentList = []
                  ,childList = [1,5]
                  ,siblingList = [6,7]

}
ilhami ::Person
ilhami =Person{name ="İlhami"
                  ,lastname = "Salman"
                  ,personId = 4
                  ,sex = 1
                  ,parentList = []
                  ,childList =  []
                  ,siblingList = [2]

}
ayse :: Person
ayse = Person {name ="Ayşe"
                  ,lastname = "Salman"
                  ,personId = 5
                  ,sex = 0
                  ,parentList = [2,3]
                  ,childList = []
                  ,siblingList = [1]

}

ramazan :: Person
ramazan = Person {name ="Ramazan"
                  ,lastname = "Coskuner"
                  ,personId = 6
                  ,sex = 1
                  ,parentList = []
                  ,childList = []
                  ,siblingList = [3,7]

}
gulcan :: Person
gulcan = Person {name ="Gülcan"
                  ,lastname = "Coskuner"
                  ,personId = 7
                  ,sex = 0
                  ,parentList = []
                  ,childList = []
                  ,siblingList = [3,6]

}

sukran :: Person
sukran = Person {name ="Şükran"
                  ,lastname = "Coskuner"
                  ,personId = 9
                  ,sex = 1
                  ,parentList = []
                  ,childList = []
                  ,siblingList = []

}
enes :: Person
enes = Person {name ="Enes"
                  ,lastname = "Coskuner"
                  ,personId = 8
                  ,sex = 1
                  ,parentList = [6,9]
                  ,childList = []
                  ,siblingList = []

}



printRel :: Person -> Person -> String
printRel p1 p2
    |(isParent p1 p2) =" Cocuk"
    |(isChild p1 p2) = "Parent"
    |(isAmca2 p1 p2) = "Amca"
    |(isKardes p1 p2)  = "Kardes"
    |(isDayi p1 p2) = "Dayı"
    |(isTeyze p1 p2) ="Teyze"
    |(isYegen p1 p2) = "Yegen"
    -- |(isCousen p1 p2) = "Kuzen"
    |otherwise       = " No relation"

main :: IO ()
main = putStrLn "Hello, Haskell!"
