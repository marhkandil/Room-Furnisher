--ghc 8.0.1 /opt/ghc/8.0.1/lib/ghc-8.0.0.20160127/
import FurnitureResources

main = print $ "Hello, world!"


findFurnitureUpdate a b c [] = if(c == "right") then [(a,[[(b,c,1)],[]])]
	                            else [(a,[[],[(b,c,1)]])]
findFurnitureUpdate a b c ((s,[rs,bs]):xs) =  if(s == a) then 
	                                             if(c == "right") then ((s,[findHelper b c rs,bs]):xs) 
	                                             else ((s,[rs,findHelper b c bs]):xs)
	                                          else ((s,[rs,bs]):findFurnitureUpdate a b c xs) 

findHelper ::  [Char] -> [Char] -> [([Char],[Char],Int)] -> [([Char],[Char],Int)]
											  
findHelper b c [] = [(b,c,1)]
findHelper b c ((x,y,z):xs) = if(b == x) then my_sort ((x,y,(z+1)):xs)
	                        else my_sort ((x,y,z):(findHelper b c xs))
    

my_sort [] = []
my_sort [x] = [x]
my_sort ((x,y,z):(a,b,c):xs) = if(z > c) then ((x,y,z):(my_sort ((a,b,c):xs)))
	                           else ((a,b,c):my_sort ((x,y,z):xs))



findUP_helper [][]=[]
findUP_helper (x:xs)(y:ys)= (x,y,"below"):findUP_helper xs ys
findUP [_]=[]

findUP (x:y:ys)= (findUP_helper x y) ++ findUP(y:ys)


findRight([])=[]
findRight([_]:xs)=findRight xs
findRight ((y:y1:yx):xs)= (y,y1,"right"): findRight((y1:yx):xs)

genR room1 list_sofar = (feed (findRight room1) list_sofar)
generate room list= (feed (findUP room)(genR room list))


feed [] list =list
feed ((x,y,status):xs) list_so_far = feed xs (findFurnitureUpdate x y status list_so_far)



statsList = (statsHelper training []) 

statsHelper [] x = x
statsHelper (r:rs) x= statsHelper rs y where y= generate r x




getFurnStat x= getFurnStatHelper x statsList

getFurnStatHelper x ((name,list):ys)
								| (name==x) = list
								| otherwise = getFurnStatHelper x ys 

getPossibleNeighbour left above= getObject ((randomZeroToX ((foldr foldHelp 0 left)+(foldr foldHelp  0 above)))-1) left above

getObject x ((name,_,y):ls) [] = if (x<y) then name else getObject (x-y) ls []
getObject x ((name,_,y):ls) above = if (x<y) then name else getObject (x-y) ls above
getObject x []  ((name,_,y):ls)= if (x<y) then name else getObject (x-y)  [] ls


foldHelp (_,_,x) y= x+y



furnishRoom size firstElm= (mySplit size [] (findElem 1 size [(firstElm)]))

findElem countSoFar size list= if(countSoFar == (size*size)) then list else
		  if(countSoFar<=size) then (findElem (countSoFar+1) size ( list++[getPossibleNeighbour(getRightStats(last list)) [] ])) 
		else if((countSoFar `mod` size)==1) then (findElem (countSoFar+1) size (list++[getPossibleNeighbour [] (getBelowStats(locate (countSoFar-size) list))]) )
		else (findElem (countSoFar+1) size (list++[getPossibleNeighbour (getRightStats(last list)) (getBelowStats(locate (countSoFar-size) list))]))		

		
getRightStats x= y where([y,_])= (getFurnStat x)
getBelowStats x= y where([_,y])= (getFurnStat x)

locate count (x:xs)= if(count==0) then x else (locate (count-1) xs)

mySplit size sub [] = [sub]
mySplit size sub (x:xs) = if((length sub) == size) then (sub:(mySplit size [x] xs))
						else mySplit size (sub++[x]) xs