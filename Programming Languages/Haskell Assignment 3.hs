import Text.Read (readMaybe)


--Defining all the functions required for all the questions

--Defining a function to convert a List of String to a List of Integer
parseInputList :: String -> [Int]
parseInputList inputIntList =                         
    case readMaybe inputIntList of                                                                                                                                                                                 
        Just parsedList -> parsedList                        --Case- where we have values in the list


--Defining the prefixFunction function
prefixFunction :: Eq a => [a] -> [a] -> Bool

--Setting the base conditions
prefixFunction [] _ = True                                           --1st list as empty can be the prefixFunction for the 2nd list. So setting the empty list as first parameter as True value
prefixFunction _ [] = False                                          --2nd list cannot be empty because then first list cannot be a prefixFunction of second list, Unless the first list is also empty. So setting the empty list as the second parameter as False value


--Condition that will check whether xs(First List) is a prefixFunction of ys(Second List)
prefixFunction (x:xs) (y:ys) =                                       -- x and y will become heads where as xs and ys will become tails of the list
    x == y &&                                                -- we will compare the values of x and y as boolean combining the result using AND operator with the recursive call 
    prefixFunction xs ys                                             -- recursive call to the tail elements


--Defining the append function
append :: z ->[[z]] -> [[z]]                                        --List of a list

--Base Condition: we can append empty to a list
append _ [] = []                                                    -- Empty list appending to a list will add a empty sublist in a list

--Defining the working of the append function
append x (elements:restElements) =                                 
    (x:elements) : append x restElements


--Condition that will create sublistsFunction from a given list
sublistsFunction :: [z] -> [[z]]

--Base condition: Every list will have a blank list as a sublist
sublistsFunction [] = [[]]

--Defining the sublistsFunction function
sublistsFunction (x:xs) = 
    let templist = sublistsFunction xs                               -- We will declare a new list as templist which will store the tail elements of the list passed
    in templist ++ append x templist                                 -- We will concatenate our new list with append function(adding the x element)

--Defining the Replic function
replicFunction :: [Int] -> [[Int]]

--Base condition: for empty list we will have 0 replicFunction so a empty list
replicFunction [] = []

--Defining the replicFunctiona function 
replicFunction (x:xs) = createReplica x x : replicFunction xs        -- For every element of the list we will create replica for the head element andd then recursively call the function for tail elements
    
--Defining the createReplica Function
createReplica :: Int -> Int -> [Int]

--Base condition: for empty list we will have 0 replicFunction
createReplica _ 0 = []

--Defining the function
createReplica element a = element : createReplica element (a - 1)    --Recursively calling the function to create replicate elements


--Defining the cartesian product function 
cprodFunction :: [listA] -> [listB] -> [(listA, listB)]

--Defining the base condition:if any of the list is empty the cartesian product will be empty
cprodFunction [] _ = []
cprodFunction _ [] = []

--Defining the function to perform cartesian product using recursion method
cprodFunction (a : listA) listB =                                                      -- cprodFunction takes 2 parameters 1st will be head element from list A and 2nd will be the entire list B
    productTraversal a listB ++                                                -- We will provide the value of new function productTraversal with 2 parameters
    cprodFunction listA listB                                                          -- recursive call to cprodFunction function with tail elements
    where                                                                      -- defining the productTraversal funtion
    productTraversal _ [] = []                                                 -- Setting the base condition for empty list
    productTraversal a (b : listB) = (a, b) : productTraversal a listB         -- Providing parameters as a and head element from list b, like this we will recursively call the productTraversal for all the elements from list B 


--Defining a function to find the sum of the squares of the elements of a list
sqsumFunction listIntegers = 
    foldl (+) 0                                              --foldl(fold left) function where we will adding the elements of a list traversing from start to the end of the list with the initial value set to 0 
    (map (\integer -> integer * integer) listIntegers)     --We will use map function where \integer means for every element of the listIntegers we will calculate the square



--Calling the Functions


-- Question 1: Prefix 
prefix :: IO()
prefix = do
    --Accepting the first List
    putStrLn("Prefix function")
    putStrLn ("Enter the First list xs")
    xs <-getLine
    let parsedXs = parseInputList xs

    --Accepting the Second List
    putStrLn ("Enter the Second list ys")
    ys <-getLine
    let parsedYs = parseInputList ys

    --Calling the prefixFunctionFunction function and printing the result
    let result = prefixFunction parsedXs parsedYs
    print result


-- Question 2: Sublists
sublists :: IO()
sublists = do
    --Accepting a list
    putStrLn("Sublist function")
    putStrLn("Enter the list who's sublistsFunction are to be calculated")
    listIntegers <-getLine
    let parsedList = parseInputList listIntegers
    let resultList = sublistsFunction parsedList
    print resultList 


-- Question 3: Replica
replic :: IO()
replic = do
    --Accepting first list
    putStrLn("Replica function")
    putStrLn("Enter the list to create repilca")
    listA <- getLine
    let parsedListA = parseInputList listA
    let replicFunctionaList = replicFunction parsedListA
    print replicFunctionaList


-- Question 4: Cartesian Product
cprod :: IO()
cprod = do
    putStrLn("Cartesian function")
    --Accepting first list
    putStrLn("Enter the first list")
    listA <- getLine
    let parsedListA = parseInputList listA

    --Accepting second list
    putStrLn("Enter the second list")
    listB <- getLine
    let parsedListB = parseInputList listB

    --Calling the cartesian product function
    let resultList = cprodFunction parsedListA parsedListB
    print resultList


-- Question 5: Sum of the squares
sqsum :: IO()
sqsum = do
    putStrLn("Sum of squares function")
    print "Enter a list of integers "                   
    inputIntList <- getLine                                          --Accepting a list of integers which are comma seperated as shown in the questions example.
    let parsedList = parseInputList inputIntList                     --Calling the parseInputList function to convert from list of string to integer
    let result = sqsumFunction parsedList                            --Storing the value returned by the sqsumFunction funtion 
    print result


main = do
    print "Enter all the list as comma seperated [ a, b, c] where a,b and c are integers"
    --prefix                                              
    --sublists
    --replic
    --cprod
    --sqsum
    