-- 1. Create a data type called Tree whose elements are of type a. A Tree may either be the enumerated type Empty, or a Node that contains a single value of type a (the "payload" of the node), and a reference to two sub-trees of type a. 

data Tree a = Empty                            -- Represents an empty tree with no elements
            | Node a (Tree a) (Tree a)         -- Represents a node in the tree with a value of type 'a'
                                               -- and references to its left and right subtrees


-- 2. Create an instance of the show function to show trees. The show function in Haskell is like the toString function in Java. This is the function that gets called whenever you want to display the value of a variable.

-- Show instance for the Tree data type
instance (Show a) => Show (Tree a) where
    show Empty = ""                       -- Show an empty tree as an empty string
    show (Node n Empty Empty) = show n    -- Show a node with no left or right subtrees
    show (Node n left right) = show n ++ " (" ++ show left ++ ", " ++ show right ++ ")"   -- Show a node with both left and right subtrees


-- 3. Create a function called insertTree, with an explicit data type expressed as: Ord a => Tree a -> a -> Tree a. This function requires the elements of the tree to be "Ordered" (so you can compare two elements to each other). The first parameter should be a Tree of type a. The second parameter is a single element of type a. The result is a new Tree of type a in which the second argument has been inserted in the tree from the first argument. If the input tree is empty, just return a tree that consists of a single node that carries the element argument and has empty left and right sub-trees. If the input tree is not empty, it has a top node. Compare the input element to the top node. If the input argument is less than the top node, insert the element in the left sub-tree of the input argument and return the result. Otherwise, insert the new element in the right sub-tree. The insertTree function creates an ordered binary tree.

-- Insert an element into a binary tree
insertTree :: Ord a => Tree a -> a -> Tree a
      
      insertTree Empty a = Node a Empty Empty             -- If the tree is empty, create a new node with the given element and two empty subtrees
      insertTree (Node n left right) a | a < n = Node n (insertTree left a) right   -- If the tree is a node and the element is less than the node's value, insert the element into the left subtree
      insertTree (Node n left right) a | a >= n = Node n left (insertTree right a)  -- If the tree is a node and the element is greater than or equal to the node's value, insert the element into the right subtree


-- 4. Create a function called constuctTree which takes a list of type a as an argument, and creates a tree of type a as  a result. This is pretty simple using fold and the insertTree function you have already coded.

-- Construct a binary tree from a list of elements
constructTree :: Ord a => [a] -> Tree a
       constructTree [] = Empty                      -- If the input list is empty, return an empty tree
       constructTree xs = foldr insertTree Empty xs  -- If the input list is not empty, foldr (right fold) the insertTree function over the list to construct the binary tree


-- 5. Create a function called size which takes a tree as input, and returns the number of nodes in that tree as a result.

size :: Tree a -> Int                                         -- Calculate the number of nodes in a binary tree
       size Empty = 0                                         -- If the tree is empty, it has zero nodes
       size (Node _ left right) = 1 + size left + size right  -- If the tree is a node, count the current node (1) and recursively count the nodes in the left and right subtrees and add them up


-- 6. Create a function called depth which takes a tree as input, and returns the length of the longest path from the root of the tree to any node in that tree.

depth :: Tree a -> Int                            -- Calculate the depth (height) of a binary tree
      depth Empty = 0         -- If the tree is empty, its depth is 0
      depth (Node _ left right) = 1 + max (depth left) (depth right)     -- If the tree is a node, calculate the depth as 1 plus the maximum depth between its left and right subtrees using the 'max' function


-- 7. Create a function called efficiency which takes a tree as input, and returns a number between 0 and 1 that represents the fraction of the actual size of the tree divided by the maximum number of nodes that can appear in a tree of the same depth.

efficiency :: Tree a -> Double               -- Calculate the efficiency of a binary tree
efficiency Empty = 0                         -- If the tree is empty, its efficiency is 0

efficiency tree = actualSize / maxNodes      -- For a non-empty tree, calculate efficiency as the ratio of actual size to the maximum number of nodes that a tree of the same depth can have
  where
    depth = depth tree              -- Calculate the depth of the tree
    actualSize = size tree          -- Calculate the actual size (number of nodes) of the tree
    maxNodes = 2 ^ (depth - 1) - 1  -- Calculate the maximum number of nodes for a tree of the same depth

