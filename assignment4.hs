-- Author: Sean Fay
-- Assignment #4 Using a modified version of the "Tree" object given in the gentle introduction document.
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq, Ord)

-- Defining a lone node (aka a leaf node, or a child-less root node).
leaf :: a -> Tree a
leaf a = Node a Empty Empty

-- Defining the insert function, defaulting to adding to the left child when x == a.
insert :: (Ord a, Show a) => a -> Tree a -> Tree a
insert x Empty =  leaf x 					-- Empty case
insert x (Node a left right)				-- Non-empty case
  | x == a = Node x (insert x left) right 	-- if x == a
  | x < a  = Node a (insert x left) right 	-- when x < a insert to the left child of a
  | x > a  = Node a left (insert x right) 	-- when x > a insert to the right child of a 

-- Defining the mapTree function, very basic.
mapTree :: (Ord a, Show a, Ord b, Show b) => (a -> b) -> Tree a -> Tree b
mapTree f Empty 						= Empty
mapTree f (Node a left right)			= Node (f a) (mapTree f left) (mapTree f right)

-- Used foldr idea from gentle introduction, hope it works.
foldrTree :: (Ord a, Show a) => (a -> b -> b) -> b -> Tree a -> b
foldrTree f y (Node x Empty Empty)  	= f x y
foldrTree f y (Node x left right) 		= foldrTree f (f x (foldrTree f y right)) left

-- compose :: Int -> (a -> a) -> (a -> a) -- Did not complete this part of the assignment
