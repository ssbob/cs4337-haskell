-- Author: Sean Fay
-- Assignment #4: Using a modified version of the "Tree" construction given in the "Gentle Introduction to Haskell" document
-- write up 4 "functions" corresponding to Insert, MapTree, FoldrTree, and Compose.

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq, Ord)

-- Definition for a node without children "leaf", or child-less root node
leaf :: a -> Tree a
leaf a = Node a Empty Empty

-- Definining the insert function, defaulting to adding the node to the left of the original node when x == a
insert :: (Ord a, Show a) => a -> Tree a -> Tree a
insert x Empty = leaf x							-- Empty case
insert x (Node a left right) 					-- Non-empty case
  | x == a = Node x (insert x left) right 		-- if x == a
  | x < a  = Node a (insert x left) right		-- when x < a insert into the left child of a
  | x > a  = Node a left (insert x right)		-- when x > a insert intot he right child of a
  
-- Defining the mapTree function, very basic
mapTree :: (Ord a, Show a, Ord b, Show b) => (a -> b) -> Tree a -> Tree b
mapTree f Empty						= Empty
mapTree f (Node a left right)		= Node (f a) (mapTree f left) (mapTree f right)

-- Defining the foldrTree function, used the foldr example from the introduction document to aid in writing this, not sure if this
-- works or not
foldrTree :: (Ord a, Show a) => (a -> b -> b) -> b -> Tree a -> b
foldrTree f y (Node x Empty Empty)	= f x y
foldrTree f y (Node x left right)	= foldrTree f (f x (foldrTree f y right)) left

-- Defining the compose function
-- compose :: Int -> (a -> a) -> (a -> a)