module Final
import StdEnv

:: Tree a = Node a (Tree a) (Tree a) | Leaf
//:: RoseTree a = Node a [(RoseTree a)] | Leaf
//Below are some convenient trees to work with for
//exercises and testing.
ourTree :: (Tree Int)
ourTree = Node 15(Node 3(Node 1 Leaf Leaf)(Node 10(Node 7 Leaf (Node 8 Leaf Leaf))(Node 13 (Node 11 Leaf Leaf) Leaf)))(Node 20 (Node 18 Leaf (Node 19 Leaf Leaf)) (Node 21 Leaf (Node 26 (Node 24 Leaf Leaf) (Node 28 Leaf Leaf))))
mytree Leaf = []
mytree (Node x l r) =(mytree l) ++ [x] ++ (mytree r)
//Start = mytree ourTree

//This fun will get the left node of Tree
mytree1 (Node x l r) = l
//Start = mytree (mytree1 ourTree)
//This fun will get the right the nodes of tree
mytree2 (Node x l r) = r
//Start = mytree (mytree2 ourTree)
emptyTree = Leaf
isLeaf :: (Tree a) -> Bool
isLeaf Leaf = True
isLeaf _ = False
//Start = isLeaf ourTree
//This Fun will find the mini value of Tree
//Start = (mytree (mytree2 ourTree))
minTree (Node x Leaf _) = x
minTree (Node _ l _) = minTree l
//Start = minTree ourTree
//Start = ourTree
//This fun will reverse the order of our tree
mytree3 Leaf = Leaf

mytree3 (Node x l r) = (Node x (mytree3 r) (mytree3 l))
fun = mytree3 ourTree
//Start = hd(mytree fun)

//This fun get the list of children of the node
//getchild n Leaf = []
getchild n (Node x Leaf Leaf) = []
getchild n (Node x l r)
| x == n = [mytree1 l, mytree2 r]
| x < n = getchild n l
| x > n = getchild n r
//Start = getchild 15 ourTree
//Start = mytree ourTree

//"(")"Get the parent of a node"(")"













