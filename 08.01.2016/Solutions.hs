ata BST = Empty | Node Int BST BST

t::BST
t = (Node 11 ( Node 5 ( Node 2 Empty Empty) ( Node 7 Empty Empty) )( Node 19 Empty (Node 21 Empty Empty)))

count::BST->Int
count Empty = 0
count (Node x l r) = 1 + count( l) + count(r)

height::BST->Int
height Empty = 0
height ( Node _ l  r ) = 1 + ( max ( height l ) ( height r) )

summ::BST->Int
summ Empty = 0
summ ( Node x l r ) = x+ (summ l) + ( summ r )

inorder::BST -> [Int]
inorder Empty = []
inorder (Node x l r) = (inorder l) ++ [x] ++(inorder r)

insert::Int->BST->BST
insert n Empty = (Node n Empty Empty)
insert n (Node x l r )
  | n == x = (Node x l r)
  | n < x = ( Node x (insert n l ) r)
  | n > x = (Node x l (insert n r) )

paths::BST->[[Int]]
paths Empty = [[]]
paths (Node x Empty Empty) = [[x]]
paths(Node x Empty r ) = [[x]]++(map (x:) (paths r ))   
paths(Node x l Empty ) = [[x]]++(map (x:) (paths l ))
paths(Node x l r ) = [[x]]++(map ( x:) (paths l ) ) ++(map (x:) (paths r ))

data Polygon = Circle Double | Sq Double | Rect Double Double

q::Polygon
q = Rect 5.0 2.0

per::Polygon->Double
per (Circle r) = 2*3.14*r
per (Sq a) = a*a
per (Rect a b) = a*b


main::IO()
main = do
  print(count t)
  print(height t)
  print(inorder t)
  print(inorder (insert 18 t))
  print(paths t)
  
