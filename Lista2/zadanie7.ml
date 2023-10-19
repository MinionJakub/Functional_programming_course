type 'a left_heap = 
| Leaf
| Node of 'a left_heap * 'a * 'a left_heap * int;;

let rank heap = match heap with
| Leaf -> 0
| Node(_,_,_,x) -> x;;

let add_parent value heap1 heap2 = 
match heap1, heap2 with
| Leaf,Leaf -> Node(Leaf,value,Leaf,1)
| Leaf,heap | heap,Leaf -> Node(heap,value,Leaf,(rank Leaf) + 1)
| h1,h2 -> let r1,r2 = rank(h1),rank(h2) in if r1 < r2 then 
Node(h2,value,h1,r1+1) else Node(h1,value,h2,r2+1);;

let rec merge heap1 heap2 = 
match heap1,heap2 with
| Leaf,Leaf -> Leaf
| Leaf,heap | heap,Leaf -> heap
| Node(l1,v1,r1,p1),Node(l2,v2,r2,p2) -> if v1 < v2
then (let a = merge r1 heap2) 
in if rank(l1) < rank(a) 
then Node(a,v1,l1,rank(l1)+1)
else Node(l1,v1,a,rank(a)+1)
else (let a = merge r2 heap1) 
in if rank(l2) < rank(a)
then Node(a,v2,l2,rank(l2)+1)
else Node(l2,v2,a,rank(a)+1);;

let add_elem elem heap = merge heap (add_parent elem Leaf Leaf);;

let rm_min heap = 
match heap with
| Leaf -> Leaf
| Node(l,v,r,p) -> merge l r;;
