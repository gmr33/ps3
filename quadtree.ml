type coord = float * float
type region = coord * coord
(*a quadtree consists of a region represented by two coord and also four other quad trees 
represeting itself and ther three other regions *)
type 'a quadtree =
  Node of region * 'a quadtree * 'a quadtree * 'a quadtree * 'a quadtree
  | Leaf of region * ((coord * 'a) list)
           
let min_diagonal = 0.0000001
         
exception OutOfBounds


let new_tree (r:region) : 'a quadtree =
  Leaf (r,[])

let pick_sub_tree (c:coord) (r:region) q1 q2 q3 q4  = 
  let ((x0,y0),(x1,y1)) = r in 
    let (x,y) = c in
      let (x_mid,y_mid) = ((x0+.x1)/.2., (y1+.y0)/.2.) in
        if (x>=x_mid && y>=y_mid) then (q1,1,((x_mid,y_mid),(x1,y1)))
        else if (x<x_mid && y>=y_mid) then (q2,2,((x0,y_mid),(x_mid,y1)))
        else if (x<x_mid && y<y_mid) then (q3,3,((x0,y0),(x_mid,y_mid)))
        else (q4,4,((x_mid,y0),(x1,y_mid)))
    (*x1 always greater than x0*)    

let cal_diagonal (r:region) =
  let ((x0,y0),(x1,y1)) = r in 
    sqrt((x1-.x0)*.(x1-.x0) +. (y1-.y0)*.(y1-.y0))

let check_bounds q c = match q with 
|Node(r,q1,q2,q3,q4 )->
  let ((x0,y0),(x1,y1)) = r in 
  let (x,y) = c in
   if(x<x0 || x>x1 || y<y0 || y>y1) then raise OutOfBounds
 else true
|Leaf(r,lst) -> let ((x0,y0),(x1,y1)) = r in 
   let (x,y) = c in
   if(x<x0 || x>x1 || y<y0 || y>y1) then raise OutOfBounds
 else true
  
let new_Node r q1 q2 q3 q4 (custom_q,q_num) = 
match q_num with
  |1 -> Node(r,custom_q,q2,q3,q4)
  |2 -> Node(r,q1,custom_q,q3,q4)
  |3 -> Node(r,q1,q2,custom_q,q4)
  |4 -> Node(r,q1,q2,q3,custom_q)
  |_ -> raise (Failure "index out of bounds")


(*want to find the two regions, find the  lst is always 1 value at the end*)
let rec insert (q: 'a quadtree) (c : coord) (s:'a) : 'a quadtree =
if(check_bounds q c) then
 match q with
  | Node (r,q1,q2,q3,q4) ->( match pick_sub_tree c r q1 q2 q3 q4 with
                            |(q,n,r') -> new_Node r q1 q2 q3 q4 (insert q c s, n))
  | Leaf (r,lst) -> if (cal_diagonal(r) < min_diagonal) then Leaf(r,(c,s)::lst)
                    else
                    let ((x0,y0),(x1,y1)) = r in 
                    let (x_mid,y_mid) = ((x0+.x1)/.2., (y1+.y0)/.2.) in
                    let node = Node(r,new_tree((x_mid,y_mid),(x1,y1)),new_tree((x0,y_mid),(x_mid,y1)),new_tree((x0,y0),(x_mid,y_mid)),new_tree((x_mid,y0),(x1,y_mid))) in
                    match lst with 
                    |h::t -> insert (insert node c s) (fst h) (snd h)
                    |[] -> Leaf(r,(c,s)::lst)
else raise OutOfBounds





                
let rec fold_quad (f: 'a -> (coord * 'b)  -> 'a)
      (a: 'a) (t: 'b quadtree): 'a = match t with 
      | Leaf (r,h::t) -> List.fold_left f a (h::t)
      | Leaf (r,[]) -> a
      | Node (r,q1,q2,q3,q4) -> List.fold_left (fold_quad f) a [q1;q2;q3;q4]

let is_within (c:coord) (r:region) : bool = 
    let ((x0,y0),(x1,y1)) = r in 
    let (x,y) = c in
    if(x0<x && x1>x && y0<y && y1>y) then true
    else false

let rec in_region lst (r:region) : (coord*'a) list= 
  List.fold_left (fun accum (coord,s) -> if (is_within coord r)then (coord,s)::accum else accum) [] lst


let rec fold_region (f: 'a -> coord * 'b -> 'a) (a : 'a) (t : 'b quadtree) 
  (r : region) : 'a = match t with 
      | Leaf (rl,h::t) -> List.fold_left f a (in_region (h::t) r)
      | Leaf (rl,[]) -> a
      | Node (rn,q1,q2,q3,q4) -> List.fold_left (fun accum h -> fold_region f accum h r ) a [q1;q2;q3;q4] 


(*type coord = float * float
type region = coord * coord
type 'a quadtree =
  Node of region * 'a quadtree * 'a quadtree * 'a quadtree * 'a quadtree
  | Leaf of region * ((coord * 'a) list)
		       
let min_diagonal = 0.0000001
		     
exception OutOfBounds

let calc_diagonal (r:region) = 
  let ((x0,y0),(x1,y1)) = r in
    sqrt((x1-.x0)*.(x1-.x0) +. (y1-.y0)*.(y1-.y0))

let new_tree (r:region) : 'a quadtree = Leaf (r,[])



 (*precondition: x1,y1>x0,y0*)       
let rec insert (q: 'a quadtree) (c : coord) (s:'a) : 'a quadtree 
  =   match q with 
    Node (r,q1,q2,q3,q4) -> try insert q1 c s with _-> try insert q2 c s with _->
       try insert q3 c s with _-> 


    let (to_insert,other1,other2,other3) = order_subtrees c r q1 q2 q3 q4 in 
        Node (r,other1,other2,other3, (insert to_insert c s) )
(*Here to insert c,s to node, need to *)

  | Leaf (r,lst)-> if calc_diagonal(r)<min_diagonal then Leaf (r,(c,s)::lst) else
      let ((x0,y0),(x1,y1)) = r in
      let (x_mid,y_mid) = (x0+.x1/.2.0,y0+.y1/.2.0) in
      let (q1,q2,q3,q4) = (new_tree((x_mid,y_mid),(x1,y1)),new_tree((x0,y_mid),(x_mid,y1)),
          new_tree((x0,y0),(x_mid,y_mid)),new_tree((x_mid,y0),(x1,y_mid))) in
            match lst with 
              []    ->  Leaf (r,(c,s)::lst)
             | h::t ->  insert ( insert ( Node (r,q1,q2,q3,q4) ) c s ) (fst h) (snd h)





 (*(*precondition: x1,y1>x0,y0*)       
let rec insert (q: 'a quadtree) (c : coord) (s:'a) : 'a quadtree 
  = match q with 
    Node (r,q1,q2,q3,q4) -> insert (pick_subtree c r q1 q2 q3 q4) c s
  | Leaf (r,lst)-> if calc_diagonal(r)<min_diagonal then Leaf (r,(c,s)::lst) else
      let ((x0,y0),(x1,y1)) = r in
        let (x_mid,y_mid) = (x0+.x1/.2.0,y0+.y1/.2.0) in
          let (q1,q2,q3,q4) = (new_tree((x_mid,y_mid),(x1,y1)),new_tree((x0,y_mid),(x_mid,y1)),
             new_tree((x0,y0),(x_mid,y_mid)),new_tree((x_mid,y0),(x1,y_mid))) in
               match lst with 
                 []    ->  Leaf (r,(c,s)::lst)
               | h::t ->  insert ( insert ( Node (r,q1,q2,q3,q4) ) c s ) (fst h) (snd h)*)




(*Helper function to return subtree in which c lies 1st and other 3 2nd-4th*)
let order_subtrees c r q1 q2 q3 q4 = 
  let ((x0,y0),(x1,y1)) = r in 
  let (x_mid,y_mid) = (x0+.x1/.2.0,y0+.y1/.2.0) in
  let (x,y) = c in
    if x>=x_mid && y>=y_mid then (q1,q2,q3,q4) else
    if x<x_mid && y>=y_mid then (q2,q1,q3,q4) else
    if x<x_mid && y<y_mid then (q3,q1,q2,q3) else (q4,q1,q2,q3)

							      
let rec fold_quad (f: 'a -> (coord * 'b)  -> 'a)
		  (a: 'a) (t: 'b quadtree): 'a 
  = match t with 
    Leaf (r, h::t)       -> List.fold_left (f) a (h::t)
  | Leaf (r,[])          -> a
  | Node (r,q1,q2,q3,q4) -> List.fold_left (fold_quad f) a [q1;q2;q3;q4]
	   


let get_in_region r (coord,a)  = 
  let (x,y) = coord in
  let ((x0,y0),(x1,y1)) = r in 
     if x>x0 && x<x1 && y>y0 && y<y1 then [(coord,a)] else []


let rec fold_region (f: 'a -> coord * 'b -> 'a) (a : 'a) (t : 'b quadtree) 
  (r : region) : 'a
= match t with 
    Leaf (rl, h::t)       -> List.fold_left (f) a ( List.fold_left(fun acc b -> acc @ ( get_in_region rl b ) ) [] (h::t) )
  | Leaf (r,[])           -> a
  | Node (rn,q1,q2,q3,q4) -> let ((xn0,yn0),(xn1,yn1)) = rn in
        let ((x0,y0),(x1,y1)) = r in 
          if ( (xn0>x0 && xn0<x1) || (xn1>x0 && xn1<x1) ) && ((yn0>y0 && yn0<y1) || (yn1>y0 && yn1<y1)) then 
          List.fold_left (fun a h -> ( fold_region f a h r ) ) a [q1;q2;q3;q4]
           else a 

let l1 = Leaf ( ((0.,0.),(10.,10.)) , [] )
let l2 = Leaf ( ((-10.,0.),(0.,10.)) , [] )
let l3 = Leaf ( ((-10.,-10.),(0.,0.)) , [] )
let l4 = Leaf ( ((0.,-10.),(10.,0.)) , [] )
let n1 = Node ( ((-10.,-10.),(10.,10.)), l1, l2, l3, l4 )*) 
