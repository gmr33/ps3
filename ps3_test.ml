open City_search
open Assertions
open Quadtree



TEST_UNIT "new_tree" = assert (new_tree ((-180.,-90.),(-178.,-88.))=Leaf (((-180., -90.), (-178., -88.)), []))


let tree1 = new_tree ((-180.,-90.),(180.,90.))
let insert1 = insert tree1 (10.,10.) "(10,10)"
let insert2 = insert insert1 (-10.,-10.) "(-10,-10)"
let insert3 = insert insert2 (-10.,10.) "(-10,10)"
let too_small = new_tree ((0.0000001,0.0000001),(0.00000012,0.00000013))
let cant_split = insert too_small (0.00000011,0.00000011) "1st"

TEST_UNIT "insert-empty leaf" = assert (insert tree1 (10.,10.) "(10,10)" = Leaf (((-180., -90.), (180., 90.)), [((10., 10.), "(10,10)")]) )
TEST_UNIT "insert-exception" = assert_raises (Some OutOfBounds) ( insert tree1 (100.,100.) ) "(10,10)"
TEST_UNIT "insert-nonempty leaf" = assert (insert insert1 (-10.,-10.) "(-10,-10)"=  Node (((-180., -90.), (180., 90.)),Leaf (((0., 0.), (180., 90.)), [((10., 10.), "(10,10)")]),                   
   Leaf (((-180., 0.), (0., 90.)), []),                                         
   Leaf (((-180., -90.), (0., 0.)), [((-10., -10.), "(-10,-10)")]),
   Leaf (((0., -90.), (180., 0.)), [])) )
TEST_UNIT "insert-nonempty leaf subtree" = assert (insert insert3 (-100.,50.) "(-100,50)"= Node (((-180., -90.), (180., 90.)),Leaf (((0., 0.), (180., 90.)), [((10., 10.), "(10,10)")]),                   
   Node (((-180., 0.), (0., 90.)), Leaf (((-90., 45.), (0., 90.)), []),
    Leaf (((-180., 45.), (-90., 90.)), [((-100., 50.), "(-100,50)")]),
    Leaf (((-180., 0.), (-90., 45.)), []),
    Leaf (((-90., 0.), (0., 45.)), [((-10., 10.), "(-10,10)")])),
   Leaf (((-180., -90.), (0., 0.)), [((-10., -10.), "(-10,-10)")]),
   Leaf (((0., -90.), (180., 0.)), [])))
TEST_UNIT "insert-too small to split" = assert (insert cant_split (0.000000111,0.000000111) "2nd" = 
	Leaf (((1e-07, 1e-07), (1.2e-07, 1.3e-07)),[((1.11e-07, 1.11e-07), "2nd"); ((1.1e-07, 1.1e-07), "1st")]) )
(* -exception
  -insert Node
  -insert leaf empty
  -insert non-empty leaf
  -doesn't split if less than min_diagonal
  *)

let l1 = Leaf ( ((0.,0.),(10.,10.)) , [((1.,1.),"leaf1")] )
let l2 = Leaf ( ((-10.,0.),(0.,10.)) , [((-1.,1.),"leaf2")] )
let l3 = Leaf ( ((-10.,-10.),(0.,0.)) , [((-1.,-1.),"leaf3")] )
let l4 = Leaf ( ((0.,-10.),(10.,0.)) , [((1.,-1.),"leaf4" )] )
let n1 = Node ( ((-10.,-10.),(10.,10.)), l1, l2, l3, l4 )


TEST_UNIT "fold_quad"   = assert (fold_quad ( fun a ((x,y),name)-> name :: a ) [] n1 = ["leaf4"; "leaf3"; "leaf2"; "leaf1"])
TEST_UNIT "fold_quad"   = assert (fold_quad ( fun a ((x,y),name)-> name :: a ) [] l1 = ["leaf1"])

(*folds correctly over leaf
 fold correctly over node of leaves
 folds correctly over node of nodes/leaves
 *)

TEST_UNIT "fold_region" = assert (fold_region ( fun a ((x,y),name)-> name :: a ) [] n1 ((-10.,-10.),(10.,10.)) = 
	["leaf4"; "leaf3"; "leaf2"; "leaf1"] )
TEST_UNIT "fold_region" = assert (fold_region ( fun a ((x,y),name)-> name :: a ) [] n1 ((-10.,-10.),(0.,0.)) = 
	["leaf3"] )
TEST_UNIT "fold_region" = assert (fold_region ( fun a ((x,y),name)-> name :: a ) [] n1 ((-10.,-10.),(2.,0.)) = ["leaf4"; "leaf3"] )
TEST_UNIT "fold_region" = assert (fold_region ( fun a ((x,y),name)-> name :: a ) [] n1 ((-10.,-10.),(0.5,10.)) = ["leaf3"; "leaf2"])
TEST_UNIT "fold_region" = assert (fold_region ( fun a ((x,y),name)-> name :: a ) [] n1 ((-100.,-100.),(-80.,-80.)) = [] )

TEST_UNIT "load_city_data" = assert (load_city_data "test.csv" = Node (((-180., -90.), (180., 90.)), 
	Leaf (((0., 0.), (180., 90.)), [((1., 1.), "q1")]),Leaf (((-180., 0.), (0., 90.)), [((-1., 1.), "q4")]),
	Leaf (((-180., -90.), (0., 0.)), [((-1., -1.), "q4")]),Leaf (((0., -90.), (180., 0.)), [((1., -1.), "q2")])))

let city_tree = load_city_data "test.csv"
TEST_UNIT "city_search" = assert (city_search city_tree ((-180.,-90.),(-100.,-80.))=[])
TEST_UNIT "city_search" = assert (city_search city_tree ((-180.,-90.),(180.,90.))=["q1"; "q4"; "q4"; "q2"])