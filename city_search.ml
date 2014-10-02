open Parser
open Quadtree


let load_city_data (s:string) : string quadtree = 
      let t_init = new_tree((-180.,-90.),(180.,90.)) in 
      let city_list =  parse s in
        List.fold_left (fun t (y,x,name) -> insert t (x,y) name ) t_init city_list




let city_search (q: string quadtree) (r : region) : string list = 
	fold_region ( fun a (coord,name) -> a @ [name] ) [] q r
