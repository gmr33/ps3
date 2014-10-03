(* BEGIN: DO NOT CHANGE THIS CODE, except for adding comments
   to NATN as required in exercise 1. *)
module type NATN = sig
  type t
 ((*zero is a representation of the natural number 0 *))
  val zero : t
  (*one is a representation of the natural number 1 *)
  val one : t
  (*precondition: takes in 2 natural numbers such that the sum can be represented as a natural number 
    postcondition: returns the sum represented as a natural number
    identity element: 0 ; a+0=a
    associative     : (a+b) + c === a+(b+c)
    communitative   : a+b===b+a
    *)
  val ( + ) : t -> t -> t
    (*precondition: takes in 2 natural numbers such that the product can be represented as a natural number 
    postcondition: returns the product represented as a natural number
    identity element: 1 ; a*1=a
    associative     : (a*b) * c === a*(b*c)
    communitative   : a*b===b*a
     distributive   : a*(b+c) === a*b + a*c 
   *)
  val ( * ) : t -> t -> t 
      (*precondition: takes in 2 valid representations of natural numbers  
    postcondition: returns boolean true if the first input is less than the second input and false otherwise *)
  val ( < ) : t -> t -> bool
      (*precondition: takes in 2 valid representations of natural numbers
    postcondition: returns true if the value of the natural number each represents is equal and false otherwise*)
  val ( === ) : t -> t -> bool
			    
  exception Unrepresentable
 
    (*precondition: takes in a valid representation of a natural number 
    postcondition: returns that number represented as an int value as long as it is within the range of 
    values representable by the int type in ocaml, ie: if it's greater function raises Unrepresentable exception  *)
  val int_of_nat: t -> int
      (*precondition: takes in a valid int
    postcondition: returns that number represented as an natural number, unlesss this number is not a natural number, then
    raisises Unrepresentable exception *)
  val nat_of_int: int -> t
end



module type AlienMapping = sig
  type aliensym

  val int_of_aliensym: aliensym -> int
  val one: aliensym
  val zero: aliensym
end

module AlienNatFn (M : AlienMapping) : NATN = struct
  type t = M.aliensym list
  let zero = M.zero
  let one = M.one
  exception Unrepresentable
  let ( + ) a b = 
    try a @ b with _-> raise Unrepresentable
  let ( * ) a b = try List.fold_left ( fun acc b -> acc @ a) [] b with _-> raise Unrepresentable
  let ( < ) a b = a<b
  let ( === ) a b = a=b        
  let int_of_nat sym_list = 
    try List.fold_left ( fun acc sym -> acc + ( M.int_of_aliensym  sym) ) 0 sym_list with _-> raise Unrepresentable
  let nat_of_int i = if i<0 then raise Unrepresentable else i

end

type sign = Positive | Negative
let sign_int (n:int) : sign = 
  if n >= 0 then Positive else Negative
let sum_overflows (i1:int) (i2:int) : bool = 
  sign_int i1 = sign_int i2 && sign_int(i1 + i2) <> sign_int i1
(* END: DO NOT CHANGE THIS CODE *)

(* Add your solution here for IntNat, ListNat, NatConvertFn, 
   and AlienNatFn, being careful to use the declarations and
   types specified in the problem set. *)

let append a b = List.fold_left (fun accum hd -> hd::accum) a b

module IntNat : NATN = struct
  type t = int

  exception Unrepresentable


  let zero = 0
  let one = 1
  let ( + ) a b = if (sum_overflows a b) then raise Unrepresentable else a+b
  let rec multiply a b result = match b with |0 -> result |_-> multiply a (b-1) (result + a)
  let ( * ) a b = multiply a b 0
  let ( < ) a b = a < b
  let ( === ) a b = a==b 
          

  let int_of_nat n = try (n) with _-> raise Unrepresentable
  let nat_of_int i = if(i<0)then raise Unrepresentable else i
end



module ListNat : NATN = struct
  type t = int list

  exception Unrepresentable

  let zero = []
  let one = [0]
  let ( + ) a b = try (append a b) with _-> raise Unrepresentable
  let ( * ) a b = try (List.fold_left (fun accum b -> ( + ) accum a) [] b) with _ -> raise Unrepresentable
  let ( < ) a b = (List.length a) < (List.length b)
  let ( === ) a b = (List.length a) == (List.length b)
  
let rec keepAdding n list_so_far = match n==0 with 
   true -> list_so_far
  |false -> keepAdding (n-1) (0::list_so_far)        

  let int_of_nat n = try List.length n with _-> raise Unrepresentable
  let nat_of_int i = if(i>=0) then try keepAdding i [] with _-> raise Unrepresentable else raise Unrepresentable
end

module NatConvertFn ( N : NATN ) = struct
let int_of_nat (n : N . t ): int = N.int_of_nat n
let nat_of_int (n : int ): N . t = N.nat_of_int n
end

module AlienNatFn (M:AlienMapping) : NATN = struct
  type t = M.aliensym list

  exception Unrepresentable

  let zero = [M.zero]
  let one = [M.one]
  let ( + ) a b = try (append a b) with _-> raise Unrepresentable
  let ( * ) a b = try (List.fold_left (fun accum b -> accum+a) [] b)with _ -> raise Unrepresentable
  (*0 == equal 1 == a smaller 2== b smaller*)
  let rec compare_lst a b = match a,b with
                        |h1::t1,h2::t2 -> let sub = h1-h2 in 
                                            if sub>0 then compare (sub::t1) t2 
                                            else if sub<0 then compare t1 (sub::t2) 
                                            else compare t1 t2
                        |[],[] -> 0 
                        |h::t,[] -> 2
                        |[], h::t -> 1

  let to_intLst alst = List.fold_left (fun accum aliensym -> (M.int_of_aliensym aliensym)::accum) [] alst

  let ( < ) a b = compare (to_intLst a) (to_intLst b) == 1
  let ( === ) a b = compare (to_intLst a) (to_intLst b) == 0

  let rec keepAddingAlien n list_so_far = match n==0 with 
   true -> []
  |false -> keepAddingAlien (n-1) (zero :: list_so_far)        

  let int_of_nat n = try List.fold_left (fun accum b -> Pervasives.(+) accum (M.int_of_aliensym b)) 0 n with _-> raise Unrepresentable
  let nat_of_int i = if(i>=0) then try keepAddingAlien i [] with _-> raise Unrepresentable else raise Unrepresentable
end


