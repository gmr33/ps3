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

module type IntNat : NATN = struct
  type t = int
  let zero = 0
  let one = 1
  exception Unrepresentable
  let ( + ) a b = 
    try a + b with _-> raise Unrepresentable
  let ( * ) a b =
    try a * b with _-> raise Unrepresentable
  let ( < ) a b = a<b
  let ( === ) a b = a=b        
  let int_of_nat n = n
  let nat_of_int i = if i<0 then raise Unrepresentable else i

end



module type ListNat : NATN = struct
  type t = int list
  let zero = []
  let one = [0]
  exception Unrepresentable
  let ( + ) a b = 
    try a @ b with _-> raise Unrepresentable
  let ( * ) a b = try List.fold_left ( fun acc b -> acc @ a) [] b with _-> raise Unrepresentable
  let ( < ) a b = List.length(a) < List.length(b)
  let ( === ) a b = List.length(a) < List.length(b)     
  let rec keepAdding n list_so_far = match n==0 with
       true  -> []
     | false -> keepAdding (n-1) (list_so_far@[0])
  let int_of_nat n =  length (n)
  let nat_of_int i = if i>=0 then try keepAdding i [] with _-> raise Unrepresentable else Uraise nrepresentable

end




