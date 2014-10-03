open Assertions
open Nats


(*testing for IntNat*)
TEST_UNIT "IntNat_zero_test1" = assert_true(IntNat.zero = IntNat.nat_of_int 0)
TEST_UNIT "IntNat_one_test1" = assert_true(IntNat.one = IntNat.nat_of_int 1)
(*checking for + with its identity elemnt 0*)
TEST_UNIT "IntNat_add_test1" = assert_true(IntNat.(+) (IntNat.nat_of_int 0) (IntNat.nat_of_int 10) = IntNat.nat_of_int 10)
TEST_UNIT "IntNat_add_test2" = assert_true(IntNat.(+) (IntNat.nat_of_int 10) (IntNat.zero) = IntNat.nat_of_int 10)
TEST_UNIT "IntNat_add_test3" = assert_true(IntNat.(+) (IntNat.nat_of_int 5) (IntNat.nat_of_int 5) = IntNat.nat_of_int 10)
TEST_UNIT "IntNat_add_test4" = assert_true(IntNat.(+) (IntNat.one) (IntNat.nat_of_int 5) = IntNat.nat_of_int 6)
(*checking for Int overflow*)
TEST_UNIT "IntNat_add_test5" = assert_raises (Some IntNat.Unrepresentable) (IntNat.(+) (IntNat.nat_of_int max_int)) (IntNat.nat_of_int max_int)
TEST_UNIT "IntNat_add_test6" = assert_raises (Some IntNat.Unrepresentable) (IntNat.(+) (IntNat.nat_of_int max_int)) (IntNat.nat_of_int 1 )

(*checking for * with its identity elemnt 0*)
TEST_UNIT "IntNat_multiply_test1" = assert_true(IntNat.( * ) (IntNat.one) (IntNat.nat_of_int 10) = IntNat.nat_of_int 10)
TEST_UNIT "IntNat_multiply_test2" = assert_true(IntNat.( * ) (IntNat.zero) (IntNat.nat_of_int 10) = IntNat.zero)
TEST_UNIT "IntNat_multiply_test3" = assert_true(IntNat.( * ) (IntNat.nat_of_int 10) (IntNat.nat_of_int 10) = IntNat.nat_of_int 100)
TEST_UNIT "IntNat_multiply_test4" = assert_true(IntNat.( * ) (IntNat.nat_of_int 6) (IntNat.nat_of_int 10) = IntNat.nat_of_int 60)
(*checking for Int overflow*)
TEST_UNIT "IntNat_add_test5" = assert_raises (Some IntNat.Unrepresentable) (IntNat.( * ) (IntNat.nat_of_int max_int)) (IntNat.nat_of_int max_int)
TEST_UNIT "IntNat_add_test6" = assert_raises (Some IntNat.Unrepresentable) (IntNat.( * ) (IntNat.nat_of_int max_int)) (IntNat.nat_of_int 2 )


(*checking (===) *)
TEST_UNIT "IntNat_equal_test1" = assert_true(IntNat.( === ) IntNat.zero IntNat.zero)
TEST_UNIT "IntNat_equal_test2" = assert_false(IntNat.( === ) IntNat.zero IntNat.one)
(*checking ( < ) *)
TEST_UNIT "IntNat_smaller_test1" = assert_true(IntNat.( < ) IntNat.zero IntNat.one)
TEST_UNIT "IntNat_smaller_test2" = assert_false(IntNat.( < ) IntNat.one IntNat.zero)

TEST_UNIT "IntNat_int_of_nat_test1" = assert_true(IntNat.int_of_nat IntNat.one = 1)
TEST_UNIT "IntNat_int_of_nat_test2" = assert_true(IntNat.int_of_nat IntNat.zero = 0)

TEST_UNIT "IntNat_nat_of_int_test1" = assert_true(IntNat.nat_of_int 0 = IntNat.zero)
TEST_UNIT "IntNat_nat_of_int_test2" = assert_true(IntNat.nat_of_int 1 = IntNat.one)
(*fix negative*)
TEST_UNIT "IntNat_nat_of_int_test3" = assert_raises (Some IntNat.Unrepresentable) (IntNat.nat_of_int ) (-1)


(*-----------------------------------------------------------------------------------------------------------*)


(*testing for ListNat*)
TEST_UNIT "ListNat_zero_test1" = assert_true(ListNat.zero = ListNat.nat_of_int 0)
TEST_UNIT "ListNat_one_test1" = assert_true(ListNat.one = ListNat.nat_of_int 1)
(*checking for + with its identity elemnt 0*)
TEST_UNIT "ListNat_add_test1" = assert_true(ListNat.(+) (ListNat.nat_of_int 0) (ListNat.nat_of_int 3) = ListNat.nat_of_int 3)
TEST_UNIT "ListNat_add_test2" = assert_true(ListNat.(+) (ListNat.nat_of_int 10) (ListNat.zero) = ListNat.nat_of_int 10)
TEST_UNIT "ListNat_add_test3" = assert_true(ListNat.(+) (ListNat.nat_of_int 100000) (ListNat.nat_of_int 100000) = ListNat.nat_of_int 200000)
TEST_UNIT "ListNat_add_test4" = assert_true(ListNat.(+) (ListNat.one) (ListNat.nat_of_int 5) = ListNat.nat_of_int 6)

(*checking for Int overflow
TEST_UNIT "ListNat_add_test5" = assert_raises (Some ListNat.Unrepresentable) (ListNat.(+) (ListNat.nat_of_int max_int)) (ListNat.nat_of_int max_int)
TEST_UNIT "ListNat_add_test6" = assert_raises (Some ListNat.Unrepresentable) (ListNat.(+) (ListNat.nat_of_int max_int)) (ListNat.nat_of_int 1 )
*)

(*checking for * with its identity elemnt 0*)
TEST_UNIT "ListNat_multiply_test1" = assert_true(ListNat.( * ) (ListNat.one) (ListNat.nat_of_int 10) = ListNat.nat_of_int 10)
TEST_UNIT "ListNat_multiply_test2" = assert_true(ListNat.( * ) (ListNat.zero) (ListNat.nat_of_int 10) = ListNat.zero)
TEST_UNIT "ListNat_multiply_test3" = assert_true(ListNat.( * ) (ListNat.nat_of_int 10) (ListNat.nat_of_int 10) = ListNat.nat_of_int 100)

(*checking for Int overflow
TEST_UNIT "ListNat_add_test5" = assert_raises (Some ListNat.Unrepresentable) (ListNat.( * ) (ListNat.nat_of_int max_int)) (ListNat.nat_of_int max_int)
TEST_UNIT "ListNat_add_test6" = assert_raises (Some ListNat.Unrepresentable) (ListNat.( * ) (ListNat.nat_of_int max_int)) (ListNat.nat_of_int 2 )
*)

(*checking (===) *)
TEST_UNIT "ListNat_equal_test1" = assert_true(ListNat.( === ) ListNat.zero ListNat.zero)
TEST_UNIT "ListNat_equal_test2" = assert_false(ListNat.( === ) ListNat.zero ListNat.one)
(*checking ( < ) *)
TEST_UNIT "ListNat_smaller_test1" = assert_true(ListNat.( < ) ListNat.zero ListNat.one)
TEST_UNIT "ListNat_smaller_test2" = assert_false(ListNat.( < ) ListNat.one ListNat.zero)

TEST_UNIT "ListNat_int_of_nat_test1" = assert_true(ListNat.int_of_nat ListNat.one = 1)
TEST_UNIT "ListNat_int_of_nat_test2" = assert_true(ListNat.int_of_nat ListNat.zero = 0)

TEST_UNIT "ListNat_nat_of_int_test1" = assert_true(ListNat.nat_of_int 0 = ListNat.zero)
TEST_UNIT "ListNat_nat_of_int_test2" = assert_true(ListNat.nat_of_int 1 = ListNat.one)
(*fix negative*)
(*TEST_UNIT "ListNat_nat_of_int_test3" = assert_raises (Some ListNat.Unrepresentable) (ListNat.nat_of_int ((~-)1))*)
 (*-------------------------------------------------------------------------------------------------*)
 (*Nat Conversion*)


 (*Alien Mapping *)
(*maps a b c d e f -> 0 1 2 3 4 5*)
 (*
 module StringMapping : AlienMapping = struct
  type t = string
  let int_of_aliensym sym = match sym with
  						| a -> 0 | b -> 1| c -> 2 | d -> 3 | e -> 4 | f -> 5
  let one = b
  let zero = a 							
 end	*)

