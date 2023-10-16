// open OUnit2;
// open ReasonEulerProject;
// let testsP13MonolithRecursive =
//   "test suite for problem 13 monolith recursive"
//   >::: [
//     "single number"
//     >:: (
//       _ =>
//         assert_equal("1234567890", p13monolithRecursive("123456789012345"))
//     ),
//     "easy case"
//     >:: (
//       _ =>
//         assert_equal(
//           "2222222222",
//           p13monolithRecursive("111111111111111
// 111111111111111"),
//         )
//     ),
//     "should fail (input too short)"
//     >:: (
//       _ =>
//         assert_raises(Invalid_argument("String.sub / Bytes.sub"), () =>
//           p13monolithRecursive("12345")
//         )
//     ),
//   ];
// let testsP13MonolithTailRecursive =
//   "test suite for problem 13 monolith tail recursive"
//   >::: [
//     "single number"
//     >:: (
//       _ =>
//         assert_equal(
//           "1234567890",
//           p13monolithTailRecursive("123456789012345"),
//         )
//     ),
//     "easy case"
//     >:: (
//       _ =>
//         assert_equal(
//           "2222222222",
//           p13monolithTailRecursive("111111111111111
// 111111111111111"),
//         )
//     ),
//     "should fail (input too short)"
//     >:: (
//       _ =>
//         assert_raises(Invalid_argument("String.sub / Bytes.sub"), () =>
//           p13monolithTailRecursive("12345")
//         )
//     ),
//   ];
// let testsP18MonolithRecursive =
//   "test suite for problem 18 monolith recursive"
//   >::: [
//     "single number" >:: (_ => assert_equal(24, p18monolithRecursive("24"))),
//     "easy case"
//     >:: (_ => assert_equal(10, p18monolithRecursive("1
// 2 3
// 4 5 6"))),
//     "should fail (cannot cast empty string to int)"
//     >:: (
//       _ =>
//         assert_raises(Failure("int_of_string"), () =>
//           p18monolithRecursive("")
//         )
//     ),
//   ];
// let testsP18MonolithTailRecursive =
//   "test suite for problem 18 monolith tail recursive"
//   >::: [
//     "single number"
//     >:: (_ => assert_equal(24, p18monolithTailRecursive("24"))),
//     "easy case"
//     >:: (_ => assert_equal(10, p18monolithTailRecursive("1
// 2 3
// 4 5 6"))),
//     "should fail (cannot cast empty string to int)"
//     >:: (
//       _ =>
//         assert_raises(Failure("int_of_string"), () =>
//           p18monolithTailRecursive("")
//         )
//     ),
//   ];
// let testsP13Modular =
//   "test suite for problem 13 modular"
//   >::: [
//     "single number"
//     >:: (_ => assert_equal("1234567890", p13modular("123456789012345"))),
//     "easy case"
//     >:: (
//       _ =>
//         assert_equal(
//           "2222222222",
//           p13modular("111111111111111
// 111111111111111"),
//         )
//     ),
//     "should fail (input too short)"
//     >:: (
//       _ =>
//         assert_raises(Invalid_argument("String.sub / Bytes.sub"), () =>
//           p13modular("12345")
//         )
//     ),
//   ];
// let testsP18Modular =
//   "test suite for problem 18 modular"
//   >::: [
//     "single number" >:: (_ => assert_equal(24, p18modular("24"))),
//     "easy case" >:: (_ => assert_equal(10, p18modular("1
// 2 3
// 4 5 6"))),
//     "should fail (cannot cast empty string to int)"
//     >:: (_ => assert_raises(Failure("int_of_string"), () => p18modular(""))),
//   ];
// run_test_tt_main(testsP13MonolithRecursive);
// run_test_tt_main(testsP13MonolithTailRecursive);
// run_test_tt_main(testsP18MonolithRecursive);
// run_test_tt_main(testsP18MonolithTailRecursive);
// run_test_tt_main(testsP13Modular);
// run_test_tt_main(testsP18Modular);
