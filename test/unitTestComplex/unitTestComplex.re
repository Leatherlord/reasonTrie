open OUnit2;

module StringTrie =
  ReasonTrie.Make({
    type keyPath = string;
    type keyStep = char;
    let pathSize = String.length;
    let getStepFromPath = String.get;
    let compareSteps = Char.compare;
    let getListFromPath = str =>
      List.init(String.length(str), String.get(str));
    let getPathFromList = lst =>
      String.concat("", List.map(String.make(1), lst));
  });

let testTrie = StringTrie.create();
let testComplexOperations =
  "test suite for Trie map, fold, filter, sub and combine functions"
  >::: [
    "test map function"
    >:: (
      _ => {
        let localTestTrie =
          testTrie
          |> StringTrie.set(_, "First", 228)
          |> StringTrie.set(_, "Second", 1337)
          |> StringTrie.set(_, "Third", 1488)
          |> StringTrie.map(num => Some(Int.to_string(num)));

        assert_equal("228", StringTrie.find(localTestTrie, "First"));
        assert_equal("1337", StringTrie.find(localTestTrie, "Second"));
        assert_equal("1488", StringTrie.find(localTestTrie, "Third"));
      }
    ),
    "test fold function"
    >:: (
      _ => {
        let foldFunction = (_, value, acc) =>
          switch (value) {
          | None => acc
          | Some(value) => acc + value
          };

        let foldResult =
          testTrie
          |> StringTrie.set(_, "First", 228)
          |> StringTrie.set(_, "Second", 1337)
          |> StringTrie.set(_, "Third", 1488)
          |> StringTrie.fold(foldFunction, _, 0);

        assert_equal(228 + 1337 + 1488, foldResult);
      }
    ),
    "test filter function"
    >:: (
      _ => {
        let localTestTrie =
          testTrie
          |> StringTrie.set(_, "First", 228)
          |> StringTrie.set(_, "Second", 1337)
          |> StringTrie.set(_, "Third", 1488)
          |> StringTrie.filter(num => num mod 2 == 1);

        assert_equal(false, StringTrie.exists(localTestTrie, "First"));
        assert_equal(true, StringTrie.exists(localTestTrie, "Second"));
        assert_equal(false, StringTrie.exists(localTestTrie, "Third"));
      }
    ),
    "test sub function"
    >:: (
      _ => {
        let firstLocalTestTrie =
          testTrie
          |> StringTrie.set(_, "Test First", 228)
          |> StringTrie.set(_, "Test Second", 1337)
          |> StringTrie.set(_, "Test Third", 1488);

        let secondLocalTestTrie =
          testTrie
          |> StringTrie.set(_, "First", 228)
          |> StringTrie.set(_, "Second", 1337)
          |> StringTrie.set(_, "Third", 1488);

        assert_equal(
          secondLocalTestTrie,
          StringTrie.sub(firstLocalTestTrie, "Test "),
        );
        assert_equal(
          testTrie,
          StringTrie.sub(firstLocalTestTrie, "Queres?"),
        );
      }
    ),
    "test combine function"
    >:: (
      _ => {
        let firstLocalTestTrie =
          testTrie
          |> StringTrie.set(_, "First", 228)
          |> StringTrie.set(_, "Second", 1337)
          |> StringTrie.set(_, "Third", 1488);

        let secondLocalTestTrie =
          testTrie
          |> StringTrie.set(_, "First", 228)
          |> StringTrie.set(_, "Third", 1488);

        let thirdLocalTestTrie =
          testTrie
          |> StringTrie.set(_, "Second", 1337)
          |> StringTrie.set(_, "Third", 0);

        assert_equal(
          firstLocalTestTrie,
          StringTrie.combine(Int.max, firstLocalTestTrie, testTrie),
        );

        assert_equal(
          firstLocalTestTrie,
          StringTrie.combine(Int.max, testTrie, firstLocalTestTrie),
        );

        assert_equal(
          firstLocalTestTrie,
          StringTrie.combine(
            Int.max,
            secondLocalTestTrie,
            thirdLocalTestTrie,
          ),
        );
      }
    ),
  ];

run_test_tt_main(testComplexOperations);
