open OUnit2;
open ReUtils;

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

let testBasicOperations =
  "test suite for Trie set, unset, exists and find functions"
  >::: [
    "single set"
    >:: (
      _ => {
        let localTestTrie = StringTrie.set(testTrie, "Test string", 228);

        assert_equal(true, StringTrie.exists(localTestTrie, "Test string"));
        assert_equal(228, StringTrie.find(localTestTrie, "Test string"));

        let localTestTrie = StringTrie.unset(localTestTrie, "Test string");

        assert_equal(false, StringTrie.exists(localTestTrie, "Test string"));
        assert_raises(NotFound, () =>
          StringTrie.find(localTestTrie, "Test string")
        );
      }
    ),
    "multiple sets, no intersection"
    >:: (
      _ => {
        let localTestTrie =
          testTrie
          |> StringTrie.set(_, "First", 228)
          |> StringTrie.set(_, "Second", 1337)
          |> StringTrie.set(_, "Third", 1488);

        assert_equal(true, StringTrie.exists(localTestTrie, "First"));
        assert_equal(true, StringTrie.exists(localTestTrie, "Second"));
        assert_equal(true, StringTrie.exists(localTestTrie, "Third"));

        assert_equal(228, StringTrie.find(localTestTrie, "First"));
        assert_equal(1337, StringTrie.find(localTestTrie, "Second"));
        assert_equal(1488, StringTrie.find(localTestTrie, "Third"));

        let localTestTrie =
          localTestTrie
          |> StringTrie.unset(_, "First")
          |> StringTrie.unset(_, "Second")
          |> StringTrie.unset(_, "Third");

        assert_equal(false, StringTrie.exists(localTestTrie, "First"));
        assert_equal(false, StringTrie.exists(localTestTrie, "Second"));
        assert_equal(false, StringTrie.exists(localTestTrie, "Third"));

        assert_raises(NotFound, () =>
          StringTrie.find(localTestTrie, "First")
        );
        assert_raises(NotFound, () =>
          StringTrie.find(localTestTrie, "Second")
        );
        assert_raises(NotFound, () =>
          StringTrie.find(localTestTrie, "Third")
        );
      }
    ),
    "multiple sets with intersections"
    >:: (
      _ => {
        let localTestTrie =
          testTrie
          |> StringTrie.set(_, "Test First", 228)
          |> StringTrie.set(_, "Test Second", 1337)
          |> StringTrie.set(_, "Test Third", 1488);

        assert_equal(true, StringTrie.exists(localTestTrie, "Test First"));
        assert_equal(true, StringTrie.exists(localTestTrie, "Test Second"));
        assert_equal(true, StringTrie.exists(localTestTrie, "Test Third"));

        assert_equal(228, StringTrie.find(localTestTrie, "Test First"));
        assert_equal(1337, StringTrie.find(localTestTrie, "Test Second"));
        assert_equal(1488, StringTrie.find(localTestTrie, "Test Third"));

        let localTestTrie =
          localTestTrie
          |> StringTrie.unset(_, "Test First")
          |> StringTrie.unset(_, "Test Second")
          |> StringTrie.unset(_, "Test Third");

        assert_equal(false, StringTrie.exists(localTestTrie, "Test First"));
        assert_equal(false, StringTrie.exists(localTestTrie, "Test Second"));
        assert_equal(false, StringTrie.exists(localTestTrie, "Test Third"));

        assert_raises(NotFound, () =>
          StringTrie.find(localTestTrie, "Test First")
        );
        assert_raises(NotFound, () =>
          StringTrie.find(localTestTrie, "Test Second")
        );
        assert_raises(NotFound, () =>
          StringTrie.find(localTestTrie, "Test Third")
        );
      }
    ),
    "should fail (empty key set)"
    >:: (
      _ => assert_raises(NotFound, () => StringTrie.set(testTrie, "", 228))
    ),
    "should fail (no value found)"
    >:: (
      _ => assert_raises(NotFound, () => StringTrie.find(testTrie, "Queres?"))
    ),
    "should fail (nonexistent key unset)"
    >:: (
      _ =>
        assert_raises(NotFound, () => StringTrie.unset(testTrie, "Queres?"))
    ),
  ];

run_test_tt_main(testBasicOperations);
