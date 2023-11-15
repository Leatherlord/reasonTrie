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

let listForTrieGenerator =
  ReQCheck.(
    listOfSize(
      ReGen.intRange(1, 100),
      QCheck.pair(stringOfSize(ReGen.intRange(10, 20)), smallNat),
    )
  );

let monoidTest =
  QCheck.Test.make(
    ~count=1000,
    ~name="Trie is monoid",
    QCheck.(
      tup3(listForTrieGenerator, listForTrieGenerator, listForTrieGenerator)
    ),
    triplet =>
    switch (triplet) {
    | (first, second, third) =>
      let rec aux = (tree, lst) =>
        switch (lst) {
        | [] => tree
        | [hd, ...tl] =>
          switch (hd) {
          | (key, value) => StringTrie.set(tree, key, value) |> aux(_, tl)
          }
        };
      let firstTrie = first |> aux(StringTrie.create());
      let secondTrie = second |> aux(StringTrie.create());
      let thirdTrie = third |> aux(StringTrie.create());
      let neutralTrie = StringTrie.create();

      let checkAssociativity =
        StringTrie.combine(
          Int.max,
          firstTrie,
          StringTrie.combine(Int.max, secondTrie, thirdTrie),
        )
        == StringTrie.combine(
             Int.max,
             StringTrie.combine(Int.max, firstTrie, secondTrie),
             thirdTrie,
           );

      let checkNeutralElement =
        StringTrie.combine(Int.max, firstTrie, neutralTrie)
        == StringTrie.combine(Int.max, neutralTrie, firstTrie)
        && StringTrie.combine(Int.max, firstTrie, neutralTrie) == firstTrie;

      checkAssociativity && checkNeutralElement;
    }
  );

let commutativityTest =
  QCheck.Test.make(
    ~count=1000,
    ~name="Test for commutativity",
    QCheck.(pair(listForTrieGenerator, listForTrieGenerator)),
    pair =>
    switch (pair) {
    | (first, second) =>
      let rec aux = (tree, lst) =>
        switch (lst) {
        | [] => tree
        | [hd, ...tl] =>
          switch (hd) {
          | (key, value) => StringTrie.set(tree, key, value) |> aux(_, tl)
          }
        };
      let firstTrie = first |> aux(StringTrie.create());
      let secondTrie = second |> aux(StringTrie.create());

      let shouldBeCommutative =
        StringTrie.combine(Int.max, firstTrie, secondTrie)
        == StringTrie.combine(Int.max, secondTrie, firstTrie);

      let takeSecond = (_: int, second: int) => second;

      let shouldNotBeCommutative =
        StringTrie.compareEmptyFalse(
          StringTrie.combine(takeSecond, firstTrie, secondTrie),
          StringTrie.combine(takeSecond, secondTrie, firstTrie),
        );

      shouldBeCommutative && shouldNotBeCommutative;
    }
  );

let setFindTest =
  QCheck.Test.make(
    ~count=1000,
    ~name="Set is setting and Find is finding",
    listForTrieGenerator,
    genList => {
      let rec aux = (tree, lst) =>
        switch (lst) {
        | [] => true
        | [hd, ...tl] =>
          switch (hd) {
          | (key, value) =>
            let newTree = StringTrie.set(tree, key, value);
            switch (StringTrie.find(newTree, key) == value) {
            | false => false
            | true => aux(newTree, tl)
            };
          }
        };
      genList |> aux(StringTrie.create());
    },
  );

let foldTest =
  QCheck.Test.make(
    ~count=1000,
    ~name="Fold result is correct",
    listForTrieGenerator,
    genList => {
      let rec aux = (tree, lst, acc) =>
        switch (lst) {
        | [] => (tree, acc)
        | [hd, ...tl] =>
          switch (hd) {
          | (key, value) =>
            StringTrie.set(tree, key, value) |> aux(_, tl, acc + value)
          }
        };

      let foldFunction = (_, value, acc) =>
        switch (value) {
        | None => acc
        | Some(value) => acc + value
        };

      let (tree, sum) = genList |> aux(StringTrie.create(), _, 0);
      sum == StringTrie.fold(foldFunction, tree, 0);
    },
  );

QCheck_runner.run_tests(
  ~verbose=true,
  [monoidTest, commutativityTest, setFindTest, foldTest],
);
