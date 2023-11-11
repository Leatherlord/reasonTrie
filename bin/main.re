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

let firstTrie = StringTrie.create();
let str = "Hello!";
let vl = 10;

let firstTrie = StringTrie.set(firstTrie, str, vl);
// let firstTrie = StringTrie.set(Char.compare, firstTrie, "World!", vl);

let secondTrie = StringTrie.create();

let secondTrie = StringTrie.set(secondTrie, str, vl);
let secondTrie = StringTrie.set(secondTrie, "World!", 20);
let secondTrie = StringTrie.unset(secondTrie, "World!");

let thirdTrie = StringTrie.combine(Int.compare, firstTrie, secondTrie);

let fourthTrie = StringTrie.combine(Int.compare, secondTrie, firstTrie);

print_endline(
  if (thirdTrie == fourthTrie) {
    "Equals";
  } else {
    "Not equals";
  },
);

// StringTrie.print(0, Char.escaped, Int.to_string, newTrie);
