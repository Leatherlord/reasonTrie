// open ReUtils;

let combineInts = (first, second) => first * second;

let newTrie = ReasonTrie.create();

let str = "Hello!";
let vl = 10;
let lst = List.init(String.length(str), String.get(str));

let newTrie = ReasonTrie.set(Char.compare, newTrie, lst, vl);

let str = "Hell";
let vl = 100;
let lst = List.init(String.length(str), String.get(str));

let newTrie = ReasonTrie.set(Char.compare, newTrie, lst, vl);

let str = "Helicopter";
let vl = 228;
let lst = List.init(String.length(str), String.get(str));

let newTrie = ReasonTrie.set(Char.compare, newTrie, lst, vl);

let newTrie2 = ReasonTrie.create();
let str = "SHell";
let vl = 100;
let lst = List.init(String.length(str), String.get(str));

let newTrie2 = ReasonTrie.set(Char.compare, newTrie2, lst, vl);

let str = "Helios";
let vl = 100;
let lst = List.init(String.length(str), String.get(str));

let newTrie2 = ReasonTrie.set(Char.compare, newTrie2, lst, vl);

let newTrie =
  ReasonTrie.combine(Char.compare, combineInts, newTrie, newTrie2);

ReasonTrie.print(
  0,
  Char.escaped,
  Int.to_string,
  ReasonTrie.sub(newTrie, List.init(String.length("He"), String.get("He")))
  |> ReasonTrie.filter(Int.equal(100)),
);
