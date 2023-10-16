// open ReUtils;

let newTrie = ReasonTrie.create();

let str = "Hello!";
let vl = 10;
let lst = List.init(String.length(str), String.get(str));

let newTrie = ReasonTrie.set(newTrie, lst, vl);

let str = "Hell";
let vl = 100;
let lst = List.init(String.length(str), String.get(str));

let newTrie = ReasonTrie.set(newTrie, lst, vl);

let str = "Helicopter";
let vl = 228;
let lst = List.init(String.length(str), String.get(str));

let newTrie = ReasonTrie.set(newTrie, lst, vl);

let str = "SHell";
let vl = 100;
let lst = List.init(String.length(str), String.get(str));

let newTrie = ReasonTrie.set(newTrie, lst, vl);

let str = "Helios";
let vl = 100;
let lst = List.init(String.length(str), String.get(str));

let newTrie = ReasonTrie.set(newTrie, lst, vl);

// let isPresent = ReasonTrie.exists(newTrie, lst);

// if (isPresent) {
//   let value = ReasonTrie.find(newTrie, lst);
//   print_string("Result is: ");
//   print_int(value);
//   print_newline();
// } else {
//   printEndline("Not working! :(");
// };

ReasonTrie.print(0, Char.escaped, Int.to_string, newTrie);
