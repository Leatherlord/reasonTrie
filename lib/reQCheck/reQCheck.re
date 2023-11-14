let listOfSize = QCheck.list_of_size;
let stringOfSize = QCheck.string_of_size;
let smallNat = QCheck.small_nat;

module type IReGen = {
  let intRange: (int, int) => QCheck.Gen.t(int);
};

module ReGen: IReGen = {
  let intRange = QCheck.Gen.int_range;
};
