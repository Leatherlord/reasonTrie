open ReUtils;

module Node = {
  type t('a, 'b) = {
    key: 'a,
    value: option('b),
    children: list(t('a, 'b)),
  };

  let empty = key => {key, value: None, children: []};

  let getValue = node => {
    switch (node.value) {
    | None => raise(NotFound)
    | Some(value) => value
    };
  };

  let setValue = (node, value) => {...node, value: Some(value)};

  let setChildren = (node, children) => {...node, children};
};

type t('a, 'b) = list(Node.t('a, 'b));

let existsOnNode = (nodes, key) =>
  List.exists(n => n.Node.key == key, nodes);

let findOnNode = (nodes, key) => List.find(n => n.Node.key == key, nodes);

let replaceNode = (nodes, key, node) => {
  let rec aux =
    fun
    | [] => []
    | [h, ...tl] when h.Node.key == key => [node, ...tl]
    | [h, ...tl] => [h, ...aux(tl)];

  aux(nodes);
};

let removeNode = (nodes, key) => {
  let rec aux =
    fun
    | [] => raise(NotFound)
    | [h, ...tl] when h.Node.key == key => tl
    | [h, ...tl] => [h, ...aux(tl)];

  aux(nodes);
};

let create = () => [];

let rec map = (f, tree) => {
  let aux = node => {
    let value =
      switch (node.Node.value) {
      | None => None
      | Some(value) => f(value)
      };

    {...node, Node.value, Node.children: map(f, node.Node.children)};
  };

  List.filter(
    n => n.Node.value != None || n.Node.children != [],
    List.map(aux, tree),
  );
};

let rec fold = (f, tree, acc) => {
  let aux = (accu, node) =>
    fold(f, node.Node.children, f(node.Node.key, node.Node.value, accu));

  ReList.foldLeft(aux, acc, tree);
};

let rec subOnNode = tree =>
  fun
  | [] => raise(Not_found)
  | [h, ...t] when existsOnNode(tree, h) => {
      let node = findOnNode(tree, h);
      if (t == []) {
        node;
      } else {
        subOnNode(node.Node.children, t);
      };
    }
  | [_, ..._] => raise(NotFound);

let sub = (tree, path) =>
  try(subOnNode(tree, path).Node.children) {
  | NotFound => []
  };

let find = (tree, path) => Node.getValue(subOnNode(tree, path));

let rec exists = tree =>
  fun
  | [] => false
  | [h, ...t] when existsOnNode(tree, h) => {
      let node = findOnNode(tree, h);
      if (t == []) {
        node.Node.value != None;
      } else {
        exists(node.Node.children, t);
      };
    }
  | [_, ..._] => false;

let rec setOnNode = (node, path, value) =>
  if (path == []) {
    Node.setValue(node, value);
  } else {
    let children = set(node.Node.children, path, value);
    Node.setChildren(node, children);
  }
and set = (tree, path, value) =>
  switch (path) {
  | [] => raise(NotFound)
  | [h, ...t] =>
    if (existsOnNode(tree, h)) {
      let node = findOnNode(tree, h);
      replaceNode(tree, h, setOnNode(node, t, value));
    } else {
      let node = Node.empty(h);
      [setOnNode(node, t, value), ...tree];
    }
  };

let rec unset = tree =>
  fun
  | [] => tree
  | [h, ...t] when existsOnNode(tree, h) => {
      let node = findOnNode(tree, h);
      let children = unset(node.Node.children, t);
      let newNode =
        if (t == []) {
          Node.setChildren(Node.empty(h), children);
        } else {
          Node.setChildren(node, children);
        };

      if (children == [] && newNode.Node.value == None) {
        removeNode(tree, h);
      } else {
        replaceNode(tree, h, newNode);
      };
    }
  | [_, ..._] => raise(NotFound);

let rec print =
        (
          indent: int,
          aToString: 'a => string,
          bToString: 'b => string,
          tree: t('a, 'b),
        ) => {
  switch (tree) {
  | [] => ()
  | [h, ...t] =>
    let spaces = String.make(indent, ':');
    print_string(spaces);
    print_string("(");
    print_string(aToString(h.Node.key));
    print_string(":");
    switch (h.Node.value) {
    | None => print_string("_")
    | Some(value) => print_string(bToString(value))
    };
    print_string(")");
    printEndline("->");
    print(indent + 1, aToString, bToString, h.Node.children);
    print(indent, aToString, bToString, t);
  };
};
