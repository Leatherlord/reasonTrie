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
    printString(spaces);
    printString("(");
    printString(aToString(h.Node.key));
    printString(":");
    switch (h.Node.value) {
    | None => printString("_")
    | Some(value) => printString(bToString(value))
    };
    printString(")");
    printEndline("->");
    print(indent + 1, aToString, bToString, h.Node.children);
    print(indent, aToString, bToString, t);
  };
};

let isKeyInNodes = (nodes, key) =>
  List.exists(n => n.Node.key == key, nodes);

let findByKeyInNodes = (nodes, key) =>
  List.find(n => n.Node.key == key, nodes);

let replaceNodeByKey = (nodes, key, node) => {
  let rec aux =
    fun
    | [] => []
    | [h, ...tl] when h.Node.key == key => [node, ...tl]
    | [h, ...tl] => [h, ...aux(tl)];

  aux(nodes);
};

let removeNodeByKey = (nodes, key) => {
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

let rec getParentOfSub = tree =>
  fun
  | [] => raise(NotFound)
  | [h, ...t] when isKeyInNodes(tree, h) => {
      let node = findByKeyInNodes(tree, h);
      if (t == []) {
        node;
      } else {
        getParentOfSub(node.Node.children, t);
      };
    }
  | _ => raise(NotFound);

let sub = (tree, path) =>
  try(getParentOfSub(tree, path).Node.children) {
  | NotFound => []
  };

let find = (tree, path) => Node.getValue(getParentOfSub(tree, path));

let rec exists = tree =>
  fun
  | [] => false
  | [h, ...t] when isKeyInNodes(tree, h) => {
      let node = findByKeyInNodes(tree, h);
      if (t == []) {
        node.Node.value != None;
      } else {
        exists(node.Node.children, t);
      };
    }
  | _ => false;

let rec setOnNode = (compareKeys, node, path, value) =>
  if (path == []) {
    Node.setValue(node, value);
  } else {
    let children = set(compareKeys, node.Node.children, path, value);
    Node.setChildren(node, children);
  }
and set = (compareKeys, tree, path, value) =>
  switch (path) {
  | [] => raise(NotFound)
  | [h, ...t] when isKeyInNodes(tree, h) =>
    let node = findByKeyInNodes(tree, h);
    replaceNodeByKey(tree, h, setOnNode(compareKeys, node, t, value));
  | [h, ...t] =>
    let node = Node.empty(h);
    List.sort(
      (node1, node2) => compareKeys(node1.Node.key, node2.Node.key),
      [setOnNode(compareKeys, node, t, value), ...tree],
    );
  };

let rec unset = (tree, path) =>
  switch (path) {
  | [] => tree
  | [h, ...t] when isKeyInNodes(tree, h) =>
    let node = findByKeyInNodes(tree, h);
    let children = unset(node.Node.children, t);
    let newNode =
      if (t == []) {
        Node.setChildren(Node.empty(h), children);
      } else {
        Node.setChildren(node, children);
      };

    if (children == [] && newNode.Node.value == None) {
      removeNodeByKey(tree, h);
    } else {
      replaceNodeByKey(tree, h, newNode);
    };
  | _ => raise(NotFound)
  };

let rec combine = (compareKeys, combineValues, firstTree, secondTree) => {
  switch (firstTree, secondTree) {
  | ([], second) => second
  | (first, []) => first
  | (firstTree, [hs, ...ts]) when isKeyInNodes(firstTree, hs.Node.key) =>
    let similarNode = findByKeyInNodes(firstTree, hs.Node.key);
    let newValue =
      switch (similarNode.Node.value, hs.Node.value) {
      | (None, None) => None
      | (None, Some(secondVal)) => Some(secondVal)
      | (Some(firstVal), None) => Some(firstVal)
      | (Some(firstVal), Some(secondVal)) =>
        Some(combineValues(firstVal, secondVal))
      };

    let newNode =
      switch (newValue) {
      | None => Node.empty(similarNode.Node.key)
      | Some(value) => Node.setValue(similarNode, value)
      };

    let combinedNode =
      Node.setChildren(
        newNode,
        combine(
          compareKeys,
          combineValues,
          similarNode.Node.children,
          hs.Node.children,
        ),
      );

    let combinedFirstTree =
      List.map(
        elem =>
          if (elem.Node.key == combinedNode.Node.key) {
            combinedNode;
          } else {
            elem;
          },
        firstTree,
      );

    List.sort(
      (node1, node2) => compareKeys(node1.Node.key, node2.Node.key),
      combine(compareKeys, combineValues, combinedFirstTree, ts),
    );

  | ([hf, ...tf], [hs, ...ts]) =>
    List.sort(
      (node1, node2) => compareKeys(node1.Node.key, node2.Node.key),
      combine(compareKeys, combineValues, [hf, hs, ...tf], ts),
    )
  };
};

let rec filter = (filterFunction: 'b => bool, tree: t('a, 'b)) => {
  switch (tree) {
  | [] => []
  | [hd, ...tl] =>
    let children = filter(filterFunction, hd.Node.children);
    let newNode =
      if (hd.Node.value == None || filterFunction(Node.getValue(hd))) {
        Node.setChildren(hd, children);
      } else {
        Node.setChildren(Node.empty(hd.Node.key), children);
      };

    if (children == [] && newNode.Node.value == None) {
      filter(filterFunction, tl);
    } else {
      [newNode, ...filter(filterFunction, tl)];
    };
  };
};
