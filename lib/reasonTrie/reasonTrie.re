open ReUtils;

module type IterablePath = {
  type keyPath;
  type keyStep;
  let pathSize: keyPath => int;
  let getStepFromPath: (keyPath, int) => keyStep;
  let compareSteps: (keyStep, keyStep) => int;
  let getListFromPath: keyPath => list(keyStep);
  let getPathFromList: list(keyStep) => keyPath;
};

module type TrieFunctor =
  (Iterable: IterablePath) =>
   {
    type t('b);
    let create: unit => t('b);
    let map: ('b => option('c), t('b)) => t('c);
    let fold: ((Iterable.keyStep, option('b), 'c) => 'c, t('b), 'c) => 'c;
    let sub: (t('b), Iterable.keyPath) => t('b);
    let find: (t('b), Iterable.keyPath) => 'b;
    let exists: (t('b), Iterable.keyPath) => bool;
    let set: (t('b), Iterable.keyPath, 'b) => t('b);
    let unset: (t('b), Iterable.keyPath) => t('b);
    let combine: (('b, 'b) => 'b, t('b), t('b)) => t('b);
    let filter: ('b => bool, t('b)) => t('b);
    let compareEmptyFalse: (t('b), t('b)) => bool;
  };

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

module Make: TrieFunctor =
  (Iterable: IterablePath) => {
    type t('b) = list(Node.t(Iterable.keyStep, 'b));
    let create = () => [];

    let isKeyInNodes = (nodes, key) =>
      ReList.exists(n => n.Node.key == key, nodes);

    let findByKeyInNodes = (nodes, key) =>
      ReList.find(n => n.Node.key == key, nodes);

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

    let rec map = (f, tree) => {
      let aux = node => {
        let value =
          switch (node.Node.value) {
          | None => None
          | Some(value) => f(value)
          };
        {...node, Node.value, Node.children: map(f, node.Node.children)};
      };
      ReList.filter(
        n => n.Node.value != None || n.Node.children != [],
        ReList.map(aux, tree),
      );
    };

    let rec fold = (f, tree, acc) => {
      let aux = (accu, node) =>
        fold(f, node.Node.children, f(node.Node.key, node.Node.value, accu));
      ReList.foldLeft(aux, acc, tree);
    };

    let rec getParentOfSub = (tree, path) =>
      switch (path) {
      | [] => raise(NotFound)
      | [h, ...t] when isKeyInNodes(tree, h) && t == [] =>
        findByKeyInNodes(tree, h)
      | [h, ...t] when isKeyInNodes(tree, h) =>
        findByKeyInNodes(tree, h).Node.children |> getParentOfSub(_, t)
      | _ => raise(NotFound)
      };

    let sub = (tree, path) =>
      try(getParentOfSub(tree, Iterable.getListFromPath(path)).Node.children) {
      | NotFound => []
      };

    let find = (tree, path) =>
      Node.getValue(getParentOfSub(tree, Iterable.getListFromPath(path)));

    let rec exists = (tree, path) =>
      switch (Iterable.getListFromPath(path)) {
      | [] => false
      | [h, ...t] when isKeyInNodes(tree, h) =>
        let node = findByKeyInNodes(tree, h);
        if (t == []) {
          node.Node.value != None;
        } else {
          exists(node.Node.children, Iterable.getPathFromList(t));
        };
      | _ => false
      };

    let rec setOnNode = (node, path, value) =>
      switch (Iterable.getListFromPath(path)) {
      | [] => Node.setValue(node, value)
      | _ => set(node.Node.children, path, value) |> Node.setChildren(node)
      }

    and set = (tree, path, value) =>
      switch (Iterable.getListFromPath(path)) {
      | [] => raise(NotFound)
      | [h, ...t] when isKeyInNodes(tree, h) =>
        findByKeyInNodes(tree, h)
        |> setOnNode(_, Iterable.getPathFromList(t), value)
        |> replaceNodeByKey(tree, h)
      | [h, ...t] =>
        List.sort(
          (node1, node2) =>
            Iterable.compareSteps(node1.Node.key, node2.Node.key),
          [
            setOnNode(Node.empty(h), Iterable.getPathFromList(t), value),
            ...tree,
          ],
        )
      };

    let rec unset = (tree, path) =>
      switch (Iterable.getListFromPath(path)) {
      | [] => tree
      | [h, ...t] when isKeyInNodes(tree, h) =>
        let node = findByKeyInNodes(tree, h);
        let children =
          unset(node.Node.children, Iterable.getPathFromList(t));
        let newNode =
          switch (t) {
          | [] => Node.setChildren(Node.empty(h), children)
          | _ => Node.setChildren(node, children)
          };
        if (children == [] && newNode.Node.value == None) {
          removeNodeByKey(tree, h);
        } else {
          replaceNodeByKey(tree, h, newNode);
        };
      | _ => raise(NotFound)
      };

    let rec combine = (combineValues, firstTree, secondTree) => {
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
              combineValues,
              similarNode.Node.children,
              hs.Node.children,
            ),
          );
        let combinedFirstTree =
          ReList.map(
            elem =>
              if (elem.Node.key == combinedNode.Node.key) {
                combinedNode;
              } else {
                elem;
              },
            firstTree,
          );
        List.sort(
          (node1, node2) =>
            Iterable.compareSteps(node1.Node.key, node2.Node.key),
          combine(combineValues, combinedFirstTree, ts),
        );
      | ([hf, ...tf], [hs, ...ts]) =>
        List.sort(
          (node1, node2) =>
            Iterable.compareSteps(node1.Node.key, node2.Node.key),
          combine(combineValues, [hf, hs, ...tf], ts),
        )
      };
    };

    let rec filter = (filterFunction: 'b => bool, tree: t('b)) => {
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

    let compareEmptyFalse = (first, second) =>
      switch (first, second) {
      | ([], []) => false
      | (first, second) => first == second
      };
  };
