# ReasonML: Prefix Tree (Trie) based dictionary
## Functional Programming course second assignment. 
Plymorphyc monoid Trie implemented as Functor in Reason ML.

# Лабораторная работа №2 по предмету: «Функциональное программирование»

Выполнил:
Бойко В. А.

В рамках данной лабораторной работы мной был реализован функтор `ReasonTrie.Make`, позволяющий путем его параметризации получить модуль, реализующий словарь на префиксном дереве. Список поддерживаемых операций:
- `let create: unit => t('b);` -- создание пустого экземпляра коллекции
- `let map: ('b => option('c), t('b)) => t('c);` -- отображение
- `let fold: ((Iterable.keyStep, option('b), 'c) => 'c, t('b), 'c) => 'c;` -- свертка
- `let sub: (t('b), Iterable.keyPath) => t('b);` -- выделение поддерева
- `let find: (t('b), Iterable.keyPath) => 'b;` -- поиск по ключу
- `let exists: (t('b), Iterable.keyPath) => bool;` -- поиск ключа
- `let set: (t('b), Iterable.keyPath, 'b) => t('b);` -- вставка значения по ключу
- `let unset: (t('b), Iterable.keyPath) => t('b);` -- удаление значения по ключу
- `let combine: (('b, 'b) => 'b, t('b), t('b)) => t('b);` -- комбинация словарей, обеспечивающая моноидность структуры
- `let filter: ('b => bool, t('b)) => t('b);` -- фильтрация

Код самых интересных функций приведен ниже (функции из модуля ReList, используемые для простоты реализации, тоже реализованы вручную - см. `/lib/reList/reList.re`):

```Reason
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
  };
  . . .
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
  . . .
    let rec fold = (f, tree, acc) => {
      let aux = (accu, node) =>
        fold(f, node.Node.children, f(node.Node.key, node.Node.value, accu));
      ReList.foldLeft(aux, acc, tree);
    };
  . . .
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
  . . .
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
```

Так же в рамках работы были реализованы Unit-тесты и Property-based тестирование. Пример кода PBT:

```Reason
let monoidTest =
  QCheck.Test.make(
    ~count=1000,
    ~name="Trie is monoid",
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
      let neutralTrie = StringTrie.create();

      StringTrie.combine(Int.max, firstTrie, secondTrie)
      == StringTrie.combine(Int.max, secondTrie, firstTrie)
      && StringTrie.combine(Int.max, firstTrie, neutralTrie)
      == StringTrie.combine(Int.max, neutralTrie, firstTrie)
      && StringTrie.combine(Int.max, firstTrie, neutralTrie) == firstTrie;
    }
  );
```

Результаты тестирования:

```
......
Ran: 6 tests in: 0.10 seconds.
OK
.....
Ran: 5 tests in: 0.10 seconds.
OK

random seed: 294315798
================================================================================
success (ran 3 tests)
```

# Заключение

Эту лабу, почему-то, было делать не так приятно, как первую. Было сложно заставить себя сесть и написать эти несчастные 600 строк кода, и не совсем понятно, почему именно. Хотя после завершения работы чувство Accomplishment'а пришло. Понравилась концепция функтора - высокоуровневая штука, я такое люблю. Касательно PBT - ничего особенного. Писать генераторы не круто, даже когда за тебя половину работы делает фреймворк. В целом отношение к лабе - нейтральное.
