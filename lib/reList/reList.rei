let foldLeft: (('a, 'b) => 'a, 'a, list('b)) => 'a;
let foldRight: (('a, 'b) => 'b, list('a), 'b) => 'b;
let ofSeq: Seq.t('a) => list('a);
