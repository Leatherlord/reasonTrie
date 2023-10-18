type t('a, 'b);
let create: unit => t('a, 'b);
let map: ('b => option('c), t('a, 'b)) => t('a, 'c);
let fold: (('a, option('b), 'c) => 'c, t('a, 'b), 'c) => 'c;
let sub: (t('a, 'b), list('a)) => t('a, 'b);
let find: (t('a, 'b), list('a)) => 'b;
let exists: (t('a, 'b), list('a)) => bool;
let set: (('a, 'a) => int, t('a, 'b), list('a), 'b) => t('a, 'b);
let unset: (t('a, 'b), list('a)) => t('a, 'b);
let combine:
  (('a, 'a) => int, ('b, 'b) => 'b, t('a, 'b), t('a, 'b)) => t('a, 'b);
let print: (int, 'a => string, 'b => string, t('a, 'b)) => unit;
