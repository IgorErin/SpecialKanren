open OCanren

let false_and_true x y = conde [ x =/= !!true &&& (y === !!false); y === !!true ]
