# SpecialKanren

An attempt to specialize relational programs by arguments with a finite domain.

## Relational programming.
Relational programming is all about relationships.

Having expressed a certain problem as an relation, you can run it in different directions. Each such direction represents a separate function. For example, the ratio is less than or equal to, represents a function that tells two numbers whether the above-mentioned ratio is satisfies them, as its last argument. However, if we explicitly say that the relation holds, then we can get a generator of all such numbers.

``` ocaml
 let rec le x y is =
    conde
      [ x === !!O &&& (is === !!true)
      ; x =/= !!O &&& (y === !!O) &&& (!!false === is)
      ; Fresh.two (fun x' y' -> 
        x === !!(S x') 
        &&& (y === !!(S y'))
        &&& le x' y' is)
      ]
  ;;

```

But as always, the implementation of such an abstract approach leaves much to be desired in terms of performance.

## Target

Therefore, in this work, an attempt is made to specialize each relation under pre-known parameters. Namely, the parameters with the final domain.

``` ocaml 

  let rec le_false x y = 
    ((x =/= (!! O)) &&& (y === (!! O)))
    ||| (Fresh.two (fun x' y' ->
        (x === (!! (S (x')))) 
        &&& ((y === (!! (S (y'))))
         &&& (le_false x' y'))))

  and le_true x y =
   (x === (!! O)) 
   ||| (Fresh.two (fun x' y' -> 
    (x === (!! (S (x')))) 
    &&& ((y === (!! (S (y')))) 
    &&& (le_true x' y'))))

```

Thus, the ratio less than or equal to the one mentioned above will be expressed as two ratios, for two values of its parameter, `false` and `true` respectively. 

And it seems to me that this is the very specialization that each of us has dreamed of realizing since early childhood!

It's all very interesting, but what about performance.

## A bit more fun

Parity ratio.

``` ocaml 

let rec is_even n res =
  conde
    [ n === !!O &&& (res === !!true)
    ; Fresh.two (fun pred_n not_res ->
        n
        === !!(S pred_n)
        &&& conde
              [ res === !!true &&& (not_res === !!false)
              ; res === !!false &&& (not_res === !!true)
              ]
        &&& is_even pred_n not_res)
    ]
;;

```
The parity ratio, well, what's complicated here. However, the implementation presented above depends exponentially on `n'.

But the specialized program depends linearly.

``` ocaml 

let rec is_even_false n = 
    Fresh.two (fun not_res pred_n -> 
        (n === (!! (S (pred_n)))) &&& ((not_res === (!! true)) &&& (is_even_true pred_n)))

and is_even_true n = 
    (n === (!! O)) 
    ||| (Fresh.two (fun not_res pred_n -> 
        (n === (!! (S (pred_n)))) &&& ((not_res === (!! false)) &&& (is_even_false pred_n))))

```
## Benchmarks 
TODO()

## Further work
- Reduce unnecessary variables. This becomes difficult with recursive calls and with the =/= relation.
- The dnf acts as an intermediate representation. We need a more cunning way of detecting inappropriate conjuncts.
- When processing calls
  - The number of their parameters may decrease
  - With specialization, they can be reduced (satisfied always or never).

## License 
SpecialKanren is distributed under the MIT License.
