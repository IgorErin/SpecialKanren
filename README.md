# SpecialKanren

An attempt to specialize relational programs by arguments with a finite domain.

## Relational programming
Relational programming is all about relations.

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
The parity ratio, well, what's complicated here. However, the implementation presented above depends exponentially on `n`.

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
# Benchmark 
The benchmark is presented below. The comparison was made using the [ocaml-benchmark](https://github.com/Chris00/ocaml-benchmarkgit) library. By `true` and `false`, we mean the execution of the original functions with the appropriate parameters. Under `spec_false` and `spec_true` --- specialized.

A large number is better. The percentage shows the gain of the execution of the function from the column on the left relative to the corresponding function in the upper row.

## Is_even

#### False 
|number = 10 |  Rate |  false | spec_false |
| ---------- | ----- | ------- | ---------- |
|     false | 25698+-1390/s     |    --   |    -46% |
|spec_false | 47221+-1352/s  |      84%  |       -- |

| number = 100 | Rate |  false| spec_false |
| ------------- | ---- | ---- |---------- |
|     false | 485+-1/s    |     --   |    -30% |
| spec_false | 695+-4/s   |     43%  |       -- |


| number = 1000   | Rate | false | spec_false |
| --------------- | ---- | ----- | ----------|
|     false |4.35+-0.00/s  |       -- |      -37% |
|spec_false| 6.94+-0.07/s    |    59%     |    -- |

#### True 

|number = 10|  Rate | true | spec_true |
| --------- | ----- | ---- | ---------- |
|     true | 26872+- 529/s    |    --   |   -41% |
| spec_true | 45772+-1258/s   |    70%   |     -- |

| number = 100| Rate |  true | spec_true|
| ----------- |------| ------| --------|
 |    true|  486+-1/s   |     --   |   -29% |
| spec_true | 688+-2/s   |    42%  |      --|

| number = 1000 |  Rate | true | spec_true |
| ------------- | ----- | ---- | --------- |
|     true | 4.47+-0.01/s  |      --  |    -35% |
|spec_true| 6.91+-0.01/s     |  55%    |    -- |


The benchmark code is located [here](https://github.com/IgorErin/SpecialKanren/blob/master/tests/is_even/bench.ml).


## Sub

#### Some 
|      count: 5    |   Rate          |  some  |spec_some |
| ---------| ------------- | ----- | ------- |  
|     some | 28543+- 554/s |       --   |  [-6%] |
| spec_some | 30279+-3918/s |     [6%] |        -- |

|     count: 25     |  Rate      |    some | spec_some |
| ---------| -------    | -------- | ------- |
|     some |1071+- 1/s |        -- |    [-4%] |
| spec_some| 1113+-10/s |      [4%] |        --|

|    count: 125      |  Rate    |  spec_some |     some  |
| ---------- |------- | --------- | --------- |
|spec_some |  13.6+-0.0/s|        --  |   [-0%]  |
|     some | 13.7+-0.0/s |      [0%] |       -- |

#### None

|     count: 5     |   Rate       |     none | spec_none |
| -------- | ------------ | --------- | -------- |
|     none | 53668+-182/s |       -- |     -41%  |
|spec_none | 91614+-930/s |      71% |       --  |

|  count: 25         | Rate      |    none | spec_none |
| --------  | --------- | -------| -------- |
|     none | 5598+-27/s   |     --   |   -24% |
| spec_none | 7329+-85/s  |     31%  |      -- |

|   count: 125  | Rate |  none| spec_none |
| ------- | -------| -----| -------|
 |    none |314+-0/s     |   --    |   -5%|
| spec_none | 330+-0/s |        5% |      --|


The benchmark code is located [here](https://github.com/IgorErin/SpecialKanren/blob/master/tests/sub/bench.ml).

## Further work
- Reduce unnecessary variables. This becomes difficult with recursive calls and with the =/= relation.
- The dnf acts as an intermediate representation. We need a more cunning way of detecting inappropriate conjuncts.
- When processing calls
  - The number of their parameters may decrease
  - With specialization, they can be reduced (satisfied always or never).

## License 
SpecialKanren is distributed under the MIT License.
