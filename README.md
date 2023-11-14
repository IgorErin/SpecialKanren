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
### Benchmark 
The benchmark is presented below. The comparison was made using the [ocaml-benchmark](https://github.com/Chris00/ocaml-benchmarkgit) library. By `true` and `false`, we mean the execution of the original functions with the appropriate parameters. Under `spec_false` and `spec_true` --- specialized.

| Answers: 10  |     Rate      | `false` | `true` | `spec_false` | `spec_true` |
| ------------ | ------------- | ------- | ------ | ------------ | ----------- |
| `false`      | 27596+- 205/s |   ---   |   -2%  |      -32%    |     -34%    |
| `true`       | 28254+-1524/s |    2%   |   ---  |      -30%    |     -33%    |
| `spec_false` | 40559+-2354/s |    47%  |    44% |      ---     |      -3%    |
| `spec_true`  | 41860+- 249/s |    52%  |    48% |      3%      |      ---    |

| Answers: 100 |  Rate    | `false` | `true` | `spec_true` | `spec_false`|
| ------------ | -------- | ------- | ------ | ----------- | ----------- |
| `false`      | 512+-3/s |   ---   |   -8%  |      -27%   |     -28%    |
| `true`       | 516+-2/s |    1%   |   ---  |      -27%   |     -27%    |
| `spec_true`  | 705+-3/s |    38%  |    37% |      ---    |      0%    |
| `spec_false` | 707+-3/s |    38%  |    37% |       0%    |      ---    |

| Answers: 1000 | Rate   | `false` | `true` | `spec_false` | `spec_true` |
| ------------- | -----  | ------- | ------ | ------------ | ----------- |
| `false`       | 4.73/s |   ---   |   -2%  |      -36%    |     -36%    |
| `true`        | 4.85/s |    2%   |   ---  |      -34%    |     -34%    |
| `spec_false`  | 7.33/s |    55%  |    51% |      ---     |      0%     |
| `spec_true`   | 7.34/s |    55%  |    51% |       0%     |      ---    |

The benchmark code is located [here](https://github.com/IgorErin/SpecialKanren/blob/master/tests/is_even/bench.ml).


## A bit more sadness

But for the subtraction ratio, things aren't so fun anymore. But still the version specialized by `None` sometimes wins.

| Answers: 5          |   Rate   | `some` | `spec_some` | none | `spec_none` |
| --------- | -------- | --------- | --------- | --------- | -------- |
| `some` | 22321/s  |      ---   |   -22%    |  -56%     |   -69%    |
| `spec_some` | 28653/s  |     28%   |     ---    |  -44%     | -60%      |
| `none` | 51020/s  |   129%    |   78%     |   ---      | -29%      |
| `spec_none` | 71429/s  |   220%    | 149%      |   40%     |   ---      |


| Answers: 25          |  Rate   | just_some | spec_some | just_none | spec_none |
| --------- | ----   |  -------  |  -------- | --------- | --------  |
| `some` | 1066/s |    ---     |    -0%    |  -78%     |  -85%     |
| `spec_some` | 1070/s |   0%      |    ---     | -77%      | -85%      |
| `none` | 4753/s |   346%    |    344%   |     ---    |   -32%    |
| `spec_none` | 6993/s |   556%    |    554%   |    47%    |     ---    |

| Answers: 125  | Rate   | `some` | `spec_some` | `none` | `spec_none` |
| --------- | -------|  ------- | ------- | ------- | --------- |    
| `some` | 13.3/s |       --- |      -5% |   -96% |     -96%  |
| `spec_some` | 14.0/s |       5% |       --- |   -95% |     -95%  |
| `none` | 304/s  |   2182%  |   2067%  |    ---  |     -1%   |
| `spec_none` | 308/s  |   2212%  |   2095%  |   1%   |     ---    |

| Answers: 625  |  Rate      |   `some` | `spec_some` | `spec_none` | `none` |
| ----------| ---------- | --------- | --------- | --------- | --------- |          
| `some` | 0.0954423/s|        --- |      -2%  |    -99%   |   -99%     |
| `spec_some` | 0.0972737/s|        2% |       ---  |    -99%   |   -99%     |
| `spec_none` |     10.9/s |   11313%  |  11098%   |     ---    |   -7%      |
| `none` |     11.7/s |   12185%  |  11953%   |     8%    |    ---      |

The benchmark code is located [here](https://github.com/IgorErin/SpecialKanren/blob/master/tests/sub/bench.ml).

## Further work
- Reduce unnecessary variables. This becomes difficult with recursive calls and with the =/= relation.
- The dnf acts as an intermediate representation. We need a more cunning way of detecting inappropriate conjuncts.
- When processing calls
  - The number of their parameters may decrease
  - With specialization, they can be reduced (satisfied always or never).

## License 
SpecialKanren is distributed under the MIT License.
