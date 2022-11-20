# lox-interpreters
Interpreters for Lox, following https://craftinginterpreters.com.

Literals and identifiers
```
<number>  :=   <digits> | <digits>.<digits>
<digits>  :=   <digit><digits>
<digit>   :=    0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9

<string>  :=   "<chars>"
<chars>   :=   <char><chars>
<char>    :=   a | b | c | ...
```

Expression language
```
<exp>      :=   <binexp1>
<ternexp>  :=   <binexp1> ? <ternexp> : <ternexp> | <binexp1>
<binexp1>  :=   <binexp1> <binop1> <binexp2> | <binexp2>
<binexp2>  :=   <binexp2> <binop2> <binexp3> | <binexp3>
<binexp3>  :=   <binexp3> <binop3> <binexp4> | <binexp4>
<binexp4>  :=   <binexp4> <binop4> <unexp> | <unexp>
<unexp>    :=   <unop> <unexp> | <primary>
<prim>     :=   <lit> | (<exp>)
<lit>      :=   <number> | <string> | true | false | nil

<binop1>   :=   == | !=
<binop2>   :=   < | <= | > | >=
<binop3>   :=   + | -
<binop4>   :=   * | /
<unop>     :=   - | !
```

Statement language
```
<program>  :=   EOF | <stmt> <program>
<stmt>     :=   <exp>; | print <exp>;
```

## hasklox

To run, enter the `hasklox` directory and run `cabal run hasklox`.
TODO: Figure out why I need to keep running `cabal new-clear` before it will rebuild...