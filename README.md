# lox-interpreters
Interpreters for Lox, following https://craftinginterpreters.com.

Literals and identifiers
```
<number>  :=   <digits> | <digits>.<digits>
<digits>  :=   <digit><digits>
<digit>   :=    0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9

<string>  :=   "<chars>"
<chars>   :=   <char><chars>
<char>    :=   a | b | ...

<ident>   :=   <ifst> | <ifst><ichars>
<ifst>    :=   a | b | ... | z | A | B | ... | Z
<ichars>  :=   <ichar><ichars>
<ichar>   :=   a | b | ... | z | A | B | ... | Z | 0 | 1 | ... | 9 | _
```

Expression language
```
<exp>      :=   <assnexp> | <ternexp>

<assnexp>  :=   <lvalue> = <exp>

<ternexp>  :=   <orexp> ? <ternexp> : <ternexp> | <orexp>
<orexp>    :=   <orexp> or <andexp> | <andexp>
<andexp>   :=   <andexp> and <binexp1> | <binexp1>
<binexp1>  :=   <binexp1> <binop1> <binexp2> | <binexp2>
<binexp2>  :=   <binexp2> <binop2> <binexp3> | <binexp3>
<binexp3>  :=   <binexp3> <binop3> <binexp4> | <binexp4>
<binexp4>  :=   <binexp4> <binop4> <unexp> | <unexp>
<unexp>    :=   <unop> <unexp> | <primary>

<lvalue>   :=   <ident>
<prim>     :=   <lit> | (<exp>) | <ident>
<lit>      :=   <number> | <string> | true | false | nil

<binop1>   :=   == | !=
<binop2>   :=   < | <= | > | >=
<binop3>   :=   + | -
<binop4>   :=   * | /
<unop>     :=   - | !
```

Statement language
```
<program>  :=   EOF | <blkstmt> <program>
<blkstmt>  :=   <declexp> | <stmt>
<declstmt> :=   var <lvalue>; | var <lvalue> = <exp>;
<stmt>     :=   <exp>; | print <exp>; | <block> 
              | if (<exp>) <stmt> | if (<exp>) <stmt> else <stmt>
              | while (<exp>) <stmt> | for (<forinit> <expopt> ; <expopt> ) <stmt>
<forinit>  :=   <varstmt> | <exp>; | ;
<expopt>   :=   <exp> | _

<block>    :=   { <blkstmts> }
<blkstmts> :=   _ | <blkstmt><blkstmts>
```

## hasklox

To run, enter the `hasklox` directory and run `cabal run hasklox`.
TODO: Figure out why I need to keep running `cabal new-clear` before it will rebuild...