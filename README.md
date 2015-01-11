protolang
=====

静的型付けで型推論をもつ試験的な言語です  
整数型と真偽値型と関数型の3つしかデータ型を持ちません  


使い方
=====

SBCLをインストールしquicklispを入れてください  
~/.quicklisp/local-projects で  
git clone https://github.com/moratori/protolang.git してください.

あとはSBCLのREPLで
```
  (ql:quickload :protolang)
  (protolang::main)
```
してください


REPL
====

コマンドライン引数を渡さないで起動するとREPLにはいります

```
>>> 1+2
3 : Integer

>>> [x] -> x+1
([x] -> +[x,1]) : (Integer -> Integer)

>>> def fact [n] -> if (n == 0) 1 n * fact[n-1]
fact : (Integer -> Integer)

>>> def search[f,n] -> if f[n] n search[f,n+1]
search : ((Integer -> Boolean) -> (Integer -> Integer))

>>> fact[search[[x] -> (x % 23) == 4 , 30]]
30414093201713378043612608166064768844377641568960512000000000000 : Integer

>>> @+1

>>> def iszero[x:Int]:Bool -> x == 0

DEBUG PRINT (S-Expression)

 (PROGN
  (DEFVAR iszero
    (LET (iszero)
      (SETF iszero
              (LAMBDA (x)
                (FUNCALL (FUNCALL (LAMBDA (X) (LAMBDA (Y) (= X Y))) x)
                         (THE INTEGER 0))))
      iszero)))


iszero : (Integer -> Boolean)

>>> 
```

@+1,@+2,@+3 とかするとデバッグプリントを表示します


コンパイルする
====

実行可能バイナリを作ることができます(今のところSBCLで動かしてるときだけ)  

まずコンパイラを作ります。

以下のようなファイルを適当に作ってsbclでloadしてください
```   
  (ql:quickload :protolang)
  (sb-ext:save-lisp-and-die "COMPILER"
    :toplevel #'protolang::main
    :executable t
    :purify t)
```

そうするとカレントディレクトリにCOMPILERができると思うので  

適当なソースコードを書いてコマンドライン引数であたえてください。  


test.pl
```
  def fib[n] ->
    if (n < 2) 
      n
      fib[n-2] + fib[n-1]

  fib[10]
```

```
  $ COMPILER test.pl test.out
  $ ./test.out
    55
  $ 
  
```


