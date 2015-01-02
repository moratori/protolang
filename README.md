protolang
=====

型推論実装のための試験的な言語です

以下のような式に型推論・型検査を行います

```
(λ f -> 
  if (f 1) 
    then f
    else λ x -> x == 5) :: (Int -> Bool) -> (Int -> Bool)
```
