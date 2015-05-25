##Signal の　カタログ

http://giisyu.github.io/elm-signalCatalog/

Signalの性質がひと目で分かる、かもしれないSignalのデモ。
Signalによっては画面をクリックしてみたり、十字キーを押したりする必要があります。

##表示の見方

```
1
map : (a -> result) -> Signal a -> Signal result
                () ------------------------()----------- Mouse.clicks
                4                3                        2
```

 1. 関数の型
 2. Signal a になるコード
 3. タイムライン、右から左に時間が流れている。シグナルが出る（発火する）と、その時の型が出る。ズレてる。
 4. Signal を　Element.show した値です。つまりSignalの__現在の__型です。さらに言い換えれば画面に表示した時（View関数に持ってきた時）こうなるということです。ブラウザをリロードすると、初期値がわかります。  




