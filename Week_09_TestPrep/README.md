# Задачи за упражнение

1) Да се напише функция, която получава стойност ***x*** и цяло число ***k*** и пресмята прибличено стойността на израза ***e^x*** по развитието на Тейлър.
```haskell
expKthTerm :: Int -> Double -> Double
```

2) Да се напише програма, която получава списък от едноместни функции и целочислена стойност ***x***. Ако х е четно число да му се приложи композиция на всички функции, които са на нечетни позиции(първо се прилага функцията на първо място, после тази на трето и т.н.), иначе - композиция на тези, които са на четни позиции.
```haskell
funnyComposition :: [(Int -> Int)] -> Int -> Int
```

3) По подаден списък от цели числа да се пресметне списък от кумулативните суми, т.е. [1,2,0,3] -> [1,3,3,6].
```haskell
calcCummulativeSums :: [Int] -> [Int]
```

4) Да се напише функция, която по подадени списък от едноместни функции и списък от елементи(от еднакъв тип) прилага на първия елемент първата функция, на втория - втората и т.н., като при изчерпване на списъка с функции се започва наново.
```haskell
alternatingMap :: [a -> b] -> [a] -> [b]
```

5) Нещо от сборника