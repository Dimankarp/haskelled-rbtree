# Функциональное программирование - ЛР 2

## Введение

- Студент: ***Хороших Дмитрий Максимович 367597***
- Вариант: ***Red-Black Tree Dictionary***

## Описание

Словарь на основе Чёрно-красного дерева-моноида. Алгоритм вставки
реализован на основе *Okasaki|(Functional Data Structures)*, а алгоритм удаления - *(Matt Might)|[блог](https://matt.might.net/articles/red-black-delete/)*.

Во вставке и в удалении используется **bottom-up approach**, потому что его значительно проще реализовать с использованием рекурсивных вызовов, нежели top-down с поддержкой инварианты.

### Insertion

Производится поиск места под вставку, элемент вставляется, и вызывается
*balance* - функция, восстанавливающая структуру дерева, решая конфликты Red-Red связей.

```haskell
...

insert' :: (Ord a) => a -> b -> RBD a b -> RBD a b
insert' k v d = new {color = B}
  where
    new = insertImpl' k v d

insertImpl' :: (Ord a) => a -> b -> RBD a b -> RBD a b
insertImpl' k v Leaf = Node R k v Leaf Leaf
insertImpl' k v n@Node {key = nk, left = nl, right = nr}
  | k < nk = balance (n {left = insertImpl' k v nl})
  | k == nk = n {val = v}
  | k > nk = balance (n {right = insertImpl' k v nr})
  | otherwise = error "unreachable"
insertImpl' _ _ BBLeaf = error "double black leaf in lookup context"

...

```

### Removal

API функция красит корень в чёрный цвет и вызывает вспомогательную функции.

```haskell
remove' :: (Ord a) => a -> RBD a b -> RBD a b
remove' k n@Node {} = new
  where
    new = case removeImpl' k n of
      node@Node {} -> node {color = B}
      Leaf -> Leaf
      BBLeaf -> Leaf
remove' _ n = n
```

Вспомогательная функция ищет удаляемый элемент и вызывает на него функцию *removeNode*, которая и производит удаление, в зависимости от случая заменяя элемент листом или ребёнком.

При рекурсивных вызовах часто вызов оборачивается в функцию *bubble*, которая протягивает чёрный узел вверх к корню, чтобы восстановить баланс дерева.

В алгоритме примечательно использование 2-х новых цветов: двойного чёрного и негативного чёрного, упрощающих случаи при восстановлении после удаления.

```haskell
removeImpl' :: (Ord a) => a -> RBD a b -> RBD a b
removeImpl' _ Leaf = Leaf
removeImpl' k n@Node {key = nk, left = nl, right = nr}
  | k < nk = bubble $ n {left = removeImpl' k nl}
  | k == nk = removeNode n
  | k > nk = bubble $ n {right = removeImpl' k nr}
  | otherwise = error "unreachable"
removeImpl' _ _ = error "removeImpl called for a leaf"

removeNode :: (Ord a) => RBD a b -> RBD a b
removeNode Node {color = R, left = Leaf, right = Leaf} = Leaf
removeNode Node {color = B, left = Leaf, right = Leaf} = BBLeaf
removeNode Node {color = B, left = Leaf, right = nr@Node {color = R}} = nr {color = B}
removeNode Node {color = B, left = nl@Node {color = R}, right = Leaf} = nl {color = B}
removeNode n@Node {left = Node {}, right = nr@Node {}} = bubble $ n {val = val minNode, key = key minNode, right = removeMin nr}
  where
    minNode = findMin nr
removeNode _ = error "all expected matchings failed, either invatiant is broken or BBLeaf in removeNode context"

bubble :: (Ord a) => RBD a b -> RBD a b
bubble n@Node {left = nl, right = nr}
  | isBB nl || isBB nr = balance $ blacker n {left = redder nl, right = redder nr}
  | otherwise = n
bubble n = n
```

### Misc

Реализованы дополнительные операции над словарём: *foldr''*, *foldl''*, *map'*, *filter'*.

```haskell
foldr'' :: (Ord a) => ((a, b) -> c -> c) -> c -> RBD a b -> c
foldr'' _ acc Leaf = acc
foldr'' f acc n@Node {} = foldr'' f rightAcc (left n)
  where
    rightAcc = f (key n, val n) $ foldr'' f acc (right n)
foldr'' _ _ BBLeaf = error "Double Black leaf in foldr context"

foldl'' :: (Ord a) => ((a, b) -> c -> c) -> c -> RBD a b -> c
foldl'' _ acc Leaf = acc
foldl'' f acc n@Node {} = foldl'' f (f (key n, val n) leftAcc) (right n)
  where
    leftAcc = foldl'' f acc (left n)
foldl'' _ _ BBLeaf = error "Double Black leaf in foldl context"

map' :: (Ord a) => (b -> c) -> RBD a b -> RBD a c
map' _ Leaf = Leaf
map' p n@Node {} = n {val = p $ val n, left = map' p $ left n, right = map' p $ right n}
map' _ BBLeaf = error "Double Black leaf in map context"

filter' :: (Ord a) => (b -> Bool) -> RBD a b -> RBD a b
filter' p = foldr'' (\(k, v) d -> if p v then insert' k v d else d) (fromList' [])

```

А также класс *Monoid* с операцией слияние через вставку.

```haskell
instance (Ord a) => Semigroup (RBDictionary a b) where
  (<>) = foldr'' (\(k, v) acc -> insert' k v acc)

instance (Ord a) => Monoid (RBDictionary a b) where
  mempty = Leaf
  mconcat dicts = go dicts Leaf
    where
      go [] n = n
      go (d : ds) n = go ds (n <> d)

```

### Testing

Тесты API написаны при помощи HUnit. Тестируются: вставка, поиск, проверка на рвенство, удаление, свёртки, маппинг.
При помощи QuickCheck проводятся property-based testing. Проверяются:

1. Свойства Моноида (ассоциотивность, left & right identity)
2. Сохранение инвариант и балансировки RB дерева после операций:

```haskell
isColorlyValid :: (Ord a) => RBD a b -> Bool
isColorlyValid Leaf = True
isColorlyValid d@Node {color = B} = go d
  where
    go Leaf = True
    go Node {color = B, left = nl, right = nr} = go nr && go nl
    go Node {color = R, left = nl@Node {color = B}, right = nr@Node {color = B}} = go nr && go nl
    go Node {color = R, left = nl@Node {color = B}, right = Leaf} = go nl
    go Node {color = R, left = Leaf, right = nr@Node {color = B}} = go nr
    go Node {color = R, left = Leaf, right = Leaf} = True
    go _ = False
isColorlyValid _ = False

isHeightvalid :: (Ord a) => RBD a b -> Bool
isHeightvalid Leaf = True
isHeightvalid d@Node {} = snd $ go d
  where
    go Leaf = (1 :: Integer, True)
    go Node {color = col, left = nl, right = nr} =
      ( fst l
          + if col == B
            then 1
            else 0,
        snd l && snd r && (fst l == fst r)
      )
      where
        l = go nl
        r = go nr
    go _ = (0, False)
isHeightvalid _ = False
```

## Вывод

1. Перевод классических алгоритмов в функциональный контекст часто не тривиален.

2. Алгоритмы, заключающиеся в исоплнении определённых инструкций для множества частных случаев, удобно реализуются
при помощи pattern-matching'а. Яркий пример из работы - функция *balance*.

3. Property-based тестирование - сильный инструмент, позволяющий при правильном алгоритме генерации входных значений обеспечить высокий уровень покрытия.
