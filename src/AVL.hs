
{-# LANGUAGE InstanceSigs #-}

module AVL (AVL, nilAVL, singletonAVL, insertAVL, deleteAVL, isBalanced, largest, smallest, belongsAVL, appendAVLs) where

data AVL a = EmptyA | NodeA a (AVL a) (AVL a) Int
    deriving (Show, Eq)

instance Ord a => Semigroup (AVL a) where
    (<>) :: Ord a => AVL a -> AVL a -> AVL a
    (<>) = appendAVLs

instance Ord a => Monoid (AVL a) where
    mempty :: Ord a => AVL a
    mempty = nilAVL

instance Foldable AVL where
    foldMap :: Monoid m => (a -> m) -> AVL a -> m
    foldMap _ EmptyA            = mempty
    foldMap f (NodeA x ti td _) = f x <> foldMap f ti <> foldMap f td

    foldr :: (a -> b -> b) -> b -> AVL a -> b
    foldr _ z EmptyA            = z
    foldr f z (NodeA x ti td _) = foldr f (f x (foldr f z td)) ti

{-
 INV.REP.: Siendo T el conjunto de AVLs posibles y A el conjunto de elementos para esos AVLs:
    * ∀t∈T: esNodeA t => (element . leftTree)  t <= element t 
    * ∀t∈T: esNodeA t => (element . rightTree) t >  element t
    * ∀t∈T: esNodeA t => diff t < 2
    * ∀t∈T,∃a∈A: (not . esNodeA) t => (height . insertAVL a) t == 1

    DEFS:
    esNodeA :: Ord a => AVL a -> Bool
    esNodeA (NodeA _ _ _ _) = True
    esNodeA EmptyA          = False
 -}

-- Interfaz
{-
PROP: Retorna un arbol sin elementos.
COSTE: O(1).
-}
nilAVL :: Ord a => AVL a

{-
PROP: Dado un elemento, retorna un arbol con ese elemento.
COSTE: O(1).
-}
singletonAVL :: Ord a => a -> AVL a

{-
PROP: Dado un elemento y un arbol, agrega el elemento al arbol dado dejandolo ordenado y balanceado.
COSTE: O(log(k)). k: la cantidad de elementos del arbol.
-}
insertAVL :: Ord a => a -> AVL a -> AVL a

{-
PROP: Dado un elemento y un arbol, borra el elemento al arbol dado dejandolo ordenado y balanceado.
PREC: El elemento dado debe existir en el arbol.
COSTE: O(log(k)). k: la cantidad de elementos del arbol.
-}
deleteAVL :: Ord a => a -> AVL a -> AVL a

{-
PROP: Dado un arbol, determina si el arbol está balanceado.
COSTE: O(k). k: la cantidad de elementos del arbol.
-}
isBalanced :: Ord a => AVL a -> Bool

{-
PROP: Dado un arbol, retorna el elemento más grande del arbol y el mismo arbol sin ese elemento.
PREC: El arbol dado no debe ser vacío.
COSTE: O(log(k)). k: la cantidad de elementos del arbol.
-}
largest :: AVL a -> (a, AVL a)

{-
PROP: Dado un arbol, retorna el elemento más chico del arbol y el mismo arbol sin ese elemento.
PREC: El arbol dado no debe ser vacío.
COSTE: O(log(k)). k: la cantidad de elementos del arbol.
-}
smallest :: AVL a -> (a, AVL a)

{-
PROP: Dado un elemento y un arbol, determina si el elemento dado está en el árbol.
COSTE: O(log(k)). k: la cantidad de elementos del arbol.
-}
belongsAVL :: Ord a => a -> AVL a -> Bool

{-
PROP: Dado un elemento y un arbol, determina si el elemento dado está en el árbol.
COSTE: O(k * log(j)). k: la cantidad de elementos del arbol A, j: la cantidad de elementos del arbol B.
-}
appendAVLs :: Ord a => AVL a -> AVL a -> AVL a
-- *** END ***

-- Funciones de Interfaz
nilAVL = EmptyA

singletonAVL x = NodeA x EmptyA EmptyA 1

insertAVL x EmptyA                    = singletonAVL x
insertAVL x (NodeA y EmptyA EmptyA h) = if x > y
                                        then NodeA y EmptyA (singletonAVL x) (h+1)
                                        else NodeA y (singletonAVL x) EmptyA (h+1)
insertAVL x (NodeA y EmptyA td     h) = if x > y
                                        then singleBalance (NodeA y EmptyA (insertAVL x td) (h+1))
                                        else NodeA y (singletonAVL x) td h
insertAVL x (NodeA y ti     EmptyA h) = if x > y
                                        then NodeA y ti (singletonAVL x) h
                                        else singleBalance (NodeA y (insertAVL x ti) EmptyA (h+1))
insertAVL x (NodeA y ti     td     _) = if x > y
                                        then singleBalance (NodeA y ti (insertAVL x td) (calculateHeight ti (insertAVL x td)))
                                        else singleBalance (NodeA y (insertAVL x ti) td (calculateHeight (insertAVL x ti) td))

deleteAVL _ EmptyA                    = error "The element you're trying to delete isn't in the tree!"
deleteAVL x (NodeA y EmptyA EmptyA _)
    | x == y    = EmptyA
    | otherwise = deleteAVL x EmptyA -- Tiro error
deleteAVL x (NodeA y EmptyA td     _)
    | x == y    = td -- td NO tiene hijos porque se cumple la propiedad 3!
    | x > y     = singleBalance (NodeA y EmptyA (deleteAVL x td) (height (deleteAVL x td) + 1))
    | otherwise = deleteAVL x EmptyA -- Tiro error
deleteAVL x (NodeA y ti     EmptyA _)
    | x == y    = ti -- ti NO tiene hijos porque se cumple la prioridad 3!
    | x > y     = deleteAVL x EmptyA -- Tiro error
    | otherwise = singleBalance (NodeA y (deleteAVL x ti) EmptyA (height (deleteAVL x ti) + 1))
deleteAVL x (NodeA y ti     td     _)
    | x == y    = let (z, ti') = largest ti
                  in NodeA z ti' td (calculateHeight ti' td)
    | x > y     = singleBalance (NodeA y ti (deleteAVL x td) (calculateHeight ti (deleteAVL x td)))
    | otherwise = singleBalance (NodeA y (deleteAVL x ti) td (calculateHeight (deleteAVL x ti) td))

isBalanced EmptyA              = True
isBalanced t@(NodeA _ ti td _) = diff t < 2 && isBalanced ti && isBalanced td

largest EmptyA                = error "There was no largest element!"
largest (NodeA x ti EmptyA _) = (x, ti)
largest (NodeA x ti td     h) = let (y, t) = largest td
                                in (y, NodeA x ti t h)

smallest EmptyA                = error "There was no smallest element!"
smallest (NodeA x EmptyA td _) = (x, td)
smallest (NodeA x ti     td h) = let (y, t) = smallest ti
                                 in (y, NodeA x t td h)

belongsAVL _ EmptyA            = False
belongsAVL x (NodeA y ti td _) = x == y || belongsAVL x ti || belongsAVL x td

appendAVLs EmptyA t            = t
appendAVLs (NodeA x ti td _) t = insertAVL x (appendAVLs ti (appendAVLs td t))
-- *** END ***

-- Funciones de Acceso
leftTree :: Ord a => AVL a -> AVL a
leftTree EmptyA           = error "There was no left tree!"
leftTree (NodeA _ ti _ _) = ti

rightTree :: Ord a => AVL a -> AVL a
rightTree EmptyA           = error "There was no right tree!"
rightTree (NodeA _ _ td _) = td

element :: Ord a => AVL a -> a
element EmptyA          = error "There was no element!"
element (NodeA x _ _ _) = x

height :: Ord a => AVL a -> Int
height EmptyA          = 0
height (NodeA _ _ _ n) = n
-- *** END ***

-- Funciones Aux
calculateHeight :: Ord a => AVL a -> AVL a -> Int
calculateHeight t1 t2 = max (height t1) (height t2) + 1

singleBalance :: Ord a => AVL a -> AVL a
singleBalance EmptyA = EmptyA
singleBalance t@(NodeA x ti td h)
    | diff t < 2 = t
    | height ti > height td =
        if (height . leftTree) ti > (height . rightTree) ti
        then bubbleUpLeft x ti                   td h
        else bubbleUpLeft x (semiRotateRight ti) td h
    | height ti <= height td =
        if (height . leftTree) td <= (height . rightTree) td
        then bubbleUpRight x ti td h
        else bubbleUpRight x ti (semiRotateLeft td) h
    | otherwise = error "It makes no sense to get here! you should've gotten inside a body with the is balanced question!"

semiRotateRight :: Ord a => AVL a -> AVL a
semiRotateRight EmptyA            = error "It makes no sense to semi rotate an EmptyA!"
semiRotateRight (NodeA x ti td h) = NodeA (element td) (leftTree td) (NodeA x ti (leftTree td) (height td)) h

semiRotateLeft :: Ord a => AVL a -> AVL a
semiRotateLeft EmptyA            = error "It makes no sense to semi rotate an EmptyA!"
semiRotateLeft (NodeA x ti td h) = NodeA (element ti) (leftTree ti) (NodeA x (rightTree ti) td (height ti)) h

bubbleUpLeft :: Ord a => a -> AVL a -> AVL a -> Int -> AVL a
bubbleUpLeft _ EmptyA               _  _ = error "It makes no sense to bubble up an EmptyA!"
bubbleUpLeft x (NodeA y ti' td' h') td h = NodeA y ti' (redo x td' td h') (h-1)

bubbleUpRight :: Ord a => a -> AVL a -> AVL a -> Int -> AVL a
bubbleUpRight _ _  EmptyA               _ = error "It makes no sense to bubble up an EmptyA!"
bubbleUpRight x ti (NodeA y ti' td' h') h = NodeA y (redo x ti ti' h') td' (h-1)

redo :: Ord a => a -> AVL a -> AVL a -> Int -> AVL a
redo x t1 t2 h = NodeA x t1 t2 (h-1)

diff :: Ord a => AVL a -> Int
diff t = abs ((height . leftTree) t - (height . rightTree) t)
-- *** END ***
