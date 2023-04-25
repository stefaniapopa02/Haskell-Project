module AlgebraicGraph where

import qualified Data.Set as S

data AlgebraicGraph a
    = Empty
    | Node a
    | Overlay (AlgebraicGraph a) (AlgebraicGraph a)
    | Connect (AlgebraicGraph a) (AlgebraicGraph a)
    deriving (Eq, Show)

-- (1, 2), (1, 3)
angle :: AlgebraicGraph Int
angle = Connect (Node 1) (Overlay (Node 2) (Node 3))

-- (1, 2), (1, 3), (2, 3)
triangle :: AlgebraicGraph Int
triangle = Connect (Node 1) (Connect (Node 2) (Node 3))

{-
    *** TODO ***

    Mulțimea nodurilor grafului.

    Hint: S.union
-}
nodes :: Ord a => AlgebraicGraph a -> S.Set a
--nodes graph = undefined
nodes Empty = S.empty
nodes (Node a) = S.fromList [a]
nodes (Overlay g1 g2) = S.union (nodes g1) (nodes g2)
nodes (Connect g1 g2) = S.union (nodes g1) (nodes g2)

{-
    *** TODO ***

    Mulțimea arcelor grafului.

    Hint: S.union, S.cartesianProduct
-}
edges :: Ord a => AlgebraicGraph a -> S.Set (a, a)
--edges graph = undefined
edges Empty = S.empty
edges (Node a) = S.empty
edges (Overlay g1 g2) = S.union (edges g1) (edges g2) 
edges (Connect g1 g2) = S.union (S.cartesianProduct (nodes g1) (nodes g2)) (edges (Overlay g1 g2))

{-
    *** TODO ***

    Mulțimea nodurilor înspre care pleacă arce dinspre nodul curent.

    ATENȚIE! NU folosiți funcția edges definită mai sus, pentru că ar genera
    prea multe muchii inutile.
-}
outNeighbors :: Ord a => a -> AlgebraicGraph a -> S.Set a
--outNeighbors node graph = undefined
outNeighbors node Empty = S.empty
outNeighbors node (Node a) = S.empty
outNeighbors node (Overlay g1 g2) = S.union (outNeighbors node g1) (outNeighbors node g2)
outNeighbors node (Connect g1 g2) = if (S.member node (nodes g1)) 
                                        then S.union (outNeighbors node g1) (nodes g2) 
                                        else (outNeighbors node g2)

{-
    *** TODO ***

    Mulțimea nodurilor dinspre care pleacă arce înspre nodul curent.

    ATENȚIE! NU folosiți funcția edges definită mai sus, pentru că ar genera
    prea multe muchii inutile.
-}
inNeighbors :: Ord a => a -> AlgebraicGraph a -> S.Set a
--inNeighbors node graph = undefined
inNeighbors node Empty = S.empty
inNeighbors node (Node a) = S.empty
inNeighbors node (Overlay g1 g2) = S.union (inNeighbors node g1) (inNeighbors node g2)
inNeighbors node (Connect g1 g2) = if (S.member node (nodes g2)) 
                                        then S.union (inNeighbors node g2) (nodes g1) 
                                        else (inNeighbors node g1)

{-
    *** TODO ***

    Întoarce graful rezultat prin eliminarea unui nod și a arcelor în care
    acesta este implicat. Dacă nodul nu există, se întoarce același graf.

    Hint: Definiți o funcție recursivă locală (de exemplu, în where),
    care să primească drept parametri doar entități variabile de la un apel
    recursiv la altul, astfel încât să nu copiați la fiecare apel parametrii
    nemodificați. De exemplu, parametrul node nu se modifică, în timp ce
    parametrul graph se modifică.
-}
removeNode :: Eq a => a -> AlgebraicGraph a -> AlgebraicGraph a
--removeNode node graph = undefined
--remove node Empty = Empty
--remove node (Node a) = helper 

removeNode node graph = (rmHelper graph) where
                            rmHelper graph = case graph of  
                                Empty -> Empty
                                (Node a) -> if (a == node) then  Empty else graph
                                (Overlay g1 g2) -> Overlay (rmHelper g1) (rmHelper g2)
                                (Connect g1 g2) -> Connect (rmHelper g1) (rmHelper g2)
-- removeNode node Empty = Empty
-- removeNode node (Node a) = if (a == node) then Empty else (Node a)
-- removeNode node (Overlay g1 g2) = Overlay (removeNode node g1) (removeNode node g2)
-- removeNode node (Connect g1 g2) = Connect (removeNode node  g1) (removeNode node g2)

{-
    *** TODO ***

    Divizează un nod în mai multe noduri, cu eliminarea nodului inițial.
    Arcele în care era implicat vechiul nod trebuie să devină valabile
    pentru noile noduri.
    
    Hint: Funcție recursivă locală, ca la removeNode.
-}
splitNode :: Eq a
          => a                 -- nodul divizat
          -> [a]               -- nodurile cu care este înlocuit
          -> AlgebraicGraph a  -- graful existent
          -> AlgebraicGraph a  -- graful obținut
--splitNode old news graph = undefined

splitNode old news graph = (spHelper graph news) where
                                spHelper g news = case g of
                                    Empty -> Empty
                                    (Node a) -> if (a == old ) 
                                        then if (null news) 
                                                then Empty 
                                                else (Overlay (Node (head news)) (spHelper g (tail news)))  
                                        else g
                                    (Overlay g1 g2) -> Overlay (spHelper g1 news) (spHelper g2 news)
                                    (Connect g1 g2) -> Connect (spHelper g1 news) (spHelper g2 news)


{-
    *** TODO ***

    Îmbină mai multe noduri într-unul singur, pe baza unei proprietăți
    respectate de nodurile îmbinate, cu eliminarea acestora. Arcele în care
    erau implicate vechile noduri vor referi nodul nou.

    Hint: Funcție recursivă locală, ca la removeNode.
-}
mergeNodes :: (a -> Bool)       -- proprietatea îndeplinită de nodurile îmbinate
           -> a                 -- noul nod
           -> AlgebraicGraph a  -- graful existent
           -> AlgebraicGraph a  -- graful obținut
--mergeNodes prop node graph = undefined

mergeNodes prop node graph = (mrgHelper graph) where
                                mrgHelper g = case g of
                                    Empty -> Empty
                                    (Node a) -> if (prop a) then (Node node) else g
                                    (Overlay g1 g2) -> Overlay (mrgHelper g1) (mrgHelper g2)
                                    (Connect g1 g2) -> Connect (mrgHelper g1) (mrgHelper g2)
