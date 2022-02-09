module Tree (
    Node (..),
    getNodeValue, flatten, nodeJoin, resolve, resolveBy
)
where

import Text.Printf (printf, PrintfArg)

-- tree node for file tree, with Functor and Foldable instances
data Node a = Tip | Branch (Node a) a (Node a)

instance Functor Node where
    fmap _ Tip = Tip
    fmap f (Branch left a right) = Branch (fmap f left) (f a) (fmap f right)

foldMapDepthFirst :: (Monoid m) => (a -> m) -> Node a -> m
foldMapDepthFirst _ Tip = mempty
foldMapDepthFirst f (Branch left a right) = (f a) <> (foldMapDepthFirst f left) <> (foldMapDepthFirst f right)

foldMapBreadthFirst :: (Monoid m) => (a -> m) -> Node a -> m
foldMapBreadthFirst _ Tip = mempty
foldMapBreadthFirst f (Branch left a right) = (f a) <> (foldMapBreadthFirst f right) <> (foldMapBreadthFirst f left)

foldMapMiddle :: (Monoid m) => (a -> m) -> Node a -> m
foldMapMiddle _ Tip = mempty
foldMapMiddle f (Branch left a right) = (foldMapMiddle f left) <> (f a) <> (foldMapMiddle f right)

instance Foldable Node where
    foldMap = foldMapBreadthFirst

getNodeValue :: Node a -> a
getNodeValue (Branch _ a _) = a

flatten :: Node a -> [a]
flatten Tip = []
flatten root = foldr (:) [] root

nodeJoin :: Node a -> Node a -> Node a
nodeJoin (Branch left a1 _) n2 = Branch left a1 n2

resolve :: (Eq a) => [a] -> Node a -> Maybe (Node a)
resolve = resolveBy (==)

resolveBy :: (a -> b -> Bool) -> [a] -> Node b -> Maybe (Node b)
resolveBy f path root = resolveBy' f path root root

resolveBy' :: (a -> b -> Bool) -> [a] -> Node b -> Node b -> Maybe (Node b)
resolveBy' _ [] _ prev = Just prev
resolveBy' _ _ Tip _ = Nothing
resolveBy' _ _ _ Tip = Nothing
resolveBy' rf path root prev
    | rf (head path) (getNodeValue root) = resolveBy' rf (tail path) (getChild root) root
    | otherwise = resolveBy' rf path (getSibling root) root
    where
        getChild :: Node b -> Node b
        getChild (Branch child _ _) = child
        getSibling :: Node b -> Node b
        getSibling (Branch _ _ sib) = sib