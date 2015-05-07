import Comonad
import Data.Functor
import Control.Monad

-- A 1d cellular automata
data Linear a = Linear [a] a [a]

leftL :: Linear a -> Linear a
leftL (Linear (l:ls) m rs) = Linear ls l (m:rs)
leftL _ = error "can't move left"

rightL :: Linear a -> Linear a
rightL (Linear ls m (r:rs)) = Linear (m:ls) r rs
rightL _ = error "can't move right"

extractL :: Linear a -> a
extractL (Linear _ a _) = a

toListL :: Int -> Linear a -> [a]
toListL size (Linear ls c rs) =
  (reverse $ take size ls) ++ [c] ++ take size rs

instance Functor Linear where
  fmap f (Linear ls c rs) = Linear (map f ls) (f c) (map f rs)

instance Show a => Show (Linear a) where
  show l = concatMap show $ toListL 5 l

{----------------------------------------------------------------------------}

-- A 2d cellular automata
data Planar a = Planar (Linear (Linear a))

up :: Planar a -> Planar a
up (Planar lines) = Planar $ leftL lines

down :: Planar a -> Planar a
down (Planar lines) = Planar $ rightL lines

left :: Planar a -> Planar a
left (Planar lines) = Planar $ leftL <$> lines

right :: Planar a -> Planar a
right (Planar lines) = Planar $ rightL <$> lines

extractP :: Planar a -> a
extractP (Planar lines) = extractL $ extractL lines

instance Functor Planar where
  fmap f (Planar lines) = Planar (fmap (fmap f) lines)

instance Show a => Show (Planar a) where
  -- not strictly correct since the plane is infinite...
  --   but thats not very useful so show a subset of it
  show (Planar lines) = unlines $ toListL 5 $ show <$> lines

{----------------------------------------------------------------------------}

move :: (a -> a) -> (a -> a) -> a -> Linear a
move l r z = Linear ls z rs
  where
    ls = iterate' l z
    rs = iterate' r z
    iterate' f = tail . iterate f

fromList :: (Monoid a) => [[a]] -> Planar a
fromList ls = Planar $ Linear (repeat deadLine) deadLine rs
    where
      rs = (map mkLine ls) ++ repeat deadLine
      deadLine = mkLine []
      mkLine l = Linear (repeat mempty) mempty (l ++ (repeat mempty))

instance Comonad Planar where
  extract = extractP
  duplicate p = Planar $ (move left right) <$> (move up down p)

{----------------------------------------------------------------------------}

type Rule2d a = (Planar a -> a)

-- The 'neighbours' of a given cell are those left, right, above and below
--  it and all the compositions thereof. If we move the plane in each of those
--  directions and just extract we have the neighbours. We also include the
--  center cell for convenience
neighbours :: Planar a -> [a]
neighbours p = 
  map (extract.($p))
  $ (.) <$> [up,id,down] <*> [left, id, right]
  -- sometimes haskell makes me feel like a wizard...

-- Allow a rule that operates on some list of concrete neighbours to be
--  'lifted' into the 2d automata
liftRule2d :: ([a] -> a) -> Rule2d a
liftRule2d rule = rule.neighbours

data Cell = Alive | Dead deriving (Eq)
instance Show Cell where
  show Alive = "*"
  show Dead  = " "
instance Read Cell where
  readsPrec _ "*" = [(Alive,"")]
  readsPrec _ " " = [(Dead,"")]
instance Monoid Cell where
  mempty = Dead
  -- we never actually use mappend but this will do
  mappend Dead x = x      
  mappend Alive _ = Alive

type Board = Planar Cell

{----------------------------------------------------------------------------}

ruleGol :: Rule2d Cell
ruleGol = liftRule2d gol
  where
    gol [ a, b, c,
          d, x, f,
          g, h, i] =
        case aliveNeighours of
          2 -> x
          3 -> Alive
          _ -> Dead
      where
        aliveNeighours = (length $ filter (==Alive) $ [a,b,c,d,f,g,h,i])
    gol _ = error "gol applied to invalid board"

glider :: Board
glider = fromList $ map (map read) [
                    [ " ", "*", " " ],
                    [ " ", " ", "*" ],
                    [ "*", "*", "*" ]]

blinker :: Board
blinker = fromList $ map (map read) [[ "*", "*", "*" ]]

(=>>>) :: Board -> Rule2d Cell -> [Board]
plane =>>> rule = iterate (=>> rule) plane

main = do
  forM_ (take 10 (glider =>>> ruleGol)) $ do
    print

  putStrLn "-----"

  forM_ (take 10 (blinker =>>> ruleGol)) $ do
    print
