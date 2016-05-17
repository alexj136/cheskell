module Chess where
import Data.List  ( intersperse )
import Data.Maybe ( fromJust    )

-------------------
-- Chess data types
-- ----------------

data Rank         = King | Queen | Bishop | Knight | Rook | Pawn
                  deriving (Show, Eq, Ord)
data Colour       = Black | White
                  deriving (Show, Eq, Ord)
type Piece        = (Rank, Colour)
newtype EightOf a = EightOf (a, a, a, a, a, a, a, a) deriving Show
type Row          = EightOf (Maybe Piece)
type Board        = EightOf Row
type Location     = (Int, Int)
type Move         = (Location, Location)

----------------------------------
-- Helper functions for data types
----------------------------------

-- Index into an EightOf
i8 :: Int -> EightOf a -> Maybe a
i8 0 (EightOf (c, _, _, _, _, _, _, _)) = Just c
i8 1 (EightOf (_, c, _, _, _, _, _, _)) = Just c
i8 2 (EightOf (_, _, c, _, _, _, _, _)) = Just c
i8 3 (EightOf (_, _, _, c, _, _, _, _)) = Just c
i8 4 (EightOf (_, _, _, _, c, _, _, _)) = Just c
i8 5 (EightOf (_, _, _, _, _, c, _, _)) = Just c
i8 6 (EightOf (_, _, _, _, _, _, c, _)) = Just c
i8 7 (EightOf (_, _, _, _, _, _, _, c)) = Just c
i8 _ (EightOf (_, _, _, _, _, _, _, _)) = Nothing

eightOf :: a -> EightOf a
eightOf a = EightOf (a, a, a, a, a, a, a, a)

toList :: EightOf a -> [a]
toList (EightOf (a, b, c, d, e, f, g, h)) = [a, b, c, d, e, f, g, h]

fromList :: [a] -> Maybe (EightOf a)
fromList [a, b, c, d, e, f, g, h] = Just $ EightOf (a, b, c, d, e, f, g, h)
fromList _ = Nothing

-- Label elements of an EightOf by index
labelIdx :: EightOf a -> EightOf (Int, a)
labelIdx (EightOf (a, b, c, d, e, f, g, h)) =
    EightOf ((0, a), (1, b), (2, c), (3, d), (4, e), (5, f), (6, g), (7, h))

-- For a test and an EightOf, determine which members of the EightOf (by index)
-- pass the test
test :: (a -> Bool) -> EightOf a -> [Int]
test t = map fst . filter snd . toList . labelIdx . fmap t

instance Functor EightOf where
    fmap f = fromJust . fromList . map f . toList

-- Human-readable indexing functions
row :: Int -> Board -> Maybe Row
row = i8
col :: Int -> Row -> Maybe (Maybe Piece)
col = i8
idx :: Int -> Int -> Board -> Maybe (Maybe Piece)
idx r c b = row r b >>= col c

-- Get the locations of all White or Black Pieces on a given board
colourLocs :: Colour -> Board -> [Location]
colourLocs clr = concat . map (\p -> map (\n -> (fst p, n)) (snd p))
    . filter ((/= []) . snd) . toList . labelIdx
    . fmap (test (maybe False id . fmap ((== clr) . snd)))

-- A board in the initial configuration
start :: Board
start = EightOf
    ( fromJust . fromList . map (\p -> Just(p, Black)) $
        [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]
    , eightOf (Just (Pawn, Black))
    , eightOf Nothing
    , eightOf Nothing
    , eightOf Nothing
    , eightOf Nothing
    , eightOf (Just (Pawn, White))
    , fromJust . fromList . map (\p -> Just(p, White)) $
        [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]
    )

----------------------------
-- Pretty-printing functions
----------------------------

ppRank :: Rank -> String
ppRank r = if r == Knight then "N" else [head (show r)]

ppColour :: Colour -> String
ppColour c = case c of { Black -> "B" ; White -> "W" }

ppSquare :: Maybe Piece -> String
ppSquare = maybe "[]" (\p -> ppRank (fst p) ++ ppColour (snd p))

ppRow :: Row -> String
ppRow = concat . intersperse " " . map ppSquare . toList

ppBoard :: Board -> String
ppBoard = (++ "\n") . concat . intersperse "\n" . map ppRow . toList

-- Move information
--movesOf :: Board -> Location -> [(Location, Maybe Piece)]

--daugters :: Colour -> Board -> [Board]
