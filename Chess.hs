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
type Piece        =
    ( Rank   -- The kind of piece
    , Colour -- The colour
    , Bool   -- Has it moved yet?
    )
type Square       = Maybe Piece
newtype EightOf a = EightOf (a, a, a, a, a, a, a, a) deriving Show
type Row          = EightOf Square
type Board        =
    ( EightOf Row    -- The board layout
    , Maybe Move     -- The previous move (Nothing only before the first move)
    , Maybe Move     -- Other piece that moved last turn (if player castled)
    , Maybe Location -- Location of piece captured by an en passant last turn
    )
type Location     = (Int, Int)
type Move         = (Location, Location)

----------------------------
-- Pretty-printing functions
----------------------------

ppRank :: Rank -> String
ppRank r = if r == Knight then "N" else [head (show r)]

ppColour :: Colour -> String
ppColour c = case c of { Black -> "B" ; White -> "W" }

ppPiece :: Piece -> String
ppPiece (r, c, _) = ppRank r ++ ppColour c

ppSquare :: Square -> String
ppSquare = maybe "[]" ppPiece

ppRow :: Row -> String
ppRow = concat . intersperse " " . map ppSquare . toList

ppBoard :: Board -> String
ppBoard (grid, _, _, _) =
    (++ "\n") . concat . intersperse "\n" . map ppRow . toList $ grid

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
passerIdxs :: (a -> Bool) -> EightOf a -> [Int]
passerIdxs t = map fst . filter snd . toList . labelIdx . fmap t

instance Functor EightOf where
    fmap f = fromJust . fromList . map f . toList

-- Human-readable indexing functions
row :: Int -> Board -> Maybe Row
row r (g, _, _, _) = i8 r g
col :: Int -> Row -> Maybe Square
col = i8
idx :: Location -> Board -> Maybe Square
idx (r, c) b = row r b >>= col c

rank :: Piece -> Rank
rank (r, _, _) = r
clr :: Piece -> Colour
clr (_, c, _) = c
hasMoved :: Piece -> Bool
hasMoved (_, _, h) = h

-- Get the locations of all White or Black Pieces on a given board
colourLocs :: Colour -> Board -> [Location]
colourLocs colr (grid, _, _, _) =
    concat . map (\p -> map (\n -> (fst p, n)) (snd p))
    . filter ((/= []) . snd) . toList . labelIdx
    . fmap (passerIdxs (maybe False id . fmap ((== colr) . clr))) $ grid

-- A board in the initial configuration
start :: Board
start =
    ( EightOf
      ( fromJust . fromList . map (\p -> Just(p, Black, False)) $
          [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]
      , eightOf (Just (Pawn, Black, False))
      , eightOf Nothing
      , eightOf Nothing
      , eightOf Nothing
      , eightOf Nothing
      , eightOf (Just (Pawn, White, False))
      , fromJust . fromList . map (\p -> Just(p, White, False)) $
          [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]
      )
    , Nothing
    , Nothing
    , Nothing
    )

-- Move information
movesOf :: Location -> Board -> [(Location, Square)]
movesOf l b = case idx l b of
    Nothing -> []
    Just _  -> undefined -- TODO

--daugters :: Colour -> Board -> [Board]
