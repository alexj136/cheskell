module Chess where
import Data.List  ( intersperse , findIndices )

-------------------
-- Chess data types
-- ----------------

data Rank = King Bool      -- Castleable
          | Queen
          | Bishop
          | Knight
          | Rook Bool      -- Castleable
          | Pawn Bool Bool -- Double-moveable / En passant-able
          deriving (Show, Eq, Ord)
data Colour   = Black | White deriving (Show, Eq, Ord)
type Piece    = (Rank, Colour)
type Square   = Maybe Piece
type Row      = [Square]
type Board    = [Row]
type Location = (Int, Int)
type Move     = (Location, Location)

----------------------------
-- Pretty-printing functions
----------------------------

ppPiece :: Piece -> String
ppPiece piece = case piece of
    ( King _   , clr ) -> if clr == White then "♔" else "♚"
    ( Queen    , clr ) -> if clr == White then "♕" else "♛"
    ( Rook _   , clr ) -> if clr == White then "♖" else "♜"
    ( Bishop   , clr ) -> if clr == White then "♗" else "♝"
    ( Knight   , clr ) -> if clr == White then "♘" else "♞"
    ( Pawn _ _ , clr ) -> if clr == White then "♙" else "♟"

ppSquare :: Square -> String
ppSquare = maybe " " ppPiece

ppRow :: Row -> String
ppRow = concat . map ppSquare

ppBoard :: Board -> String
ppBoard = (++ "\n") . concat . intersperse "\n" . map ppRow

----------------------------------
-- Helper functions for data types
----------------------------------

addAllInBounds :: Location -> [Location] -> [Location]
addAllInBounds = (filter inBounds .) . map . addLocation

inBounds :: Location -> Bool
inBounds (r, c) = r <= 7 && r >= 0 && c <= 7 && c >= 0

addLocation :: Location -> Location -> Location
addLocation (r1, c1) (r2, c2) = (r1 + r2, c1 + c2)

-- Human-readable indexing functions
row :: Int -> Board -> Row
row = flip (!!)

col :: Int -> Row -> Square
col = flip (!!)

idx :: Location -> Board -> Square
idx (r, c) = col c . row r

isClr :: Colour -> Square -> Bool
isClr clr Nothing          = False
isClr clr (Just (_, clr')) = clr == clr'

otherClr :: Colour -> Colour
otherClr White = Black
otherClr Black = White

rank :: Piece -> Rank
rank = fst
clr :: Piece -> Colour
clr = snd

setSquare :: Location -> Square -> Board -> Board
setSquare l s b = undefined

-- A board in the initial configuration
start :: Board
start =
    [ map (\p -> Just(p, Black)) $
        [Rook False, Knight, Bishop, Queen,
            King False, Bishop, Knight, Rook False]
    , replicate 8 (Just (Pawn False False, Black))
    , replicate 8 Nothing
    , replicate 8 Nothing
    , replicate 8 Nothing
    , replicate 8 Nothing
    , replicate 8 (Just (Pawn False False, White))
    , map (\p -> Just(p, White)) $
        [Rook False, Knight, Bishop, Queen,
            King False, Bishop, Knight, Rook False]
    ]

knightMoves :: [Location]
knightMoves =
    [(1, 2), (-1, 2), (1, -2), (-1, -2), (2, 1), (-2, 1), (2, -1), (-2, -1)]

rookMoves :: [Location]
rookMoves =
    [ ( 0,  1), ( 0,  2), ( 0,  3), ( 0,  4), ( 0,  5), ( 0,  6), ( 0,  7)
    , ( 0, -1), ( 0, -2), ( 0, -3), ( 0, -4), ( 0, -5), ( 0, -6), ( 0, -7)
    , ( 1,  0), ( 2,  0), ( 3,  0), ( 4,  0), ( 5,  0), ( 6,  0), ( 7,  0)
    , (-1,  0), (-2,  0), (-3,  0), (-4,  0), (-5,  0), (-6,  0), (-7,  0)
    ]

bishopMoves :: [Location]
bishopMoves =
    [ ( 1,  1), ( 2,  2), ( 3,  3), ( 4,  4), ( 5,  5), ( 6,  6), ( 7,  7)
    , ( 1, -1), ( 2, -2), ( 3, -3), ( 4, -4), ( 5, -5), ( 6, -6), ( 7, -7)
    , (-1,  1), (-2,  2), (-3,  3), (-4,  4), (-5,  5), (-6,  6), (-7,  7)
    , (-1, -1), (-2, -2), (-3, -3), (-4, -4), (-5, -5), (-6, -6), (-7, -7)
    ]

queenMoves :: [Location]
queenMoves = rookMoves ++ bishopMoves

kingMoves :: [Location]
kingMoves = 
    [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]

-- Move information
movesOf :: Location -> Board -> [Move]
movesOf loc board = case idx loc board of
    Nothing                 -> []
    Just (Knight   , clr  ) ->
        map (\l -> (loc, l)) $
        filter (\l -> not (isClr clr (idx l board))) $
        addAllInBounds loc knightMoves
    Just (Pawn True  _ , White) ->
        map (\l -> (loc, l)) $ (
            filter (\l -> idx l board == Nothing) $
            addAllInBounds loc [(1, 0)]
        ) ++ (
            filter (\l -> isClr Black (idx l board)) $
            addAllInBounds loc [(1, -1), (1, 1)]
        ) -- also check for en passants
    Just (Pawn False _ , White) -> undefined  -- TODO
    Just (Pawn True  _ , Black) -> undefined  -- TODO
    Just (Pawn False _ , Black) -> undefined  -- TODO
    Just (Bishop       , White) -> undefined  -- TODO
    Just (Bishop       , Black) -> undefined  -- TODO
    Just (Queen        , White) -> undefined  -- TODO - perhaps we can simplify
    Just (Queen        , Black) -> undefined  -- TODO   queen by unifying rook
    Just (Rook True    , White) -> undefined  -- TODO   and bishop?
    Just (Rook False   , White) -> undefined  -- TODO
    Just (Rook True    , Black) -> undefined  -- TODO
    Just (Rook False   , Black) -> undefined  -- TODO
    Just (King True    , White) -> undefined  -- TODO
    Just (King False   , White) -> undefined  -- TODO
    Just (King True    , Black) -> undefined  -- TODO
    Just (King False   , Black) -> undefined  -- TODO
