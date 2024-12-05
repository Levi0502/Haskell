module NagyBead where

import Data.Either
import Data.Maybe
import Text.Read (readMaybe)



data Dir = InfixL | InfixR
  deriving (Show,Eq,Ord)

data Tok a = BrckOpen
           | BrckClose
           | TokLit a
           | TokBinOp (a -> a -> a) Char Int Dir

instance Show a => Show (Tok a) where
  show (BrckOpen) = "BrckOpen"
  show (BrckClose) = "BrckClose"
  show (TokLit a) = "TokLit " ++ show a
  show (TokBinOp _ c p d) = "TokBinOp '" ++ [c] ++ "' " ++ show p ++ " " ++ show d

instance Eq a => Eq (Tok a) where
  BrckOpen == BrckOpen = True
  BrckClose == BrckClose = True
  (TokLit x) == (TokLit y) = x == y
  (TokBinOp _ c1 p1 d1) == (TokBinOp _ c2 p2 d2) = 
        c1 == c2 && p1 == p2 && d1 == d2
  _ == _ = False
    
basicInstances = 0 -- Mágikus tesztelőnek kell ez, NE TÖRÖLD!

type OperatorTable a = [(Char, (a -> a -> a, Int, Dir))]

tAdd, tMinus, tMul, tDiv, tPow :: (Floating a) => Tok a
tAdd = TokBinOp (+) '+' 6 InfixL
tMinus = TokBinOp (-) '-' 6 InfixL
tMul = TokBinOp (*) '*' 7 InfixL
tDiv = TokBinOp (/) '/' 7 InfixL
tPow = TokBinOp (**) '^' 8 InfixR

operatorTable :: (Floating a) => OperatorTable a
operatorTable =
    [ ('+', ((+), 6, InfixL))
    , ('-', ((-), 6, InfixL))
    , ('*', ((*), 7, InfixL))
    , ('/', ((/), 7, InfixL))
    , ('^', ((**), 8, InfixR))
    ]

operatorFromChar :: OperatorTable a -> Char -> Maybe (Tok a)
operatorFromChar table c = case lookup c table of
  Just (op, prec, dir) -> Just (TokBinOp op c prec dir)
  Nothing -> Nothing

getOp :: (Floating a) => Char -> Maybe (Tok a)
getOp = operatorFromChar operatorTable

parseTokens :: Read a => OperatorTable a -> String -> Maybe [Tok a]
parseTokens table str = parse' str []
  where
    parse' [] acc = Just (reverse acc)
    parse' (c:cs) acc = case c of
      ' ' -> parse' cs acc
      '(' -> parse' cs (BrckOpen : acc)
      ')' -> parse' cs (BrckClose : acc)
      _   -> case operatorFromChar table c of
        Just op -> parse' cs (op : acc)
        Nothing -> case span (\x -> (x >= '0' && x <= '9') || x == '.') (c:cs) of
          (numStr, rest) -> case readMaybe numStr of
            Just num -> parse' rest (TokLit num : acc)
            Nothing -> Nothing

parse :: String -> Maybe [Tok Double]
parse = parseTokens operatorTable

shuntingYardBasic :: [Tok a] -> ([a], [Tok a])
shuntingYardBasic = process [] []
  where
    process literals operators [] = (literals, operators)
    process literals operators (token:rest) = case token of
        TokLit x -> process (x:literals) operators rest
        BrckOpen -> process literals (token:operators) rest
        op@(TokBinOp _ _ _ _) -> process literals (op:operators) rest
        BrckClose -> 
            let (ops, remaining) = break isBrckOpen operators
                newLiterals = foldl applyOp literals ops
                newOperators = drop 1 remaining
            in process newLiterals newOperators rest
        
    applyOp (x:y:rest) (TokBinOp f _ _ _) = (f y x):rest
    applyOp literals _ = literals

    isBrckOpen BrckOpen = True
    isBrckOpen _ = False


parseAndEval :: (String -> Maybe [Tok a]) -> ([Tok a] -> ([a], [Tok a])) -> String -> Maybe ([a], [Tok a])
parseAndEval parse eval input = maybe Nothing (Just . eval) (parse input)

syNoEval :: String -> Maybe ([Double], [Tok Double])
syNoEval = parseAndEval parse shuntingYardBasic

syEvalBasic :: String -> Maybe ([Double], [Tok Double])
syEvalBasic = parseAndEval parse (\t -> shuntingYardBasic $ BrckOpen : (t ++ [BrckClose]))

shuntingYardPrecedence :: [Tok a] -> ([a], [Tok a])
shuntingYardPrecedence = process [] []
  where
    process literals operators [] = (literals, operators)
    process literals operators (token:rest) = case token of
        TokLit x -> process (x:literals) operators rest
        BrckOpen -> process literals (token:operators) rest
        BrckClose -> 
            let (ops, remaining) = break isBrckOpen operators
                newLiterals = foldl applyOp literals ops
                newOperators = drop 1 remaining
            in process newLiterals newOperators rest
        op@(TokBinOp _ _ prec dir) -> 
            let (toEval, remaining) = span shouldEvaluate operators
                newLiterals = foldl applyOp literals toEval
            in process newLiterals (op:remaining) rest
          where
            shouldEvaluate (TokBinOp _ _ p d) = 
              p > prec || (dir == InfixL && p >= prec)
            shouldEvaluate _ = False

    applyOp (x:y:rest) (TokBinOp f _ _ _) = (f y x):rest
    applyOp literals _ = literals

    isBrckOpen BrckOpen = True
    isBrckOpen _ = False

syEvalPrecedence :: String -> Maybe ([Double], [Tok Double])
syEvalPrecedence = parseAndEval parse (\t -> shuntingYardPrecedence $ BrckOpen : (t ++ [BrckClose]))

-- eqError-t vedd ki a kommentből, ha megcsináltad az 1 pontos "Hibatípus definiálása" feladatot
-- eqError = 0 -- Mágikus tesztelőnek szüksége van rá, NE TÖRÖLD!

{-
-- Ezt akkor vedd ki a kommentblokkból, ha a 3 pontos "A parser és az algoritmus újradefiniálása" feladatot megcsináltad.
parseAndEvalSafe ::
    (String -> ShuntingYardResult [Tok a]) ->
    ([Tok a] -> ShuntingYardResult ([a], [Tok a])) ->
    String -> ShuntingYardResult ([a], [Tok a])
parseAndEvalSafe parse eval input = either Left eval (parse input)

sySafe :: String -> ShuntingYardResult ([Double], [Tok Double])
sySafe = parseAndEvalSafe
  (parseSafe operatorTable)
  (\ts -> shuntingYardSafe (BrckOpen : ts ++ [BrckClose]))
-}

{-
-- Ezt akkor vedd ki a kommentblokkból, ha az 1 pontos "Függvénytábla és a típus kiegészítése" feladatot megcsináltad.
tSin, tCos, tLog, tExp, tSqrt :: Floating a => Tok a
tSin = TokFun sin "sin"
tCos = TokFun cos "cos"
tLog = TokFun log "log"
tExp = TokFun exp "exp"
tSqrt = TokFun sqrt "sqrt"

functionTable :: (RealFrac a, Floating a) => FunctionTable a
functionTable =
    [ ("sin", sin)
    , ("cos", cos)
    , ("log", log)
    , ("exp", exp)
    , ("sqrt", sqrt)
    , ("round", (\x -> fromIntegral (round x :: Integer)))
    ]
-}

{-
-- Ezt akkor vedd ki a kommentblokkból, ha a 2 pontos "Függvények parse-olása és kiértékelése" feladatot megcsináltad.
syFun :: String -> Maybe ([Double], [Tok Double])
syFun = parseAndEval
  (parseWithFunctions operatorTable functionTable)
  (\t -> shuntingYardWithFunctions $ BrckOpen : (t ++ [BrckClose]))
-}

{-
-- Ezt akkor vedd ki a kommentblokkból, ha minden más feladatot megcsináltál ez előtt.
syComplete :: String -> ShuntingYardResult ([Double], [Tok Double])
syComplete = parseAndEvalSafe
  (parseComplete operatorTable functionTable)
  (\ts -> shuntingYardComplete (BrckOpen : ts ++ [BrckClose]))
-}
