{-# LANGUAGE FlexibleContexts #-}
import Text.Parsec
import Data.Map
import Debug.Trace

type M = Data.Map.Map String Int
type P = Parsec String M String
sp = spaces

fromRight :: Show a => Either a p -> p
fromRight (Left e) = error $ show e
fromRight (Right x) = x

fromRight' :: Show a => Either a String -> String
fromRight' (Left e) = error $ show e
fromRight' (Right x) = x

tryStr :: String -> P
tryStr = try . string

aabaab = t forLoop empty "for (i=0;i<2;i++) {for (j=0;j<2;j++) {a;b;}}"
nest   = t forLoop empty "for (i=0;i<2;i++) for (j=0;j<2;j++) {q;}"

main :: IO ()
main = interact go

go :: String -> String
go str = fromRight $ runParser program empty "" str

t p m = runParser p m ""

program :: P
program = do
  fmap concat $ many1 stmt

stmt :: P
stmt = forLoop <|> try assignment <|> try simpleStmt <|> many1 space

assignment :: P
assignment = do
  sp
  var <- many1 idChar
  sp
  char '='
  sp
  val <- fmap read $ many1 digit
  sp
  char ';'
  modifyState $ insert var val
  return ""

simpleStmt :: P
simpleStmt = do
  s1 <- many space
  b <- many1 (noneOf ";")
  char ';'
  return $ s1 ++ b ++ ";"

k = t forLoop empty "for (i=0;i<2;i++) pr(i);"

forLoop :: P
forLoop = do
  try $ sp >> string "for"
  sp
  (var, a) <- header   -- e.g. var is "i" and a is [0,1,2]
  b <- body
  env <- getState
  return $ concatMap
      (f var env b)
      --(const b)
      a

-- f "i" empty "pr(i);" 3 = "pr(3);"
f :: String -> M -> String -> Int -> String
f var env b val =
  let str = subst' var env b val
  in fromRight' $ runParser program empty "" str
  --in str ++ " -- " ++ (fromRight' $ runParser program empty "" str)

-- subst "i" empty "pr(i)" 3 = "pr(3)"
subst' :: String -> M -> String -> Int -> String
subst' var env b val = subst (insert var val env) b

-- subst (fromList [("i",3)]) "i+i" = "3+3"
subst :: M -> String -> String
subst env str = fromRight $ runParser substParser env "" str

substParser :: P
substParser = fmap concat $ many1 substToken

substToken :: P
substToken = many1 space <|> substVar
  <|> many1 (noneOf $
       ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " _\t\n")

substVar :: P
substVar = do
  var <- many1 idChar
  env <- getState
  return $
    case (Data.Map.lookup var env) of
      Nothing -> var
      Just val -> show val

body :: P
body = sp >> (bodyBrace' <|> bodyStmt)

bodyStmt :: P
bodyStmt = do
  b <- bodyHelper' "{})];"
  s <- string ";" <|> bodyBrace
  return $ b ++ s ++ "\n"

bodyBrace' :: P
bodyBrace' = do
  char '{'
  s <- bodyHelper "})]"
  char '}'
  return s

bodyBrace :: P
bodyBrace = do
  char '{'
  s <- bodyHelper "})]"
  char '}'
  return $ "{" ++ s ++ "}"

bodyParen :: P
bodyParen = do
  char '('
  s <- bodyHelper "})]"
  char ')'
  return $ "(" ++ s ++ ")"

bodyBracket :: P
bodyBracket = do
  char '['
  s <- bodyHelper "})]"
  char ']'
  return $ "[" ++ s ++ "]"

bodyHelper :: String -> P
bodyHelper str = fmap concat $ many $ bodyParen <|> bodyBrace <|> bodyBracket <|> noneOfAsStr str

bodyHelper' :: String -> P
bodyHelper' str = fmap concat $ many $ bodyParen <|>
    bodyBracket <|> noneOfAsStr str

noneOfAsStr :: String -> P
noneOfAsStr str = do
  c <- noneOf str
  return [c]

idChar :: Parsec String M Char
idChar = letter <|> digit <|> char '_'

idStr :: P
idStr = many1 idChar

header :: Parsec String M (String, [Int])
header = do
  env <- getState
  char '(' >> sp
  var <- (tryStr "int" >> sp >> idStr) <|> idStr
  sp
  firstStr <- sp >> char '=' >> sp >> many (noneOf " ;")
  let firstVal = fromRight $ runParser eval env "" firstStr
  condStr <- sp >> char ';' >> sp >> many (noneOf ";")
  var' <- sp >> char ';' >> sp >> idStr
  incr <- sp >> (string "++" <|> string "--")
  sp >> char ')' >> many newline
  return $ (var, range (head incr) var env firstVal condStr)

-- range '+' "i" empty 0 "i<3" = [0,1,2]
range :: Char -> String -> M -> Int -> String -> [Int]
range '+' var env firstVal cond =
  takeWhile (testCond var env cond) [firstVal..]
range '-' var env firstVal cond =
  takeWhile (testCond var env cond) [firstVal, firstVal-1 ..]

-- testCond "i" empty "i<3" 2 = True
testCond :: String -> M -> String -> Int -> Bool
testCond var env condStr val = fromRight $
  runParser condParser (insert var val env) "" condStr

condParser :: Parsec String M Bool
condParser = do
  x <- eval
  op <- sp >> many1 (oneOf "!<>=")
  y <- sp >> eval
  return $ cond op x y

cond :: String -> Int -> Int -> Bool
cond ">" x y = x > y
cond ">=" x y = x >= y
cond "<" x y = x < y
cond "<=" x y = x <= y
cond "==" x y = x == y
cond "!=" x y = x /= y

-- eval (fromList [("i",3)]) "i+1" = 4
eval :: Parsec String M Int
eval = try termOpTerm <|> term

-- term (fromList [("i",3)]) "i" = 3
-- term (fromList [("i",3)]) "5" = 5
term :: Parsec String M Int
term = termInt <|> termVar

termInt :: Parsec String M Int
termInt = fmap read $ many1 digit

termVar :: Parsec String M Int
termVar = do
  env <- getState
  str <- idStr
  return $ env ! str

termOpTerm :: Parsec String M Int
termOpTerm = do
  x <- term
  op <- sp >> oneOf "+-"
  y <- sp >> term
  return $ case op of
    '+' -> x+y
    '-' -> x-y
