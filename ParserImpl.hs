-- Put your Parser implmenetation in this file.
module ParserImpl where

import Types
import Text.ParserCombinators.Parsec
import Data.Char(ord)

--------------------------------------------------------------------------------
--------------------------------- Atomics --------------------------------------
--------------------------------------------------------------------------------

keywords =
    ["and", "false", "if", "implies", "in", "is", "not", "or", "true", "unless"]

pName :: Parser String
pName = lexeme $ do
    c  <- letter
    cs <- many pLetterDigitUnderscore
    let name = c:cs
    if name `elem` keywords
        then unexpected $ name ++ " : is reserved keyword"
        else return name

pLetterDigitUnderscore :: Parser Char
pLetterDigitUnderscore =
    char '_' <|> letter <|> digit


pStringConst :: Parser String
pStringConst = lexeme $ do
    between (char '"') (char '"') (many pStrChar)

pStrChar :: Parser Char
pStrChar =
      try(do char '"'; char '"')
      <|> do satisfy isPrintableAscii

isPrintableAscii :: Char -> Bool
isPrintableAscii = \x -> let c = ord x in 32 <= c && c <= 127 && c /= 34


--------------------------------------------------------------------------------
-------------------------------- Conditions ------------------------------------
--------------------------------------------------------------------------------

pCond :: Parser Cond -- lowest precedence. right ass.
pCond  = try(do  pCond1 `chainr1` (do keyword "implies";
                                      return $ \x y -> COr (CNot x) y))
         <|> pCond1

pCond1 :: Parser Cond -- left ass.
pCond1 = try(do  pCond2 `chainl1` (do keyword "or"; return COr))
         <|> pCond2

pCond2 :: Parser Cond -- left ass.
pCond2 = try(do  pCond3 `chainl1` (do keyword "and"; return CAnd))
         <|> pCond3

pCond3 :: Parser Cond -- high precedence. nestable.
pCond3 = try(do  keyword "not"; c <- pCond3; return $ CNot c)
         <|> pCond4

pCond4 :: Parser Cond
pCond4 = do symbol "("; c <- pCond; symbol ")"; return c
     <|>
     try(do keyword "true";  return CTrue)
     <|>
     try(do keyword "false"; return $ CNot CTrue)
     <|>
     try(do t1 <- pTerm; keyword "is"; keyword "not"; t2 <- pTerm
            return $ CNot $ CEq t1 t2)
     <|>
     try(do t1 <- pTerm; keyword "is"; t2 <- pTerm
            return $ CEq t1 t2)
     <|> do a <- pAtom; return $ CAtom a

--------------------------------------------------------------------------------
---------------------------------- Atom ----------------------------------------
--------------------------------------------------------------------------------
pAtom :: Parser Atom
pAtom = do pname <- pName; symbol "("; tz <- pTerms; symbol ")";
           return $ Atom pname tz

--------------------------------------------------------------------------------
--------------------------------- Terms ----------------------------------------
--------------------------------------------------------------------------------

pTerms :: Parser [Term]
pTerms = sepBy pTerm $ symbol ","

pTerm :: Parser Term
pTerm = do vname <- pName;        return $ TVar vname
    <|> do sdata <- pStringConst; return $ TData sdata

--------------------------------------------------------------------------------
---------------------------------- Rule ----------------------------------------
--------------------------------------------------------------------------------

pRule :: Parser Rule
pRule =
    try(do a <- pAtom; keyword "if"; c <- pCond
           return $ Rule a c)
    <|>
    try(do a <- pAtom; keyword "unless"; c <- pCond
           return $ Rule a $ CNot (c))
    <|>
    try(do a <- pAtom
           return $ Rule a CTrue)

--------------------------------------------------------------------------------
--------------------------------- Program --------------------------------------
--------------------------------------------------------------------------------

pProgram :: Parser Program
pProgram = do endBy pRule (symbol ".")

pFullProgram :: Parser Program
pFullProgram = do
    whitespace
    p <- pProgram
    eof
    return p


-- API ;D
parseString :: String -> Either ErrMsg Program
parseString s = case parse pFullProgram "" s of
                    Left err  -> Left $ EUser $ show err
                    Right res -> return res

--------------------------------------------------------------------------------
--------------------------- Whitespace 'N' Helpers -----------------------------
--------------------------------------------------------------------------------

-- eats anychar until meets *)
pComment :: Parser ()
pComment = do
    string "(*"
    manyTill anyChar $ string "*)"
    return ()

whitespace :: Parser ()
whitespace = skipMany (() <$ space <|> try(pComment))

-- Remove whitespace after input parser p done parsing. return p's result.
lexeme :: Parser a -> Parser a
lexeme p = do a <- p; whitespace; return a

-- eats s and whitespace after
symbol :: String -> Parser ()
symbol s = lexeme $ do string s; return ()

keyword :: String -> Parser String
keyword s = lexeme $ do
    a <- string s
    notFollowedBy pLetterDigitUnderscore
    return a

