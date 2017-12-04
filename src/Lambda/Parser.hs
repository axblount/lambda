{-
Right now the parser probably does too much.
I should return a syntax tree that includes names.
Then I can walk through it, bind variables, and check for undefined vars.
This would allow me to change the environment.
-}

module Lambda.Parser
  ( parseString
  ) where

import Lambda.Prim

import Text.Parsec
import Control.Monad (void)
import Control.Monad.Identity
import Control.Monad.Reader

-- ParserState
-- The first item represents how many abstractions deep we are.
-- the second is a map from bound names to the level they were bound on.
data ParserState = ParserState {
  abstLevel :: Int,
  boundVars :: [(String, Int)]
}

bindVar :: String -> ParserState -> ParserState
bindVar name (ParserState l vs) = ParserState l $ (name, l):vs

nextLevel :: ParserState -> ParserState
nextLevel (ParserState l vs) = ParserState (l+1) vs

-- We parse strings using ParserState as the state.
type LambdaParser = ParsecT String () (ReaderT ParserState Identity)

reqSpace :: LambdaParser ()
reqSpace = void (many1 $ oneOf " \t\n\r") <?> "whitespace"

optSpace :: LambdaParser ()
optSpace = void $ many $ oneOf " \t\n\r"

parseInt :: LambdaParser Int
parseInt = (char '0' >> return 0)
       <|> do pm <- option '+' $ oneOf "+-"
              h <- oneOf "123456789"
              t <- many digit
              return $ (if pm == '-' then negate else id) $ read $ h:t
       <?> "an integer"

parseIndex :: LambdaParser Expr
parseIndex = fmap Index (char '#' >> parseInt)

parseName :: LambdaParser String
parseName = many1 letter

parseExpr :: LambdaParser Expr
parseExpr = parseAbst
        <|> parseAppl
        <?> "abstraction or application"

parseAppl :: LambdaParser Expr
parseAppl = foldl Appl <$> parseItem
                       <*> many (reqSpace >> parseAppl)

parseItem :: LambdaParser Expr
parseItem = parseVar
        <|> parseIndex
        <|> do char '(' >> optSpace
               e <- parseExpr
               optSpace >> void (char ')') <?> "closing ')'"
               return e
        <?> "variable, index, or parenthized expression"

parseVar :: LambdaParser Expr
parseVar = do name <- parseName
              env <- ask
              case lookup name (boundVars env) of
                -- the name is bound. calculate how many abstractions up it was bound.
                Just pos -> return $ Index $ (abstLevel env) - pos
                -- the name isn't bound. Find the first unbound index at the root level.
                -- Then bind this var.
                Nothing -> fail $ "Unbound variable: " ++ name

parseAbst :: LambdaParser Expr
parseAbst = do oneOf "λ\\" >> optSpace
               name <- parseName
               optSpace >> char '.' >> optSpace
               body <- local (bindVar name . nextLevel) parseExpr
               return $ Abst body

parseString :: String -> Either ParseError Expr
parseString s = runIdentity $ runReaderT $ (runPT parseExpr () "(string)" s) (ParserState (-1) [])
