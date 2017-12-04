module ComplexParser
  (
  ) where

import Lambda.Prim

import Text.Parsec
import Control.Monad (void)
import Control.Monad.Identity (Identity)

data AST = NameAST String
         | NumberAST Int
         | AbstAST [String] AST
         | ApplAST AST AST
         deriving (Eq)

type ASTParser = ParsecT String () Identity

reqSpace :: ASTParser ()
reqSpace = void (many1 $ oneOf " \t\n\r") <?> "whitespace"

optSpace :: ASTParser ()
optSpace = void $ many $ oneOf " \t\n\r"

parseInt :: ASTParser Int
parseInt = (char '0' >> return 0)
       <|> do pm <- option '+' $ oneOf "+-"
              h <- oneOf "123456789"
              t <- many digit
              return $ (if pm == '-' then negate else id) $ read $ h:t
       <?> "an integer"

parseNumber :: ASTParser AST
parseNumber = fmap NumberAST parseInt

parseName :: ASTParser String
parseName = many1 letter

parseItem :: ASTParser AST
parseItem = fmap NameAST parseName
        <|> fmap NumberAST parseInt
        <|> do char '(' >> optSpace
               e <- parseExpr
               optSpace >> void (char ')') <?> "closing ')'"
               return e
        <?> "name, literal, or parenthesized expression"

parseAbst :: ASTParser AST
parseAbst = do oneOf "λ\\" >> optSpace
               args <- sepBy1 parseName reqSpace
               optSpace >> char '.' >> optSpace
               body <- parseExpr
               return $ AbstAST args body

parseAppl :: ASTParser AST
parseAppl = foldl ApplAST <$> parseItem
                          <*> many (reqSpace >> parseAppl)

parseExpr :: ASTParser AST
parseExpr = parseAbst
        <|> parseAppl
        <?> "abstraction or application"

parseAST :: ASTParser [AST]
parseAST = optSpace >> sepBy1 parseExpr reqSpace <* optSpace