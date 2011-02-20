module PkgCheckParser (parseCheck, BrokenPackage (..)) where
import Text.Parsec
import Text.Parsec.String
import Control.Applicative hiding ((<|>), many)
data BrokenPackage = P { packageID  :: String
                       , brokenDeps :: [String]
                       } deriving (Show, Eq, Ord)


parseCheck :: String -> Either ParseError [BrokenPackage]
parseCheck = parse (many1 check) ""

check :: Parser BrokenPackage
check = string "There are problems in package "
        >> P <$> packName <* string ":\n" <*> many1 errDep

ident :: Parser String
ident = many (alphaNum <|> oneOf "-.")

packID, packName :: Parser String
packID = ident
packName = ident

errDep :: Parser String
errDep = string "  dependency \"" *> packID <* string "\" doesn't exist\n"
