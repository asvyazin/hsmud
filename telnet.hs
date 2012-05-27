import Text.ParserCombinators.Parsec

iac = '\255'
_do = '\253'
dont = '\254'
will = '\251'
wont = '\252'
sb = '\250'
se = '\240'

data Token = Do Char
           | Dont Char
           | Will Char
           | Wont Char
           | Subnego String
           | Command Char
           | TelnetChar Char
             
instance Show Token where
  show (Do c) = "DO " ++ show c
  show (Dont c) = "DONT "++ show c
  show (Will c) = "WILL " ++ show c
  show (Wont c) = "WONT " ++ show c
  show (Subnego s) = "SUBNEGO "++ show s
  show (Command c) = "COMMAND " ++ show c
  show (TelnetChar c) = show c
             
doParser :: Parser Token
doParser = char _do >> anyChar >>= return . Do
dontParser :: Parser Token
dontParser = char dont >> anyChar >>= return . Dont
willParser :: Parser Token
willParser = char will >> anyChar >>= return . Will
wontParser :: Parser Token
wontParser = char wont >> anyChar >>= return . Wont
subnegoParser :: Parser Token
subnegoParser = between (char sb) subnegoEnd subnegoContent >>= return . Subnego
  where
    subnegoEnd = char iac >> char se
    subnegoContent = many $ noneOf "\255"
commandParser :: Parser Token
commandparser = anyChar >>= return . Command
iac2Parser :: Parser Token
iac2Parser = char iac >> return (TelnetChar iac)
iacParser :: Parser Token
iacParser = char iac >> afterIac
  where
    afterIac = doParser <|> 
               dontParser <|> 
               willParser <|> 
               wontParser <|> 
               subnegoParser <|> 
               commandParser <|> 
               iac2Parser
telnetChar :: Parser Token
telnetChar = anyChar >>= return . TelnetChar
token :: Parser Token
token = iacParser <|> telnetChar
