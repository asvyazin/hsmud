import Text.ParserCombinators.Parsec

iac :: Parser Char
iac = char '\255'
_do :: Parser Char
_do = char '\253'
dont :: Parser Char
dont = char '\254'
will :: Parser Char
will = char '\251'
wont :: Parser Char
wont = char '\252'
sb:: Parser Char
sb = char '\250'
se :: Parser Char
se = char '\240'

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
             
iacParser :: Parser Token
iacParser = iac >> afterIac
  where
    opt p c = p >> anyChar >>= return . c
    doParser = opt _do Do
    dontParser = opt dont Dont
    willParser = opt will Will
    wontParser = opt wont Wont
    subnegoEnd = iac >> se
    subnegoContent = many $ noneOf "\255"
    subnegoParser = between sb subnegoEnd subnegoContent >>= return . Subnego
    commandParser = anyChar >>= return . Command
    iac2Parser = iac >> return (TelnetChar '\255')
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
