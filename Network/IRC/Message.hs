module Network.IRC.Message
  ( Message(..)
  , Command(..)
  , Prefix(..)
  , message
  , userName, hostName, nickName
  , channel
  , crlf, special
  , module Text.Parsec
  , module Text.Parsec.String
  ) where
import Control.Applicative ((<$>), (<*>), (<*), (*>), liftA2)
import Control.Monad
import Data.Word
import Text.Parsec
import Text.Parsec.String

type User = String
type Host = String

data Message = Message
  { msgPrefix :: Maybe Prefix
  , msgCommand :: Command
  , msgParams :: [String]
  } deriving (Show, Eq, Ord)

data Command
  = SCommand String
  | NCommand Word8
  deriving (Show, Eq, Ord)

data Prefix
  = ServerName String
  | NickName
      { nick :: String
      , nickUser :: Maybe User
      , nickHost :: Maybe Host
      }
  deriving (Show, Eq, Ord)

message :: Parser Message
message = Message
  <$> optionMaybe (char ':' *> prefix <* skipMany1 (char ' '))
  <*> command
  <*> params []
  <* crlf

command :: Parser Command
command =
  ((SCommand <$> many1 letter <?> "expected <letter>s")
   <|>
   (NCommand . read <$> count 3 digit <?> "expected <number>s")
  ) <?> "expected <command>"
middle :: Parser String
middle = skipMany1 (char ' ') >> (:) <$> noneOf " \NUL\r\n:" <*> many (noneOf " \NUL\r\n")
trailing :: Parser String
trailing = skipMany1 (char ' ') *> char ':' *> many (noneOf "\NUL\r\n")

params :: [String] -> Parser [String]
params ps = do
  mp <- optionMaybe (try trailing <|> middle)
  case mp of
    Nothing -> return $ reverse ps
    Just p -> params (p:ps)

prefix :: Parser Prefix
prefix =
   (ServerName <$> hostName)
   <|>
   (NickName <$> nickName
     <*> optionMaybe (char '!' *> userName)
     <*> optionMaybe (char '@' *> hostName))

userName :: Parser String
userName = many1 (noneOf "@ \NUL\r\n")
  <?> "expected <user>"

hostName :: Parser String
hostName = (try dnsHostURL <|> try ipv4Address <|> ipv6Address)
  <?> "expected <host>"

nickName :: Parser String
nickName = ((:) <$> letter <*> upTo 8 (letter <|> digit <|> special))
  <?> "expected <nick>"

channel :: Parser String
channel = ((:) <$> oneOf "#@" <*> upTo 200 (noneOf " ,\BEL\NUL\r\n"))
  <?> "expected <channel>"

-- | Tolerates accidental extra LFs.
crlf :: Parser String
crlf = ((:) <$> char '\r' <*> many1 (char '\n')) <?> "expected <crlf>"

special :: Parser Char
special = oneOf "-[]\\`^{}" <?> "expected <special>"
  

dnsHostURL :: Parser String
dnsHostURL = do
  let seps = ".-/!" -- XXX
  front <- (:) <$> letter <*> many (alphaNum <|> oneOf seps)
  back <- option '.' alphaNum
  case back of
    '.' -> if last front `elem` seps
            then fail "expected final char is <alphanum>"
            else return front
    otherwise -> return (front ++ [back])
ipv4Address :: Parser String
ipv4Address =
 foldl (+) snum (replicate 3 (char '.' * snum))
  where
   (*) = liftA2 (:)
   (+) = liftA2 (++)
   snum = do
     n <- digit * upTo 2 digit <?> "expected <word8>"
     if (read n :: Word16) > 255
       then fail "IPv4Address octet too large"
       else return n
ipv6Address :: Parser String  
ipv6Address =
 foldl (+) hex (replicate 7 (char ':' * hex))
  where
   hex = hexDigit * upTo 3 hexDigit
   (*) = liftA2 (:)
   (+) = liftA2 (++)


maybeEof :: (Stream s m t, Show t) => ParsecT s u m a -> ParsecT s u m (Maybe a)
maybeEof p = liftM Just p <|> (eof >> return Nothing)

upToMaybeAccum :: Stream s m t => Int -> [a] -> ParsecT s u m (Maybe a) -> ParsecT s u m [a]
upToMaybeAccum n accum p
 | n <= 0 = return $ reverse accum
 | otherwise = do
    x <- p
    case x of
      Nothing -> return $ reverse accum
      Just v -> upToMaybeAccum (n - 1) (v:accum) p

upToEofAccum :: (Stream s m t, Show t) => Int -> [a] -> ParsecT s u m a -> ParsecT s u m [a]
upToEofAccum n accum p = upToMaybeAccum n accum (maybeEof p)
upToEof :: (Stream s m t, Show t) => Int -> ParsecT s u m a -> ParsecT s u m [a]
upToEof n = upToEofAccum n []

upToAccum :: Stream s m t => Int -> [a] -> ParsecT s u m a -> ParsecT s u m [a]
upToAccum n accum p = upToMaybeAccum n accum (optionMaybe p)
upTo :: Stream s m t => Int -> ParsecT s u m a -> ParsecT s u m [a]
upTo n = upToAccum n []
