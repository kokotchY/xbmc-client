import Network.HTTP
import Network.HTTP.Auth
import Network.URI
import Text.ParserCombinators.Parsec hiding ((<|>),many)
import Control.Applicative
import Numeric

json = "{\"jsonrpc\": \"2.0\", \"method\": \"Player.GetItem\", \"params\": { \"properties\": [\"title\", \"album\", \"artist\", \"duration\", \"thumbnail\", \"file\", \"fanart\", \"streamdetails\"], \"playerid\": 0 }, \"id\": \"AudioGetItem\"}"


pause = "{\"jsonrpc\": \"2.0\", \"method\": \"Player.PlayPause\", \"params\": { \"playerid\": 0 }, \"id\": 1}"

stringUrl = "http://192.168.1.42:8080/jsonrpc"
url = case parseURI stringUrl of
    Just uri -> uri

myRequest :: String -> Request String
myRequest body = Request
    { rqURI = url
    , rqMethod = POST
    , rqHeaders = [ mkHeader HdrContentType "application/json"
                  , mkHeader HdrContentLength $ show $ length body
                  , mkHeader HdrAuthorization $ withAuthority myAuth (postRequest stringUrl)
                  ]
    , rqBody = body
    }

myAuth = AuthBasic
    { auRealm = "XBMC"
    , auUsername = "xbmc"
    , auPassword = "xbmc"
    , auSite = url
    }

bla request = do
    query <- simpleHTTP request
    response <- getResponseBody query
    parseTest p_text $ response

data JValue = JString String
    | JNumber Double
    | JBool Bool
    | JNull
    | JObject (JObj JValue)
    | JArray (JAry JValue)
    deriving (Eq, Ord, Show)

newtype JAry a = JAry {
    fromJAry :: [a]
} deriving (Eq, Ord, Show)

newtype JObj a = JObj {
    fromJObj :: [(String, a)]
} deriving (Eq, Ord, Show)



p_text :: CharParser () JValue
p_text = spaces *> text
    <?> "JSON text"
    where
        text = JObject <$> p_object
           <|> JArray <$> p_array

p_series :: Char -> CharParser () a -> Char -> CharParser () [a]
p_series left parser right =
    between (char left <* spaces) (char right) $
            (parser <* spaces) `sepBy` (char ',' <* spaces)

p_array :: CharParser () (JAry JValue)
p_array = JAry <$> p_series '[' p_value ']'

p_object :: CharParser () (JObj JValue)
p_object = JObj <$> p_series '{' p_field '}'
    where p_field = (,) <$> (p_string <* char ':' <* spaces) <*> p_value

p_value :: CharParser () JValue
p_value = value <* spaces
    where
        value = JString <$> p_string
            <|> JNumber <$> p_number
            <|> JObject <$> p_object
            <|> JArray <$> p_array
            <|> JBool <$> p_bool
            <|> JNull <$ string "null"
            <?> "JSON value"

p_bool :: CharParser () Bool
p_bool = True <$ string "true"
    <|> False <$ string "false"

p_number :: CharParser () Double
p_number = do
    s <- getInput
    case readSigned readFloat s of
        [(n, s')] -> n <$ setInput s'
        _ -> empty

p_string :: CharParser () String
p_string = between (char '\"') (char '\"') (many jchar)
    where
        jchar = char '\\' *> (p_escape <|> p_unicode)
            <|> satisfy (`notElem` "\"\\")

p_escape = choice (zipWith decode "bnfrt\\\"/" "\b\n\f\r\t\\\"/")
    where
        decode c r = r <$ char c

p_unicode :: CharParser () Char
p_unicode = char 'u' *> (decode <$> count 4 hexDigit)
    where
        decode x = toEnum code
            where ((code,_):_) = readHex x
