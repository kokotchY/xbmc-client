import Network.HTTP
import Network.HTTP.Auth
import Network.URI
import Text.ParserCombinators.Parsec hiding ((<|>),many)
import Control.Applicative
import Numeric
import Data.List (intersperse)
import Control.Arrow ((&&&))

json = "{\"jsonrpc\": \"2.0\", \"method\": \"Player.GetItem\", \"params\": { \"properties\": [\"title\", \"album\", \"artist\", \"duration\", \"thumbnail\", \"file\", \"fanart\", \"streamdetails\"], \"playerid\": 0 }, \"id\": \"AudioGetItem2\"}"

pause = "{\"jsonrpc\": \"2.0\", \"method\": \"Player.PlayPause\", \"params\": { \"playerid\": 0 }, \"id\": 1}"


data MethodName = ActivateWindow
    | Add
    | Back
    | Clean
    | Clear
    | ContextMenu
    | Down
    | Download
    | EjectOpticalDrive
    | ExecuteAction
    | ExecuteAddon
    | Export
    | GetActivePlayers
    | GetAddonDetails
    | GetAddons
    | GetAlbumDetails
    | GetAlbums
    | GetArtistDetails
    | GetArtists
    | GetChannelDetails
    | GetChannelGroupDetails
    | GetChannelGroups
    | GetChannels
    | GetConfiguration
    | GetDirectory
    | GetEpisodeDetails
    | GetEpisodes
    | GetFileDetails
    | GetGenres
    | GetInfoBooleans
    | GetInfoLabels
    | GetItem
    | GetItems
    | GetMovieDetails
    | GetMovieSetDetails
    | GetMovieSets
    | GetMovies
    | GetMusicVideoDetails
    | GetMusicVideos
    | GetPlaylists
    | GetProperties
    | GetRecentlyAddedAlbums
    | GetRecentlyAddedEpisodes
    | GetRecentlyAddedMovies
    | GetRecentlyAddedMusicVideos
    | GetRecentlyAddedSongs
    | GetRecentlyPlayedAlbums
    | GetRecentlyPlayedSongs
    | GetSeasons
    | GetSongDetails
    | GetSongs
    | GetSources
    | GetTVShowDetails
    | GetTVShows
    | GoTo
    | Hibernate
    | Home
    | Info
    | Insert
    | Introspect
    {-| Left-}
    | Move
    | NotifyAll
    | Open
    | Permission
    | Ping
    | PlayPause
    | PrepareDownload
    | Quit
    | Reboot
    | Record
    | Remove
    | RemoveEpisode
    | RemoveMovie
    | RemoveMusicVideo
    | RemoveTVShow
    {-| Right-}
    | Rotate
    | Scan
    | Seek
    | Select
    | SendText
    | SetAddonEnabled
    | SetAlbumDetails
    | SetArtistDetails
    | SetAudioStream
    | SetConfiguration
    | SetEpisodeDetails
    | SetFullscreen
    | SetMovieDetails
    | SetMusicVideoDetails
    | SetMute
    | SetPartymode
    | SetRepeat
    | SetShuffle
    | SetSongDetails
    | SetSpeed
    | SetSubtitle
    | SetTVShowDetails
    | SetVolume
    | ShowCodec
    | ShowNotification
    | ShowOSD
    | Shutdown
    | Stop
    | Suspend
    | Swap
    | Up
    | Version
    | Zoom
    deriving (Show, Eq, Enum, Bounded)
data MethodGroup = Addons
    | Application
    | AudioLibrary
    | Files
    | GUI
    | Input
    | JSONRPC
    | PVR
    | Player
    | Playlist
    | System
    | VideoLibrary
    | XBMC
    deriving (Show, Eq, Enum, Bounded)

allEnumWithString :: (Enum a, Bounded a, Show a) => [(a, String)]
allEnumWithString = map (id &&& show) $ [minBound..maxBound]

createCall :: MethodGroup -> MethodName -> [(String, JValue)] -> JValue
createCall methodGroup methodName params = JObject (JObj
    [ ("jsonrpc", JString "2.0")
    , ("method", JString method)
    , ("params", JObject (JObj params))
    , ("id", JNumber 1)
    ])
    where
        methodGroupString = case lookup methodGroup allEnumWithString of
            Just name -> name
            Nothing -> "Unkown"
        methodNameString = case lookup methodName allEnumWithString of
            Just name -> name
            Nothing -> "Unkown"
        method = methodGroupString ++ "." ++ methodNameString

createCallNoParams :: MethodGroup -> MethodName -> JValue
createCallNoParams group name = createCall group name []

pause2 = JObject (JObj
    [ ("jsonrpc", JString "2.0")
    , ("method", JString "Player.PlayPause")
    , ("params", JObject (JObj
        [ ("playerid", JInteger 0)
        ]))
    , ("id", JNumber 1)
    ])

createKeyValue :: String -> String -> (String, JValue)
createKeyValue key value = (key, JString value)

createKeyArray :: String -> [String] -> (String, JValue)
createKeyArray key value = (key, JArray (JAry $ map JString value))

createKeyIntValue :: String -> Double -> (String, JValue)
createKeyIntValue key value = (key, JInteger value)

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

queryXbmc :: JValue -> IO (Either String JValue)
queryXbmc json = do
    q <- simpleHTTP $ myRequest $ convertJsonToString json
    response <- getResponseBody q
    case parse p_text "FromXBMC" response of
        Left error -> return $ Left $ "Error when trying to parse response: " ++ show error
        Right parsedResponse -> return $ Right parsedResponse

getResult :: JValue -> JValue
getResult bla@(JObject obj) = case lookup "result" (fromJObj obj) of
    Nothing -> case lookup "error" (fromJObj obj) of
        Nothing -> JString "No result and no error"
        Just (JObject error) -> case lookup "message" (fromJObj error) of
            Nothing -> JString "Error without error message"
            Just message -> message
    Just obj2 -> obj2

getRight :: Either a b -> b
getRight (Right r) = r

playPauseResult :: Double -> IO JValue
playPauseResult playerId = do
    response <- queryXbmc $ createCall Player PlayPause [createKeyIntValue "playerid" playerId]
    return $ case response of
        Right res -> getResult res
        Left string -> JString string

nextSong :: IO (Either String JValue)
nextSong =
    queryXbmc $ createCall Player GoTo [createKeyIntValue "playerid" 0, createKeyValue "to" "next"]


getVolume :: IO (Maybe Double)
getVolume = do
    response <- queryXbmc $ createCall Application GetProperties [createKeyArray "properties" ["volume"]]
    return $ case response of
        Left string -> Nothing
        Right (JObject res) -> case lookup "result" (fromJObj res) of
            Nothing -> Nothing
            Just (JObject res) -> case lookup "volume" (fromJObj res) of
                Nothing -> Nothing
                Just (JInteger nb) -> Just nb
                Just (JNumber nb) -> Just nb
            Just res -> Nothing
        Right _ -> Nothing

setVolume :: Double -> IO (Either String JValue)
setVolume volume =
    queryXbmc $ createCall Application SetVolume [createKeyIntValue "volume" $ normalizeVolume volume]
    where
        normalizeVolume volume
            | volume >= 100 = 100
            | volume <= 0 = 0
            | otherwise = volume

volumeUp :: IO (Either String JValue)
volumeUp = changeVolumeBy (+5)

volumeDown :: IO (Either String JValue)
volumeDown = changeVolumeBy (\x -> x - 5)

getNewVolume :: (Double -> Double) -> Double -> Double
getNewVolume func current = func current


changeVolumeBy :: (Double -> Double) -> IO (Either String JValue)
changeVolumeBy func = do
    volume <- getVolume
    case volume of
        Nothing -> return $ Left "No volume detected"
        Just volume -> setVolume $ func volume

queryAndParse request = do
    query <- simpleHTTP request
    response <- getResponseBody query
    parseTest p_text $ response

data JValue = JString String
    | JNumber Double
    | JInteger Double
    | JBool Bool
    | JNull
    | JObject (JObj JValue)
    | JArray (JAry JValue)
    deriving (Eq, Ord, Show)

convertJsonToString :: JValue -> String
convertJsonToString (JString string) = "\"" ++ string ++ "\""
convertJsonToString (JNumber number) = show number
convertJsonToString (JInteger number) = show $ truncate number
convertJsonToString (JBool True) = "true"
convertJsonToString (JBool False) = "false"
convertJsonToString JNull = "null"
convertJsonToString (JObject bla) = "{" ++convertJsonObjToString bla ++ "}"
convertJsonToString (JArray array) = "[" ++ convertJsonAryToString array ++ "]"

convertJsonObjToString :: JObj JValue -> String
convertJsonObjToString bla = concat $ intersperse "," $ map (\(x,y) -> "\"" ++ x ++ "\": " ++ convertJsonToString y) elements
    where
        elements = fromJObj bla

convertJsonAryToString :: JAry JValue -> String
convertJsonAryToString array = concat $ intersperse "," $ map convertJsonToString elements
    where
        elements = fromJAry array


obj = JObject (JObj {fromJObj = [("id",JString "AudioGetItem2"),("jsonrpc", JString "2.0")]})

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
            <|> JInteger <$> p_number
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
