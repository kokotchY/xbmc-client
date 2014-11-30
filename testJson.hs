{-# LANGUAGE OverloadedStrings #-}
import Data.Aeson
import Data.Text
import Control.Applicative
import Control.Monad

data Person = Person
    { name :: Text
    , age :: Int
    } deriving Show

instance FromJSON Person where
    parseJSON (Object v) = Person <$>
                           v .: "name" <*>
                           v .: "age"
    parseJSON _ = mzero

instance ToJSON Person where
    toJSON (Person name age) = object ["name" .= name, "age" .= age]
