{-# LANGUAGE OverloadedStrings #-}

module QDoc.Helpers where

import           Control.Monad.Catch

import           Data.Text                    (Text)
import qualified Data.Text                    as T

import           Safe                         (readMay)

import           GHC.Stack

myError :: (HasCallStack) => String -> a
myError = error

mapInitLast :: (a -> b) -> (a -> b) -> [a] -> [b]
mapInitLast _ _ []     = []
mapInitLast _ g [x]    = [g x]
mapInitLast f g (x:xs) = f x : mapInitLast f g xs

splitOnCommas :: Text -> [Text]
splitOnCommas = T.split (== ',')

treadM :: (MonadThrow m, Read r, HasCallStack) => Text -> m r
treadM txt = case (txt |> T.unpack |> readMay)
             of Just r  -> pure r
                Nothing -> ["failed to read: ", txt]
                           |> mconcat |> T.unpack |> myError

tshow :: (Show s) => s -> Text
tshow = show .> T.pack

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)
infixl 0 |>

(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) = flip (.)
infixl 9 .>

(<#>) :: (Functor f) => f a -> (a -> b) -> f b
(<#>) = flip (<$>)
infixr 4 <#>
