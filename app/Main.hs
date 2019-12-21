{-# LANGUAGE OverloadedStrings #-}
module Main where

-- import           Control.Monad.Trans.Resource
-- import           Data.Graph
-- import           Lib
-- import           Streaming
import           Control.Error.Util
import           Control.Monad.IO.Class
import           Data.ByteString                ( ByteString )
import           Network.HTTP.Req
-- import           Streaming.Osm
import           Streaming.Osm.Types
import           Xeno.DOM
-- import qualified Streaming.Prelude             as S

matchId :: Int -> Way -> Bool
matchId infoId way = case _winfo way of
  Nothing   -> False
  Just info -> _id info == infoId

main :: IO ()
main = runReq defaultHttpConfig $ do
  -- let payload = "(node(id:2661538045,1558201809); <;);out;"
  let payload = "(way(142380557););out;"
  r <- req POST
           (https "overpass-api.de" /: "api" /: "interpreter")
           (ReqBodyBs payload)
           bsResponse
           mempty
  let mayNode = hush . parse $ responseBody r
  case mayNode of
    Just n ->
      liftIO
      -- $ print (filter (\x -> name x == ("node" :: ByteString)) $ children n)
        $ print
        $ head (filter (\x -> name x == ("way" :: ByteString)) $ children n)
    _ -> return ()


data Location = Location {latitude :: Double, longitude :: Double} deriving Show

earthRad :: Double
earthRad = 6371.0 :: Double

toRad :: (Fractional a, Floating a) => a -> a
toRad = (*) (pi / 180)

haversineDistance :: Location -> Location -> Double
haversineDistance a b = earthRad * angle
 where
  angle = 2 * atan2 (sqrt z) (sqrt (1 - z))
  z =
    sin (toRad (latitude a - latitude b) / 2)
      ^ 2
      + cos (toRad $ latitude a)
      * cos (toRad $ latitude b)
      * sin (toRad (longitude a - longitude b) / 2)
      ^ 2
