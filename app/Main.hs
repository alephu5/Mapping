{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

-- import           Control.Error.Util
-- import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Graph.Inductive.Graph     ( LEdge
                                                , LNode
                                                , mkGraph
                                                , size
                                                )
import           Data.Graph.Inductive.PatriciaTree
import           Data.Graph.Inductive.Query.SP
import qualified Data.Map                      as M
import           Data.Maybe
import           Streaming.Osm
import qualified Streaming.Osm.Types           as OSM
import qualified Streaming.Prelude             as S
-- import           Xeno.DOM

matchId :: Int -> OSM.Way -> Bool
matchId infoId way = case OSM._winfo way of
  Nothing   -> False
  Just info -> OSM._id info == infoId

createNode :: (Location, OSM.Info) -> LNode Location
createNode (loc, OSM.Info {..}) = (_id, loc)

type Distance = Double

createEdges :: [Int] -> M.Map Int Location -> [LEdge Double]
createEdges points nodeMap =
  let distance a b = haversineDistance (fromJust $ M.lookup a nodeMap)
                                       (fromJust $ M.lookup b nodeMap)
  in  case points of
        (a : b : rest) -> (a, b, distance a b) : createEdges (b : rest) nodeMap
        _              -> []


main :: IO ()
main = do
  nodeList <-
    runResourceT
    $ S.toList_
    $ S.mapMaybe
        (\case
          (OSM.Node lat lng (Just OSM.Info {..}) _) ->
            Just (_id, Location lat lng)
          (OSM.Node _ _ Nothing _) -> Nothing
        )
    $ nodes
    . blocks
    $ blobs "barcelona.pbf"
  let nodeMap = M.fromList nodeList
  graphWays <-
    runResourceT
    $ S.toList_
    $ S.mapMaybe
        (\case
          (OSM.Way ns (Just OSM.Info {..}) _) -> Just ns
          (OSM.Way _  Nothing              _) -> Nothing
        )
    $ ways
    . blocks
    $ blobs "barcelona.pbf"
  print $ sp
    6900562800
    501829282
    (mkGraph nodeList (concatMap (`createEdges` nodeMap) graphWays) :: Gr
        Location
        Double
    )

data Location = Location {latitude :: Double, longitude :: Double} deriving Show

earthRad :: Double
earthRad = 6371.0 * 1e3 :: Double

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
