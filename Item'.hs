{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Item where
import           Control.Lens
import           Data.List ((\\))
import qualified Data.Map as Map

qualityAdj :: [String]
qualityAdj = ["", "miserable ", "poor ", "sub-par ", "average ", "good ", "great ", "excellent ", "legendary "]

data Category = Category { unCategory :: String }
  deriving (Show, Eq)

data Item = Item { _name         :: String
                 , _quality      :: Int
                 , _weight       :: Int
                 , _category     :: Category
                 , _valuePer     :: Int
                 }
            deriving (Eq)

instance Show Item where
  show (Item n q w _ _) = qual ++ n ++ wt
                              where
                                qual = qualityAdj !! q
                                wt = " (" ++ show w ++ ") "

type Inventory = Map.Map Char (Item,Int)

data Player = Player { _inventory        :: Inventory
                     , _armorcraft       :: Float
                     , _weaponcraft      :: Float
                     , _tailoring        :: Float
                     , _materialEff      :: Float
                     }
  deriving (Show, Eq)

data Game = Game { _player :: Player }

$(makeLenses ''Item)
$(makeLenses ''Player)

addItem :: (Char,(Item,Int)) -> Player -> Player
addItem (l,(i,n)) player = case i `elem` map (fst.snd) (Map.toList $ player^.inventory) of
                         True -> over (player.traverse.inventory) (Map.adjust (\(x,s) -> (x,s+n)) l (player^.inventory)) player
