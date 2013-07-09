{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Item where
import           GHC.Generics (Generic)
import           Control.Lens
import           Data.List ((\\))
import           Data.Vector (Vector,empty)

qualityAdj :: [String]
qualityAdj = ["", "miserable ", "poor ", "sub-par ", "average ", "good ", "great ", "excellent ", "legendary "]

data Category = Material | Weapon | Armor | Tool
  deriving (Show, Read, Eq, Ord)

data ItemPropKey = Name | Quality | Weight | ValuePer | Category | Letter
  deriving (Show, Read, Eq, Ord, Generic)

data PropVal = Cat Category | Scalar Int | Nominal String

data Item = Item { _name         :: String
                 , _quality      :: Int
                 , _itemLetter   :: Char
                 , _weight       :: Int
                 , _category     :: Category
                 , _stackSize    :: Int
                 , _valuePer     :: Int
                 }
            deriving (Read, Eq)

instance Show Item where
  show (Item n q l w _ s _) = l : " - " ++ qual ++ n ++ wt ++ show s
                              where
                                qual = qualityAdj !! q
                                wt = " (" ++ show w ++ ") "

anaxe = [(Category, Cat Tool),(ValuePer, Scalar 10),(Weight, Scalar 4),(Quality, Scalar 2),(Name, Nominal "axe")]


type Inventory = Vector Item

data Player = Player { _inventory :: Inventory }
  deriving (Show, Read, Eq)

data Game = Game { _player :: Player }

joe = Player empty
theaxe = Item "axe" 4 'a' 3 Tool 1 10
asword = Item "sword" 6 'b' 4 Weapon 1 10
someiron = Item "bar of iron" 0 'c' 2 Material 3 5

$(makeLenses ''Item)
$(makeLenses ''Player)

addItem :: Player -> Item -> Player
addItem player item = case (player^.inventory & ifind p) of
                        Just (i,_) -> Player $ player^.inventory & ix i %~ stackItems
                        Nothing    -> Player $ item <| player^.inventory
    where
      stackItems i = i & stackSize +~ item^.stackSize
      -- availLetter = head $ (['a'..'z'] ++ ['A'..'Z']) \\ (map _itemLetter (_inventory player))
      p _ i = i^.name    == item^.name
           && i^.quality == item^.quality

{-removeItem player item
  | haveItem item player =
  | otherwise = "Item not found in inventory!"-}