{-# LANGUAGE TemplateHaskell #-}
module Crafting where
import           Control.Lens
import qualified Data.Map     as Map
import           Item
import           Player


data Recipe = Recipe { _produced     :: ItemSlot
                     , _required     :: Player
                     }

data Ingredient = DiscreteItem ItemSlot | AbstractItem Int Category

$(makeLenses ''Recipe)

recAxe = Recipe { _produced    = ItemSlot 1 'a' theaxe
                , _required    = Player [ItemSlot 1 'a' someiron] (Map.fromList [(Weaponsmith, 3),(Armorsmith, 1)])
                }
