{-# LANGUAGE TemplateHaskell #-}
module Player where
import           Control.Lens
import           Data.List    (intersperse)
import qualified Data.Map     as Map
import           Item

data Skill = Weaponsmith | Armorsmith | Tailor | MaterialEfficiency
  deriving (Show, Read, Eq, Ord)

data Player = Player { _inventory :: [ItemSlot]
                     , _skills    :: Map.Map Skill Float
                     }
  deriving (Show, Read, Eq)

$(makeLenses ''Player)

collectDisplayCat :: Player -> InvType -> [ItemSlot]
collectDisplayCat thePlayer cat = thePlayer^.inventory^..folded.filtered predicate
    where
      predicate i = i^.item^.displayCat == cat

showDisplayCat :: Player -> InvType -> String
showDisplayCat thePlayer cat = concat $ intersperse "\n" $ map show $ collectDisplayCat thePlayer cat

showMainInventory :: Player -> String
showMainInventory thePlayer = concat $ map (showDisplayCat thePlayer) invTypes
  where
    invTypes = [Weapon,Armor,Tool,CraftMat]


joe = Player [ItemSlot 4 'a' someiron] (Map.fromList [(Weaponsmith, 4),(Armorsmith, 2),(Tailor,1),(MaterialEfficiency,6)])
