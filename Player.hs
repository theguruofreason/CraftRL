{-# LANGUAGE TemplateHaskell #-}
module Player where
import           Control.Lens
import           Data.List    (intersperse, (\\))
import qualified Data.Map     as Map
import           Item

data Skill = Weaponsmith | Armorsmith | Tailor | MaterialEfficiency
  deriving (Show, Read, Eq, Ord)

data Player = Player { _inventory :: [ItemSlot]
                     , _skills    :: Map.Map Skill Float
                     }
  deriving (Show, Read, Eq)

$(makeLenses ''Player)


-- Modify Inventory --

addItem :: ItemSlot -> Player -> Player
addItem itemslot player = case findItem player p of
                            Just (i,_) -> player & inventory %~ over (ix i) stackItems
                            Nothing    -> checkIfLetterAvailable
    where
      stackItems i = i & stackSize +~ itemslot^.stackSize
      availLetters = (['a'..'z'] ++ ['A'..'Z']) \\ (player & toListOf (inventory.traverse.itemLetter))
      p _ i = i^.item == itemslot^.item
      checkIfLetterAvailable = case (itemslot^.itemLetter) `elem` availLetters of
                                 True  -> player & inventory %~ cons itemslot
                                 False -> player & inventory %~ cons (itemslot & itemLetter .~ head availLetters)

removeItem ::  ItemSlot -> Player -> Player
removeItem itemslot player = case findItem player p of
                           Just (i,_) -> player & inventory %~ over (ix i) deleteItem
                           Nothing    -> player
    where
      deleteItem i = i & stackSize -~ itemslot^.stackSize
      p _ i = i^.item      == itemslot^.item
           && i^.stackSize >= itemslot^.stackSize

findItem :: Player -> (Int -> ItemSlot -> Bool) -> Maybe (Int, ItemSlot)
findItem player p = player^.inventory & ifind p

haveItem :: Player -> ItemSlot -> Bool
haveItem player itemslot = case findItem player p of
                         Just _ -> True
                         Nothing -> False
    where
      p _ i = i^.item      == itemslot^.item
           && i^.stackSize >= itemslot^.stackSize


-- Display Inventory --

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


-- Test Players --

joe = Player [ItemSlot 4 'a' someiron] (Map.fromList [(Weaponsmith, 4),(Armorsmith, 2),(Tailor,1),(MaterialEfficiency,6)])
