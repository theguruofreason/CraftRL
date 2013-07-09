{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Item where
import           GHC.Generics (Generic)
import           Control.Lens
import           Data.List ((\\))
import           Data.Vector (Vector,empty,toList)


qualityAdj :: [String]
qualityAdj = ["", "miserable ", "poor ", "sub-par ", "average ", "good ", "great ", "excellent ", "legendary "]

data MaterialMetal = Iron | Copper | Steel | Bronze
  deriving (Show, Read, Eq, Ord)

data MaterialCloth = Cloth | Leather
  deriving (Show, Read, Eq, Ord)

data Category = CraftMaterial | Weapon | Armor | Tool
  deriving (Show, Read, Eq, Ord)

data CraftMaterial = MaterialMetal | MaterialCloth           
  deriving (Show, Read, Eq, Ord)

data Item = Item { _name         :: String
                 , _quality      :: Int
                 , _weight       :: Int
                 , _category     :: Category
                 , _valuePer     :: Int
                 , _material     :: [CraftMaterial]
                 }
            deriving (Read, Eq)

instance Show Item where
  show (Item n q w _ _ _) = qual ++ n ++ wt
                              where
                                qual = qualityAdj !! q
                                wt = " (" ++ show w ++ ") "

data ItemSlot = ItemSlot { _stackSize  :: Int
                         , _itemLetter :: Char
                         , _item       :: Item
                         }
                deriving (Read, Eq)

instance Show ItemSlot where
  show (ItemSlot s l i) = l : " - " ++ show i ++ " x" ++ show s

type Inventory = Vector ItemSlot

data Skill = Weaponsmith | Armorsmith | Tailor | MaterialEfficiency

data Player = Player { _inventory   :: Inventory
                     , _weaponsmith :: Float
                     , _armorsmith  :: Float
                     , _tailor      :: Float
                     , _materialeff :: Float
                     }
  deriving (Show, Read, Eq)

data Recipe = Recipe { _produced    :: [ItemSlot]
                     , _ingredients :: [(Int,CraftMaterial)]
                     , _toolsreq    :: [ItemSlot]
                     , _skillreq    :: [(Int,Skill)]
                     }

-- Some quick junk stuff to test with --
joe = Player empty 0 0 0 0
theaxe = Item "axe" 4 3 Tool 10 []
asword = Item "sword" 6 4 Weapon 10 []
someiron = Item "bar of iron" 0 2 CraftMaterial 5 []

recAxe ing1 ing2 = Recipe { _produced    = [ItemSlot 1 'a' (Item "axe" 4 3 Tool 10 [ing1,ing2])]
                          , _ingredients = [(1,ing1),(1,ing2)]
                          , _toolsreq    = []
                          , _skillreq    = []
                          }

$(makeLenses ''Item)
$(makeLenses ''ItemSlot)
$(makeLenses ''Player)
$(makeLenses ''Recipe)

addItem :: Player -> ItemSlot -> Player
addItem player itemslot = case findItem player p of
                            Just (i,_) -> player & inventory %~ over (ix i) stackItems
                            Nothing    -> player & inventory %~ cons (itemslot & itemLetter .~ availLetter)
    where
      stackItems i = i & stackSize +~ itemslot^.stackSize
      availLetter = head $ (['a'..'z'] ++ ['A'..'Z']) \\ (player & toListOf (inventory.traverse.itemLetter))
      p _ i = i^.item == itemslot^.item

removeItem :: Player -> ItemSlot -> Either String Player
removeItem player itemslot = case findItem player p of
                           Just (i,_) -> Right $ player & inventory %~ over (ix i) deleteItem
                           Nothing    -> Left "Failed to find sufficient item."
    where
      deleteItem i = i & stackSize -~ itemslot^.stackSize
      p _ i = i^.item      == itemslot^.item
           && i^.stackSize >= itemslot^.stackSize

findItem :: Player -> (Int -> ItemSlot -> Bool) -> Maybe (Int, ItemSlot)
findItem player p = player^.inventory & ifind p

haveItem :: Player -> ItemSlot -> Bool
haveItem player itemslot = case findItem player p of
                         Just (_,_) -> True
                         Nothing -> False
    where
      p _ i = i^.item      == itemslot^.item
           && i^.stackSize >= itemslot^.stackSize

craft player recipe = over (recipe.traverse.ingredients) removeItem player
