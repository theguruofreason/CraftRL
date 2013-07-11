{-# LANGUAGE TemplateHaskell #-}
module Item where
import           Control.Lens
import           Data.List    ((\\))
import           Data.Vector  (Vector, empty)

qualityAdj :: [String]
qualityAdj = ["", "miserable ", "poor ", "sub-par ", "average ", "good ", "great ", "excellent ", "legendary "]

newtype Category = Category { unCategory :: String }
    deriving (Read,Eq)

newtype Name = Name { unName :: String }
    deriving (Read,Eq)

data Item = Composite { _name       :: Name
                      , _quality    :: Int
                      , _weight     :: Int
                      , _category   :: Category
                      , _valuePer   :: Int
                      , _components :: [Item]
                      }
          | Primitive { _name     :: Name
                      }
            deriving (Read, Eq)

instance Show Item where
  show (Composite n q w _ _ _) = qual ++ unName n ++ wt
                              where
                                qual = qualityAdj !! q
                                wt = " (" ++ show w ++ ") "
  show (Primitive n) = unName n

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
                     , _ingredients :: [ItemSlot]
                     , _toolsreq    :: [ItemSlot]
                     , _skillreq    :: [(Int,Skill)]
                     }

-- Some quick junk stuff to test with --
joe = Player empty 0 0 0 0
theaxe = Composite (Name "axe") 4 3 (Category "tool") 10 []
asword = Composite (Name "sword") 6 4 (Category "weapon") 10 []
someiron = Primitive (Name "iron")

recAxe ing1 ing2 = Recipe { _produced    = [ItemSlot 1 'a' theaxe]
                          , _ingredients = [ItemSlot 2 'b' someiron]
                          , _toolsreq    = []
                          , _skillreq    = []
                          }

$(makeLenses ''Item)
$(makeLenses ''ItemSlot)
$(makeLenses ''Player)
$(makeLenses ''Recipe)

addItem :: ItemSlot -> Player -> Player
addItem itemslot player = case findItem player p of
                            Just (i,_) -> player & inventory %~ over (ix i) stackItems
                            Nothing    -> player & inventory %~ cons (itemslot & itemLetter .~ availLetter)
    where
      stackItems i = i & stackSize +~ itemslot^.stackSize
      availLetter = head $ (['a'..'z'] ++ ['A'..'Z']) \\ (player & toListOf (inventory.traverse.itemLetter))
      p _ i = i^.item == itemslot^.item

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
                         Just (_,_) -> True
                         Nothing -> False
    where
      p _ i = i^.item      == itemslot^.item
           && i^.stackSize >= itemslot^.stackSize

takeIngredients player recipe = (recipe^.traverse.ingredients) & foldrOf folded removeItem player 

{-
craft player recipe = (addProducts . takeIngredients)
  where
    takeIngredients = foldMapOf player removeItem (recipe.traverse.ingredients) 
    addProducts = foldMapOf player addItem (recipe.traverse.produced)
-}
