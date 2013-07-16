{-# LANGUAGE TemplateHaskell #-}
module Item where
import           Control.Lens
import           Data.List     ((\\),intersperse)
import qualified Data.Map      as Map

qualityAdj :: [String]
qualityAdj = ["", "miserable ", "poor ", "sub-par ", "average ", "good ", "great ", "excellent ", "legendary "]

newtype Category = Category { unCategory :: String }
    deriving (Read,Eq)

data InvType = Weapon | Armor | Tool | CraftMat
  deriving (Read, Eq, Ord)

instance Show InvType where
  show Armor = "Armor"
  show Weapon = "Weapons"
  show Tool = "Tools"
  show CraftMat = "Raw Materials"

newtype Name = Name { unName :: String }
    deriving (Read,Eq)

data Item = Composite { _name       :: Name
                      , _displayCat :: InvType
                      , _quality    :: Int
                      , _weight     :: Int
                      , _category   :: Category
                      , _valuePer   :: Int
                      , _components :: [Item]
                      }
            deriving (Read, Eq)

instance Show Item where
  show (Composite n _ q w _ _ _ ) = qual ++ unName n ++ wt
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

$(makeLenses ''Item)
$(makeLenses ''ItemSlot)

-- Some quick junk stuff to test with --
theaxe = Composite (Name "axe") Tool 4 3 (Category "tool") 10 []
asword = Composite (Name "sword") Weapon 6 4 (Category "weapon") 5 []
someiron = Composite (Name "bar of iron") CraftMat 0 3 (Category "metalbar") 5 []

