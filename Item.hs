{-# LANGUAGE TemplateHaskell #-}
module Item where
import           Control.Lens
import           Data.List     ((\\),intersperse)
import qualified Data.Map      as Map
import           System.Random

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

data Skill = Weaponsmith | Armorsmith | Tailor | MaterialEfficiency
  deriving (Show, Read, Eq, Ord)

data Player = Player { _inventory :: [ItemSlot]
                     , _skills    :: Map.Map Skill Float
                     }
  deriving (Show, Read, Eq)

data Recipe = Recipe { _produced :: [ItemSlot]
                     , _required :: Player
                     }

data Ingredient = DiscreteItem ItemSlot | AbstractItem Int Category

$(makeLenses ''Item)
$(makeLenses ''ItemSlot)
$(makeLenses ''Player)
$(makeLenses ''Recipe)

-- Some quick junk stuff to test with --
joe = addItem (ItemSlot 4 'a' someiron) $ Player [] (Map.fromList [(Weaponsmith, 4),(Armorsmith, 2),(Tailor,1),(MaterialEfficiency,6)])
theaxe = Composite (Name "axe") Tool 4 3 (Category "tool") 10 []
asword = Composite (Name "sword") Weapon 6 4 (Category "weapon") 10 []
someiron = Composite (Name "bar of iron") CraftMat 0 3 (Category "metalbar") 5 []

recAxe = Recipe { _produced    = [ItemSlot 1 'a' theaxe]
                , _required    = Player [] (Map.fromList [(Weaponsmith, 3),(Armorsmith, 1)])
                }

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

craft :: Player -> [ItemSlot] -> [ItemSlot] -> Player
craft player takeThese giveThese = (takeIngredients player takeThese) `addProducts` giveThese
  where
    takeIngredients = foldr removeItem
    addProducts = foldr addItem

skillChance :: Recipe -> Skill -> Player -> Float
skillChance theRecipe recSkill thePlayer = chanceCalc playerSkillVal reqSkillVal
  where
    chanceCalc p r = ( 1 + ( p - r )  / ( 0.25 + abs ( p - r ) ) ) / 2
    reqSkillVal = (theRecipe^.required.skills)  Map.! recSkill
    playerSkillVal = (thePlayer^.skills) Map.! recSkill

netSkillCalc :: Recipe -> Player -> Float
netSkillCalc theRecipe thePlayer =
    (sum $ map (\s ->
                    (skillChance theRecipe s thePlayer)
                    *
                    (skillWeight s))
     reqSkills)
    / skillWeightsSum
  where
    reqSkills = map fst $ reqSkillsList
    skillWeight skill = (theRecipe^.required.skills) Map.! skill
    reqSkillsList = Map.toList (theRecipe^.required.skills)
    skillWeightsSum = sum (map snd reqSkillsList)

skillGain :: Player -> Recipe -> Player
skillGain thePlayer theRecipe = foldr (\s ->
                                           skills %~ (Map.adjust (+ gain s) s)
                                      ) thePlayer reqSkills
  where
    gain skill = ((1 - skillChance theRecipe skill thePlayer) * (theRecipe^.required.skills) Map.! skill) / 2
    reqSkills = map fst $ Map.toList (theRecipe^.required.skills)

collectDisplayCat :: InvType -> Player -> [ItemSlot]
collectDisplayCat cat thePlayer = thePlayer^.inventory^..folded.filtered predicate
    where
      predicate i = i^.item^.displayCat == cat
