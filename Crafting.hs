{-# LANGUAGE TemplateHaskell #-}
module Crafting where
import           Control.Lens
import qualified Data.Map     as Map
import           Item
import           Player


data Recipe = Recipe { _produced    :: ItemSlot
                     , _required    :: Player
                     , _ingredients :: [Ingredient]
                     }

data Ingredient = DiscreteItem ItemSlot | AbstractItem Int Category

$(makeLenses ''Recipe)


-- Chance Calculations --

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

experimentChance :: Player -> Recipe -> Int -> Float
experimentChance thePlayer theRecipe points = (netSkillCalc theRecipe thePlayer) ^ points


-- Product Modifications --

giveQualityBonus :: Int -> (Recipe -> Recipe)
giveQualityBonus points = produced.item.quality +~ points

giveValueBonus :: Recipe -> Recipe
giveValueBonus theRecipe = theRecipe & valuePers *~ qualities
  where
    qualities = (theRecipe^.produced^.item.quality)
    valuePers = produced.item.valuePer

giveMaterialValueBonus :: Recipe -> Recipe
giveMaterialValueBonus theRecipe = foldr (valuePers +~) theRecipe matValues
  where
    matValues = theRecipe^.required.inventory^..traverse.item.valuePer
    valuePers = produced.item.valuePer


-- Recipes --

recAxe = Recipe { _produced    = ItemSlot 1 'a' theaxe
                , _required    = Player [] (Map.fromList [(Weaponsmith, 3),(Armorsmith, 1)])
                , _ingredients = [AbstractItem 1 (Category "metal"), AbstractItem 1 (Category "wood")]
                }
