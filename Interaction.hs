{-# LANGUAGE TemplateHaskell #-}
module Interaction where
import           Control.Lens
import           Crafting
import           Data.List    ((\\))
import qualified Data.Map     as Map
import           Item
import           Player

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

craft :: Player -> Recipe -> Player
craft thePlayer theRecipe = (theRecipe^.produced) `addItem` (takeIngredients)
  where
    takeIngredients = foldr removeItem thePlayer $ theRecipe^.required.inventory

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
