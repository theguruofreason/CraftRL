{-# LANGUAGE TemplateHaskell #-}
module Interaction where
import           Control.Lens
import           Crafting
import           Item
import           Player

craftInvModify :: Recipe -> Player -> Player
craftInvModify theRecipe thePlayer = (theRecipe^.produced) `addItem` (takeIngredients)
  where
    takeIngredients = foldr removeItem thePlayer $ theRecipe^.required.inventory

modRecPlayerInv :: (ItemSlot -> Player -> Player) -> ItemSlot -> Recipe -> Recipe
modRecPlayerInv f itemslot theRecipe = theRecipe & (required) %~ f itemslot

modRecIngredients :: (Ingredient -> [Ingredient] -> [Ingredient]) -> Ingredient -> Recipe -> Recipe
modRecIngredients f theIngredient theRecipe = theRecipe & (ingredients) %~ (f theIngredient)

