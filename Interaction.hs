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
