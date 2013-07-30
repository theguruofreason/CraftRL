{-# LANGUAGE TemplateHaskell #-}
module Main where
import           Control.Applicative
import           Control.Lens
import qualified Data.Map            as Map
import           FRP.Helm
import qualified FRP.Helm.Automaton  as Auto
import           FRP.Helm.Keyboard
import           FRP.Helm.Text
import           FRP.Helm.Time
import qualified FRP.Helm.Window     as Window
import qualified Graphics.UI.SDL     as SDL
import           Item
import           Player
import           System.Directory
import           System.FilePath

type Display = [Element]

type World = Map.Map (Int,Int) Tile

data Tile = Tile { _inv      :: [ItemSlot]
                 , _sprite   :: Element
                 , _creature :: Maybe Player
                 }

data GameState = GameState { _thePlayer :: Player
                           , _theWorld  :: World
                           }

$(makeLenses ''Tile)
$(makeLenses ''GameState)

initialGameState :: FilePath -> GameState
initialGameState dir = GameState (Player [] (Map.fromList [])) (testWorld dir)

render :: Element -> (Int, Int) -> Element
render screen (w, h) = collage w h [(toForm screen)]

main :: IO ()
main = do
  SDL.setCaption "CraftRL" "CraftRL"
  currentDir <- getCurrentDirectory
  run $ fmap (fmap $ render $ createMap $ testWorld currentDir) Window.dimensions

featSS :: FilePath -> FilePath
featSS dir = dir </> "resources" </> "img" </> "feat.png"
treeTile, doorTile :: FilePath -> Tile
treeTile dir = Tile [] (croppedImage (0,0) 30 32 (featSS dir)) Nothing
doorTile dir = Tile [] (croppedImage (62,32) 32 32 (featSS dir)) Nothing

testWorld :: FilePath -> World
testWorld dir = Map.fromList [((0,0), treeTile dir),((1,0), doorTile dir)]

createMap :: World -> Element
createMap world = collage (32 * 40) (32 * 30) (map getTileSprite (Map.toList world))
  where
    getTileSprite ((x,y),tile) = move (fromIntegral(32 * x), fromIntegral(32 * y)) (toForm $ tile^.sprite)
