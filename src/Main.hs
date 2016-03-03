{-# LANGUAGE OverloadedStrings #-}
module Main where

import SDL 
import Linear
import Linear.Affine
import qualified Data.Sequence as S
import Control.Monad (unless, forM_)
import System.Random (randomIO)
import Data.Maybe (fromJust, isJust)

backgroundColor, borderColor, cellColor :: (Num a) => V4 a
backgroundColor = V4 75  75  75  0
borderColor     = V4 0   0   0   0
cellColor       = V4 255 255 0   0 -- brigthest color off a cell.

windowDim, gridSize, cellSize, gridArea, genDelay :: (Num a, Integral a) => a
windowDim = 1000
gridSize  = 500
cellSize  = windowDim `div` gridSize
gridArea  = gridSize * gridSize
genDelay  = 150 -- ms

type Generation = S.Seq Bool
type NeighbourMap = S.Seq [Int]

main :: IO ()
main = do
  initializeAll
  win <- createWindow "Game of life" $ defaultWindow {windowInitialSize = pure windowDim}
  ren <- createRenderer win (-1)  $ defaultRenderer {rendererType = AcceleratedVSyncRenderer}
  let nm = makeNeighbourMap
  initGen <- S.replicateM gridArea randomIO
  mainLoop ren initGen nm
  destroyRenderer ren
  destroyWindow win

mainLoop :: Renderer -> Generation -> NeighbourMap -> IO ()
mainLoop ren gen nm = do
  events <- pollEvents
  unless (shouldClose events) $ do
    draw gen ren nm
    let newGen = nextGeneration gen nm
    delay genDelay
    mainLoop ren newGen nm
  where shouldClose e = QuitEvent `elem` map eventPayload e

draw :: Generation -> Renderer -> NeighbourMap -> IO ()
draw gen ren nm = do
  rendererDrawColor ren $= backgroundColor
  clear ren
  forM_ (rectangles gen) $ uncurry (drawCell ren gen nm)
  present ren
  where rectangles = fmap fromJust . S.filter isJust . S.mapWithIndex cellToMaybeRectangle

drawCell :: Renderer -> Generation -> NeighbourMap -> Int -> Rectangle Int -> IO ()
drawCell ren gen nm i rect = do
  let n = realToFrac $ aliveNeighbours gen nm i :: Double
  rendererDrawColor ren $= round <$> (cellColor ^/ 8) ^* n
  fillRect ren rect'
  rendererDrawColor ren $= borderColor
  drawRect ren rect'
  where rect' = Just (fromIntegral <$> rect)
        
cellToMaybeRectangle :: Int -> Bool -> Maybe (Int, Rectangle Int)
cellToMaybeRectangle _ False = Nothing
cellToMaybeRectangle i _     = Just (i, Rectangle (P (cellSize *^ p)) (pure cellSize))
  where p = uncurry V2 $ fromIndex i

makeNeighbourMap :: NeighbourMap
makeNeighbourMap = S.fromList $ map n [0..gridArea -1]
  where n i = [toIndex (x,y) | x <- [ix-1, ix, ix+1]
                             , y <- [iy-1, iy, iy+1]
                             , inRange x
                             , inRange y
                             , (x,y) /= (ix,iy)]
          where (ix, iy) = fromIndex i
        inRange k = (k >= 0) && (k < gridSize)
        
nextGeneration :: Generation -> NeighbourMap -> Generation
nextGeneration gen nm = S.mapWithIndex (aliveNext gen nm) gen

aliveNeighbours :: Generation -> NeighbourMap -> Int -> Int
aliveNeighbours gen nm i = length . filter id $ map (S.index gen) (S.index nm i)

aliveNext :: Generation -> NeighbourMap -> Int -> Bool -> Bool
aliveNext gen nm pos curr = case aliveNeighbours gen nm pos of
  2 -> curr
  3 -> True
  _ -> False

toIndex :: (Int, Int) -> Int
toIndex (x,y) = y + (gridSize*x)

fromIndex :: Int -> (Int, Int)
fromIndex i = divMod i gridSize
