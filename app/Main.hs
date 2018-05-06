{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Exception
import Control.Monad
import Data.Array
import Data.List
import qualified Data.Vector as V
import Data.IORef
import Data.Reflection
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Environment (getArgs)
import System.Random.MWC
import System.IO.Unsafe (unsafePerformIO)

{-
# 1. glossを使ってウィンドウを表示する
# 2. セルの表示
# 3. セルの状態をもたせて、オープンなセルの表示
  glossの座標系はX軸右が正、Y軸上が正なのでそれに合わせる
  index (0,0)が左下に対応
# 4. マウスクリックでオープンにする
# 5. 爆弾と周囲のマスの数値を状態にもたせる
# 6. 周囲の爆弾の数を表す数値を計算する
# 7. オープンなセルに数値を表示する
# 8. 0のマスを開いたときは0でなくなるまで周囲のマスを開ける
# 9. 右クリックでフラッグを立てる
# 10. フラッグを消せるようにする
# 11. 爆弾を開いたらゲームオーバーにする
# 12. ゲームオーバーからゲームを再開できるようにする
# 13. 爆弾の個数を決めておいた値にする
# 14. フラッグを立てたセルはオープンできないようにする && フラッグを立てた位置がオープンされたらフラッグを回収する
# 15. ゲームクリアの処理を行う
# 16. コマンドライン引数でサイズと爆弾の数を変えられるようにする
# 17. ひとまず完成
-}

chooser :: Ix i => Int -> (i,i) -> GenIO -> IO [i]
chooser count bnd gen = do
  ref <- newIORef []
  candRef <- newIORef $ range bnd

  forM_ [0..count-1] $ \i -> do
    candidates <- readIORef candRef
    pick <- (candidates !!) <$> uniformR (0, length candidates - 1) gen
    modifyIORef ref ((:) pick)
    modifyIORef candRef (delete pick)

  readIORef ref

cellSize :: Float
cellSize = 30

boardSize :: Given Setting => Float
boardSize = cellSize * fromIntegral (cellCount given)

margin :: Float
margin = 100

toCellIndex :: Given Setting => (Float,Float) -> (Int,Int)
toCellIndex (x,y) =
  (floor $ (x + boardSize / 2 + cellSize / 2) / cellSize, floor $ (y + boardSize / 2 + cellSize / 2) / cellSize)

data CellState = Bomb | Neighbors Int
  deriving (Eq, Show)

data GameScene = Initialize | InGame | Clear | GameOver

data GameState
  = GameState
  { openCells :: Array (Int,Int) Bool
  , bombCells :: Array (Int,Int) CellState
  , flags :: [(Int,Int)]
  , bombs :: [(Int,Int)]
  , scene :: GameScene
  , gen :: GenIO
  }

genBombs :: Given Setting => GenIO -> IO [(Int,Int)]
genBombs gen = chooser (bombCount given) ((0,0), (cellCount given - 1, cellCount given - 1)) gen

calcNeighbors :: Given Setting => [(Int,Int)] -> Array (Int,Int) CellState
calcNeighbors bombCells = foldl' calc arr bombCells
  where
    arr = listArray ((0, 0), (cellCount given - 1, cellCount given - 1)) (repeat (Neighbors 0))
          // [((i,j), Bomb) | (i,j) <- bombCells]
    calc arr (i,j)
      = arr // [((i',j'), (\(Neighbors n) -> Neighbors (n+1)) $ arr ! (i',j'))
               | ti <- [-1,0,1], tj <- [-1,0,1], (ti,tj) /= (0,0),
                 let i' = i + ti, let j' = j + tj,
                 inRange (bounds arr) (i',j'),
                 arr ! (i',j') /= Bomb]

placeBombs :: Given Setting => GameState -> GameState
placeBombs st =
  let bombs = unsafePerformIO (genBombs (gen st)) in
  st { bombCells = calcNeighbors bombs, bombs = bombs }

openNeighbors :: Array (Int,Int) CellState -> (Int,Int) -> Array (Int,Int) Bool -> Array (Int,Int) Bool
openNeighbors cells = \(i,j) arr -> arr // go (i,j) [] where
  bnd = bounds cells

  go (i,j) acc =
    let acc' = ((i,j), True) : acc in
    if cells ! (i,j) == Neighbors 0
    then foldl' (\acc' (i,j) -> if (i,j) `elem` fmap fst acc' then acc' else go (i,j) acc') acc' $
         [(i',j') | ti <- [-1,0,1], tj <- [-1,0,1], (ti,tj) /= (0,0),
           let i' = i + ti, let j' = j + tj,
           inRange bnd (i',j')]
    else acc'

removeFlagsInOpenCells :: GameState -> GameState
removeFlagsInOpenCells st = st { flags = filter (\idx -> not $ openCells st ! idx) $ flags st }

clearCheck :: Given Setting => GameState -> GameState
clearCheck st = if isClear st then st { scene = Clear } else st { scene = InGame } where
  isClear :: GameState -> Bool
  isClear st = length (flags st) == bombCount given && sort (flags st) == sort (bombs st)

newGameState :: Given Setting => GenIO -> GameState
newGameState gen
  = GameState
  { openCells = listArray ((0, 0), (cellCount given - 1, cellCount given - 1)) (repeat False)
  , bombCells = listArray ((0, 0), (cellCount given - 1, cellCount given - 1)) (repeat Bomb)
  , flags = []
  , bombs = []
  , scene = Initialize
  , gen = gen
  }

data Setting
  = Setting
  { cellCount :: Int
  , bombCount :: Int
  }

defSetting :: Setting
defSetting = Setting { cellCount = 10, bombCount = 10 }

-- Arguments: cellCount bombCount
main :: IO ()
main = do
  setting <- flip catch (\(SomeException _) -> return defSetting) $ do
    [cellCount, bombCount] <- map read <$> getArgs
    return $ Setting { cellCount = cellCount, bombCount = bombCount }
  gen <- createSystemRandom

  give setting $ play
    (InWindow "Nice Window" (floor $ boardSize + margin, floor $ boardSize + margin) (10, 10))
    white
    30
    (newGameState gen)
    draw
    handler
    (\_ -> update)

  where
    draw :: Given Setting => GameState -> Picture
    draw st = case scene st of
      InGame ->
        pictures [
          translate (-boardSize / 2) (-boardSize / 2) $ pictures $
            flip fmap [(i,j) | i <- [0..cellCount given-1], j <- [0..cellCount given-1]] $ \(i,j) ->
              color (if openCells st ! (i,j) then orange else dark orange) $
              translate (fromIntegral i * cellSize) (fromIntegral j * cellSize) $
              rectangleSolid (cellSize - 2) (cellSize - 2),
          translate (-boardSize / 2) (-boardSize / 2) $ pictures $
            flip fmap [(i,j) | i <- [0..cellCount given-1], j <- [0..cellCount given-1]] $ \(i,j) ->
              if openCells st ! (i,j)
              then
                color black $
                translate (- 0.4 * cellSize) (- 0.4 * cellSize) $
                translate ((fromIntegral i) * cellSize) ((fromIntegral j) * cellSize) $
                scale 0.2 0.2 $
                text $ case bombCells st ! (i,j) of
                         Bomb -> "*"
                         Neighbors n -> show n
              else blank,
          translate (-boardSize / 2) (-boardSize / 2) $ pictures $
            flip fmap (flags st) $ \(i,j) ->
              color black $
              translate (- 0.4 * cellSize) (- 0.4 * cellSize) $
              translate ((fromIntegral i) * cellSize) ((fromIntegral j) * cellSize) $
              scale 0.2 0.2 $
              text $ "F"
          ]
      GameOver ->
        pictures [
          translate (-boardSize/2) 100 $
            scale 0.3 0.3 $
            text "Game Over",
          translate (-boardSize/2) (-50) $
            scale 0.15 0.15 $
            text "Press any key..."
        ]
      Clear ->
        pictures [
          translate (-boardSize/2) 100 $
            scale 0.3 0.3 $
            text "Game Clear",
          translate (-boardSize/2) (-50) $
            scale 0.15 0.15 $
            text "New game?"
        ]
      _ -> blank

    handler :: Given Setting => Event -> GameState -> GameState
    handler ev st = case scene st of
      InGame -> case ev of
        EventKey (MouseButton LeftButton) Up _ (x,y) ->
          let (i,j) = toCellIndex (x,y) in
            if bombCells st ! (i,j) == Bomb then
              st { scene = GameOver }
            else if inRange (bounds (openCells st)) (i,j) && (i,j) `notElem` flags st then
              clearCheck $ removeFlagsInOpenCells $ st { openCells = openNeighbors (bombCells st) (i,j) $ openCells st }
            else st
        EventKey (MouseButton RightButton) Up _ (x,y) ->
          let (i,j) = toCellIndex (x,y) in
            if inRange (bounds (openCells st)) (i,j) && not (openCells st ! (i,j)) then
              clearCheck $ st { flags = if (i,j) `elem` flags st then delete (i,j) (flags st) else (i,j) : flags st }
            else st
        _ -> st
      GameOver -> case ev of
        EventKey (MouseButton LeftButton) Up _ _ ->
          st { scene = Initialize }
        _ -> st
      Clear -> case ev of
        EventKey (MouseButton LeftButton) Up _ _ ->
          st { scene = Initialize }
        _ -> st
      _ -> st

    update :: Given Setting => GameState -> GameState
    update st = case scene st of
      Initialize -> placeBombs st
        { scene = InGame
        , openCells = listArray ((0, 0), (cellCount given - 1, cellCount given - 1)) (repeat False)
        , flags = []
        }
      _ -> st

