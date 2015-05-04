import UI.HSCurses.Curses
import Control.Monad (forever)
import Reactive.Banana
import Reactive.Banana.Frameworks
import System.IO (BufferMode(..), hSetEcho, hSetBuffering, stdin)

import UI.HSCurses.Curses


data Position = Position { x :: Int, y :: Int } deriving Show

-- this will get more complicated
getDelta c = case c of
  'h' -> incr (-1, 0)
  'j' -> incr (0, -1)
  'k' -> incr (0, 1)
  'l' -> incr (1, 0)
  _   ->  incr (0, 0)
  where
    incr :: (Int, Int) -> Position -> Position
    incr (dx,dy) = (\p -> Position {x = (x p) + dx, y = (y p) + dy})



drawState :: Window -> Position -> IO ()
drawState scr p =  do
  refresh
  move 0 0
  wAddStr scr ("current " ++ (show $ x  p) ++ "   "  ++ (show $ y p) )


makeNetworkDescription :: Frameworks t => AddHandler Char -> Window -> Moment t ()
makeNetworkDescription keyEvent scr = do
  echar <- fromAddHandler keyEvent -- Event t Char
  let bchar = stepper 'a' echar -- Behaviour t Char
      edelta = getDelta <$> echar -- Event t (Position -> Position)
      bposition = accumB (Position {x = 0, y = 0}) edelta -- Behaviour t Position
  eposition <- changes bposition -- Event t (Future Position) 
  reactimate' $ (fmap (drawState scr) <$> eposition)
  -- reactimate'  $ (fmap (drawState scr) <$> eCharChanged)


main :: IO ()
main = do
    scr <- initScr
    initCurses
    erase
    refresh
    (addKeyEvent, fireKey) <- newAddHandler
    network <- compile (makeNetworkDescription addKeyEvent scr)
    actuate network
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    forever (getChar >>= fireKey)
