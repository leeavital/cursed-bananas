import UI.HSCurses.Curses
import Control.Monad (forever)
import Reactive.Banana
import Reactive.Banana.Frameworks
import System.IO (BufferMode(..), hSetEcho, hSetBuffering, stdin)

import UI.HSCurses.Curses
import UI.HSCurses.Widgets
import UI.HSCurses.CursesHelper



actorSize :: Size
actorSize = (2,2)

actor = OpaqueWidget actorSize

actorStyle = Style BlackF  BlackB



data Position = Position { x :: Int, y :: Int } deriving Show

-- this will get more complicated
getDelta c = case c of
  'h' -> incr (-1, 0)
  'j' -> incr (0, 1)
  'k' -> incr (0, -1)
  'l' -> incr (1, 0)
  _   ->  incr (0, 0)
  where
    incr :: (Int, Int) -> Position -> Position
    incr (dy,dx) = (\p -> Position {x = (x p) + dx, y = (y p) + dy})


-- isQuit = (== 'q')



drawState :: Window -> Position -> IO ()
drawState scr p =  do
  erase
  let xpos = x p
      ypos = y p
  move xpos ypos
  draw (xpos, ypos) actorSize DHNormal actor
  wAddStr scr ("current " ++ (show $ x  p) ++ "   "  ++ (show $ y p) )
  refresh


makeNetworkDescription :: Frameworks t => AddHandler Char -> Window -> Moment t ()
makeNetworkDescription keyEvent scr = do
  echar <- fromAddHandler keyEvent -- Event t Char
  let bchar = stepper 'a' echar -- Behaviour t Char
      edelta = getDelta <$> echar -- Event t (Position -> Position)
      bposition = accumB (Position {x = 0, y = 0}) edelta -- Behaviour t Position
  eposition <- changes bposition -- Event t (Future Position) 
  reactimate' $ (fmap (drawState scr) <$> eposition)
  -- reactimate $ (\n -> if n then endWin else return () ) <$> equit


main :: IO ()
main = do
    scr <- initScr
    [actorStyleC] <- convertStyles [actorStyle] -- not 100% sure why this is necessary
    initCurses
    erase
    refresh
    (addKeyEvent, fireKey) <- newAddHandler
    network <- compile (makeNetworkDescription addKeyEvent scr)
    actuate network
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    forever (getChar >>= fireKey)
