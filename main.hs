import UI.HSCurses.Curses
import Control.Monad (forever)
import Reactive.Banana
import Reactive.Banana.Frameworks
import System.IO (BufferMode(..), hSetEcho, hSetBuffering, stdin)

import UI.HSCurses.Curses
import UI.HSCurses.Widgets
import UI.HSCurses.CursesHelper


data Position = Position { x :: Int, y :: Int } deriving Show

data Styles = Styles { actorStyle :: CursesStyle, wallStyle :: CursesStyle }


-- a record of all the styles used in the game
gameStyle :: IO Styles
gameStyle =
  let a = Style WhiteF BlackB
      w = Style WhiteF PurpleB
  in
  do [a', w'] <- convertStyles [a, w]
     return $ Styles { actorStyle = a', wallStyle = w' }

-- this will get more complicated
getDelta :: Char -> Position -> Position
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
  [style] <- convertStyles [(Style CyanF BlackB)]
  setStyle style
  let xpos = x p
      ypos = y p
  move xpos ypos
  wAddStr scr ("  ")
  move xpos (ypos + 1)
  wAddStr scr ("  ")
  refresh


makeNetworkDescription :: Frameworks t => AddHandler Char -> Window -> Moment t ()
makeNetworkDescription keyEvent scr = do
  echar <- fromAddHandler keyEvent -- Event t Char
  let bchar = stepper 'a' echar -- Behaviour t Char
      edelta = getDelta <$> echar -- Event t (Position -> Position)
      bposition = accumB (Position {x = 0, y = 0}) edelta -- Behaviour t Position
  eposition <- changes bposition -- Event t (Future Position) 
  reactimate' $ (fmap (drawState scr) <$> eposition)


main :: IO ()
main = do
    -- do curses set up
    scr <- initScr
    initCurses
    erase
    refresh

    -- get a handler
    (addKeyEvent, fireKey) <- newAddHandler

    -- compile and actuate network
    network <- compile (makeNetworkDescription addKeyEvent scr)
    actuate network

    fireKey 'a'

    -- feed new characters into the key handler forever
    forever (getChar >>= fireKey)
