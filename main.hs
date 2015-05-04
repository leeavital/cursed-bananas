import UI.HSCurses.Curses
import Control.Monad (forever)
import Reactive.Banana
import Reactive.Banana.Frameworks
import System.IO (BufferMode(..), hSetEcho, hSetBuffering, stdin)

import UI.HSCurses.Curses
import UI.HSCurses.Widgets
import UI.HSCurses.CursesHelper


data Position = Position { x :: Int, y :: Int } deriving Show

data GameStyle = GameStyle { actorStyle :: CursesStyle, wallStyle :: CursesStyle }


-- a record of all the styles used in the game
gameStyle :: IO GameStyle
gameStyle =
  let a = Style WhiteF BlackB
      w = Style WhiteF PurpleB
  in
  do [a', w'] <- convertStyles [a, w]
     return $ GameStyle { actorStyle = a', wallStyle = w' }

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


mkBox :: Window -> CursesStyle -> Int -> Int -> IO ()
mkBox win sty x y  =
  let row = concat [" " | _ <- [1..x]]
      write 0 = return ()
      write y = do
        wAddStr win row
        (ypos, xpos) <- getYX win
        move  (ypos + 1) (xpos - x)
        write (y - 1)
  in do
    setStyle sty
    write y



drawState :: Window -> GameStyle -> Position -> IO ()
drawState scr sty p =  do
  erase
  -- setStyle (actorStyle  sty)
  let xpos = x p
      ypos = y p
  move xpos ypos
  mkBox scr (actorStyle sty) 4 2
  -- wAddStr scr ("  ")
  -- move xpos (ypos + 1)
  -- wAddStr scr ("  ")
  refresh


makeNetworkDescription :: Frameworks t => AddHandler Char -> (Position -> IO ()) -> Moment t ()
makeNetworkDescription keyEvent draw = do
  echar <- fromAddHandler keyEvent -- Event t Char
  let bchar = stepper 'a' echar -- Behaviour t Char
      edelta = getDelta <$> echar -- Event t (Position -> Position)
      bposition = accumB (Position {x = 0, y = 0}) edelta -- Behaviour t Position
  eposition <- changes bposition -- Event t (Future Position)
  reactimate' $ (fmap (draw) <$> eposition)


main :: IO ()
main = do
    -- do curses set up
    scr <- initScr
    initCurses
    erase
    refresh
    styles <- gameStyle

    let
      draw :: Position -> IO ()
      draw = drawState scr styles

    -- get a handler
    (addKeyEvent, fireKey) <- newAddHandler

    -- compile and actuate network
    network <- compile (makeNetworkDescription addKeyEvent draw)
    actuate network

    fireKey 'a'

    -- feed new characters into the key handler forever
    forever (getChar >>= fireKey)
