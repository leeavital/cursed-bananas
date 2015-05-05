import UI.HSCurses.Curses
import Control.Monad (forever)
import Reactive.Banana
import Reactive.Banana.Frameworks
import System.IO (BufferMode(..), hSetEcho, hSetBuffering, stdin)

import UI.HSCurses.Curses
import UI.HSCurses.Widgets
import UI.HSCurses.CursesHelper

import System.Random


data Board = Board { player :: Position, obstacles :: [Position] }

data Position = Position { x :: Int, y :: Int } deriving Show

data GameStyle = GameStyle { actorStyle :: CursesStyle, obstacleStyle :: CursesStyle }



initialBoard :: IO Board
initialBoard = do
    -- get some random obstacles
    g <- newStdGen
    let
      xs = [ x `mod` 25 | x <- take 10 $ (randoms g :: [Integer])]
      ys = [ y `mod` 25 | y <- take 10 $ drop 10 $ (randoms g :: [Integer])]
      obstacles' = [ Position {x = fromIntegral x', y = fromIntegral y'} | (x', y') <- zip xs ys]

      start = Position { x = 0, y = 0 }
    return $ Board {player = start, obstacles = obstacles' }


-- a record of all the styles used in the game
gameStyle :: IO GameStyle
gameStyle =
  let a = Style WhiteF BlackB
      o = Style WhiteF DarkGreenB
  in
  do [a', o'] <- convertStyles [a, o]
     return $ GameStyle { actorStyle = a', obstacleStyle = o' }

-- this will get more complicated
getDelta :: Char -> Board -> Board
getDelta c = case c of
  'h' -> incr (-1, 0)
  'j' -> incr (0, 1)
  'k' -> incr (0, -1)
  'l' -> incr (1, 0)
  _   ->  incr (0, 0)
  where
    incr :: (Int, Int) -> Board -> Board 
    incr (dy,dx) brd =
      let p = player brd
          p' = Position {x = (x p) + dx, y = (y p) + dy }
      in  brd { player = p' }


mkBox :: Window -> CursesStyle -> Position -> Size -> IO ()
mkBox win sty p s =
  let w = getWidth s
      h = getHeight s
      row = concat [" " | _ <- [1..w]]
      write 0 = return ()
      write y = do
        wAddStr win row
        (ypos, xpos) <- getYX win
        move  (ypos + 1) (xpos - w)
        write (y - 1)
  in do
    move (x p) (y p) 
    setStyle sty
    write h



drawState :: Window -> GameStyle -> Board -> IO ()
drawState scr sty brd =  do
  erase
  let
    p = player brd
    os = obstacles brd
  mkBox scr (actorStyle sty) p (2, 4)
  mapM_  (\p -> mkBox scr (obstacleStyle sty) p (1,1)) os
  refresh



makeNetworkDescription :: Frameworks t => AddHandler Char -> Board -> (Board -> IO ()) -> Moment t ()
makeNetworkDescription keyEvent start draw = do
  echar <- fromAddHandler keyEvent -- Event t Char
  let bchar = stepper 'a' echar -- Behaviour t Char
      edelta = getDelta <$> echar -- Event t (Board -> Board)
      bposition = accumB start edelta -- Behaviour t Position
  eposition <- changes bposition -- Event t (Future Position)
  reactimate' $ (fmap (draw) <$> eposition)


main :: IO ()
main = do
    -- do curses set up
    scr <- initScr
    initCurses
    resizeTerminal 300 300
    erase
    refresh
    styles <- gameStyle


    let
      draw :: Board -> IO ()
      draw = drawState scr styles

    -- get a handler
    (addKeyEvent, fireKey) <- newAddHandler

    -- start stae
    start <- initialBoard

    -- compile and actuate network
    network <- compile (makeNetworkDescription addKeyEvent start draw)
    actuate network

    fireKey 'a'

    -- feed new characters into the key handler forever
    forever (getChar >>= fireKey)
