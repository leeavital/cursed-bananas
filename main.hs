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

data GameStyle = GameStyle {    actorStyle :: CursesStyle,
                                obstacleStyle :: CursesStyle,
                                bgStyle :: CursesStyle }

main :: IO ()
main = do
    -- do curses set up
    scr <- initScr
    initCurses
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
    -- forever (getChar >>= fireKey)
    loop fireKey where
      loop f = do
        c <- getChar
        f c
        if (c == 'q') then
          endWin
        else
          loop f



-- constants
sizePlayer :: Size
sizePlayer = (2, 4)

sizeObstacle :: Size
sizeObstacle = (1,2)

numObstacles :: Int
numObstacles = 25


initialBoard :: IO Board
initialBoard = do
    -- get some random obstacles
    g <- newStdGen
    let
      xs = [ x `mod` 24 | x <- take numObstacles $ (randoms g :: [Integer])]
      ys = [ y `mod` 24 | y <- take numObstacles $ drop numObstacles $ (randoms g :: [Integer])]
      obstacles' = [ Position {x = fromIntegral x', y = fromIntegral y'} | (x', y') <- zip xs ys]

      start = Position { x = 0, y = 0 }
    return $ Board {player = start, obstacles = obstacles' }


bound :: Size -> (Int -> Int, Int -> Int)
bound s = let boundW = (min $ 24 - (getWidth s)) . (max 0)
              boundH = (min $ 25 - (getHeight s) + 1) . (max 0)
          in (boundW, boundH)


-- a record of all the styles used in the game
gameStyle :: IO GameStyle
gameStyle =
  let a = Style WhiteF BlackB
      o = Style WhiteF DarkGreenB
      b = Style WhiteF WhiteB
  in
  do [a', o', b'] <- convertStyles [a, o, b]
     return $ GameStyle { actorStyle = a', obstacleStyle = o', bgStyle = b'}

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
          p' = Position {x = boundW $ (x p) + dx, y = boundH $ (y p) + dy }
      in  brd { player = p' }

    (boundW, boundH) = bound sizePlayer
    


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
  mkBox scr (bgStyle sty) (Position {x = 0, y = 0}) (25, 25) -- draw background
  mkBox scr (actorStyle sty) p sizePlayer -- draw actor
  mapM_  (\p -> mkBox scr (obstacleStyle sty) p sizeObstacle) os -- draw obstacles
  refresh


makeNetworkDescription :: Frameworks t => AddHandler Char -> Board -> (Board -> IO ()) -> Moment t ()
makeNetworkDescription keyEvent start draw = do
  echar <- fromAddHandler keyEvent -- Event t Char
  let bchar = stepper 'a' echar -- Behaviour t Char
      edelta = getDelta <$> echar -- Event t (Board -> Board)
      bposition = accumB start edelta -- Behaviour t Position
  eposition <- changes bposition -- Event t (Future Position)
  reactimate' $ (fmap (draw) <$> eposition)
