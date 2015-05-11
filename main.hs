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

data Position = Position { x :: Int, y :: Int } deriving Eq

data GameStyle = GameStyle {    actorStyle :: CursesStyle,
                                obstacleStyle :: CursesStyle,
                                bgStyle :: CursesStyle,
                                text :: CursesStyle }

main :: IO ()
main = do
    -- set up curses
    scr <- initScr
    initCurses
    erase
    refresh
    styles <- gameStyle


    -- capture some parameters in the draw function
    let
      draw :: (Board, Bool) -> IO ()
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
sizePlayer = (2, 4)

sizeObstacle = (1,2)

numObstacles = 25

origin = Position {x = 0, y = 0 }
winPosition = Position {x = 21, y = 23}

collision :: (Position, Size) -> (Position, Size) -> Bool
collision (p1, s1) (p2, s2) =
  let
    x1 = x p1
    w1 = getWidth s1
    y1 = y p1
    h1 = getHeight s1

    x2 = x p2
    w2 = getWidth s2
    y2 = y p2
    h2 = getHeight s2
    rightC = x1 >= x2 && x1 < (x2 + w2)
    leftC = (x1 + w1) > x2 && (x1 + w1) <= (x2 + w2)
    topC = y1 >= y2 && (y1 < (y2 + h2))
    bottomC = (y1 + h1) > y2 && (y1 + h1) <= (y2 + h2)
    xInside = x1 < x2 && (x1 + w1) > (x2 + w2)
    yInside = y1 < y2 && (y1 + h1) > (y2 + h2)
  in (rightC || leftC || xInside) && (topC || bottomC || yInside)

-- detect victory condition
victory :: Board -> Bool
victory b = player b == winPosition
  



-- randomly generate a new board
initialBoard :: IO Board
initialBoard = do
    -- get some random obstacles
    g <- newStdGen
    let

      notOriginOrWin o = let
        originC = collision (o, sizeObstacle) (origin, sizePlayer)
        winC = collision (o, sizeObstacle) (winPosition, sizePlayer)
        in not (originC && winC)

      obstacles = [Position {x = x' `mod` 24, y = y' `mod` 24} | (x', y') <- singleZip (randoms g)]
      obstacles' = filter notOriginOrWin obstacles
      obstacles'' = take numObstacles obstacles'


    return $ Board {player = origin, obstacles = obstacles''}


-- create functions to bind a box of given size within the 25x25 board
bound :: Size -> (Int -> Int, Int -> Int)
bound s = let boundW = (min (25 - (getWidth s))) . (max 0)
              boundH = (min $ 25 - (getHeight s)) . (max 0)
          in (boundW, boundH)


-- a record of all the styles used in the game
gameStyle :: IO GameStyle
gameStyle =
  let a = Style WhiteF BlackB
      o = Style WhiteF DarkGreenB
      b = Style WhiteF WhiteB
      t = Style WhiteF BlackB
  in
  do [a', o', b', t'] <- convertStyles [a, o, b, t]
     return $ GameStyle { actorStyle = a', obstacleStyle = o', bgStyle = b', text = t' }


-- get the board delta for a given input character
getDelta :: Char -> Board -> Board
getDelta c = case c of
  'h' -> incr (-1, 0)
  'j' -> incr (0, 1)
  'k' -> incr (0, -1)
  'l' -> incr (1, 0)
  _   ->  incr (0, 0)
  where
    incr :: (Int, Int) -> Board -> Board
    incr (dx,dy) brd =
      let p = player brd
          p' = Position {x = boundW ((x p) + dx), y = boundH ((y p) + dy) }

          collisions = foldl (\a o -> a ||  (collision (p', sizePlayer) (o, sizeObstacle))) False (obstacles brd)

          p'' = if collisions then p else p'
      in  brd { player = p'' }

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
    move  (y p) (x p)
    setStyle sty
    write h


drawState :: Window -> GameStyle -> (Board, Bool) -> IO ()
drawState scr sty (brd, vic) =  do
  erase
  let
    p = player brd
    os = obstacles brd
  mkBox scr (bgStyle sty) origin (25, 25) -- draw background
  mkBox scr (actorStyle sty) p sizePlayer -- draw actor
  mapM_  (\p -> mkBox scr (obstacleStyle sty) p sizeObstacle) os -- draw obstacles
  -- draw some text
  setStyle (text sty)
  move 25 0
  wAddStr scr "use hjkl to move. press q to quit"
  if vic then drawVictory else (return ())
  refresh
  where
    drawVictory = do
      move 12 8
      wAddStr scr "YOU WIN!!!!"


makeNetworkDescription :: Frameworks t => AddHandler Char -> Board -> ((Board, Bool) -> IO ()) -> Moment t ()
makeNetworkDescription keyEvent start draw = do
  echar <- fromAddHandler keyEvent  -- Event t Char
  let bchar = stepper 'a' echar     -- Behaviour t Char
      edelta = getDelta <$> echar   -- Event t (Board -> Board)
      bboard = accumB start edelta -- Behavior t Board 
      bvictory = victory <$> bboard -- Behavior t Bool
      bbrdvictory = (\x y-> (y, x))<$> bvictory <*> bboard -- Behaviour t (Board, Bool)
  eboard <- changes bbrdvictory       -- Event t (Future Board, Bool)
  reactimate' $ ( (fmap draw) <$> eboard)


-- c = collision (Position {x = 1, y = 3}, (2,4)) (Position {x = 4, y = 2}, (1,2))


singleZip (x:y:xs) = ((x,y)):(singleZip xs)
