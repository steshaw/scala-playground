---------------------------------------------------------------------
-- Not in Prelude yet.
---------------------------------------------------------------------

unless : Applicative f => Bool -> Lazy (f ()) -> f ()
unless condition action = when (not condition) action

(=<<) : Monad m => (a -> m b) -> m a -> m b
(=<<) = flip (>>=)
infixl 3 =<<
---------------------------------------------------------------------

assertIt : Bool -> String -> IO ()
assertIt condition msg =
  unless condition $ do
    putStrLn "ARGH ["
    putStrLn $ "  " ++ msg
    putStrLn "]"

data Direction = North | East | South | West

Show Direction where
  show North = "North"
  show East = "East"
  show South = "South"
  show West = "West"

Eq Direction where
  North == North = True
  East == East = True
  South == South = True
  West == West = True
  _ == _ = False

data Status = Idle | Moving

Show Status where
  show Idle = "Idle"
  show Moving = "Moving"

Eq Status where
  Idle == Idle = True
  Moving == Moving = True
  _ == _ = False

-- Command before after
data Command : Status -> Status -> Type where
  Face : Direction -> Command Idle Idle
  Start : Command Idle Moving
  Stop : Command Moving Idle
  Chain : Command a b -> Command b c -> Command a c

-- ECommand - erased command.
data ECommand
  = EFace Direction
  | EStart
  | EStop
  | EChain ECommand ECommand

Eq ECommand where
  EFace d1 == EFace d2 = d1 == d2
  EStart == EStart = True
  EStop == EStop = True
  EChain lc1 lc2 == EChain rc1 rc2 = lc1 == rc1 && lc2 == rc2
  _ == _ = False

Show (Command status1 status2) where
  show (Face dir) = "(Face " ++ show dir ++ ")"
  show Start = "Start"
  show Stop = "Stop"
  show (Chain cmd1 cmd2) =
    "(Chain " ++ show cmd1 ++ ", " ++ show cmd2 ++ ")"

-- Erase the fancy types so that we can do Eq.
erase : Command s1 s2 -> ECommand
erase (Face d) = EFace d
erase Start = EStart
erase Stop = EStop
erase (Chain cmd1 cmd2) = EChain (erase cmd1) (erase cmd2)

Eq (Command status1 status2) where
  cmd1 == cmd2 = erase cmd1 == erase cmd2

record State where
  constructor MkState
  statePath : List Direction
  stateDir : Direction
  stateMoving : Status

Show State where
  show (MkState p d m) =
    "State(" ++ show p ++ ", " ++ show d ++ ", " ++ show m ++ ")"

Eq State where
  (MkState p1 d1 m1) == (MkState p2 d2 m2) =
    p1 == p2 && d1 == d2 && m1 == m2

partial
apply : Command before after -> State -> State
apply cmd state =
  let (MkState path dir moving) = state
  in case cmd of
    Face newDir =>
      if moving == Moving then
        idris_crash $ "Trying to face " ++ show newDir ++ " when moving!"
      else
        MkState path newDir Idle
    Start =>
      if moving == Moving then
        idris_crash "Trying to start while moving!"
      else
        MkState (path ++ [dir]) dir Moving
    Stop =>
      if moving == Moving then
        MkState path dir Idle
      else
        idris_crash "Trying to stop while not moving!"
    Chain cmd1 cmd2 =>
      apply cmd2 (apply cmd1 state)

applyE : Command before after -> State -> Either String State
applyE cmd state =
  let (MkState path dir moving) = state
  in case cmd of
    Face newDir =>
      if moving == Moving then
        Left $ "Trying to face " ++ show newDir ++ " when moving!"
      else
        Right $ MkState path newDir Idle
    Start =>
      if moving == Moving then
        Left $ "Trying to start while moving!"
      else
        Right $ MkState (path ++ [dir]) dir Moving
    Stop =>
      if moving == Moving then
        Right $ MkState path dir Idle
      else
        Left $ "Trying to stop while not moving!"
    Chain cmd1 cmd2 => do
      applyE cmd2 =<< applyE cmd1 state

label : Direction -> String
label North = "north"
label East = "east"
label South = "south"
label West = "west"

-- Smarter exhaustivity checking.
movingLabel : Command Moving after -> String
movingLabel cmd =
  case cmd of
    Stop => "stop"
    _ => "chain"

(~>) : Command a b -> Command b c -> Command a c
(~>) = Chain
infixl 3 ~>

start : Command Idle Moving
start = Start

stop : Command Moving Idle
stop = Stop

face : Direction -> Command Idle Idle
face = Face

north : Direction
north = North
east : Direction
east = East
south : Direction
south = South
export west : Direction
west = West

startStop : Command Idle Idle
startStop = start ~> stop

faceNorth : Command Idle Idle
faceNorth = face north

faceSouth : Command Idle Idle
faceSouth = face south

faceTwice : Command Idle Idle
faceTwice = face north ~> face south

move : Direction -> Command Idle Idle
move dir = face dir ~> start ~> stop

cmds1 : Command Idle Idle
cmds1 =
  Chain
    (Chain
      (Chain
        (Face East)
        (Chain Start Stop)
      )
      (Face West)
    )
    (Chain Start Stop)

cmds2 : Command Idle Idle
cmds2 = move east ~> move west

defaultState : State
defaultState = MkState {
  statePath = [],
  stateDir = North,
  stateMoving = Idle
}

partial
goE : IO ()
goE = do
  let state0 = defaultState
  printLn state0
  let (Right state1) = applyE (Face North) state0
  printLn state1
  let (Right state2) = applyE (Face West) state1
  printLn state2
  let (Right state3) = applyE (Face South) state2
  printLn state3
  let (Right state4) = applyE (Face East) state3
  printLn state4
  let (Right state5) = applyE Start state4
  printLn state5
  let (Right state6) = applyE Stop state5
  printLn state6

partial
go : IO ()
go = do
  let state0 = defaultState
  printLn state0
  let state1 = apply (Face North) state0
  printLn state1
  let state2 = apply (Face West) state1
  printLn state2
  let state3 = apply (Face South) state2
  printLn state3
  let state4 = apply (Face East) state3
  printLn state4
  let state5 = apply Start state4
  printLn state5
  let state6 = apply Stop state5
  printLn state6

partial
originalMain : IO ()
originalMain = do
  putStrLn (label West)

  putStrLn (movingLabel stop)
  --putStrLn (movingLabel start)

  -- cmds1 and cmds2 seems equivalent but do so via different
  -- nesting of Chains.
  assertIt (cmds1 /= cmds2) "Commands should not be equal"
  assertIt (cmds1 == cmds1) "cmd1 should equal itself!"
  printLn cmds1
  printLn cmds2

  let finalState1 = apply cmds1 defaultState
  printLn finalState1
  let expectedState = MkState [East, West] West Idle
  assertIt (finalState1 == expectedState) "final state not expected :("

  let finalState2 = apply cmds2 defaultState
  printLn finalState2
  assertIt (statePath finalState2 == [East, West]) "state paths disagree :("

  -- Final state of cmds1 and cmd2 are the same.
  assertIt (finalState1 == finalState2) "States of cmd1 and cmds2 disagree :("

  putStrLn "-- go --"
  go
  putStrLn "-- goE --"
  goE

partial
main : IO ()
main = do
  putStrLn "Hello, Idris2!"
  printLn $ North == North
  printLn $ East == North
  printLn $ Idle == Idle
  printLn $ Moving == Moving
  printLn $ Idle == Moving
  printLn $ defaultState == defaultState
  printLn $ defaultState == MkState [] North Idle
  putStrLn ""
  putStrLn "----- Original main"
  originalMain
