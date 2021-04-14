{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

--{-# LANGUAGE NoStarIsType #-}
--{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Gundam where

import Control.Exception
import Control.Monad (unless)
import Data.Kind (Type)
import GHC.Stack

boom = error

data BoomException = BoomException String CallStack

instance Exception BoomException

instance Show BoomException where
  show (BoomException msg cs) =
    "BoomException: " ++ msg ++ "\n" ++ prettyCallStack cs

-- TODO: show line number here.
assertIt :: HasCallStack => Bool -> String -> IO ()
assertIt condition msg =
  unless condition $
    throwIO $ BoomException msg callStack

data Direction = North | East | South | West
  deriving stock Eq
  deriving stock Show

{-
data Idle
data Moving
-}

data Status = Idle | Moving
  deriving stock Eq
  deriving stock Show

-- Command before after
data Command :: Status -> Status -> Type where
  Face :: Direction -> Command Idle Idle
  Start :: Command Idle Moving
  Stop :: Command Moving Idle
  Chain :: Command a b -> Command b c -> Command a c

instance Eq (Command a b) where
  Face dir1 == Face dir2 = dir1 == dir2
  Start == Start = True
  Stop == Stop = True
  Chain lc1 lc2 == Chain rc1 rc2 = lc1 == rc1 && lc2 == rc2
  -- FIXME!
--  Chain a1 a2 == Chain b1 b2 = a1 == b1 && a2 == b2
  _ == _ = False

deriving instance Show (Command before after)

data State = State
  { statePath :: [Direction]
  , stateDir :: Direction
  , stateMoving :: Bool
  }
  deriving stock Eq
  deriving stock Show

apply :: Command before after -> State -> State
apply cmd state =
  let (State path dir moving) = state
  in
  case cmd of
    Face newDir ->
      if moving then
        boom $ "Trying to face " ++ show newDir ++ " when moving!"
      else State path newDir False
    Start ->
      if moving then
        boom "Trying to start while moving!"
      else
        State (path ++ [dir]) dir True
    Stop ->
      if moving then
        State path dir False
      else
        boom "Trying to stop while not moving!"
    Chain cmd1 cmd2 ->
      apply cmd2 (apply cmd1 state)

label North = "north"
label East = "east"
label South = "south"
label West = "west"

-- Smarter exhaustivity checking.
movingLabel :: Command Moving after -> String
movingLabel cmd =
  case cmd of
    Stop -> "stop"
    _ -> "chain"

(~>) = Chain
infixl 3 ~>

start :: Command Idle Moving
start = Start

stop :: Command Moving Idle
stop = Stop

face :: Direction -> Command Idle Idle
face = Face

north = North
east = East
south = South
west = West

startStop :: Command Idle Idle
startStop = start ~> stop

faceNorth :: Command Idle Idle
faceNorth = face north

faceSouth :: Command Idle Idle
faceSouth = face south

faceTwice :: Command Idle Idle
faceTwice = face north ~> face south

move :: Direction -> Command Idle Idle
move dir = face dir ~> start ~> stop

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

cmds2 = move east ~> move west

defaultState = State {
  statePath = [],
  stateDir = North,
  stateMoving = False
}

go :: IO ()
go = do
  let state0 = defaultState
  print state0
  let state1 = apply (Face North) state0
  print state1
  let state2 = apply (Face West) state1
  print state2
  let state3 = apply (Face South) state2
  print state3
  let state4 = apply (Face East) state3
  print state4
  let state5 = apply Start state4
  print state5
  let state6 = apply Stop state5
  print state6

main = do
  print (label West)

  print (movingLabel stop)
  --print (movingLabel start)

  -- cmds1 and cmds2 seems equivalent but do so via different
  -- nesting of Chains.
  assertIt (cmds1 /= cmds2) "Commands should not be equal"
  assertIt (cmds1 == cmds1) "cmd1 should equal itself!"
  print cmds1
  print cmds2

  let finalState1 = apply cmds1 defaultState
  print finalState1
  let expectedState = State [East, West] West False
  assertIt (finalState1 == expectedState) "final state not expected :("

  let finalState2 = apply cmds2 defaultState
  print finalState2
  assertIt (statePath finalState2 == [East, West]) "state paths disagree :("

  -- Final state of cmds1 and cmd2 are the same.
  assertIt (finalState1 == finalState2) "States of cmd1 and cmds2 disagree :("

  go
