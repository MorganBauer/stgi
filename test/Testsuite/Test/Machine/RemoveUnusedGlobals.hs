{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Test.Machine.RemoveUnusedGlobals (tests) where



import qualified Data.Map             as M
import           Data.Monoid
import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Stg.Language.Prelude as Stg
import           Stg.Machine
import           Stg.Machine.Types
import           Stg.Parser

import           Test.Orphans         ()



tests :: TestTree
tests = testGroup "Remove unused globals"
    [ simple
    , selfReferential
    , redundantPrelude
    ]

simple :: TestTree
simple = testCase "Simple program" test
  where
    prog = [stgProgram|
        unused1 = () \n () -> bar    ();
        unused2 = () \u () -> Unused ();
        used    = () \n () -> Used   ();
        main    = () \u () -> used   () |]
    StgState{stgGlobals = Globals leftOver}
      = removeUnusedGlobals (initialState "main" prog)
    test = do
        assertBool "'main' should not have been removed"
                   ("main" `M.member` leftOver)
        assertBool "'unused1' should have been removed"
                   ("unused1" `M.notMember` leftOver)
        assertBool "'unused2' should have been removed"
                   ("unused2" `M.notMember` leftOver)
        assertBool "'used' should not have been removed"
                   ("used" `M.member` leftOver)



selfReferential :: TestTree
selfReferential = testCase "Self-referential globals" test
  where
    prog = [stgProgram|
        one = () \n () -> two ();
        two = () \n () -> three ();
        three = () \n () -> one ();

        self = () \n () -> self ();

        main = () \u () -> self (one) |]
    initial@StgState{stgGlobals = initialGlobals} = initialState "main" prog
    StgState{stgGlobals = leftOverGlobals} = removeUnusedGlobals initial
    test =
        assertBool "Nothing should have been removed"
                   (leftOverGlobals == initialGlobals)




redundantPrelude :: TestTree
redundantPrelude = testCase "Redundant Prelude includes" test
  where
    prog = mconcat
        [ Stg.foldl
        , Stg.sort
        , Stg.filter
        , Stg.listOfNumbers "list" [1..10]]
        <> [stgProgram|
        five = () \n () -> Int# (5#);
        used = () \n () -> Used (five);
        main = () \u () -> foldl (used) |]
    StgState{stgGlobals = Globals leftOver}
      = removeUnusedGlobals (initialState "main" prog)
    test = do
        assertBool "Only foldl, used, five, main should remain"
                   (M.keysSet leftOver == ["five", "foldl", "used", "main"])
