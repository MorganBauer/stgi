module Test.Machine (tests) where



import           Test.Tasty

import qualified Test.Machine.Evaluate            as Evaluate
import qualified Test.Machine.GarbageCollection   as GarbageCollection
import qualified Test.Machine.Heap                as Heap
import qualified Test.Machine.RemoveUnusedGlobals as RemoveUnusedGlobals



tests :: TestTree
tests = testGroup "Machine"
    [ Heap.tests
    , Evaluate.tests
    , GarbageCollection.tests
    , RemoveUnusedGlobals.tests ]
