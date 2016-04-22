{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

-- | User-facing API to work with STG programs.
module Stg.Machine (
    initialState,
    evalStep,
    evalUntil,
) where



import qualified Data.Map             as M
import qualified Data.Text            as T

import           Stg.Language
import           Stg.Machine.Env
import           Stg.Machine.Evaluate
import qualified Stg.Machine.Heap     as H
import           Stg.Machine.Types
import           Stg.Util



-- | Create a suitable initial state for an STG.
initialState
    :: Var -- ^ Main
    -> Program
    -> StgState
initialState mainVar (Program binds) = StgState
    { stgCode        = Eval (AppF mainVar []) mempty
    , stgArgStack    = mempty
    , stgReturnStack = mempty
    , stgUpdateStack = mempty
    , stgHeap        = heap
    , stgGlobals     = globals
    , stgTicks       = 0
    , stgInfo        = Info StateInitial [] }
  where
    globalVars :: [Var]
    globalVals :: [LambdaForm]
    (globalVars, globalVals) = let Binds b = binds in unzip (M.assocs b)

    globals :: Globals
    globals = makeGlobals (zipWith (\n a -> (n, Addr a)) globalVars addrs)

    addrs :: [MemAddr]
    heap :: Heap
    (addrs, heap) = H.allocMany (map liftClosure globalVals) mempty

    -- TODO: Unify this with the other liftClosure
    liftClosure :: LambdaForm -> Closure
    liftClosure lf@(LambdaForm free _ _ _) =
        let freeVals :: [Value]
            freeVals = case traverse (globalVal globals) free of
                Success x -> x
                Failure e -> (error ("liftClosure in initial state: " ++ T.unpack e))
        in Closure lf freeVals
