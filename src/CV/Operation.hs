{-# LANGUAGE GADTs,FlexibleInstances #-}

-- |Composes operation based on GADTs.
-- |Composed operations will be executed efficiently outside Haskell.

module CV.Operation where

import CV.Core
import CV.Types
import CV.FFI

import Foreign.C -- get the C types
import Foreign.Ptr (Ptr)
import Foreign.ForeignPtr (ForeignPtr,newForeignPtr)
import Foreign.ForeignPtr.Safe (withForeignPtr)
import Foreign.Marshal.Alloc (finalizerFree)
import System.IO.Unsafe (unsafePerformIO)

-- import Data.Monoid
-- import Control.Monad.Free

data MatExpr e where
	MAdd :: MatExpr (MatT a b) -> MatExpr (MatT a b) -> MatExpr (MatT a b)
	MMul :: MatExpr (MatT a b) -> MatExpr (MatT a b) -> MatExpr (MatT a b)
	MEq :: MatExpr (MatT a b) -> MatExpr (MatT a b) -> MatExpr Bool
	MId :: MatT a b -> MatExpr (MatT a b)
	MConvert :: MatExpr (MatT a b) -> MatExpr (MatT c d)
	FindMax :: MatExpr (MatT a b) -> MatExpr [Pos]
	Apply :: LinearFilter -> MatExpr (MatT a b) -> MatExpr (MatT c d)
	Iterate :: Int -> LinearFilter -> MatExpr (MatT a b) -> MatExpr (MatT c d)

instance Num (MatExpr (MatT a b)) where
	a + b = MAdd a b

data LinearFilter = Gauss Int Double deriving Show

(+%) = MAdd
(*%) = MMul
(==%) = MEq
{-}
-- Evaluate MatExpr without copying data.
calcInPlace :: MatExpr a -> b
calcInPlace (FindMax expr) = unsafePerformIO $ do
	mat <- runExt (instruction expr)
	peaks <- runExt (instruction (FindMax (MId mat)))
	return mat
calcInPlace expr
  = unsafePerformIO $ runExt (instruction expr)
-}
runExt :: String -> IO (MatT a b)
runExt inst = do
	let mat = undefined
	-- mat <- runInst_ inst
	return (MatT mat)


(+:+) :: MatT a b -> MatT a b -> MatT a b
(MatT a) +:+ (MatT b) 
  = unsafePerformIO $ do
      withForeignPtr a $ \aa -> do
        withForeignPtr b $ \bb -> do
          mat_ptr <- c_addMat aa bb
          mat <- newForeignPtr cmatFree mat_ptr
          return (MatT mat)

instruction :: MatExpr a -> String
instruction (MId (MatT mid)) = show mid
instruction (MAdd e1 e2) = instruction e1 ++" + "++instruction e2
instruction (MMul e1 e2) = instruction e1 ++ " * " ++instruction e2
instruction (FindMax e) = "find_max("++instruction e++");"
-- instruction (Apply filt ) = "apply_filter('"++show filt++"',"++show e++")"
instruction (Iterate n filt e) = "iterate("++show n++", "++show filt++")"

{-
data Preset =
	Gauss Int Double |
	Mean Int
	deriving (Show,Eq)

data CVOp a b =
	OpId |
	OpUserFunc (MatT a b -> MatT a b) |
	OpPreset Preset |
	OpSeq [CVOp a b] |
	OpCond {opCond :: a -> Bool, ifTrue :: CVOp a b, ifFalse :: CVOp a b}

instance Show (CVOp a b) where
	show OpId = "OpId"
	show (OpPreset pre) = "OpPreset " ++ show pre
	show (OpSeq ops) = "OpSeq" ++ show ops
	show _ = "N/A"

data CVOps a b img
	= CVOps {
		op :: (CVOp a b),
		image :: img }

data WindowSize = WOne | WThree | WFive | WSeven

execute :: CVOps a b (MatT a b) -> MatT a b
execute (CVOps OpId img) = img

gauss :: WindowSize -> Double -> CVOp a b
gauss ws sigma = OpPreset (Gauss w sigma)
	where
		w = case ws of 
				WOne -> 1
				WThree -> 3
				WFive -> 5
				WSeven -> 7

iter :: Int -> CVOp a b -> CVOp a b
iter n op = OpSeq (replicate n op)

{-
-- |Compose operations
instance Monad (CVOps a b) where
	return img = CVOps OpId img
	(CVOps (OpSeq ops) img) >>= (CVOps ops2 _) = CVOps (OpSeq (OpPreset (ops++ops2))) img
-}
instance Monoid (CVOp a b) where
	OpSeq ops `mappend` OpSeq ops2 = mconcat (ops ++ ops2)
	op `mappend` OpSeq ops2 = mconcat (op:ops2)
	OpSeq ops `mappend` op2 = mconcat (ops ++ [op2])
	op `mappend` op2 = mconcat [op,op2]
	mempty = OpId
	mconcat xs = OpSeq (filter f xs)
		where
			f OpId = False
			f _ = True

class NativeInst a where
	toInst :: a -> String
	toInsts :: [a] -> String
	toInsts xs = concatMap toInst xs

instance NativeInst (CVOp a b) where
	toInst OpId = "id();"
	toInst (OpPreset (Gauss ws sigma)) = "gauss("++show ws++","++show sigma++");"
	toInst (OpSeq ops) = concatMap toInst ops
	toInst _ = "error();" -- not supported yet.
-}
{-
instance Monad CVOps where
	return a = CVOps a
	(CVOps a) >>= (CVOps b) =  -}