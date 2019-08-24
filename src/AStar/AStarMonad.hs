{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module AStar.AStarMonad (
	AStarM, runAStarM, headNode, putNode, putOpen, Dist, Switch(..) ) where

import Prelude as P

import Control.Arrow
import Control.Monad.State
import Data.Map.Strict
import Data.Heap

import qualified Data.Map as M
import qualified Data.Heap as H

import AStar.Tools

data Switch = Open | Close deriving (Show, Eq, Ord)

switch :: a -> a -> Switch -> a
switch x _ Open = x
switch _ y Close = y

type Dist = Word

type Node d n = (Switch, (d, n))

tag :: Node d n -> Switch
tag = fst

dist :: Node d n -> d
dist = fst . snd

node :: Node d n -> n
node = snd . snd

type AStarM d n = StateT (Moment d n) Maybe
type Moment d n = (Heap (Node d n), Map n n)

runAStarM :: AStarM d n a -> Maybe (a, Moment d n)
runAStarM = (`runStateT` (H.empty, M.empty))

putOpen :: (Ord d, Ord n) => d -> n -> n -> AStarM d n ()
putOpen d n pr = do
	ms <- shortest n <$> gets fst
	case ms of
		Just s | s <= d -> return ()
		_ -> putNode Open d n >> setParent n pr

shortest :: (Ord d, Eq n) => n -> Heap (Switch, (d, n)) -> Maybe d
shortest n = uncurry minMaybe
	. ((dist <$>) . headHeap *** (dist <$>) . headHeap)
	. H.partition ((== Open) . tag) . H.filter ((== n) . node)

headNode :: AStarM d n (d, n)
headNode = do
	(h, m) <- get
	((tg, dn), h') <- lift $ uncons h
	put (h', m)
	switch (return dn) (lift Nothing) tg

putNode :: (Ord d, Ord n) => Switch -> d -> n -> AStarM d n ()
putNode t d n = modify . first $ H.insert (t, (d, n))

setParent :: Ord n => n -> n -> AStarM d n ()
setParent c p = modify . second $ M.insert c p
