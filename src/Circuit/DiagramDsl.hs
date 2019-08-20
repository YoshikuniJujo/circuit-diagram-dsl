{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Circuit.DiagramDsl (
	-- * drawDiagram
	drawDiagram,

	-- * DiagramMapM, DiagramMap, ElementIdable and so on
	DiagramMapM, runDiagramMapM, execDiagramMapM,
	DiagramMap, ElementIdable(..), ElementId, Pos,

	-- * putElement
	IsElementDiagram, Element,
	putElementEnd, putElement, newElementEnd, newElement,

	-- * No Input Lines
	Element0, ElementDiagram0, constGateD,

	-- * 1 Input Line
	Element1,
	ElementDiagram1, notGateD, delayD, hLineD, hLineTextD,
	inputPosition0, connectLine0,

	-- * 2 Input Lines
	Element2,
	ElementDiagram2, andGateD, orGateD, triGateD, branchD,
	inputPosition1, inputPosition2, connectLine1, connectLine2,

	-- * Element Block
	ElementBlock,
	ElementBlockDiagram, blockD,
	putElementBlockEnd, putElementBlock, newElementBlockEnd, newElementBlock,
	inputPositionBlock, connectLineBlock
	) where


import Control.Monad.Trans
import Data.Word

import Circuit.Diagram.Draw.Draw
import Circuit.Diagram.Map hiding (
	putElementGen,
	inputPosition1, inputPosition2, connectLine1, connectLine2,
	constGateD, notGateD, delayD, hLineD, hLineTextD,
	andGateD, orGateD, triGateD, branchD, blockD, ElementDiagram )

import qualified Circuit.Diagram.Map as M

class IsElementDiagram ed where
	type Element ed :: * -> *
	putElementGen :: ElementIdable eid => Bool -> eid -> ed ->
		Int -> Maybe Int -> DiagramMapM (Maybe (Element ed eid))
	
putElementEnd :: (IsElementDiagram ed, ElementIdable eid) =>
	eid -> ed -> DiagramMapM (Maybe (Element ed eid))
putElementEnd eid e = do
	sp <- getSpace
	putElementGen True eid e sp Nothing

putElement :: (IsElementDiagram ed, ElementIdable eid) =>
	eid -> ed -> Pos -> DiagramMapM (Maybe (Element ed eid))
putElement eid e (Pos x y) = putElementGen False eid e x (Just y)

newElementEnd :: (IsElementDiagram ed, ElementIdable eid) =>
	eid -> ed -> DiagramMapM (Element ed eid)
newElementEnd eid e = maybe (lift $ Left "Oops!") return =<< putElementEnd eid e

newElement :: (IsElementDiagram ed, ElementIdable eid) =>
	eid -> ed -> Pos -> DiagramMapM (Element ed eid)
newElement eid e pos = maybe (lift $ Left "Oops!") return =<< putElement eid e pos

data Element0 a = Element0 deriving Show

data ElementDiagram0 = ConstGateD Word64 deriving Show

instance IsElementDiagram ElementDiagram0 where
	type Element ElementDiagram0 = Element0
	putElementGen b eid ed x my = do
		_ <- M.putElementGen b [eid] (newToDiagram0 ed) x my
		return $ Just Element0

constGateD :: Word64 -> ElementDiagram0
constGateD = ConstGateD

newToDiagram0 :: ElementDiagram0 -> M.ElementDiagram
newToDiagram0 (ConstGateD c) = M.constGateD c

data Element1 a = NewElement1 a LinePos deriving Show

data ElementDiagram1
	= NotGateD | DelayD Word8 | HLineD | HLineTextD String String 
	deriving Show

instance IsElementDiagram ElementDiagram1 where
	type Element ElementDiagram1 = Element1
	putElementGen b eid ed x my = do
		lp <- M.putElementGen b [eid] (newToDiagram1 ed) x my
		return $ NewElement1 eid <$> lp

notGateD :: ElementDiagram1
notGateD = NotGateD

delayD :: Word8 -> ElementDiagram1
delayD = DelayD

hLineD :: ElementDiagram1
hLineD = HLineD

hLineTextD :: String -> String -> ElementDiagram1
hLineTextD = HLineTextD

newToDiagram1 :: ElementDiagram1 -> M.ElementDiagram
newToDiagram1 NotGateD = M.notGateD
newToDiagram1 (DelayD d) = M.delayD d
newToDiagram1 HLineD = M.hLineD
newToDiagram1 (HLineTextD t1 t2) = M.hLineTextD t1 t2

inputPosition0 :: Element1 eid -> DiagramMapM Pos
inputPosition0 (NewElement1 _ lps) = inputPosition lps

connectLine0 :: ElementIdable eid => Element1 eid -> eid -> DiagramMapM ()
connectLine0 (NewElement1 eid1 _) eid2 = connectLine eid1 eid2

data Element2 a = NewElement2 a LinePos deriving Show

data ElementDiagram2 =  AndGateD | OrGateD | TriGateD String String | BranchD
	deriving Show

instance IsElementDiagram ElementDiagram2 where
	type Element ElementDiagram2 = Element2
	putElementGen b eid ed x my = do
		lp <- M.putElementGen b [eid] (newToDiagram2 ed) x my
		return $ NewElement2 eid <$> lp

andGateD, orGateD :: ElementDiagram2
andGateD = AndGateD
orGateD = OrGateD

triGateD :: String -> String -> ElementDiagram2
triGateD = TriGateD

branchD :: ElementDiagram2
branchD = BranchD

newToDiagram2 :: ElementDiagram2 -> M.ElementDiagram
newToDiagram2 AndGateD = M.andGateD
newToDiagram2 OrGateD = M.orGateD
newToDiagram2 (TriGateD t1 t2) = M.triGateD t1 t2
newToDiagram2 BranchD = M.branchD

inputPosition1, inputPosition2 :: Element2 eid -> DiagramMapM Pos
inputPosition1 (NewElement2 _ lps) = M.inputPosition1 lps
inputPosition2 (NewElement2 _ lps) = M.inputPosition2 lps

connectLine1, connectLine2 :: ElementIdable eid => Element2 eid -> eid -> DiagramMapM ()
connectLine1 (NewElement2 eid1 _) eid2 = M.connectLine1 eid1 eid2
connectLine2 (NewElement2 eid1 _) eid2 = M.connectLine2 eid1 eid2

data ElementBlock a = ElementList a LinePos deriving Show

data ElementBlockDiagram = BlockD Int String deriving Show

blockD :: Int -> String ->ElementBlockDiagram
blockD = BlockD

toDiagramList :: Int -> ElementBlockDiagram -> M.ElementDiagram
toDiagramList os (BlockD is t)  = M.blockD is os t

putElementGenList :: ElementIdable eid =>
	Bool -> [eid] -> ElementBlockDiagram -> Int -> Maybe Int ->
	DiagramMapM (Maybe (ElementBlock eid))
putElementGenList b eids ed x my = do
	lp <- M.putElementGen b eids (toDiagramList (length eids) ed) x my
	return $ ElementList (head eids) <$> lp

putElementBlockEnd :: ElementIdable eid =>
	[eid] -> ElementBlockDiagram -> DiagramMapM (Maybe (ElementBlock eid))
putElementBlockEnd eids e = do
	sp <- getSpace
	putElementGenList True eids e sp Nothing

putElementBlock :: ElementIdable eid =>
	[eid] -> ElementBlockDiagram -> Pos ->
	DiagramMapM (Maybe (ElementBlock eid))
putElementBlock eids e (Pos x y) = putElementGenList False eids e x (Just y)

newElementBlockEnd :: ElementIdable eid =>
	[eid] -> ElementBlockDiagram -> DiagramMapM (ElementBlock eid)
newElementBlockEnd eid e =
	maybe (lift $ Left "Oops!") return =<< putElementBlockEnd eid e

newElementBlock :: ElementIdable eid =>
	[eid] -> ElementBlockDiagram -> Pos -> DiagramMapM (ElementBlock eid)
newElementBlock eid e pos =
	maybe (lift $ Left "Oops!") return =<< putElementBlock eid e pos

inputPositionBlock :: Int -> ElementBlock eid -> DiagramMapM Pos
inputPositionBlock i (ElementList _ lps) = M.inputPositionMulti i lps

connectLineBlock :: ElementIdable eid => Int -> ElementBlock eid -> eid -> DiagramMapM ()
connectLineBlock i (ElementList eid1 _) eid2 = M.connectLineMulti i eid1 eid2
