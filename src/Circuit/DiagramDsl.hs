{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Circuit.DiagramDsl (
	-- * drawDiagram
	drawDiagram,

	-- * DiagramMapM, DiagramMap, ElementIdable and so on
	DiagramMapM, runDiagramMapM, execDiagramMapM,
	DiagramMap, ElementIdable(..), ElementId, Pos,

	-- * No Input Lines
	Element0, ElementDiagram0, constGateD,
	putElementEnd0, putElement0, newElementEnd0, newElement0,

	-- * 1 Input Line
	Element1,
	ElementDiagram1, notGateD, delayD, hLineD, hLineTextD,
	putElementEnd1, putElement1, newElementEnd1, newElement1,
	inputPosition0, connectLine0,
	-- * 2 Input Lines
	Element2,
	ElementDiagram2, andGateD, orGateD, triGateD, branchD,
	putElementEnd2, putElement2, newElementEnd2, newElement2,
	inputPosition1, inputPosition2, connectLine1, connectLine2
	) where

import Data.Word

import Circuit.Diagram.Draw
import Circuit.Diagram.Map hiding (
	putElement0, newElement0,
	inputPosition1, inputPosition2, connectLine1, connectLine2,
	constGateD, notGateD, delayD, hLineD, hLineTextD,
	andGateD, orGateD, triGateD, branchD )

import qualified Circuit.Diagram.Map as M

data Element0 a = NewElement0 a LinePos deriving Show

data ElementDiagram0 = ConstGateD Word64 deriving Show

constGateD :: Word64 -> ElementDiagram0
constGateD = ConstGateD

newToDiagram0 :: ElementDiagram0 -> ElementDiagram
newToDiagram0 (ConstGateD c) = M.constGateD c

putElementEnd0 :: ElementIdable eid => eid -> ElementDiagram0 -> DiagramMapM ()
putElementEnd0 eid ed = () <$ M.putElement0 eid (newToDiagram0 ed)

putElement0 ::
	ElementIdable eid => eid -> ElementDiagram0 -> Pos -> DiagramMapM ()
putElement0 eid ed pos = () <$ putElement eid (newToDiagram0 ed) pos

newElementEnd0 :: ElementIdable eid => eid -> ElementDiagram0 -> DiagramMapM ()
newElementEnd0 eid ed = () <$ M.newElement0 eid (newToDiagram0 ed)

newElement0 ::
	ElementIdable eid => eid -> ElementDiagram0 -> Pos -> DiagramMapM ()
newElement0 eid ed pos = () <$ newElement eid (newToDiagram0 ed) pos

data Element1 a = NewElement1 a LinePos deriving Show

data ElementDiagram1
	= NotGateD | DelayD Word8 | HLineD | HLineTextD String String 
	deriving Show

notGateD :: ElementDiagram1
notGateD = NotGateD

delayD :: Word8 -> ElementDiagram1
delayD = DelayD

hLineD :: ElementDiagram1
hLineD = HLineD

hLineTextD :: String -> String -> ElementDiagram1
hLineTextD = HLineTextD

newToDiagram1 :: ElementDiagram1 -> ElementDiagram
newToDiagram1 NotGateD = M.notGateD
newToDiagram1 (DelayD d) = M.delayD d
newToDiagram1 HLineD = M.hLineD
newToDiagram1 (HLineTextD t1 t2) = M.hLineTextD t1 t2

putElementEnd1 :: ElementIdable eid =>
	eid -> ElementDiagram1 -> DiagramMapM (Maybe (Element1 eid))
putElementEnd1 eid ed = do
	lps <- M.putElement0 eid $ newToDiagram1 ed
	return $ NewElement1 eid <$> lps

putElement1 :: ElementIdable eid => eid ->
	ElementDiagram1 -> Pos -> DiagramMapM (Maybe (Element1 eid))
putElement1 eid ed pos = do
	lps <- putElement eid (newToDiagram1 ed) pos
	return $ NewElement1 eid <$> lps

newElementEnd1 :: ElementIdable eid =>
	eid -> ElementDiagram1 -> DiagramMapM (Element1 eid)
newElementEnd1 eid ed = do
	lps <- M.newElement0 eid $ newToDiagram1 ed
	return $ NewElement1 eid lps

newElement1 :: ElementIdable eid =>
	eid -> ElementDiagram1 -> Pos -> DiagramMapM (Element1 eid)
newElement1 eid ed pos = do
	lps <- newElement eid (newToDiagram1 ed) pos
	return $ NewElement1 eid lps

inputPosition0 :: Element1 eid -> DiagramMapM Pos
inputPosition0 (NewElement1 _ lps) = inputPosition lps

connectLine0 :: ElementIdable eid => Element1 eid -> eid -> DiagramMapM ()
connectLine0 (NewElement1 eid1 _) eid2 = connectLine eid1 eid2

data Element2 a = NewElement2 a LinePos deriving Show

data ElementDiagram2 =  AndGateD | OrGateD | TriGateD String String | BranchD
	deriving Show

andGateD, orGateD :: ElementDiagram2
andGateD = AndGateD
orGateD = OrGateD

triGateD :: String -> String -> ElementDiagram2
triGateD = TriGateD

branchD :: ElementDiagram2
branchD = BranchD

newToDiagram2 :: ElementDiagram2 -> ElementDiagram
newToDiagram2 AndGateD = M.andGateD
newToDiagram2 OrGateD = M.orGateD
newToDiagram2 (TriGateD t1 t2) = M.triGateD t1 t2
newToDiagram2 BranchD = M.branchD

putElementEnd2 :: ElementIdable eid =>
	eid -> ElementDiagram2 -> DiagramMapM (Maybe (Element2 eid))
putElementEnd2 eid ed = do
	lps <- M.putElement0 eid $ newToDiagram2 ed
	return $ NewElement2 eid <$> lps

putElement2 :: ElementIdable eid =>
	eid -> ElementDiagram2 -> Pos -> DiagramMapM (Maybe (Element2 eid))
putElement2 eid ed pos = do
	lps <- putElement eid (newToDiagram2 ed) pos
	return $ NewElement2 eid <$> lps

newElementEnd2 :: ElementIdable eid =>
	eid -> ElementDiagram2 -> DiagramMapM (Element2 eid)
newElementEnd2 eid ed = do
	lps <- M.newElement0 eid $ newToDiagram2 ed
	return $ NewElement2 eid lps

newElement2 :: ElementIdable eid =>
	eid -> ElementDiagram2 -> Pos -> DiagramMapM (Element2 eid)
newElement2 eid ed pos = do
	lps <- newElement eid (newToDiagram2 ed) pos
	return $ NewElement2 eid lps

inputPosition1, inputPosition2 :: Element2 eid -> DiagramMapM Pos
inputPosition1 (NewElement2 _ lps) = M.inputPosition1 lps
inputPosition2 (NewElement2 _ lps) = M.inputPosition2 lps

connectLine1, connectLine2 :: ElementIdable eid => Element2 eid -> eid -> DiagramMapM ()
connectLine1 (NewElement2 eid1 _) eid2 = M.connectLine1 eid1 eid2
connectLine2 (NewElement2 eid1 _) eid2 = M.connectLine2 eid1 eid2
