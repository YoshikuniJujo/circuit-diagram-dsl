{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Circuit.DiagramDsl (
	drawDiagram,
	DiagramMapM, runDiagramMapM, execDiagramMapM,
	DiagramMap, ElementIdable(..), ElementId, Pos,
	Element0, ElementDiagram0(..),
	putElementEnd0, putElement0, newElementEnd0, newElement0,
	Element1, ElementDiagram1(..),
	putElementEnd1, putElement1, newElementEnd1, newElement1,
	inputPosition0, connectLine0,
	Element2, ElementDiagram2(..),
	newPutElementEnd2, newPutElement2, newNewElementEnd2, newNewElement2,
	newInputPosition1, newInputPosition2, newConnectLine1, newConnectLine2
	) where

import Data.Word

import Circuit.Diagram.Draw
import Circuit.Diagram.Map hiding (putElement0, newElement0)

import qualified Circuit.Diagram.Map as M

data Element0 a = NewElement0 a LinePos deriving Show

data ElementDiagram0 = ConstGateD Word64 deriving Show

newToDiagram0 :: ElementDiagram0 -> ElementDiagram
newToDiagram0 (ConstGateD c) = constGateD c

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
newToDiagram1 :: ElementDiagram1 -> ElementDiagram
newToDiagram1 NotGateD = notGateD
newToDiagram1 (DelayD d) = delayD d
newToDiagram1 HLineD = hLineD
newToDiagram1 (HLineTextD t1 t2) = hLineTextD t1 t2

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

newToDiagram2 :: ElementDiagram2 -> ElementDiagram
newToDiagram2 AndGateD = andGateD
newToDiagram2 OrGateD = orGateD
newToDiagram2 (TriGateD t1 t2) = triGateD t1 t2
newToDiagram2 BranchD = branchD

newPutElementEnd2 :: ElementIdable eid =>
	eid -> ElementDiagram2 -> DiagramMapM (Maybe (Element2 eid))
newPutElementEnd2 eid ed = do
	lps <- M.putElement0 eid $ newToDiagram2 ed
	return $ NewElement2 eid <$> lps

newPutElement2 :: ElementIdable eid =>
	eid -> ElementDiagram2 -> Pos -> DiagramMapM (Maybe (Element2 eid))
newPutElement2 eid ed pos = do
	lps <- putElement eid (newToDiagram2 ed) pos
	return $ NewElement2 eid <$> lps

newNewElementEnd2 :: ElementIdable eid =>
	eid -> ElementDiagram2 -> DiagramMapM (Element2 eid)
newNewElementEnd2 eid ed = do
	lps <- M.newElement0 eid $ newToDiagram2 ed
	return $ NewElement2 eid lps

newNewElement2 :: ElementIdable eid =>
	eid -> ElementDiagram2 -> Pos -> DiagramMapM (Element2 eid)
newNewElement2 eid ed pos = do
	lps <- newElement eid (newToDiagram2 ed) pos
	return $ NewElement2 eid lps

newInputPosition1, newInputPosition2 :: Element2 eid -> DiagramMapM Pos
newInputPosition1 (NewElement2 _ lps) = inputPosition1 lps
newInputPosition2 (NewElement2 _ lps) = inputPosition2 lps

newConnectLine1, newConnectLine2 :: ElementIdable eid => Element2 eid -> eid -> DiagramMapM ()
newConnectLine1 (NewElement2 eid1 _) eid2 = connectLine1 eid1 eid2
newConnectLine2 (NewElement2 eid1 _) eid2 = connectLine2 eid1 eid2
