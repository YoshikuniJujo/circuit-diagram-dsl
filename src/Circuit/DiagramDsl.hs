{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Circuit.DiagramDsl (
	drawDiagram,
	DiagramMapM, runDiagramMapM, execDiagramMapM,
	DiagramMap, ElementIdable(..), ElementId,
	NewElement0, NewElementDiagram0(..),
	newPutElementEnd0, newPutElement0, newNewElementEnd0, newNewElement0,
	NewElement1, NewElementDiagram1(..),
	newPutElementEnd1, newPutElement1, newNewElementEnd1, newNewElement1,
	newInputPosition0, newConnectLine0,
	NewElement2, NewElementDiagram2(..),
	newPutElementEnd2, newPutElement2, newNewElementEnd2, newNewElement2,
	newInputPosition1, newInputPosition2, newConnectLine1, newConnectLine2
	) where

import Data.Word

import Circuit.Diagram.Draw
import Circuit.Diagram.Map

data NewElement0 a = NewElement0 a LinePos deriving Show

data NewElementDiagram0 = NewConstGateD Word64 deriving Show

newToDiagram0 :: NewElementDiagram0 -> ElementDiagram
newToDiagram0 (NewConstGateD c) = constGateD c

newPutElementEnd0 ::
	ElementIdable eid => eid -> NewElementDiagram0 -> DiagramMapM ()
newPutElementEnd0 eid ed = () <$ putElement0 eid (newToDiagram0 ed)

newPutElement0 ::
	ElementIdable eid => eid -> NewElementDiagram0 -> Pos -> DiagramMapM ()
newPutElement0 eid ed pos = () <$ putElement eid (newToDiagram0 ed) pos

newNewElementEnd0 ::
	ElementIdable eid => eid -> NewElementDiagram0 -> DiagramMapM ()
newNewElementEnd0 eid ed = () <$ newElement0 eid (newToDiagram0 ed)

newNewElement0 ::
	ElementIdable eid => eid -> NewElementDiagram0 -> Pos -> DiagramMapM ()
newNewElement0 eid ed pos = () <$ newElement eid (newToDiagram0 ed) pos

data NewElement1 a = NewElement1 a LinePos deriving Show

data NewElementDiagram1
	= NewNotGateD | NewDelayD Word8
	| NewHLineD | NewHLineTextD String String

newToDiagram1 :: NewElementDiagram1 -> ElementDiagram
newToDiagram1 NewNotGateD = notGateD
newToDiagram1 (NewDelayD d) = delayD d
newToDiagram1 NewHLineD = hLineD
newToDiagram1 (NewHLineTextD t1 t2) = hLineTextD t1 t2

newPutElementEnd1 :: ElementIdable eid =>
	eid -> NewElementDiagram1 -> DiagramMapM (Maybe (NewElement1 eid))
newPutElementEnd1 eid ed = do
	lps <- putElement0 eid $ newToDiagram1 ed
	return $ NewElement1 eid <$> lps

newPutElement1 :: ElementIdable eid => eid ->
	NewElementDiagram1 -> Pos -> DiagramMapM (Maybe (NewElement1 eid))
newPutElement1 eid ed pos = do
	lps <- putElement eid (newToDiagram1 ed) pos
	return $ NewElement1 eid <$> lps

newNewElementEnd1 :: ElementIdable eid =>
	eid -> NewElementDiagram1 -> DiagramMapM (NewElement1 eid)
newNewElementEnd1 eid ed = do
	lps <- newElement0 eid $ newToDiagram1 ed
	return $ NewElement1 eid lps

newNewElement1 :: ElementIdable eid =>
	eid -> NewElementDiagram1 -> Pos -> DiagramMapM (NewElement1 eid)
newNewElement1 eid ed pos = do
	lps <- newElement eid (newToDiagram1 ed) pos
	return $ NewElement1 eid lps

newInputPosition0 :: NewElement1 eid -> DiagramMapM Pos
newInputPosition0 (NewElement1 _ lps) = inputPosition lps

newConnectLine0 :: ElementIdable eid => NewElement1 eid -> eid -> DiagramMapM ()
newConnectLine0 (NewElement1 eid1 _) eid2 = connectLine eid1 eid2

data NewElement2 a = NewElement2 a LinePos deriving Show

data NewElementDiagram2
	=  NewAndGateD | NewOrGateD | NewTriGateD String String | NewBranchD

newToDiagram2 :: NewElementDiagram2 -> ElementDiagram
newToDiagram2 NewAndGateD = andGateD
newToDiagram2 NewOrGateD = orGateD
newToDiagram2 (NewTriGateD t1 t2) = triGateD t1 t2
newToDiagram2 NewBranchD = branchD

newPutElementEnd2 :: ElementIdable eid =>
	eid -> NewElementDiagram2 -> DiagramMapM (Maybe (NewElement2 eid))
newPutElementEnd2 eid ed = do
	lps <- putElement0 eid $ newToDiagram2 ed
	return $ NewElement2 eid <$> lps

newPutElement2 :: ElementIdable eid =>
	eid -> NewElementDiagram2 -> Pos -> DiagramMapM (Maybe (NewElement2 eid))
newPutElement2 eid ed pos = do
	lps <- putElement eid (newToDiagram2 ed) pos
	return $ NewElement2 eid <$> lps

newNewElementEnd2 :: ElementIdable eid =>
	eid -> NewElementDiagram2 -> DiagramMapM (NewElement2 eid)
newNewElementEnd2 eid ed = do
	lps <- newElement0 eid $ newToDiagram2 ed
	return $ NewElement2 eid lps

newNewElement2 :: ElementIdable eid =>
	eid -> NewElementDiagram2 -> Pos -> DiagramMapM (NewElement2 eid)
newNewElement2 eid ed pos = do
	lps <- newElement eid (newToDiagram2 ed) pos
	return $ NewElement2 eid lps

newInputPosition1, newInputPosition2 :: NewElement2 eid -> DiagramMapM Pos
newInputPosition1 (NewElement2 _ lps) = inputPosition1 lps
newInputPosition2 (NewElement2 _ lps) = inputPosition2 lps

newConnectLine1, newConnectLine2 :: ElementIdable eid => NewElement2 eid -> eid -> DiagramMapM ()
newConnectLine1 (NewElement2 eid1 _) eid2 = connectLine1 eid1 eid2
newConnectLine2 (NewElement2 eid1 _) eid2 = connectLine2 eid1 eid2
