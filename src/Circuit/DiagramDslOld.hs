{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Circuit.DiagramDslOld (
	drawDiagram,
	DiagramMapM, DiagramMapState, runDiagramMapM, execDiagramMapM,
	DiagramMap,
	ElementIdable(..), ElementId,
	ElementDiagram,
		andGateD, orGateD, notGateD, triGateD, constGateD, delayD,
		hLineD, hLineTextD, branchD,
	Pos, LinePos, putElement0, putElement, newElement0, newElement,
	inputPosition, inputPosition1, inputPosition2,
	connectLine, connectLine1, connectLine2,
	) where

import Circuit.Diagram.Draw
import Circuit.Diagram.Map
