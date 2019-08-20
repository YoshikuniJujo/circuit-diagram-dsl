{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Circuit.DiagramForTest (
	drawDiagram,
	DiagramMapM, DiagramMapState,
	execDiagramMapM, ElementId, Pos(..),
	ElementDiagram, andGateD, blockD,
	LinePos,
	putElementGen,
	inputPositionMulti,
	connectLine1, connectLine2, connectLineMulti
	) where
	

import Circuit.Diagram.Draw.Draw
import Circuit.Diagram.Map
