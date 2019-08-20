{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Circuit.DiagramForTest (
	drawDiagram,
	DiagramMapM, execDiagramMapM, ElementId, Pos(..),
	andGateD, blockD,
	putElementGen,
	inputPositionMulti,
	connectLine1, connectLine2, connectLineMulti
	) where
	

import Circuit.Diagram.Draw.Draw
import Circuit.Diagram.Map
