{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Diagrams.Prelude (mkWidth)
import Diagrams.Backend.SVG (renderSVG)

import Circuit.DiagramDsl

main :: IO ()
main = case sample2 `execDiagramMapM` 2 of
	Right s2 -> renderSVG "sample5.svg" (mkWidth 600) $ drawDiagram s2
	Left emsg -> putStrLn $ "no diagram: " ++ emsg

eid0, eid1, eid2, eid3, eid4, eid5 :: ElementId
[eid0, eid1, eid2, eid3, eid4, eid5] = ["0", "1", "2", "3", "4", "5"]

sample2 :: DiagramMapM ()
sample2 = do
	lp0 <- newNewElementEnd1 eid0 NewNotGateD
	ip0 <- newInputPosition0 lp0
	lp1 <- newNewElement2 eid1 NewAndGateD ip0
	ip1 <- newInputPosition2 lp1
	lp2 <- newNewElement2 eid2 NewOrGateD ip0
	() <$ newNewElement2 eid3 NewAndGateD ip1
	newConnectLine0 lp0 eid2
	newConnectLine1 lp2 eid3
	newConnectLine2 lp2 eid2
	newConnectLine2 lp1 eid3
	() <$ newNewElement1 eid4 NewNotGateD ip0

	() <$ newNewElementEnd2 eid5 (NewTriGateD "0:0" "63:0")
