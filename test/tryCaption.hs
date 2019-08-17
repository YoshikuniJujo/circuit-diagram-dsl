{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Diagrams.Prelude (mkWidth)
import Diagrams.Backend.SVG (renderSVG)

import Circuit.DiagramDsl

main :: IO ()
main = case sample2 `execDiagramMapM` 2 of
	Right s2 -> renderSVG "caption.svg" (mkWidth 1000) $ drawDiagram s2
	Left emsg -> putStrLn $ "Can't draw diagram: " ++ emsg

eid0, eid1, eid2, eid5, eid6, eid100, eid101, eid102, eid103, eid104 :: ElementId
[eid0, eid1, eid2, eid5, eid6, eid100, eid101, eid102, eid103, eid104] =
	["0", "1", "2", "5", "6", "100", "101", "102", "103", "104"]


sample2 :: DiagramMapM ()
sample2 = do
	lp0 <- newElementEnd1 eid0 NotGateD
	ip0 <- inputPosition0 lp0
	lp1 <- newElement1 eid100 (HLineTextD "63:32" "31:0") ip0
	ip1 <- inputPosition0 lp1
	lp2 <- newNewElement2 eid1 AndGateD ip1
	ip2 <- newInputPosition2 lp2
	il3 <- newNewElement2 eid102 BranchD ip2
	ip3 <- newInputPosition1 il3
	ip4 <- newInputPosition2 il3
	connectLine0 lp0 eid100
	connectLine0 lp1 eid1
	newConnectLine2 lp2 eid102
	lp5 <- newElement1 eid103 (HLineTextD "62:0" "63:1") ip3
	ip5 <- inputPosition0 lp5
	lp6 <- newElement1 eid104 (HLineTextD "0:0" "0:0") ip4
	ip6 <- inputPosition0 lp6
	newConnectLine1 il3 eid103
	newConnectLine2 il3 eid104
	() <$ newElement1 eid5 NotGateD ip5
	() <$  newElement1 eid6 NotGateD ip6
	connectLine0 lp5 eid5
	connectLine0 lp6 eid6

	il7 <- newElementEnd1 eid2 NotGateD
	ip7 <- inputPosition0 il7
	il8 <- newNewElement2 eid101 BranchD ip7
	connectLine0 il7 eid2
	newConnectLine1 il8 eid2
