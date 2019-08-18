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
	lp0 <- newElementEnd eid0 notGateD
	ip0 <- inputPosition0 lp0
	lp1 <- newElement eid100 (hLineTextD "63:32" "31:0") ip0
	ip1 <- inputPosition0 lp1
	lp2 <- newElement eid1 andGateD ip1
	ip2 <- inputPosition2 lp2
	il3 <- newElement eid102 branchD ip2
	ip3 <- inputPosition1 il3
	ip4 <- inputPosition2 il3
	connectLine0 lp0 eid100
	connectLine0 lp1 eid1
	connectLine2 lp2 eid102
	lp5 <- newElement eid103 (hLineTextD "62:0" "63:1") ip3
	ip5 <- inputPosition0 lp5
	lp6 <- newElement eid104 (hLineTextD "0:0" "0:0") ip4
	ip6 <- inputPosition0 lp6
	connectLine1 il3 eid103
	connectLine2 il3 eid104
	() <$ newElement eid5 notGateD ip5
	() <$  newElement eid6 notGateD ip6
	connectLine0 lp5 eid5
	connectLine0 lp6 eid6

	il7 <- newElementEnd eid2 notGateD
	ip7 <- inputPosition0 il7
	il8 <- newElement eid101 branchD ip7
	connectLine0 il7 eid2
	connectLine1 il8 eid2
