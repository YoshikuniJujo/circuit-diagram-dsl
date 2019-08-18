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
	lp0 <- newElementEnd1 eid0 notGateD
	ip0 <- inputPosition0 lp0
	lp1 <- newElement2 eid1 andGateD ip0
	ip1 <- inputPosition2 lp1
	lp2 <- newElement2 eid2 orGateD ip0
	() <$ newElement2 eid3 andGateD ip1
	connectLine0 lp0 eid2
	connectLine1 lp2 eid3
	connectLine2 lp2 eid2
	connectLine2 lp1 eid3
	() <$ newElement1 eid4 notGateD ip0

	() <$ newElementEnd2 eid5 (triGateD "0:0" "63:0")
