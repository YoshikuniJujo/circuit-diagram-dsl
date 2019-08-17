{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Diagrams.Prelude (mkWidth)
import Diagrams.Backend.SVG (renderSVG)

import Circuit.DiagramDsl

main :: IO ()
main = case sample `execDiagramMapM` 2 of
	Right d -> renderSVG "andand.svg" (mkWidth 600) $ drawDiagram d
	Left emsg -> putStrLn $ "no diagram: " ++ emsg

eid0, eid1, eid2, eid3, eid4, eid5 :: ElementId
[eid0, eid1, eid2, eid3, eid4, eid5] = ["0", "1", "2", "3", "4", "5"]

sample :: DiagramMapM ()
sample = do
	lp <- newNewElementEnd2 eid0 NewAndGateD
	ip1 <- newInputPosition1 lp
	ip2 <- newInputPosition2 lp
	() <$ newNewElement2 eid1 NewAndGateD ip1
	newConnectLine1 lp eid1
	() <$ newNewElement2 eid2 NewAndGateD ip2
	newConnectLine2 lp eid2
