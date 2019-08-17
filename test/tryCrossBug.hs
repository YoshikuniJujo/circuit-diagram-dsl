{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Diagrams.Prelude (mkWidth)
import Diagrams.Backend.SVG (renderSVG)

import Circuit.DiagramDsl

main :: IO ()
main = case sample `execDiagramMapM` 2 of
	Right d -> renderSVG "crossBug.svg" (mkWidth 600) $ drawDiagram d
	Left emsg -> putStrLn $ "no diagram: " ++ emsg

eid0, eid1, eid2, eid3, eid4, eid5 :: ElementId
[eid0, eid1, eid2, eid3, eid4, eid5] = ["0", "1", "2", "3", "4", "5"]

sample :: DiagramMapM ()
sample = do
	lp0 <- newNewElementEnd1 eid4 NewHLineD
	ip0 <- newInputPosition0 lp0
	lp <- newNewElementEnd1 eid0 NewHLineD
	ip <- newInputPosition0 lp
	lp1 <- newNewElement1 eid1 NewHLineD ip
	newConnectLine0 lp eid1
	newConnectLine0 lp1 eid1
	lp2 <- newNewElementEnd1 eid2 NewHLineD
	ip2 <- newInputPosition0 lp2
	newConnectLine0 lp2 eid1

	lp3 <- newNewElement1 eid3 NewHLineD ip2
	newConnectLine0 lp3 eid4
