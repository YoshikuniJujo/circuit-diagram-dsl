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
	lp0 <- newElementEnd1 eid4 HLineD
	ip0 <- inputPosition0 lp0
	lp <- newElementEnd1 eid0 HLineD
	ip <- inputPosition0 lp
	lp1 <- newElement1 eid1 HLineD ip
	connectLine0 lp eid1
	connectLine0 lp1 eid1
	lp2 <- newElementEnd1 eid2 HLineD
	ip2 <- inputPosition0 lp2
	connectLine0 lp2 eid1

	lp3 <- newElement1 eid3 HLineD ip2
	connectLine0 lp3 eid4
