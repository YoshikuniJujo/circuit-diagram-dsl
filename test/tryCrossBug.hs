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
	ip0 <- inputPosition =<< newElement0 eid4 hLineD
	lp <- newElement0 eid0 hLineD
	ip <- inputPosition lp
	() <$ newElement eid1 hLineD ip
	connectLine eid0 eid1
	connectLine eid1 eid1
	ip2 <- inputPosition =<< newElement0 eid2 hLineD
	connectLine eid2 eid1

	() <$ newElement eid3 hLineD ip2
	connectLine eid3 eid4
