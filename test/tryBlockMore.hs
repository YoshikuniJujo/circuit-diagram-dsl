{-# LANGUAGE OverloadedStrings, TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Diagrams.Prelude (mkWidth)
import Diagrams.Backend.SVG (renderSVG)

import Circuit.DiagramDsl

main :: IO ()
main = case sample `execDiagramMapM` 2 of
	Right d -> renderSVG "blockMore.svg" (mkWidth 600) $ drawDiagram d
	Left emsg -> putStrLn $ "no diagram: " ++ emsg

eid0, eid1, eid2, eid3, eid4, eid5, eid6, eid7,
	eid10, eid11, eid12, eid13, eid14, eid15 :: ElementId
[eid0, eid1, eid2, eid3, eid4, eid5, eid6, eid7] = ["0", "1", "2", "3", "4", "5", "6", "7"]
[eid10, eid11, eid12, eid13, eid14, eid15] = ["10", "11", "12", "13", "14", "15"]

sample :: DiagramMapM ()
sample = do
	newElementBlockEnd [eid0] (blockD 1 "foo")
--	newElementBlockEnd @ElementId [] (blockD 0 "bar")
	newElementBlockEnd @ElementId [eid1] (blockD 0 "bar")
	bbb <- newElementBlockEnd [eid2, eid3, eid4] (blockD 5 "bbb")
	lp <- newElementEnd eid10 notGateD
	ip <- inputPosition0 lp
	lp2 <- newElement eid11 orGateD ip
	ip2 <- inputPosition2 lp2
	connectLine0 lp eid11
	hoge <- newElementBlock [eid5, eid6] (blockD 1 "hoge")  ip2
	connectLine2 lp2 eid5
	connectLine1 lp2 eid3
	connectLineBlock 4 bbb eid6
	connectLineBlock 3 bbb eid6
	connectLineBlock 0 hoge eid4
