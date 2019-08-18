{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Diagrams.Prelude (mkWidth)
import Diagrams.Backend.SVG (renderSVG)

import Circuit.DiagramDsl

main :: IO ()
main = case sample `execDiagramMapM` 2 of
	Right d -> renderSVG "triGate.svg" (mkWidth 600) $ drawDiagram d
	Left emsg -> putStrLn $ "no diagram: " ++ emsg

eid0, eid1, eid2, eid3, eid4, eid5 :: ElementId
[eid0, eid1, eid2, eid3, eid4, eid5] = ["0", "1", "2", "3", "4", "5"]

sample :: DiagramMapM ()
sample = do
	lp <- newElementEnd eid0 (triGateD "0:0" "63:0")
	ip <- inputPosition1 lp
	() <$ newElement eid1 hLineD ip
	connectLine1 lp eid1
