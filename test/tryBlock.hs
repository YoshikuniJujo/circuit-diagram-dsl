{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Diagrams.Prelude (mkWidth)
import Diagrams.Backend.SVG (renderSVG)

import Circuit.DiagramForTest

main :: IO ()
main = case sample `execDiagramMapM` 3 of
	Right d -> renderSVG "block.svg" (mkWidth 600) $ drawDiagram d
	Left emsg -> putStrLn $ "no diagram: " ++ emsg

eid0, eid1, eid2, eid3, eid4, eid5 :: ElementId
[eid0, eid1, eid2, eid3, eid4, eid5] = ["0", "1", "2", "3", "4", "5"]

sample :: DiagramMapM ()
sample = do
	mlp <- putElementGen True [eid0, eid1, eid2, eid3] (blockD 3 4 "foo") 5 (Just 0)
	case mlp of
		Just lp -> do
			Pos x y <- inputPositionMulti 1 lp
			putElementGen False [eid4] andGateD x (Just y)
			connectLineMulti 1 eid0 eid4
			connectLine1 eid4 eid1
			connectLineMulti 2 eid1 eid3
			connectLineMulti 0 eid2 eid2
			connectLine2 eid4 eid0
			return ()
