{-# LANGUAGE OverloadedStrings, TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Data.ByteArray (convert)
import Data.ByteString.Char8 (pack)
import Diagrams.Prelude (mkWidth)
import Diagrams.Backend.SVG (renderSVG)

import Circuit.DiagramDsl
import Crypto.Hash (hash, SHA3_256)

main :: IO ()
main = case execDiagramMapM circuitDiagram 3 of
	Right cd -> renderSVG "simple.svg" (mkWidth 800) $ drawDiagram cd
	Left emsg -> putStrLn $ "Can't draw diagram: " ++ emsg

data Elem
	= NotGate Word | TriGate Word | ConstGate Word | Delay Word
	| Caption Word | Branch Word | IdGate Word
	deriving Show

instance ElementIdable Elem where
	elementId e = convert . hash @_ @SHA3_256 . pack $ pfx ++ show n
		where (pfx, n) = case e of
			NotGate n' -> ("NotGate", n')
			TriGate n' -> ("TriGate", n')
			ConstGate n' -> ("ConstGAte", n')
			Delay n' -> ("Delay", n')
			Caption n' -> ("Caption", n')
			Branch n' -> ("Branch", n')
			IdGate n' -> ("IdGate", n')

circuitDiagram :: DiagramMapM ()
circuitDiagram = do
	lp0 <- newElementEnd1 (NotGate 0) NotGateD
	ip0 <- inputPosition0 lp0
	lp1 <- newElement1 (Caption 0) (HLineTextD "31:16" "63:32") ip0
	ip1 <- inputPosition0 lp1
	lp2 <- newElement1 (NotGate 1) NotGateD ip1
	connectLine0 lp0 (Caption 0)
	connectLine0 lp1 (NotGate 1)
	connectLine0 lp2 (NotGate 1)

	lp2 <- newElementEnd1 (NotGate 2) NotGateD
	ip2 <- inputPosition0 lp2
	lp3 <- newNewElement2 (Branch 0) BranchD ip2
	connectLine0 lp2 (NotGate 2)
	newConnectLine1 lp3 (NotGate 2)

	ip3 <- newInputPosition2 lp3
	lp4 <- newNewElement2 (TriGate 0) (TriGateD "0:0" "63:0") ip3
	newConnectLine2 lp3 (TriGate 0)
	ip4 <- newInputPosition1 lp4
	ip5 <- newInputPosition2 lp4
	lp6 <- newElement1 (NotGate 3) NotGateD ip4
	ip6 <- inputPosition0 lp6
	lp7 <- newElement1 (NotGate 4) NotGateD ip5
	ip7 <- inputPosition0 lp7
	newConnectLine1 lp4 (NotGate 3)
	newConnectLine2 lp4 (NotGate 4)

	() <$ newElement0 (ConstGate 0) (ConstGateD 0x123456789abcdef0) ip6
	connectLine0 lp6 (ConstGate 0)

	lp8 <- newElement1 (Delay 0) (DelayD 255) ip7
	ip8 <- inputPosition0 lp8
	connectLine0 lp7 (Delay 0)

	lp9 <- newElement1 (NotGate 5) NotGateD ip8
	ip9 <- inputPosition0 lp9
	connectLine0 lp8 (NotGate 5)

	() <$ newElement1 (IdGate 0) HLineD ip9
	connectLine0 lp9 (IdGate 0)
