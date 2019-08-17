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
	elementIdGen e = convert . hash @_ @SHA3_256 . pack $ pfx ++ show n
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
	lp0 <- newNewElementEnd1 (NotGate 0) NewNotGateD
	ip0 <- newInputPosition0 lp0
	lp1 <- newNewElement1 (Caption 0) (NewHLineTextD "31:16" "63:32") ip0
	ip1 <- newInputPosition0 lp1
	lp2 <- newNewElement1 (NotGate 1) NewNotGateD ip1
	newConnectLine0 lp0 (Caption 0)
	newConnectLine0 lp1 (NotGate 1)
	newConnectLine0 lp2 (NotGate 1)

	lp2 <- newNewElementEnd1 (NotGate 2) NewNotGateD
	ip2 <- newInputPosition0 lp2
	lp3 <- newNewElement2 (Branch 0) NewBranchD ip2
	newConnectLine0 lp2 (NotGate 2)
	newConnectLine1 lp3 (NotGate 2)

	ip3 <- newInputPosition2 lp3
	lp4 <- newNewElement2 (TriGate 0) (NewTriGateD "0:0" "63:0") ip3
	newConnectLine2 lp3 (TriGate 0)
	ip4 <- newInputPosition1 lp4
	ip5 <- newInputPosition2 lp4
	lp6 <- newNewElement1 (NotGate 3) NewNotGateD ip4
	ip6 <- newInputPosition0 lp6
	lp7 <- newNewElement1 (NotGate 4) NewNotGateD ip5
	ip7 <- newInputPosition0 lp7
	newConnectLine1 lp4 (NotGate 3)
	newConnectLine2 lp4 (NotGate 4)

	() <$ newNewElement0 (ConstGate 0) (NewConstGateD 0x123456789abcdef0) ip6
	newConnectLine0 lp6 (ConstGate 0)

	lp8 <- newNewElement1 (Delay 0) (NewDelayD 255) ip7
	ip8 <- newInputPosition0 lp8
	newConnectLine0 lp7 (Delay 0)

	lp9 <- newNewElement1 (NotGate 5) NewNotGateD ip8
	ip9 <- newInputPosition0 lp9
	newConnectLine0 lp8 (NotGate 5)

	() <$ newNewElement1 (IdGate 0) NewHLineD ip9
	newConnectLine0 lp9 (IdGate 0)
