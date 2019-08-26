{-# LANGUAGE TupleSections, TypeFamilies, TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Circuit.Diagram.Map (
	DiagramMapM, DiagramMapState, runDiagramMapM, execDiagramMapM,
	DiagramMap, ElementIdable(..), ElementId,
	ElementDiagram,
	andGateD, orGateD, notGateD, triGateD, constGateD, delayD,
	hLineD, hLineTextD, branchD, blockD,
	Pos(..), LinePos,
	putElementGen,
	inputPosition, inputPosition1, inputPosition2, inputPositionMulti,
	connectLine, connectLine1, connectLine2, connectLineMulti,
	getSpace ) where

import Prelude as P

import Control.Arrow
import Control.Monad.State
import Data.Maybe
import Data.List (transpose)
import Data.Map.Strict
import Data.Bool
import Data.Word
import Data.String
import Crypto.Hash

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteArray as BA

import AStar.AStar
import Circuit.Diagram.DiagramMap hiding (stump)

import qualified Circuit.Diagram.DiagramMap as DM

andGateD, orGateD, notGateD, hLineD, branchD :: ElementDiagram
[andGateD, orGateD, notGateD, hLineD, branchD] =
	[AndGateE, OrGateE, NotGateE, HLine, BranchE]

triGateD :: String -> String -> ElementDiagram
triGateD = TriGateE

constGateD :: Word64 -> ElementDiagram
constGateD = ConstGateE

delayD :: Word8 -> ElementDiagram
delayD = DelayE

hLineTextD :: String -> String -> ElementDiagram
hLineTextD = HLineText

blockD :: Int -> Int -> String -> ElementDiagram
blockD = BlockE

newtype ElementId = ElementId BS.ByteString deriving (Show, Eq, Ord)

instance IsString ElementId where
	fromString = ElementId . BA.convert . hash @_ @SHA3_256 . BSC.pack

class ElementIdable a where
	elementId :: a -> BS.ByteString

elementId' :: ElementIdable eid => eid -> ElementId
elementId' = ElementId . elementId

instance ElementIdable ElementId where
	elementId (ElementId bs) = bs

data DiagramMapState = DiagramMapState {
	space :: Int,
	place :: Map Int Int,
	elementPos :: Map ElementId LinePos,
	diagramMap :: DiagramMap }
	deriving Show

initDiagramMapState :: Int -> DiagramMapState
initDiagramMapState sp = DiagramMapState {
	space = sp + 2,
	place = empty,
	elementPos = empty,
	diagramMap = mkDiagramMap 0 0 }

updatePlaceDMState :: DiagramMapState -> Int -> Int -> DiagramMapState
updatePlaceDMState dms x y =
	dms { place = insert x (maybe y (y `max`) $ pl !? x) pl }
	where pl = place dms

getWidthDMState, getHeightDMState :: DiagramMapState -> Int
getWidthDMState = getWidthDiagramMap . diagramMap
getHeightDMState = getHeightDiagramMap . diagramMap

setWidthDMState, setHeightDMState :: DiagramMapState -> Int -> DiagramMapState
setWidthDMState dms w =
	dms { diagramMap = setWidthDiagramMap (diagramMap dms) w }
setHeightDMState dms h =
	dms { diagramMap = setHeightDiagramMap (diagramMap dms) h }

type DiagramMapM = StateT DiagramMapState (Either String)

getSpace :: DiagramMapM Int
getSpace = gets space

getDiagramMap :: DiagramMapM DiagramMap
getDiagramMap = gets diagramMap

getWidth, getHeight :: DiagramMapM Int
getWidth = gets getWidthDMState
getHeight = gets getHeightDMState

setWidth, setHeight :: Int -> DiagramMapM ()
setWidth = modify . flip setWidthDMState
setHeight = modify . flip setHeightDMState

expandWidth, expandHeight :: Int -> DiagramMapM ()
expandWidth w = setWidth . max w =<< getWidth
expandHeight h = setHeight . max h =<< getHeight

updatePlace :: Int -> Int -> DiagramMapM ()
updatePlace = (modify .) . curry (flip $ uncurry . updatePlaceDMState)

inputPosition :: LinePos -> DiagramMapM Pos
inputPosition lp = lift . calcInputPosition lp =<< getSpace

inputPosition1, inputPosition2 :: LinePos -> DiagramMapM Pos
inputPosition1 lp = lift . calcInputPosition1 lp =<< getSpace
inputPosition2 lp = lift . calcInputPosition2 lp =<< getSpace

inputPositionMulti :: Int -> LinePos -> DiagramMapM Pos
inputPositionMulti i lp = lift . calcInputPositionMulti i lp =<< getSpace

getElementFromPos :: Pos -> DiagramMapM (Maybe ElementDiagram)
getElementFromPos pos = do
	dm <- getDiagramMap
	return $ layout dm !? pos

runDiagramMapM :: DiagramMapM a -> Int -> Either String (a, DiagramMap)
runDiagramMapM dmm sp =
	second diagramMap <$> dmm `runStateT` initDiagramMapState sp

execDiagramMapM :: DiagramMapM a -> Int -> Either String DiagramMap
execDiagramMapM dmm sp =
	diagramMap <$> dmm `execStateT` initDiagramMapState sp

maybeMaximum :: Ord a => [Maybe a] -> Maybe a
maybeMaximum [] = Nothing
maybeMaximum (Just x : xs) = Just $ maybe x (max x) $ maybeMaximum xs
maybeMaximum (Nothing : xs) = maybeMaximum xs

calculateY :: ElementDiagram -> Int -> Int -> Maybe Int -> DiagramMapM Int
calculateY e x_ x my_ = do
	stt <- get
	my <- case my_ of
		Just y -> bool Nothing my_ . ((y > h) &&) <$> placeable e (Pos x y)
		Nothing -> return Nothing
	let	y = case maybeMaximum $ (place stt !?) <$> [x_ .. x + w + 1] of
			Just yy -> yy
			Nothing -> 0
	sp <- getSpace
	return $ fromMaybe (y + h + sp) my
	where ((w, (h, _h')), _ps) = elementSpace e

putE :: Pos -> ElementDiagram -> DiagramMapM ()
putE p@(Pos x y) e = do
	stt <- get
	let	dm = diagramMap stt
		l = layout dm
		l' = insert p e l
	put stt { diagramMap = dm { layout = l' } }
	updatePlace x y

stump :: Pos -> ElementDiagram -> DiagramMapM ()
stump p e = do
	stt <- get
	let	dm = diagramMap stt
		l = layout dm
		l' = DM.stump e p l
	put stt { diagramMap = dm { layout = l' } }

putElementPos :: ElementIdable eid => eid -> LinePos -> DiagramMapM ()
putElementPos eid lp = do
	stt <- get
	put stt { elementPos = insert (elementId' eid) lp $ elementPos stt }

putElementGen :: ElementIdable eid => Bool -> [eid] -> ElementDiagram -> Int -> Maybe Int -> DiagramMapM (Maybe LinePos)
putElementGen _ [] _ _ _ = return Nothing
putElementGen b eids e x_ my_ = do
	me <- gets ((!? elementId' (head eids)) . elementPos)
	(\pe -> maybe pe (const $ return Nothing) me) $ do
		y <- calculateY e x_ x my_
		let	p = Pos x y
		lps <- lift $ linePosMulti e p
		zipWithM_ putElementPos eids lps
		putE p e >> stump p e
		putMoreLine (length eids) (length eids - 1) p
		when b $ putEnd (length eids) (length eids - 1) p
		updatePlaceAndExpand p e
		return $ listToMaybe lps
	where
	x = x_ + length eids - 1

putMoreLine :: Int -> Int -> Pos -> DiagramMapM ()
putMoreLine _ n _ | n < 1 = return ()
putMoreLine m n (Pos x y) = do
	mapM_ (\dx -> putE (Pos (x - dx - 1) (y + n)) HLine) [0 .. m - 1]
	putMoreLine m (n - 1) (Pos x y)

putEnd :: Int -> Int -> Pos -> DiagramMapM ()
putEnd _ n _ | n < 0 = return ()
putEnd m n (Pos x y) = do
	mapM_ (\dx -> putE (Pos (x - dx - 1) (y + n)) HLine) [m]
	putEnd m (n - 1) (Pos x y)

updatePlaceAndExpand :: Pos -> ElementDiagram -> DiagramMapM ()
updatePlaceAndExpand (Pos x y) e = do
	sp <- getSpace
	(`updatePlace` (y + h')) `mapM_` [x .. x + w - 1]
	expandWidth $ x + w + sp
	expandHeight $ y + h' + sp
	where ((w, (_, h')), _) = elementSpace e

getElementPos :: ElementIdable eid => eid -> DiagramMapM LinePos
getElementPos eidg = lift
	=<< gets (maybe (Left emsg) Right . (!? elementId' eidg) . elementPos)
	where emsg = "No such element: " ++
		"Circuit.Diagram.Map.getElementPos " ++ show (elementId' eidg)

getInputPos :: ElementIdable eid => eid -> DiagramMapM [Pos]
getInputPos = (inputLinePos <$>) . getElementPos

addElementOutputPos :: ElementId -> [Pos] -> DiagramMapM ()
addElementOutputPos eid ps = do
	st <- get
	let	eps = elementPos st
	lps <- lift $ maybe
		(Left $ "addElementOutputPos " ++ show eid ++ " " ++ show ps)
		Right $ eps !? eid
	let	lps' = lps { outputLinePos = outputLinePos lps ++ ps }
		eps' = insert eid lps' eps
	put $ st { elementPos = eps' }

placeable :: ElementDiagram -> Pos -> DiagramMapM Bool
placeable e pos = and <$> placeablePos `mapM` elementToPositions e pos

placeablePos :: Pos -> DiagramMapM Bool
placeablePos pos = isNothing <$> getElementFromPos pos

data LinePos = LinePos { outputLinePos :: [Pos], inputLinePos :: [Pos] }
	deriving Show

calcInputPosition, calcInputPosition1, calcInputPosition2 :: LinePos -> Int -> Either String Pos
calcInputPosition LinePos { inputLinePos = [ip] } dx = Right $ Pos (posX ip + dx) (posY ip)
calcInputPosition lp dx = Left $ "calcInputPosition " ++ show lp ++ " " ++ show dx

calcInputPosition1 LinePos { inputLinePos = [ip, _] } dx = Right $ Pos (posX ip + dx) (posY ip)
calcInputPosition1 lp dx = Left $ "calcInputPosition1 " ++ show lp ++ " " ++ show dx

calcInputPosition2 LinePos { inputLinePos = [_, ip] } dx = Right $ Pos (posX ip + dx) (posY ip)
calcInputPosition2 lp dx = Left $ "calcInputPosition2 " ++ show lp ++ " " ++ show dx

calcInputPositionMulti :: Int -> LinePos -> Int -> Either String Pos
calcInputPositionMulti i LinePos { inputLinePos = ilp } dx = Right $ Pos (posX (ilp !! i) + dx) (posY (ilp !! i))

succYs :: [Pos] -> [[Pos]]
succYs ps = transpose $ succYs1 <$> ps

succYs1 :: Pos -> [Pos]
succYs1 (Pos x y) = Pos x <$> [y ..]

linePosMulti :: ElementDiagram -> Pos -> Either String [LinePos]
linePosMulti e = either Left
	(Right . (\(LinePos op ip) -> (`LinePos` ip) <$> succYs op)) . linePos e

linePos :: ElementDiagram -> Pos -> Either String LinePos
linePos AndGateE (Pos x y) = Right LinePos {
	outputLinePos = [Pos (x - 1) y],
	inputLinePos = [Pos (x + 3) (y - 1), Pos (x + 3) (y + 1)] }
linePos OrGateE p = linePos AndGateE p
linePos NotGateE (Pos x y) =
	Right LinePos { outputLinePos = [Pos (x - 1) y], inputLinePos = [Pos (x + 2) y] }
linePos (TriGateE _ _) (Pos x y) =
	Right LinePos {
		outputLinePos = [Pos (x - 1) y],
		inputLinePos = [Pos (x + 2) (y - 3), Pos (x + 2) y] }
linePos (ConstGateE _) (Pos x y) =
	Right LinePos { outputLinePos = [Pos (x - 1) y], inputLinePos = [] }
linePos (DelayE _) (Pos x y) =
	Right LinePos { outputLinePos = [Pos (x - 1) y], inputLinePos = [Pos (x + 2) y] }
linePos HLine (Pos x y) =
	Right LinePos { outputLinePos = [Pos (x - 1) y], inputLinePos = [Pos (x + 1) y] }
linePos (HLineText _ _) (Pos x y) =
	Right LinePos { outputLinePos = [Pos (x - 1) y], inputLinePos = [Pos (x + 1) y] }
linePos BranchE (Pos x y) =
	Right LinePos {
		outputLinePos = [Pos (x - 1) y],
		inputLinePos = [Pos (x + 1) y, Pos (x + 1) (y + 3)] }
linePos (BlockE is os _) (Pos x y) =
	Right LinePos {
		outputLinePos = (`Pos` y) <$> [x - os - 1 .. x - 1],
		inputLinePos = Pos (x + 2) <$> [y, y + 2 .. y + (is - 1) * 2] }
linePos e pos = Left $ "linePos " ++ show e ++ " " ++ show pos

data DiagramMapAStar = DiagramMapAStar {
	startLine :: Pos, endLine :: [Pos], diagramMapA :: DiagramMap }
	deriving Show

distance :: [Pos] -> Pos -> Dist
distance ps p = minimum $ distance1 p <$> ps

distance1 :: Pos -> Pos -> Dist
distance1 (Pos x y) (Pos x' y') = fromIntegral $ abs (x - x') + abs (y - y')

instance AStar DiagramMapAStar where
	type AStarNode DiagramMapAStar = Pos
	type AStarDist DiagramMapAStar = Word
	startNode = startLine
	isEndNode = flip elem . endLine
	nextNodes = nextPosDiagramMap
	distToEnd = distance . endLine

nextPosDiagramMap :: DiagramMapAStar -> Pos -> [(Pos, Dist)]
nextPosDiagramMap dma (Pos x0 y0) = [ pd |
	p@(Pos x y) <- [Pos (x0 - 1) y0, Pos (x0 + 1) y0, Pos x0 (y0 - 1), Pos x0 (y0 + 1)],
	0 <= x, x < w, 0 <= y, y < h,
	pd <-	[ (p, 1) | isEndNode dma p ] ++
		[ pd' | y == y0, pd' <- maybeToList $ horizontal dma l (x - x0) p 1 ] ++
		[ pd' | x == x0, pd' <- maybeToList $ vertical dma l (y - y0) p 1 ] ]
	where
	dm = diagramMapA dma
	l = layout dm
	w = width dm
	h = height dm

horizontal, vertical :: DiagramMapAStar -> Map Pos ElementDiagram -> Int -> Pos -> Dist -> Maybe (Pos, Dist)
horizontal dma l dr p@(Pos x y) ds = case l !? p of
	Just VLine
		| isEndNode dma p' -> Just (p', ds + 1)
		| otherwise -> horizontal dma l dr p' (ds + 1)
	Just _ -> Nothing
	Nothing -> Just (p, ds)
	where p' = Pos (x + dr) y

vertical dma l dr p@(Pos x y) ds = case  l !? p of
	Just HLine
		| isEndNode dma p' -> Just (p', ds + 1)
		| otherwise -> vertical dma l dr p' (ds + 1)
	Just EndHLine
		| isEndNode dma p' -> Just (p', ds + 1)
		| otherwise -> vertical dma l dr p' (ds + 1)
	Just _ -> Nothing
	Nothing -> Just (p, ds)
	where p' = Pos x (y + dr)

connectLine, connectLine1, connectLine2 :: ElementIdable eid => eid -> eid -> DiagramMapM ()
connectLine ei eo = (`connectLine'` eo) =<< lift . single =<< getInputPos ei
connectLine1 ei eo = (`connectLine'` eo) =<< lift . oneOfTwo =<< getInputPos ei
connectLine2 ei eo = (`connectLine'` eo) =<< lift . twoOfTwo =<< getInputPos ei

connectLineMulti :: ElementIdable eid => Int -> eid -> eid -> DiagramMapM ()
connectLineMulti n ei eo = (`connectLine'` eo) . (!! n) =<< getInputPos ei

single :: [a] -> Either String a
single [x] = Right x
single _ = Left $ "Circuit.Diagram.Map.single: not single element list"

oneOfTwo, twoOfTwo :: [a] -> Either String a
oneOfTwo [x, _] = Right x
oneOfTwo _ = Left
	$ "Circuit.Diagram.Map.oneOfTwo xs: xs should be include just two elems"

twoOfTwo [_, y] = Right y
twoOfTwo _ = Left
	$ "Circuit.Diagram.Map.twoOfTwo xs: xs should be include just two elems"

connectLine' :: ElementIdable eid => Pos -> eid -> DiagramMapM ()
connectLine' p1 eidg = do
	p2 <- outputLinePos <$> getElementPos eidg
	ps <- connectLineGen p1 p2
	addElementOutputPos (elementId' eidg) ps

connectLineGen :: Pos -> [Pos] -> DiagramMapM [Pos]
connectLineGen p1 p2 = do
	stt <- get
	let	dm = diagramMap stt
		l = layout dm
	ps <- lift $ maybe (Left $ emsg dm) Right $ astar DiagramMapAStar {
		startLine = p1, endLine = p2, diagramMapA = dm }
	l' <- lift $ insertLine (processPos ps) l
	put stt { diagramMap = dm { layout = l' } }
	(\(Pos x y) -> updatePlace x y) `mapM_` processPos ps
	return $ processPos ps
	where emsg = ("astar: no route: " ++) . show
