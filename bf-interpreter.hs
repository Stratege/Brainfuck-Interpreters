import Prelude
import Data.Char (ord,chr)
import Data.Maybe
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Fold

data State a = State (Seq.Seq a) Int Int Bool Bool
data Symbol = GoLeft | GoRight | Inc | Dec | Print | Read | StartLoop Int | EndLoop Int | Error Int deriving (Show)

parse str = fromList . stringToSymList $ str

stringToSymList = map (fromJust) . filter (isJust) . map charToSym

charToSym '<' = Just GoLeft
charToSym '>' = Just GoRight
charToSym '+' = Just Inc
charToSym '-' = Just Dec
charToSym '.' = Just Print
charToSym ',' = Just Read
charToSym '[' = Just (StartLoop 0)
charToSym ']' = Just (EndLoop 0)
charToSym _ = Nothing

type ProcessStack = State Symbol


testPState = fromList [Print,GoRight,Print,GoRight,Print,Inc,Dec,GoRight,Inc,Print,GoRight,Print,GoRight,Print,GoRight,Print,GoRight,Print,GoRight,Print,GoRight,Print,GoRight,Print,GoRight,Print,GoRight,Print,GoRight,Print,GoRight,Print,GoRight,Print,GoRight,Print,GoRight,Print,GoRight,Print,GoRight,Print,GoRight,Print,GoRight,Print,GoRight,Print,GoRight,Print,GoRight]
testState = fromList ((map (fromChar) "Hello World") :: [Int])

fromList ls = (State (Seq.fromList ls) 0 0 False True)
toList (State ls _ _ _ _) = Fold.toList ls

interpretate pState state = do
		newState <- interpretateSym (getCurrentSym pState) state
		let h = (\(State l i b s f) -> (l,i,b,s,f)) newState
--		print (d,h)
		let newPState = if ((\(State _ _ _ _ f) -> f == True) newState) then (goRight pState) else (goLeft pState)
		let done = (\(State _ i _ _ _) (State l _ _ _ _) -> i >= Seq.length l) newPState pState
		if done then (putStr "\ndone\n") else
			interpretate newPState newState
	where	g (State l i _ _ _) = (Seq.length l, i)
		d = g pState


interpretateString str = interpretate (parse str) (fromList [def :: Int])


interpretateSym (StartLoop n) state = return $ startLoop state
interpretateSym (EndLoop n) state = return $ endLoop state
interpretateSym _ st@(State _ _ _ True _) = return st
interpretateSym GoLeft state = return $ goLeft state
interpretateSym GoRight state = return $ goRight state
interpretateSym Inc state = return $ contentInc state
interpretateSym Dec state = return $ contentDec state
interpretateSym Print state = contentPrint state 
interpretateSym Read state = contentRead state 


goLeft :: Incrementable a => State a -> State a
goLeft (State l i d s f)
	| i <= 0 = (State (def Seq.<| l) i d s f)
	| otherwise = (State l (i-1) d s f)


goRight :: Incrementable a => State a -> State a
goRight (State l i d s f)
	| (i+1) >= Seq.length l = (State (l Seq.|> def) (i+1) d s f)
	| otherwise = (State l (i+1) d s f)

getCurrentSym = accessContent
accessContent (State l i d s f) = Seq.index l i
updateContent st@(State l i d s f) func = (State (Seq.update i (func $ accessContent st) l) i d s f)

contentInc st = updateContent st inc
contentDec st = updateContent st dec

contentPrint st = (putChar . toChar $ accessContent st) >> return st
contentRead st = getChar >>= (\x -> return (updateContent st (\y -> fromChar x)))

startLoop st@(State l i d s f)
	| s == True && f == True = (State l i (d+1) s f)
	| s == True && f == False = if (d == 0) then (State l i d False True) else (State l i (d-1) s f)
	| s == False = if ((accessContent st) == def) then (State l i d True f) else st
endLoop st@(State l i d s f)
	| s == True && f == True = if (d == 0) then (State l i d False f) else (State l i (d-1) s f)
	| s == True && f == False = (State l i (d+1) s f)
	| s == False = if ((accessContent st) /= def) then (State l i d True False) else st


class Incrementable a where
	inc :: a -> a
	dec :: a -> a
	def :: a

instance Incrementable Int where
	inc x = if x < 255 then x + 1 else 0
	dec x = if x > 0 then x - 1 else 255
	def = 0

instance Incrementable Symbol where
	inc GoLeft = GoRight
	inc GoRight = Inc
	inc Inc = Dec
	inc Dec = Print
	inc Print = Read
	inc Read = (StartLoop 1)
	inc (StartLoop n) = (EndLoop (-1))
	inc (EndLoop n) = Error 1
	inc (Error (-1)) = GoLeft
	inc (Error n) = Error (n+1)

	dec GoLeft = Error (-1)
	dec GoRight = GoLeft
	dec Inc = GoRight
	dec Dec = Inc
	dec Print = Dec
	dec Read = Print
	dec (StartLoop n) = Read
	dec (EndLoop n) = StartLoop 1
	dec (Error 1) = EndLoop (-1)
	dec (Error n) = Error (n-1)

	def = GoLeft


class SingleLetterPrintable a where
	toChar :: a -> Char
	fromChar :: Char -> a

instance SingleLetterPrintable Int where
	toChar = chr
	fromChar = ord

instance SingleLetterPrintable Symbol where
	fromChar = fromJust . charToSym
	toChar GoLeft = '<'
	toChar GoRight = '>'
	toChar Inc = '+'
	toChar Dec = '-'
	toChar Print = '.'
	toChar Read = ','
	toChar (StartLoop n) = '['
	toChar (EndLoop n) = ']'
	toChar (Error n) = '?'

main = readFile "input.txt" >>= interpretateString