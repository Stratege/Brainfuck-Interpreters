import Data.Char (ord,chr)
import Data.Maybe

data State a = State [a] [a] a

parse str = flip fixBrackets 0 . fromList . stringToSymList $ str

fixBrackets st@(State l r (EndLoop 0)) n = fixBrackets (backToStart ((\(State l r a) -> (State l r (StartLoop n))) (multiple goLeft (State l r (EndLoop (-n))) n))) 0
fixBrackets st@(State l r (StartLoop 0)) n = fixBrackets (goRight st) 1
fixBrackets st@(State _ [] _) _ = backToStart st
fixBrackets st@(State l r a) n = fixBrackets (goRight st) (n+1)

backToStart st@(State l _ _) = multiple goLeft st (length l) 

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

data Symbol = GoLeft | GoRight | Inc | Dec | Print | Read | StartLoop Int | EndLoop Int | Error Int deriving (Show)

type ProcessStack = State Symbol


testPState = fromList [Print,GoRight,Print,GoRight,Print,Inc,Dec,GoRight,Inc,Print,GoRight,Print,GoRight,Print,GoRight,Print,GoRight,Print,GoRight,Print,GoRight,Print,GoRight,Print,GoRight,Print,GoRight,Print,GoRight,Print,GoRight,Print,GoRight,Print,GoRight,Print,GoRight,Print,GoRight,Print,GoRight,Print,GoRight,Print,GoRight,Print,GoRight,Print,GoRight,Print,GoRight]
testState = fromList ((map (fromChar) "Hello World") :: [Int])

fromList (x:xs) = (State [] xs x)
toList (State l r x) = l++(x:r)

interpretate pState state = do
	(newState,newPState) <- interpretateSym (getCurrentSym pState) state pState
	let done = (\(State _ r _) -> null r) newPState
	let newerPState = (goRight newPState)
	if done then (putStr "\ndone\n") else
		interpretate newerPState (return newState)


interpretateString str = interpretate (parse str) (return $ fromList [def :: Int])

getCurrentSym (State _ _ a) = a		

interpretateSym GoLeft state pState = f goLeft state pState
interpretateSym GoRight state pState = f goRight state pState
interpretateSym Inc state pState = f contentInc state pState
interpretateSym Dec state pState = f contentDec state pState
interpretateSym Print state pState = state >>= contentPrint >>= (\x -> return (x,pState))
interpretateSym Read state pState = state >>= contentRead >>= (\x -> return (x,pState))
interpretateSym (StartLoop n) state pState = state >>= (\x -> return (startLoop x pState n))
interpretateSym (EndLoop n) state pState = state >>= (\x -> return (endLoop x pState n))

f g x z = x >>= (\y -> return $ (g y, z))

goLeft :: Incrementable a => State a -> State a
goLeft (State l r a)
	| null l = (State [] (a:r) def)
	| otherwise = (State (tail l) (a:r) (head l))


goRight :: Incrementable a => State a -> State a
goRight (State l r a)
	| null r = (State (a:l) [] def)
	| otherwise = (State (a:l) (tail r) (head r))

contentInc :: Incrementable a => State a -> State a
contentInc (State l r a) = (State l r (inc a))

contentDec :: Incrementable a => State a -> State a
contentDec (State l r a) = (State l r (dec a))

contentPrint st@(State _ _ a) = (putChar . toChar $ a) >> return st
contentRead (State l r _) = getChar >>= (\x -> return (State l r (fromChar x)))

startLoop st pSt n = loopHelper (==) goRight st pSt n
endLoop st pSt n = loopHelper (/=) goLeft st pSt (-n)

loopHelper op f st@(State _ _ a) pSt n = if (op a def) then (st,newPSt) else (st,pSt)
	where newPSt = multiple f pSt n

multiple f st 0 = st
multiple f st n = multiple f (f st) (n -1)


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