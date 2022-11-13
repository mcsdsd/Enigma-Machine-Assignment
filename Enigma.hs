{-- Stub for the grading assignment. Fill it in, making sure you use good
 -- functional style, and add comments (including replacing those that are
 -- already here).
--}

module Enigma where
  import Data.Char  -- to use functions on characters
  import Data.Maybe -- breakEnigma uses Maybe type
  -- add extra imports if needed, but only standard library functions!

{- Part 1: Simulation of the Enigma -}

  type Rotor = (String, Int) -- the supplied type is not correct; fix it!
  type Reflector = [(Char, Char)] -- the supplied type is not correct; fix it!
  type Offsets = (Int, Int, Int) -- the supplied type is not correct; fix it!
  type Stecker = [(Char, Char)] -- the supplied type is not correct; fix it!
  
  data Enigma = SimpleEnigma Rotor Rotor Rotor Reflector Offsets
                | SteckeredEnigma Rotor Rotor Rotor Reflector Offsets Stecker

  encodeMessage :: String -> Enigma -> String
  encodeMessage message (SimpleEnigma r1 r2 r3 rf off) = encodeString capMessage (SimpleEnigma r1 r2 r3 rf off) 
    where
      capMessage = capitaliseMessage truncMessage
      truncMessage = truncateMessage message
  encodeMessage message (SteckeredEnigma r1 r2 r3 rf off pb) = encodeString capMessage (SteckeredEnigma r1 r2 r3 rf off pb) 
    where
      capMessage = capitaliseMessage truncMessage
      truncMessage = truncateMessage message

  {- You will need to add many more functions. Remember, design it carefully
   - and keep it simple! If things are feeling complicated, step back from your
   - code and think about the design again.
   -}

  encodeString :: String -> Enigma -> String
  encodeString [] (SimpleEnigma r1 r2 r3 rf off) = []
  encodeString [] (SteckeredEnigma r1 r2 r3 rf off pb) = []
  encodeString (x:xs) (SimpleEnigma r1 r2 r3 rf off) = [] ++ [encodeChar x (SimpleEnigma r1 r2 r3 rf updatedOff)] ++ encodeString xs (SimpleEnigma r1 r2 r3 rf updatedOff)
   where
    updatedOff = incrementOffsets off r1 r2 r3
  encodeString (x:xs) (SteckeredEnigma r1 r2 r3 rf off pb) = [] ++ [encodeChar x (SteckeredEnigma r1 r2 r3 rf updatedOff pb)] ++ encodeString xs (SteckeredEnigma r1 r2 r3 rf updatedOff pb)
   where
    updatedOff = incrementOffsets off r1 r2 r3
  

  encodeChar :: Char -> Enigma -> Char 
  encodeChar character (SimpleEnigma r1 r2 r3 rf off) = reversePassByRotors (reflectChar (passByRotors character off r1 r2 r3) rf) off r1 r2 r3
  encodeChar character (SteckeredEnigma r1 r2 r3 rf off pb) = steckerChar (reversePassByRotors (reflectChar (passByRotors (steckerChar character pb) off r1 r2 r3) rf) off r1 r2 r3) pb
  

  incrementOffsets :: Offsets -> Rotor -> Rotor -> Rotor -> Offsets 
  incrementOffsets (off1, off2, off3) r1 r2 r3
                                            | off1 == snd r1 && off2 /= snd r2 && off3 /= snd r3 = (off1, off2, (off3 + 1) `mod` 26)
                                            | off2 == snd r2 && off1 /= snd r1 && off3 /= snd r3 = ((off1 + 1) `mod` 26, off2, (off3 + 1) `mod` 26)
                                            | off2 == snd r2 && off1 == snd r1 && off3 /= snd r3 = ((off1 + 1) `mod` 26, off2, (off3 + 1) `mod` 26)
                                            | off1 == snd r1 && off2 /= snd r2 && off3 == snd r3 = (off1, (off2 + 1) `mod` 26, (off3 + 1) `mod` 26)
                                            | off2 == snd r2 && off1 /= snd r1 && off3 == snd r3 = ((off1 + 1) `mod` 26, (off2 + 1) `mod` 26, (off3 + 1) `mod` 26)
                                            | off2 == snd r2 && off1 == snd r1 && off3 == snd r3 = ((off1 + 1) `mod` 26, (off2 + 1) `mod` 26, (off3 + 1) `mod` 26)
                                            | off2 /= snd r2 && off1 /= snd r1 && off3 == snd r3 = (off1, (off2 + 1) `mod` 26, (off3 + 1) `mod` 26)
                                            | otherwise = (off1, off2, (off3 + 1) `mod` 26)
                                               


  
  passByRotors :: Char -> Offsets -> Rotor -> Rotor -> Rotor -> Char
  passByRotors character (off1, off2, off3) lr mr rr = passByRotor (passByRotor (passByRotor character rr off3) mr off2) lr off1

  reversePassByRotors :: Char -> Offsets -> Rotor -> Rotor -> Rotor -> Char
  reversePassByRotors character (off1, off2, off3) lr mr rr = reversePassByRotor (reversePassByRotor (reversePassByRotor character lr off1) mr off2) rr off3

  reflectChar :: Char -> Reflector -> Char
  reflectChar _ [] = 'A'
  reflectChar character (x:xs)
                                    | equalFst = snd x
                                    | equalSnd = fst x
                                    | (equalFst && equalSnd) == False = reflectChar character xs
                                    where 
                                      equalFst = (character == fst x)
                                      equalSnd = (character == snd x)


  reversePassByRotor :: Char -> Rotor -> Int -> Char
  reversePassByRotor character r n 
                            | n>0 = shiftBckrwd (charFromAlphaPos (shiftedCharIndex)) n
                            | n==0 = charFromAlphaPos (getIndex character (fst r))
                             where 
                              index = alphaPos character
                              shiftedChar = shiftFrwd character n 
                              shiftedCharIndex = getIndex shiftedChar (fst r)


  passByRotor :: Char -> Rotor -> Int -> Char
  passByRotor character r n 
                            | n>0 = shiftBckrwd ( fst r !! (shiftedCharIndex) ) n
                            | n==0 = fst r !! index 
                             where 
                              index = alphaPos character
                              shiftedChar = shiftFrwd character n 
                              shiftedCharIndex = alphaPos shiftedChar


  steckerChar :: Char -> Stecker -> Char
  steckerChar a [] = a
  steckerChar a (x:xs)
                  | (isInList a (map fst (x:xs))) || (isInList a (map snd (x:xs))) = reflectChar a (x:xs)
                  | otherwise = a 


  isInList :: Char -> [Char] -> Bool
  isInList a [] = False
  isInList a (x:xs)
                | x == a = True 
                | x /=a = isInList a xs 


{- Part 2: Finding the Longest Menu -}

  type Menu = [Int] -- the supplied type is not correct; fix it!
  type Crib = [(Char,Char)] -- the supplied type is not correct; fix it!

  longestMenu :: Crib -> Menu
  longestMenu (x:xs) = []

  generateMenu :: Crib -> [((Char,Char), Int)]
  generateMenu [] = []
  generateMenu (x:xs) = [] ++ [((fst x, snd x), getIndex (snd x) origList)] ++ generateMenu xs
    where
      origList = map fst (x:xs)

  {-generateMenu :: String -> String  -> Int -> Menu
  generateMenu "" "" 0 = []
  generateMenu plTxt cyphrTxt pos
                            | 
-}

  

  numOccurences :: Char -> [Char] -> Int 
  numOccurences _ [] = 0
  numOccurences character (x:xs)
                              | character == x = 1 + numOccurences character xs
                              | otherwise = 0 + numOccurences character xs

{- Part 3: Simulating the Bombe -}
  
  breakEnigma :: Crib -> Maybe (Offsets, Stecker)
  breakEnigma _ = Nothing

{- Useful definitions and functions -}

   -- substitution cyphers for the Enigma rotors
   -- as pairs of (wirings, knock-on position)
   -- knock-on position is where it will cause the next left wheel to
   -- advance when it moves past this position
 
        --"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  rotor1=("EKMFLGDQVZNTOWYHXUSPAIBRCJ",17::Int)
  rotor2=("AJDKSIRUXBLHWTMCQGZNPYFVOE",5::Int)
  rotor3=("BDFHJLCPRTXVZNYEIWGAKMUSQO",22::Int)
  rotor4=("ESOVPZJAYQUIRHXLNFTGKDCMWB",10::Int)
  rotor5=("VZBRGITYUPSDNHLXAWMJQOFECK",0::Int)

  {- the standard Enigma reflector (Reflector B)
    swapped A<->Y, B<->R, C<->U,D<->H, E<->Q, F<->S, G<->L, 
            I<->P, J<->X, K<->N, M<->O, T<->Z,V<->W
  -}
  reflectorB= [('A','Y'),
              ('B','R'),
              ('C','U'),
              ('D','H'),
              ('E','Q'),
              ('F','S'),
              ('G','L'),
              ('I','P'),
              ('J','X'),
              ('K','N'),
              ('M','O'),
              ('T','Z'),
              ('V','W')]

  {- alphaPos: given an uppercase letter, returns its index in the alphabet
     ('A' = position 0; 'Z' = position 25)
   -}
  alphaPos :: Char -> Int
  alphaPos c = (ord c) - ord 'A'

  charFromAlphaPos :: Int -> Char
  charFromAlphaPos i = chr ( i + ord 'A')

  shiftFrwd :: Char -> Int -> Char 
  shiftFrwd c n = charFromAlphaPos ( (apc + n) `mod` 26 )
              where
                apc = alphaPos c 

  shiftBckrwd :: Char -> Int -> Char 
  shiftBckrwd c n
              | n > apc = charFromAlphaPos ( 26 - (n-apc))
              | otherwise = charFromAlphaPos ( apc - n)
              where
                apc = alphaPos c

  getOffset :: Int -> Offsets -> Int
  getOffset 1 (a,_,_) = a
  getOffset 2 (_,b,_) = b
  getOffset 3 (_,_,c) = c 




  getIndex :: Char -> [Char] -> Int
  getIndex _ [] = 0
  getIndex character (x:xs)
                          | character == x = i - 1
                          | otherwise = i + getIndex character xs
                          where
                            i = 1

  truncateMessage :: String -> String
  truncateMessage [] = []
  truncateMessage (x:xs)
                      | isAlpha x = [] ++ [x] ++ truncateMessage xs
                      | otherwise = truncateMessage xs

  capitaliseMessage :: String -> String 
  capitaliseMessage [] = []
  capitaliseMessage (x:xs) = [] ++ [(toUpper x)] ++ capitaliseMessage xs 