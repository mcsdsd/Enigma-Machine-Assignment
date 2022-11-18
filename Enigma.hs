{-- Stub for the grading assignment. Fill it in, making sure you use good
 -- functional style, and add comments (including replacing those that are
 -- already here).
--}

module Enigma where
  import Data.List
  import Debug.Trace
  import Data.Ord
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
  encodeMessage message (SimpleEnigma rL rM rR rf off) = encodeString capMessage (SimpleEnigma rL rM rR rf off) 
    where
      capMessage = capitaliseMessage truncMessage
      truncMessage = truncateMessage message
  encodeMessage message (SteckeredEnigma rL rM rR rf off pb) = encodeString capMessage (SteckeredEnigma rL rM rR rf off pb) 
    where
      capMessage = capitaliseMessage truncMessage
      truncMessage = truncateMessage message

  {- You will need to add many more functions. Remember, design it carefully
   - and keep it simple! If things are feeling complicated, step back from your
   - code and think about the design again.
   -}

  encodeString :: String -> Enigma -> String
  encodeString [] (SimpleEnigma rL rM rR rf off) = []
  encodeString [] (SteckeredEnigma rL rM rR rf off pb) = []
  encodeString (x:xs) (SimpleEnigma rL rM rR rf off) = [] ++ [encodeChar x enigma] ++ encodeString xs enigma
    where
      updatedOff = (incrementOffsets off rL rM rR)
      enigma = (SimpleEnigma rL rM rR rf updatedOff)
  encodeString (x:xs) (SteckeredEnigma rL rM rR rf off pb) = [] ++ [encodeChar x steckeredEnigma] ++ encodeString xs steckeredEnigma
    where
      updatedOff = incrementOffsets off rL rM rR
      steckeredEnigma = (SteckeredEnigma rL rM rR rf off pb)
  

  encodeChar :: Char -> Enigma -> Char 
  encodeChar character (SimpleEnigma rL rM rR rf off) = encodedChar
    where
      fstEnc = (passByRotors character off rL rM rR)
      rfChar = (reflectChar fstEnc rf)
      encodedChar = reversePassByRotors rfChar off rL rM rR
  encodeChar character (SteckeredEnigma rL rM rR rf off pb) = encodedChar
    where
      fstStckr = (steckerChar character pb)
      fstEnc = (passByRotors fstStckr off rL rM rR)
      rfChar = (reflectChar fstEnc rf)
      rvrsEnc = (reversePassByRotors rfChar off rL rM rR)
      encodedChar = steckerChar rvrsEnc pb
  

  incrementOffsets :: Offsets -> Rotor -> Rotor -> Rotor -> Offsets 
  incrementOffsets (off1, off2, off3) r1 (_,kM) (_,kR) = (newL, newM, newR)
                                         where
                                          newR = (off3+1) `mod` 26
                                          newM | off3 == kR = (off2+1) `mod` 26
                                               | otherwise = off2
                                          newL | off2 == kM && off3 == kR = (off1+1) `mod` 26
                                               | otherwise = off1
                                               


  
  passByRotors :: Char -> Offsets -> Rotor -> Rotor -> Rotor -> Char
  passByRotors character (oL, oM, oR) rL rM rR = finalChar
              where
                rightRotor = (passByRotor character rR oR)
                middleRotor = (passByRotor rightRotor rM oM)
                finalChar =  passByRotor middleRotor rL oL

  reversePassByRotors :: Char -> Offsets -> Rotor -> Rotor -> Rotor -> Char
  reversePassByRotors character (oL, oM, oR) rL rM rR = finalChar
                where
                  leftRotor = (reversePassByRotor character rL oL)
                  middleRotor = (reversePassByRotor leftRotor rM oM)
                  finalChar = reversePassByRotor middleRotor rR oR

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
                  | (isInList a fstChars) || (isInList a sndChars) = reflectChar a (x:xs)
                  | otherwise = a
                  where
                    fstChars = (map fst (x:xs))
                    sndChars = (map snd (x:xs))

  elmRepeats :: [Char] -> [Char] -> Bool
  elmRepeats newList [] = False
  elmRepeats newList (x:xs)
                  | length xs > 0 && isInList (head xs) (newList ++ [x]) = True
                  | otherwise = elmRepeats (newList ++ [x]) xs


  isInList :: Char -> [Char] -> Bool
  isInList a [] = False
  isInList a (x:xs)
                | x == a = True 
                | x /=a = isInList a xs 


{- Part 2: Finding the Longest Menu -}

  type Menu = [Int] -- the supplied type is not correct; fix it!
  type Crib = [(Char,Char)] -- the supplied type is not correct; fix it!

  longestMenu :: Crib -> Menu
  longestMenu crib = maximumBy (comparing length) (completeMenu startedMenus crib)
    where
      startedMenus = arrangeMenus [] [0..(length crib -1)]

  nextInMenu :: Int -> Crib -> [Int]
  nextInMenu index (x:xs) = findIndices (==cyChar) crib
                      where
                        crib = map fst (x:xs)
                        cryp = map snd (x:xs)
                        cyChar = cryp !! index

  arrangeMenus :: [Int] -> [Int] -> [[Int]]
  arrangeMenus incMenu [] = []
  arrangeMenus incMenu (y:ys) = [] ++ [incMenu ++ [y]] ++ arrangeMenus incMenu ys

  completeMenu :: [[Int]] -> Crib -> [[Int]]
  completeMenu [] crib = []
  completeMenu (x:xs) crib = [] ++ menus ++ completeMenu menus newCrib ++ completeMenu xs crib
    where
      menus = arrangeMenus x nextPart
      nextPart = nextInMenu lastElm crib 
      newCrib = removeFromCrib lastElm crib
      lastElm = last x

{- Part 3: Simulating the Bombe -}
  
  breakEnigma :: Crib -> Maybe (Offsets, Stecker)
  breakEnigma _ = Nothing

  testStecker :: Offsets -> Stecker -> Crib -> Bool
  testStecker _ [] [] = False
  testStecker (oL,oM,oR) (x:xs) crib =

  isSteckerValid :: Stecker -> Bool
  isSteckerValid stecker
                    | length (intersectStecker [] stecker) > 0 || elmRepeats [] (f:fs) || elmRepeats [] (s:ss) = False
                    | otherwise = True 
                    where
                      (f:fs) = map fst stecker
                      (s:ss) = map snd stecker

  intersectStecker :: [Char] -> Stecker -> [Char]
  intersectStecker [] (x:xs) = []
  intersectStecker _ (x:xs) = steckerInt
    where
      steckerInt | (getIndex i plain)==(getIndex i crypt) = []
                 | otherwise = (i:is) ++ intersectStecker is (x:xs)
      plain = map fst (x:xs)
      crypt = map snd (x:xs)
      (i:is) = intersect plain crypt


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

  --technically I'm substituting in order to not lose the indices when doing the rest of the menu
  removeFromCrib :: Int -> Crib -> Crib
  removeFromCrib _ [] = []
  removeFromCrib index (x:xs) = fstPart ++ [('a','a')] ++ sndPart
    where
      splitCrib = splitAt index (x:xs)
      fstPart = fst splitCrib
      sndPart = tail (snd splitCrib)

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
