module WhitherPermutations where
  import qualified Data.Set as Set
  -- import Debug.Trace(trace)

  mySet :: Set.Set Int
  mySet = Set.fromList [0..5]

  factorial :: Int -> Int
  factorial n = factorial0 n 1

  factorial0 0 accu = accu
  factorial0 1 accu = accu
  factorial0 n accu = factorial0 (n - 1) (n * accu)

  data PermutationOutcome a = Permutation [a] | PermutationFailure String Int deriving (Eq, Show)

  tryPermutations :: (Eq a, Show a) => PermutationTest b a -> Set.Set a -> Int -> Int -> [(Int, PermutationOutcome a)]
--  tryPermutations test s startIndex numToTry = tryPermutations0 test s startIndex (startIndex + numToTry - 1) []
  tryPermutations test s startIndex numToTry = tryPermutations0a test s startIndex (startIndex + numToTry - 1)

  tryPermutations0 :: (Show a)  => PermutationTest b a -> Set.Set a -> Int -> Int -> [(Int, PermutationOutcome a)] -> [(Int, PermutationOutcome a)]
  tryPermutations0 test s start end accu
   | start > end = accu
   | otherwise   = tryPermutations0 test s newStart end newAccu
    where nextResult = permAtIndex test s start
          newAccu = (start, nextResult):accu
          newStart = nextMultiple start factor
          factor = case nextResult of
            Permutation _                  -> 1
            PermutationFailure _ nodesLeft -> factorial nodesLeft

  tryPermutations0a :: (Show a)  => PermutationTest b a -> Set.Set a -> Int -> Int -> [(Int, PermutationOutcome a)]
  tryPermutations0a test s start end
   | start > end = []
   | otherwise   = (start,nextResult):(tryPermutations0a test s newStart end)
    where nextResult = permAtIndex test s start
          newStart = nextMultiple start factor
          factor = case nextResult of
            Permutation _                  -> 1
            PermutationFailure _ nodesLeft -> factorial nodesLeft

  nextMultiple :: Int -> Int -> Int
  nextMultiple from factor = factor * (1 + (quot from factor))

  permAtIndex :: (Show a) => PermutationTest b a -> Set.Set a -> Int -> PermutationOutcome a
  permAtIndex test s n = case innerResult of
    -- I don't know if I like this. A PermutationOutcome can have either a
    -- backwards list or a forwards list?
    Permutation list -> Permutation (reverse list)
    PermutationFailure msg x -> PermutationFailure msg x
    where innerResult = permAtIndex0 test s (toFactoradic n (Set.size s)) []

  permAtIndex0 :: (Show a) => PermutationTest b a -> Set.Set a -> [Int] -> [a] -> PermutationOutcome a
  permAtIndex0 test s indices accu
    | (Set.null s)      = Permutation accu
    | otherwise         = case (state test) of
      EarlyExit msg -> PermutationFailure msg (Set.size s)
      Finished      -> permAtIndex0 test newS (tail indices) newAccu
      OKSoFar innerState     -> recurseHard
        where
           testFunc = func test
           newState = testFunc innerState nextElem
           recurseHard = permAtIndex0 test{state = newState} newS (tail indices) newAccu
      where nextIndex [] = 0
            nextIndex xs = head xs
            x = nextIndex indices
            newS = Set.deleteAt x s
            nextElem = Set.elemAt x s
            newAccu = nextElem:accu

  toFactoradic0 :: Int -> Int -> Int -> [Int] -> [Int]
  toFactoradic0 n counter digits accu
    | counter >= (digits + 1) = accu
    | n == 0                  = toFactoradic0 0 (counter + 1) digits (0:accu)
    | otherwise               = toFactoradic0 (quot n counter) (counter + 1) digits newAccu
    where newElement = rem n counter
          newAccu = newElement:accu

  toFactoradic :: Int -> Int -> [Int]
  toFactoradic n digits = toFactoradic0 n 2 digits []

  data TestState a = OKSoFar a | Finished | EarlyExit String deriving (Eq, Show)
  type TestFunc a b = a -> b -> TestState a
  data PermutationTest a b = PermutationTest { state :: TestState a
                                             , func :: TestFunc a b }

  applyToState :: TestFunc a i -> TestState a -> i -> TestState a
  applyToState f state input = case state of
    EarlyExit msg -> EarlyExit msg
    OKSoFar innerState -> f innerState input
    Finished -> Finished

  chainTests :: PermutationTest b i -> PermutationTest c i -> PermutationTest (TestState b, TestState c) i
  chainTests t1 t2 = PermutationTest { state = mergeStates (state t1) (state t2)
                                     , func = makeChainedFunc (func t1) (func t2)}

  makeChainedFunc :: TestFunc b i -> TestFunc c i -> TestFunc (TestState b, TestState c) i
  makeChainedFunc f1 f2 = \(s1, s2) input ->
    let s1new = applyToState f1 s1 input
        s2new = applyToState f2 s2 input
    in mergeStates s1new s2new 

  mergeStates :: TestState b -> TestState c -> TestState (TestState b, TestState c)
  mergeStates (EarlyExit msg) _ = EarlyExit msg
  mergeStates _ (EarlyExit msg) = EarlyExit msg
  mergeStates (OKSoFar x) (OKSoFar y) = OKSoFar (OKSoFar x, OKSoFar y)
  mergeStates (OKSoFar x) Finished = OKSoFar (OKSoFar x, Finished)
  mergeStates Finished (OKSoFar y) = OKSoFar (Finished, OKSoFar y)
  mergeStates Finished Finished = Finished

  mergeStateList :: [TestState a] -> TestState [TestState a]
  mergeStateList [] = Finished
  mergeStateList (x:xs) = let
    remainder = mergeStateList xs
    in case (x, remainder) of
         (EarlyExit msg, _)         -> EarlyExit msg
         (_,         EarlyExit msg) -> EarlyExit msg
         (OKSoFar q, OKSoFar ys)    -> OKSoFar ((OKSoFar q):ys)
         (OKSoFar q, Finished)      -> OKSoFar [OKSoFar q]
         (Finished,  OKSoFar ys)    -> OKSoFar ys
         (Finished,  Finished)      -> Finished

  makeListFunc :: [TestFunc a i] -> TestFunc [TestState a] i
  makeListFunc funcs = \states input ->
    let zipFunc f s = applyToState f s input
        newStates = zipWith zipFunc funcs states
    in mergeStateList newStates

  chainTestList :: [PermutationTest b i] -> PermutationTest [TestState b] i
  chainTestList tests = PermutationTest {
      state = mergeStateList (map state tests)
    , func = makeListFunc (map func tests)
    }

  alwaysTrue :: PermutationTest () a
  alwaysTrue = PermutationTest { state = Finished, func = error "why was the alwaysTrue func called" }

  data XBeforeYState = XBeforeYState Int Int Bool deriving Show
  xBeforeY :: Int -> Int -> PermutationTest XBeforeYState Int
  xBeforeY x y = PermutationTest { state = OKSoFar (XBeforeYState x y False)
                                 , func = xBeforeYFunc }

  xBeforeYFunc :: TestFunc XBeforeYState Int
  xBeforeYFunc (XBeforeYState x y lastWasX) n
    | lastWasX && n == y = Finished
    | lastWasX           = EarlyExit ("Failed, tried to append " ++ show n ++ " to " ++ show x)
    | n == y             = EarlyExit ("Failed, tried to put " ++ show y ++ " before " ++ show x)
    | n == x             = newStatus True
    | otherwise          = newStatus False
    where newStatus s = OKSoFar (XBeforeYState x y s)

  fourBeforeThree = xBeforeY 4 3

  testThreeBefore2 = tryPermutations (xBeforeY 3 2) mySet 604 1

  data TwoAdjacentState a = TwoAdjacentState a a Bool
  twoAdjacentFunc :: (Eq i, Show i) => TestFunc (TwoAdjacentState i) i
  twoAdjacentFunc (TwoAdjacentState x y lastOneWasInteresting) i
    | lastOneWasInteresting && thisOneIsInteresting = Finished
    | thisOneIsInteresting = OKSoFar (TwoAdjacentState x y True)
    | lastOneWasInteresting = EarlyExit ("Failed trying to append " ++ show i ++ " after " ++ show x ++ " or " ++ show y)
    | otherwise = OKSoFar (TwoAdjacentState x y False)
    where thisOneIsInteresting = i == x || i == y

  twoAdjacent :: (Eq a, Show a) => a -> a -> PermutationTest (TwoAdjacentState a) a
  twoAdjacent x y = PermutationTest { state = OKSoFar (TwoAdjacentState x y False)
                                    , func = twoAdjacentFunc}

  data SetAdjacentState a = SetAdjacentState (Set.Set a) Bool
  setAdjacentFunc :: (Ord i, Show i) => TestFunc (SetAdjacentState i) i
  setAdjacentFunc (SetAdjacentState set didTheyBegin) i
    | Set.null set = Finished
    | thisOneIsInteresting = OKSoFar (SetAdjacentState newSet True)
    | didTheyBegin = EarlyExit ("We were inside the set and then we tried to append " ++ show i)
    | otherwise = OKSoFar (SetAdjacentState set False)
    where thisOneIsInteresting = Set.member i set
          newSet = Set.delete i set

  setAdjacent :: (Ord a, Show a) => Set.Set a -> PermutationTest (SetAdjacentState a) a
  setAdjacent set = PermutationTest { state = OKSoFar (SetAdjacentState set False)
                                    , func = setAdjacentFunc }

  data SetMustBeginByState a = SetMustBeginByState (Set.Set a) Int
  setMustBeginByFunc :: (Ord i, Show i) => TestFunc (SetMustBeginByState i) i
  setMustBeginByFunc (SetMustBeginByState set n) i
    | n == 0 = EarlyExit ("We failed to start the set in time.")
    | Set.member i set = Finished
    | otherwise = OKSoFar (SetMustBeginByState set (n - 1))

  setMustBeginBy :: (Ord a, Show a) => Set.Set a -> Int -> PermutationTest (SetMustBeginByState a) a
  setMustBeginBy set n = PermutationTest { state = OKSoFar (SetMustBeginByState set n)
                                         , func = setMustBeginByFunc }

