module TypeClassExercise where
  
import qualified Data.Map as Map

data LockedState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerNumber = Int

type LockerMap = Map.Map LockerNumber (LockedState, Code)

lockerLookup :: LockerNumber -> LockerMap -> Either String Code
lockerLookup lockerNumber map = 
    case Map.lookup lockerNumber map of
      Nothing -> Left $ "Locker Number " ++ show lockerNumber ++ " not found"
      Just (state, code) -> if (state /= Taken)
                            then Right code
                            else Left $ "Locker Number " ++ show lockerNumber ++ " is already taken"
                            
-- Question:
-- How could change the locker's state after I get a locker?