import Control.Monad.State
import System.Random
import Data.Function 
import Data.List
type GeneratorState = State StdGen

times = 1000000
rollDie :: GeneratorState Int
rollDie = do generator <- get
             let (value, newGenerator) = randomR (1,4) generator
             put newGenerator 
             return value
rollDice :: GeneratorState (Int, Int)
rollDice = liftM2 (,) rollDie rollDie

series0 = cycle [
           "GO","A1","CC1","A2","T1","R1","B1","CH1","B2","B3",
           "JAIL","C1","U1","C2","C3","R2","D1","CC2","D2","D3",
           "FP","E1","CH2","E2","E3","R3","F1","F2","U2","F3",
           "G2J","G1","G2","CC3","G3","R4","CH3","H1","T2","H2"]
e84 = reverse 
    $ sortBy (\a b->(on compare snd) a b)
    $ map (\ls->(head ls, (on (/) fromIntegral) ((length ls)*100) times))
    $ group$sort$take times$(head series0:play series0 0 (mkStdGen 0))

play series doubles gen = 
    (head series'): play series' doubles' gen'
    where (series', doubles', gen')=proceed series doubles gen

proceed series doubles gen 
    | 3 == doubles'    = goToJail
    | h == "G2J"       = goToJail
    | (take 2 h)=="CC" = handleCC series' doubles' gen'
    | (take 2 h)=="CH" = handleCH series' doubles' gen'
    | otherwise        = (drop (v1+v2) series, doubles', gen')
    where ((v1,v2), gen') = runState rollDice gen
          doubles' = if v1==v2 then doubles+1 else 0
          series'@(h:t) = drop (v1+v2) series
          goToJail = (goto "JAIL" series', 0, gen')

handleCC series doubles gen = case val of
    1 -> goto' "GO"
    2 -> goto' "JAIL"
    _ -> (series, doubles, gen')
    where (val, gen') = (randomR (1,16) gen::(Int,StdGen))
          goto' s = (goto s series, doubles, gen')
handleCH series@(h:t) doubles gen = case val of
    1 -> goto' "GO"
    2 -> goto' "JAIL"
    3 -> goto' "C1"
    4 -> goto' "E3"
    5 -> goto' "H2"
    6 -> goto' "R1"
    7 -> goto' "R"
    8 -> goto' "R"
    9 -> goto' "U"
    10-> if h=="CH3" then handleCC back3 doubles gen'
                     else (back3, doubles, gen')
    _ -> (series, doubles, gen')
    where (val, gen') = (randomR (1,16) gen::(Int,StdGen))
          goto' s = (goto s series, doubles, gen')
          back3 = drop 37 series

goto s series = dropWhile (\sq->not$startsWithS sq) series
    where startsWithS sq = s==take (length s) sq

main = print $ map fst $ take 3 e84
