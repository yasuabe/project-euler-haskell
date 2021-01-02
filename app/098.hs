-- need refactoring
import qualified Data.Text as T
import Data.List
import Data.Maybe

anagrams = [
    ["NO","ON"],
    ["ACT","CAT"],
    ["EAT","TEA"],
    ["DOG","GOD"],
    ["HOW","WHO"],
    ["ITS","SIT"],
    ["NOW","OWN"],
    ["CARE","RACE"],
    ["DEAL","LEAD"],
    ["HATE","HEAT"],
    ["MALE","MEAL"],
    ["MEAN","NAME"],
    ["EARN","NEAR"],
    ["RATE","TEAR"],
    ["EAST","SEAT"],
    ["FILE","LIFE"],
    ["ITEM","TIME"],
    ["NOTE","TONE"],
    ["SURE","USER"],
    ["FORM","FROM"],
    ["SIGN","SING"],
    ["SHUT","THUS"],
    ["POST","SPOT","STOP"],
    ["BOARD","BROAD"],
    ["PHASE","SHAPE"],
    ["EARTH","HEART"],
    ["ARISE","RAISE"],
    ["LEAST","STEAL"],
    ["SHEET","THESE"],
    ["QUIET","QUITE"],
    ["NIGHT","THING"],
    ["THROW","WORTH"],
    ["SHOUT","SOUTH"],
    ["DANGER","GARDEN"],
    ["CREDIT","DIRECT"],
    ["CENTRE","RECENT"],
    ["EXCEPT","EXPECT"],
    ["COURSE","SOURCE"],
    ["FORMER","REFORM"],
    ["IGNORE","REGION"],
    ["CREATION","REACTION"],
    ["INTRODUCE","REDUCTION"]]

inc ch = toEnum$(+1)$fromEnum ch
f1 [] i m = m
f1 (h:t) ch m = case lookup h m of
    Nothing -> f1 t (inc ch) ((h, ch):m) 
    _       -> f1 t ch m
callF1 s = f1 s 'a' []
f2 [] m = ""
f2 (h:t) m = d:f2 t m  where d=fromJust$lookup h m

f3 left (h:[])    = [(h,left)]
f3 left (h:right) = (h,left++right):f3 (left++[h]) right
callF3 ss = f3 [] ss


f4 (w, ws) = map (\w'->f2 w' m) ws where m=callF1 w
f4' (w, ws) = map (\w'->(f2 w' m, w')) ws where m=callF1 w
f5 ws = map f4 $callF3 ws
f5' ws = concatMap (\e@(w,ws)->f4' e) $callF3 ws
f6 wss=nub$concatMap (\ws->nub$concat$f5 ws) wss

main = do let t = map (\l->map (\(n,_)->show n) l)$filter ((1<).length)$groupBy (\a b->(snd a)==(snd b))$sortBy (\a b->compare (snd a)(snd b))$map (\n->(n, sort$show n))$takeWhile (<100000) $dropWhile (<10000)$map (^2) [1..]
              t2=map (\l->maximumBy (\(_,a) (_,b)->compare a b) l)$groupBy (\(a,_) (b,_)->a==b)$sortBy (\(a,_) (b,_)->compare a b)$concatMap f5' t
              t3= f6 anagrams
          print $ maximum$map fromJust$filter (not.isNothing)$map (\e->lookup e t2) t3
