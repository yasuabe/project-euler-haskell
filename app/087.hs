import Data.Numbers.Primes
import qualified Data.IntSet as S

primes0 = takeWhile (<floor(sqrt 50000000)) primes
primes' n p=takeWhile(\prm->prm^p<n) primes0

g n p f = concatMap (\prm->map (\s->s+prm^p) $ f (n-prm^p)) $ primes' n p

e87 n = g n 4 (\m->g m 3 (\l->(map (^2)$primes' l 2)))

main =print$S.size$S.fromList$e87 50000000
