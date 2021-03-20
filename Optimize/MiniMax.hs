
module Optimize.MiniMax
( optimize
, maxi
, mini
)
where


optimize :: (Ord sc, Num sc, Integral d) =>
            (st -> [st]) -> (st -> sc) -> d -> st -> st
optimize genSts evalSt d initSt = st
  where
    (st, _sc) = maxi genSts evalSt d initSt

maxi :: (Ord sc, Num sc, Integral d) =>
        (st -> [st]) -> (st -> sc) -> d -> st -> (st,sc)
maxi _genSts evalSt 0 initSt = (initSt, evalSt initSt)
maxi  genSts evalSt d initSt = foldl1 maxStAndSc stsAndScs
  where
    sts = genSts initSt
    scs = map (snd . mini genSts evalSt (d-1)) sts
    stsAndScs = zip sts scs
    maxStAndSc x y
      | snd x > snd y = x
      | otherwise     = y

mini :: (Ord sc, Num sc, Integral d) =>
        (st -> [st]) -> (st -> sc) -> d -> st -> (st,sc)
mini _genSts evalSt 0 initSt = (initSt, evalSt initSt)
mini  genSts evalSt d initSt = foldl1 minStAndSc stsAndScs
  where
    sts = genSts initSt
    scs = map (snd . maxi genSts evalSt (d-1)) sts
    stsAndScs = zip sts scs
    minStAndSc x y
      | snd x < snd y = x
      | otherwise     = y