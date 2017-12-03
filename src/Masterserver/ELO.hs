module ELO where

-- tuning params, currently using some defaults
-- the famous k-factor, basically how quickly you ascend or descend
-- should probably be a lot lower, for example k=24 so we don't rank around too quickly
k = 64
-- expected score magnification
base = 10
-- how many points of difference we require for one magnification to happen
scale = 400


q :: Double -> Double 
q rating = base ** (rating / scale)

expected :: Double -> Double -> Double
expected first second = (q first) / (q first + q second)

win :: Double -> Double -> Double
win first second = calc first second 1

lose :: Double -> Double -> Double
lose first second = calc first second 0

calc :: Double -> Double -> Int -> Double
calc first second result = newRating first (expected first second) result

newRating :: Double -> Double -> Int -> Double
newRating rating expectation result = rating + k * (fromIntegral result - expectation) 
