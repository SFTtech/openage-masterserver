module ELO where

-- tuning params, currently using some defaults
-- the famous k-factor, basically how quickly you ascend or descend
-- the higher the k-factor the faster you rank around
-- might make this dynamic in future module versions (ie the higher your score the lower your k-factor, or maybe scale depending on amount of matches played)
k = 24
-- expected score magnification
base = 10
-- how many points of difference we require for one magnification to happen
scale = 400

-- base and exponent of the logistic curve 
q :: Double -> Double 
q rating = base ** (rating / scale)

-- basically a logistic curve for the expectation
-- calculates how the match should end according to the player rankings
-- first and second are player ratings
expected :: Double -> Double -> Double
expected first second = (q first) / (q first + q second)

-- call this for the winning player
-- first := rating of the winner
-- second := rating of the loser
win :: Double -> Double -> Double
win first second = calc first second 1

-- call this for the loser
-- first := the rating of the loser
-- second := rating of the winner
lose :: Double -> Double -> Double
lose first second = calc first second 0

-- a little wrapper around the new rating function to provide access to the expected result 
-- first and second are ratings of players
-- result is how the game actually ended
calc :: Double -> Double -> Int -> Double
calc first second result = newRating first (expected first second) result

-- returns the new rating for the given parameters
-- rating := the previous rating of the player
-- expected := the expectation of how the match would go
-- result := how the match actually went
newRating :: Double -> Double -> Int -> Double
newRating rating expectation result = rating + k * (fromIntegral result - expectation) 
