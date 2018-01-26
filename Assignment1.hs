module Assignment1 where

-- Author: Karishma Daga
-- Student Number: 20031646
-- Integrity: I am the sole writer of this code.

-- Problem 1: Determining the type of spherical shape
-- @ params
-- @ firstAxis, secondAxis, thirdAxis are all of type double and represent the
  -- lengths of the axes of an ellipsoid
-- @ return: a char representing the type of ellipsoid shape:
    -- 'S' : sphere if all of the axes are equal, 'F' : Football if two of them
      -- are equal, 'E' : ellipsoid (not S or F)
    -- if they are all different, 'X': if firstAxis, secondAxis, or thirdAxis
      -- is less than or equal to 0

shape :: Double -> Double -> Double -> Char
shape firstAxis secondAxis thirdAxis
  -- check illegal values
  | (firstAxis <= 0) || (secondAxis <= 0) || (thirdAxis <= 0) = 'X'
  -- ellipsoid: check if none equal each other
  | (firstAxis /= secondAxis) && (firstAxis /= thirdAxis) && (secondAxis /= thirdAxis) = 'E'
  -- sphere
  | (firstAxis == secondAxis) && (firstAxis == thirdAxis) && (secondAxis == thirdAxis) = 'S'
  -- football
  | (firstAxis == secondAxis) || (secondAxis == thirdAxis) || (firstAxis == thirdAxis) = 'F'


-- Problem 2: Calculating the volume of an ellipsoid.

volume :: Double-> Double -> Double -> Double
volume 0 0 0 = 0
volume firstAxis secondAxis thirdAxis
  | (firstAxis < 0 || secondAxis < 0 || thirdAxis < 0) = error "Exception: ellipsoid with negative side(s)"
  | otherwise = (4 / 3) * pi * (firstAxis) * (secondAxis) * (thirdAxis)


-- Problem 3: taking the sum of the natural logs of between two numbers, inclusive
-- a and b are natural numbers. return the sum of the natural logs of each
  -- number i such that a <= i <= b
-- @ x : the first value, @ y : the second value. Y should be greater than X.
logSum :: Int -> Int -> Double
logSum x y
  -- ln(x) is undefined when x = 0.
  | (x <= 0) || (y <= 0) = error "The log of any negative number or 0 is undefined for log. Please enter valid values in the natural number range."
  | (x > y) = 0
  | (y == x) = log(fromIntegral(x))
  | y > x = log(fromIntegral(y)) + logSum x (y - 1)


-- Problem 4: Aliens are called ETs and we need to determine how quickly they will
  -- grow based on their initial weight and the number of days they're allowed to grow
  -- number of days allowed to grow decreasing.
  -- Base case is when growthPeriod = 0
-- @ mass : the initial mass, @ growthPeriod : the period in which ET can grow
growET :: Double -> Int -> Double
growET 0 0 = error "Initial mass and days of growth are both 0"
growET mass growthPeriod
  -- error cases
  | mass < 0 = error "Initial mass is less than 0"
  | mass > 0 && growthPeriod < 0 = error "Growth period is negative"
  | mass > 0 && growthPeriod > 100 = error "Growth period is greater than 100 days"
  -- base case: if mass >0 and growthPeriod = 0
  | mass > 0 && growthPeriod == 0 = mass
  -- if mass is less than 1 g, it will double in one day
  | mass > 0 && mass < 1  = growET (mass*2) (growthPeriod - 1)
  -- if 1 <= mass < 20, new mass = 1.5*mass + 2
  | mass >= 1 && mass < 20 = growET (mass*1.5 + 2) (growthPeriod - 1)
  -- if 20 <= mass < 100, new mass = 1.2*mass + 1
  | mass >= 20 && mass < 100 = growET (mass*1.2 + 1) (growthPeriod - 1)
  -- if mass >= 100, new mass = 1.1*mass + 0.5
  | mass >= 100 = growET (mass*1.1 + 0.5) (growthPeriod - 1)
