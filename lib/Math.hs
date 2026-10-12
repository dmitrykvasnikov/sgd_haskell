module Math (
    Matrix,
    Vector,
    shape,
    norm,
    transm,
    add,
    sub,
    mul,
    hadamard,
    scale,
    scalar,
    dot,
    mkVector,
    mkMatrix,
)
where

type Vector a = Matrix a

data Matrix a where
    Matrix :: {rows :: Int, cols :: Int, mx :: [[a]]} -> Matrix a

instance (Show a) => Show (Matrix a) where
    show m = case isVector m of
        True -> "Vector: ( " ++ (intercalate " , " (map show (concat $ mx m))) ++ " )"
        False -> "Matrix: ( " ++ (intercalate "\n          " $ map showRow $ mx m) ++ " )"
      where
        showRow = intercalate " , " . map (leftPad maxLen . show)
        leftPad l s = take (l - length s) (repeat ' ') ++ s
        maxLen = maximum . map (length . show) . concat $ mx m

instance Functor Matrix where
    fmap f (Matrix r c m) = Matrix r c $ (map $ map f) m

instance Foldable Matrix where
    foldMap f (Matrix _ _ m) = foldMap (foldMap f) m

isVector :: Matrix a -> Bool
isVector m = cols m == 1

mkMatrix :: [[a]] -> Matrix a
mkMatrix m =
    case isMatrix (map length m) of
        True -> Matrix r (case m of (x : _) -> length x; [] -> 0) m
        False -> error "ERROR: can not create matrix, different length of rows"
  where
    r = length m
    isMatrix [] = True
    isMatrix (x : xs) = all (== x) xs

mkVector :: [a] -> Vector a
mkVector v = Matrix (length v) 1 (map (: []) v)

shape :: Matrix a -> (Int, Int)
shape m = (rows m, cols m)

norm :: (Floating a) => Matrix a -> a
norm = sqrt . sum . fmap (** 2)

-- Matrix transpose
transm :: Matrix a -> Matrix a
transm (Matrix r c m) = Matrix c r $ go m
  where
    go [] = replicate c []
    go (row : rest) = zipWith (:) row $ go rest

-- Basic matrix operations
binopm :: (a -> a -> a) -> Matrix a -> Matrix a -> Matrix a
binopm op (Matrix r1 c1 m1) (Matrix r2 c2 m2) =
    case (r1 == r2 && c1 == c2) of
        True -> Matrix r1 c1 $ map (uncurry (zipWith op)) (zip m1 m2)
        False -> error "ERROR: can not subtract | add | multiply  matrixes with different dimensions"

add :: (Num a) => Matrix a -> Matrix a -> Matrix a
add = binopm (+)

sub :: (Num a) => Matrix a -> Matrix a -> Matrix a
sub = binopm (-)

hadamard :: (Num a) => Matrix a -> Matrix a -> Matrix a
hadamard = binopm (*)

scale :: (Num a) => a -> Matrix a -> Matrix a
scale s = fmap (* s)

scalar :: Matrix a -> a
scalar (Matrix 1 1 [[a]]) = a
scalar _ = error "ERROR: getting scalar of matrix | vector of wrong size"

dot :: (Num a) => Vector a -> Vector a -> a
dot v1 v2
    | isVector v1 && isVector v2 = scalar . mul (transm v1) $ v2
    | otherwise = error "ERROR: dot could use only vectors as arguments"

mul :: (Num a) => Matrix a -> Matrix a -> Matrix a
mul (Matrix r1 c1 m1) m@(Matrix r2 c2 _) =
    case (c1 == r2) of
        True -> Matrix r1 c2 $ fmap (\r -> fmap (sum . zipWith (*) r) m') m1
          where
            m' = mx $ transm m
        False -> error "ERROR: can not multiply matrixes with wrong dimensions"

-- helper functions
intercalate :: [a] -> [[a]] -> [a]
intercalate i l = go l
  where
    go [] = []
    go [x] = x
    go (x : xs) = x ++ i ++ go xs
