module Math (
    shape,
    mulvv,
    mulv,
    mulmm,
    mulms,
    normv,
    transm,
    addm,
    subm,
    mulm,
    mkVector,
    mkMatrix,
)
where

data Matrix a where
    Matrix :: {rows :: Int, cols :: Int, mx :: [[a]]} -> Matrix a

instance (Show a) => Show (Matrix a) where
    show (Matrix _ _ m) = "Matrix: ( " ++ (intercalate "\n          " $ map showRow m) ++ " )"
      where
        showRow = intercalate " , " . map (leftPad maxLen . show)
        leftPad l s = take (l - length s) (repeat ' ') ++ s
        maxLen = maximum . map (length . show) . concat $ m

instance Functor Matrix where
    fmap f (Matrix r c m) = Matrix r c $ (map $ map f) m

data Vector a where
    Vector :: {vr :: [a]} -> Vector a

instance (Show a) => Show (Vector a) where
    show (Vector v) = "Vector: ( " ++ (intercalate " , " (map show v)) ++ " )"

instance Functor (Vector) where
    fmap f (Vector v) = Vector $ fmap f v

mkVector :: [a] -> Vector a
mkVector = Vector

mkMatrix :: [[a]] -> Matrix a
mkMatrix m =
    case isMatrix (map length m) of
        True -> Matrix r (if r == 0 then 0 else length . head $ m) m
        False -> error "ERROR: can not create matrix, different length of rows"
  where
    r = length m
    isMatrix [] = True
    isMatrix (x : xs) = all (== x) xs

shape :: Matrix a -> (Int, Int)
shape (Matrix r c _) = (r, c)

vtom :: Vector a -> Matrix a
vtom (Vector v) = Matrix{rows = (length v), cols = 1, mx = map (\x -> [x]) v}

mtov :: Matrix a -> Vector a
mtov (Matrix r c m) = case (r > 0 && c == 1) of
    True -> Vector $ map head m
    False -> error "ERROR: can not convert matrix to vector"

-- Vector to vector multiplication
mulvv :: (Num a) => Vector a -> Vector a -> a
mulvv (Vector v1) (Vector v2) = sum $ zipWith (*) v1 v2

mulv :: (Num a) => Vector a -> Vector a -> Vector a
mulv v1 v2 = mtov . mulm (vtom v1) $ (vtom v2)

-- Vector normalization
normv :: (Floating a) => Vector a -> a
normv (Vector v) = sqrt . sum $ fmap (** 2) v

-- Matrix transpose
transm :: Matrix a -> Matrix a
transm (Matrix r c m) = Matrix c r $ go m
  where
    go [] = repeat []
    go (row : rows) = zipWith (:) row $ go rows

-- Binary operation with matrixes of the same size
binopm :: (Num a) => (a -> a -> a) -> Matrix a -> Matrix a -> Matrix a
binopm op (Matrix r1 c1 m1) (Matrix r2 c2 m2) =
    case (r1 == r2 && c1 == c2) of
        True -> Matrix r1 c1 $ map (uncurry (zipWith op)) (zip m1 m2)
        False -> error "ERROR: can not subtract | add | multiply  matrixes with different dimensions"

addm :: (Num a) => Matrix a -> Matrix a -> Matrix a
addm = binopm (+)

subm :: (Num a) => Matrix a -> Matrix a -> Matrix a
subm = binopm (-)

mulm :: (Num a) => Matrix a -> Matrix a -> Matrix a
mulm = binopm (*)

-- Matrix scalar multiplication
mulms :: (Num a) => a -> Matrix a -> Matrix a
mulms s = fmap (* s)

-- Matrix to vector multiplication
mulmv :: (Num a) => Matrix a -> Vector a -> Vector a
mulmv (Matrix r c m) (Vector v) =
    case (c == length v) of
        True -> Vector $ map (sum . zipWith (*) v) m
        False -> error "ERROR: can not multiply matrix on vector with wrong length"

-- Matrix to matrix multiplication
mulmm :: (Num a) => Matrix a -> Matrix a -> Matrix a
mulmm (Matrix r1 c1 m1) m@(Matrix r2 c2 m2) =
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
