In this text we try to go through our example and explain it step by step. Our approach
is to put the code step by step and explain the used functions and types. Here we go

``` Haskell

data Stockdaily = Stockdaily { date :: String , high :: Double , low :: Double} deriving (Generic)
instance FromJSON Stockdaily

data Stock = Stock { chart :: [Stockdaily] }
  deriving (Generic)
instance FromJSON Stock

```

In this part we set the types that are compatible with our data written in JSON files.
The expression `instance FromJSON MyData` allows us to read data of JSON type and produce a
variable with the data as `MyData`.


``` Haskell

stockFile :: DataSource Stock
stockFile = dataSource ["Inputs", "Stock"]
                      (somePureDeserial JSONSerial)

```

Let's dig to this part. The `DataSource` is a type constructor. If we come back to its
implementation we will find that it is just a synonym:

``` Haskell

type DataSource a = VirtualFile Void a

```

Let us dig into the definition of `VirtualFile`. It is called virtual it does not yet exist and we will
put it later in our work flow such that it works properly.

``` Haskell

data VirtualFile a b = VirtualFile
  { _vfileOriginalPath      :: [LocationTreePathItem]
  , _vfileLayeredReadScheme :: LayeredReadScheme b
  , _vfileMappedByDefault   :: Bool
  , _vfileImportance        :: VFileImportance
  , _vfileDocumentation     :: Maybe T.Text
  , _vfileSerials           :: SerialsFor a b }

```

We don't go through all the attributes and we just wanted to show how it is constructed. Let us come back to our
main stream. The function `dataSource` has the type

``` Haskell
dataSource :: [LocationTreePathItem] -> SerialsFor a b -> DataSource b
```
So it takes a list paths (e.g. `["Inputs", "Stock"]`) and some serialization methods and gives back
a data source. The story is similar for

``` Haskell
data SlidingWindows = SlidingWindows { smoothcurve :: [Double] }
  deriving (Generic)
instance ToJSON SlidingWindows

modifiedStock :: DataSink SlidingWindows
modifiedStock = dataSink ["Outputs", "ModifiedStock"]
                        (somePureSerial JSONSerial)
```

where the types are

```Haskell
type DataSink a = VirtualFile a ()
dataSink :: [LocationTreePathItem] -> SerialsFor a b -> DataSink a
```

In the what follows we just calculated a simple example. This part is completely independent
of Porcupine and can be put in the workflow directly.

``` Haskell
ave :: [Double] -> Double
ave list = let s = sum list
               n = fromIntegral (length list)
               in s/n

msliding :: Int -> [a] -> [[a]]
msliding n p = case p of
  []     -> []
  (x:xs) -> [take n p] ++ (msliding n xs)

computeSmoothedCurve :: Stock -> SlidingWindows
computeSmoothedCurve s = SlidingWindows curve where
  price = getLowStock s
  curve = map ave (msliding 10 price)
```

Here we come to the most important feature of Porcupine and this is the notion of work flow. We first work with a simpler
version and we will explain then the one that deals with streams. Let us start with the following workflow:

``` Haskell
analyseOneStock :: (LogThrow m) => PTask m () ()
analyseOneStock =
  loadData stockFile >>> arr computeSmoothedCurve >>> writeData modifiedStock
```
Here we have some types to understand. The notion of a _task_ is handled via the type
`PTask m a b`. Here how it is defined

``` Haskell
newtype PTask m a b = PTask
  (AppArrow
    (Writer ReqTree)
    (RunnablePTask m)
    a b)
  deriving (Category, Arrow, ArrowError SomeException, Functor, Applicative)
```

and we consider `PTask m` as an instance of Arrow class. That means that we can use `arr` and `(>>>)` as follows:

``` Haskell
arr   :: Monad m => (a -> b) -> PTask m a b  
(>>>) :: Monad m => PTask m a b -> PTask m b c -> PTask m b c  
```

So the arrows we consider in the workflow are considered in this sense; we can use `arr` to lift a function to `PTask m` type and then
use in the workflow via arrows `(>>>)`. Let us just remind the type of functions
`loadData` and `writeData` :

``` Haskell
loadData :: (LogThrow m, Typeable a, Typeable b) => VirtualFile a b -> PTask m () b
writeData:: (LogThrow m, Typeable a, Typeable b) => VirtualFile a b -> PTask m a ()
```

We remind that `stockFile` has the type of `DataSource Stock` which is synonym to `VirtualFile Void Stock`. A similar story is
for `modifiedStock` and hence the types are matched for `loadData` and `writeData`. For playing a bit with the work flow let us
show the type of each component:

``` Haskell
stockFile :: VirtualFile () Stock
modifiedStock :: VirtualFile SlidingWindows ()
loadData stockFile :: PTask m () Stock
arr computeSmoothedCurve :: PTask m Stock SlidingWindows
writeData modifiedStock :: PTask m SlidingWindows ()
analyseOneStock :: PTask m () ()
```

If you want to load/write a stream of files in the workflow, the Porcupine allows it you to do it
via the functions that are similar to load/write data above with a slight difference:


``` Haskell
loadDataStream    :: forall idx m a b r.
                     (Show idx, LogThrow m, Typeable a, Typeable b, Monoid r)
                  => LocVariable
                  -> VirtualFile a b -- ^ Used as a 'DataSource'
                  -> PTask m (Stream (Of idx) m r) (Stream (Of (idx, b)) m r)

writeDataStream   :: (Show idx, LogThrow m, Typeable a, Typeable b, Monoid r)
                  => LocVariable
                  -> VirtualFile a b -- ^ Used as a 'DataSink'
                  -> PTask m (Stream (Of (idx, a)) m r) r
```

We just remind that the `Stream` type constructor is defined in modules `Streaming` and it is defined as
``` Haskell
data Stream f m r = Step !(f (Stream f m r)) | Effect (m (Stream f m r)) | Return r
```
and `Of a b` (and `Of a` is a Functor) is type in module `Data.Functor.Of` as following
``` Haskell
data Of a b = !a :> b
```
We should note that in the definition of `loadDataStream` we keep track of indexes we use to load the data
whilst in the `writeDataStream` we output the type of the stream that is `()` in our example.


In this spirit let us analyse the following workflow:

``` Haskell
analyseStocks :: (LogThrow m) => PTask m () ()
analyseStocks =
  arr (const (S.each ["aapl" , "google"])) >>> loadDataStream "company" stockFile
   >>> arr (S.map (\(idx,stock) -> (idx, computeSmoothedCurve stock)))
   >>> writeDataStream "company" modifiedStock
```
Despite the workflow `analyseOneStock`, the workflow `analyseStocks` tries to read a multiple data and process them and then write a new file for each. Later, we combine these data in just one big matrix. Let us analyze the former expression. We imported the module `import qualified Streaming.Prelude as S` and we called the function `each` from this module. The type of this function is as follows:
``` Haskell
S.each :: (Monad m, Foldable f) => f a -> S.Stream (S.Of a) m ()
```
In paticular, it takes a list of items (as an instance of class `Foldable`) and returns a stream of the objects. So the first component in the workflow has the type
``` haskell
arr (const (S.each ["aapl" , "google"])) :: PTask m a (Stream (Of idx) m ())
```
For the second component of the workflow, if you come back to the type of `loadDataStream` you will see that
``` haskell
loadDataStream "company" stockFile :: PTask m (Stream (Of idx) m r) (Stream (Of (idx, b)) m r)
```
We recall that
``` haskell
S.map :: Monad m => (a -> b) -> S.Stream (S.Of a) m r -> S.Stream (S.Of b) m r
```
and so
``` haskell
arr (S.map (\(idx,stock) -> (idx, computeSmoothedCurve stock))) :: PTask m (Stream (Of idx,b) m r) (Stream (Of (idx, b)) m r)
```
and finally
```haskell
writeDataStream "company" modifiedStock :: PTask m (Stream (Of idx,b) m r) r
```
