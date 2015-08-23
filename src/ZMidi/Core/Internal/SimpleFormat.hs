{-# LANGUAGE CPP                        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Core.Internal.SimpleFormat
-- Copyright   :  (c) Stephen Tetley 2010-2015
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  unstable
-- Portability :  As per dependencies.
--
-- Simple line-oriented formatting combinators.
--
--------------------------------------------------------------------------------

module ZMidi.Core.Internal.SimpleFormat
  (

    ColumnSpecs(..) 
  , ColumnSpec(..)
  , Table
  , runTable
  , execTable
  , tellFree
  , tellRow
  , tellBlank
  , tellBreak
  , localColumns
  , nextTrack
  , arbTrack
  , incrDelta

  , WString
 

  , width
  , outputDoc

  , (<+>)
  , sep
  , char
  , text
  , repeatChar

  , padl
  , padr
  , hex2  
  , hex4
  , int
  , integral

  ) where



#ifndef MIN_VERSION_GLASGOW_HASKELL
import Control.Applicative
#endif
#ifndef MIN_VERSION_GLASGOW_HASKELL
import Data.Monoid
#endif
import Data.Word
import Numeric

-- | Strings are represented as Hughes lists 
--
-- ShowS is a Hughes list representation specialized to Strings
--
type H a = [a] -> [a]


type HString = H Char
type HLines  = H String


-- | Make a HString of spaces.
--
spaceH :: Int -> HString 
spaceH n = showString $ replicate n ' '

-- | Materialize a Hughes list.
--
fromH :: H a -> [a]
fromH = ($ [])

-- | Empty Hughes list.
--
emptyH :: H a 
emptyH = id


-- | Snoc an element to the right end.
--
snocH :: H a -> a -> H a
snocH f a = f . (a:)



--------------------------------------------------------------------------------

-- | Column formats for table printing.
--
data ColumnSpecs = ColumnSpecs { col_sep :: Char, col_fmts :: [ColumnSpec] }
  deriving (Eq,Show)  

-- | Print a column with left or right padding.
-- 
data ColumnSpec = PadL Int | PadR Int
  deriving (Eq,Ord,Show)

-- | Internal state.
--
data St = St { get_tracknum :: !Int, get_acctime :: Integer }

-- | Initial state.
--
state_zero :: St
state_zero = St { get_tracknum = 0 
                , get_acctime  = 0
                }
  
-- | Monad for generating output as a table.
--
newtype Table a = Table { 
    getTable :: ColumnSpecs -> St -> HLines -> (St,HLines,a) }


instance Functor Table where
  fmap f ma = Table $ \r s ac -> 
                 let (s1,ac1,a) = getTable ma r s ac in (s1, ac1, f a)


instance Applicative Table where
  pure a    = Table $ \_ s ac -> (s,ac,a)
  mf <*> ma = Table $ \r s ac -> 
                let (s1,ac1,f) = getTable mf r s ac
                    (s2,ac2,a) = getTable ma r s1 ac1
                in (s2,ac2,f a)



instance Monad Table where
  return    = pure
  ma >>= k  = Table $ \r s ac -> 
                let (s1,ac1,a) = getTable ma r s ac in getTable (k a) r s1 ac1


-- | Run the Table monad, returning both output and answer.
--
runTable :: ColumnSpecs -> Table a -> ([String],a)
runTable hdrs ma = 
    let (_,hf,a) = getTable ma hdrs state_zero emptyH in (fromH hf,a)


-- | Exec the Table monad, returning just the output.
--
execTable :: ColumnSpecs -> Table a -> [String]
execTable hdrs = fst . runTable hdrs




-- | Tell a row with free formatting.
--
-- Row is actually a function : track_num * running_time -> WString
-- 
tellFree :: (Int -> Integer -> WString) -> Table ()
tellFree wf = Table $ \_ s ac -> 
    let next = fromH $ doch $ wf (get_tracknum s) (get_acctime s)
    in (s, ac `snocH` next, ())


-- | Tell a row with column formatting.
--
-- Row is actually a function : track_num * running_time -> [WString]
-- 
tellRow :: (Int -> Integer -> [WString]) -> Table ()
tellRow wsf = Table $ \r s ac -> 
    let next = formatRow r $ wsf (get_tracknum s) (get_acctime s)
    in (s, ac `snocH` next, ())

-- | Tell a blank line.
--
tellBlank :: Table ()
tellBlank = Table $ \_ s ac -> let next = ""
                               in (s, ac `snocH` next, ())

-- | Tell a breaking line (formatted as a dashed line). 
--
tellBreak :: Table ()
tellBreak = Table $ \r s ac -> let next = formatBreak r
                               in (s, ac `snocH` next, ())

-- | Run the Table monad with a local ColumnSpecs, cf. the
-- @local@ function of the Reader monad class.
--
localColumns :: ColumnSpecs -> Table a -> Table a
localColumns r1 ma = Table $ \_ s ac -> getTable ma r1 s ac


-- | Proceed to the next track - reset running time.
--
nextTrack :: Table ()
nextTrack = Table $ \_ s ac -> (upd s, ac, ())
  where
    upd = (\s n -> s { get_tracknum = n+1, get_acctime = 0}) <*> get_tracknum



-- | Proceed to an arbitrary track - reset running time.
--
arbTrack :: Int -> Table ()
arbTrack n = Table $ \_ _ ac -> 
    (St { get_tracknum = n, get_acctime = 0}, ac, ())



-- | Increment runninf time.
--
incrDelta :: Integer -> Table ()
incrDelta dt = Table $ \_ s ac -> (upd s, ac, ())
  where
    upd = (\s n -> s { get_acctime = n + dt }) <*> get_acctime

-- | Helper for generating a line break
--
formatBreak :: ColumnSpecs -> String
formatBreak spec = replicate (lineLength spec) '-'


-- | Helper for generating a row.
--
formatRow :: ColumnSpecs -> [WString] -> String
formatRow (ColumnSpecs ch fmts) ws = fromH $ doch $ step fmts ws
  where
    fmt1       = PadL 9
    -- if run out of formats use default...
    step []      xs     = step [fmt1] xs
    step _      []      = mempty
    step (c:_)  [x]     = format1 c x
    step (c:cs) (x:xs)  = let d1 = format1 c x; ds = step cs xs
                          in sep ch d1 ds

-- | Helper for generating a single column.
--
format1 :: ColumnSpec -> WString -> WString
format1 (PadL n) w = if n > width w then padl n w else w
format1 (PadR n) w = if n > width w then padr n w else w


-- | Calculate the line length.
--
lineLength :: ColumnSpecs -> Int
lineLength (ColumnSpecs _ fmts) = step 0 fmts
  where
    step w []     = w
    step w [x]    = w + size x
    step w (x:xs) = step (w + size x + 1) xs

    size (PadL i) = i
    size (PadR i) = i


-- | Docs represent a single line - they should not contain 
-- newlines.
--
data WString = WString { 
    -- | Width of the doc.
    width :: !Int, 

    -- | Internal representation.
    doch :: HString }

-- | Make a literal Doc from a String.
--
wstring :: String -> WString
wstring s = WString (length s) (showString s) 

-- | Unwrap a Doc making a String.
--
outputDoc :: WString -> String
outputDoc = fromH . doch


-- | Concatenation is /directly next to/ i.e. no space.
--
instance Monoid WString where
  mempty                                = WString 0 id
  WString w1 f1 `mappend` WString w2 f2 = WString (w1+w2) (f1 . f2)



infixr 6 <+>

-- | Concatenate two WStrings with a space between them.
--
(<+>) :: WString -> WString -> WString
(<+>) = sep ' '


-- | Concatenate - with a single character separator.
--
sep :: Char -> WString -> WString -> WString
sep ch (WString w1 f1) (WString w2 f2) = WString (1+w1+w2) (f1 . (ch:) . f2)


-- | Make a WString from a Char.
--
char :: Char -> WString 
char c = WString 1 (c:)

-- | Make a WString from a String.
--
text :: String -> WString
text = wstring

-- | Repeat the Char /n/ times to make a WString.
--
repeatChar :: Int -> Char -> WString
repeatChar n c = WString n (showString $ replicate n c)


-- | Pad the left with space.
--
padl :: Int -> WString -> WString
padl i d@(WString n f) | i > n     = WString i (spaceH (i-n) . f)
                       | otherwise = d

-- | Pad the right with space.
--
padr :: Int -> WString -> WString
padr i d@(WString n f) | i > n     = WString i (f . spaceH (i-n))
                       | otherwise = d

-- | Show as a two digit hex number.
--
hex2 :: Word8 -> WString
hex2 n | n < 0x10  = WString 2 (('0' :) . showHex n)
       | otherwise = WString 2 (showHex n)  

-- | Show as a four digit hex number.
--
hex4 :: Word16 -> WString
hex4 n | n < 0x10   = WString 4 (('0':) . ('0':) . ('0':) . showHex n)
       | n < 0x100  = WString 4 (('0':) . ('0':) . showHex n)
       | n < 0x1000 = WString 4 (('0':) . showHex n)
       | otherwise  = WString 4 (showHex n)  


-- | Show an Int as a base 10 number.
--
int :: Int -> WString
int = wstring . show 


-- | Show an Integral value as a base 10 number.
--
integral :: (Show a, Integral a) => a -> WString
integral = wstring . show 