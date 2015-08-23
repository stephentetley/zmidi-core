{-# LANGUAGE CPP                        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Core.Internal.ParseMonad
-- Copyright   :  (c) Stephen Tetley 2010-2015
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  unstable
-- Portability :  As per dependencies.
--
-- A parse monad - better error handling than Binary-Get.
--
--------------------------------------------------------------------------------

module ZMidi.Core.Internal.ParserMonad
  (


    RS_VoiceEvent(..)
  , ErrMsg
  , Pos
  , ParseErr(..)
  , ParserM
  , runParser

  , getRunningEvent
  , setRunningEvent

  , peek
  , cond
  , dropW8
  , word8
  , int8

  , word16be
  , word24be
  , word32be
  , char8


  , (<??>)
  , fatalError

  , count
  , gencount
  , text
  , boundRepeat
  , pair
  

  ) where


#ifndef MIN_VERSION_GLASGOW_HASKELL
import Control.Applicative
#endif
import Data.Bits
import qualified Data.ByteString.Lazy as L
import Data.Char
import Data.Int
import Data.Word

-- | Status is either OFF of the previous VoiceEvent * Channel.
--
data RS_VoiceEvent = RS_STATUS_OFF
                   | RS_NOTE_OFF    !Word8
                   | RS_NOTE_ON     !Word8
                   | RS_NOTE_AFT    !Word8
                   | RS_CONTROL     !Word8
                   | RS_PROG_CHANGE !Word8
                   | RS_CHAN_AFT    !Word8
                   | RS_PCH_BEND    !Word8
  deriving (Eq,Show)                 

-- | Position of the parser in the input stream.
-- 
-- This is exposed by the ReadFile API and may be useful for 
-- /disassembling/ a MIDI file that causes a parse failure.
-- 
type Pos = Int

data ParserState = ParserState 
       { pos                    :: !Pos
       , running_status         :: !RS_VoiceEvent
       , input                  :: !L.ByteString 
       }


-- | Error message - alias for String.
--
type ErrMsg = String

-- | ParseErr is the position of the error and a message.
--
data ParseErr = ParseErr !Pos !ErrMsg
  deriving (Eq,Show)

-- | Parser newtype.
--
newtype ParserM a = ParserM 
          { getParserM :: ParserState -> (Either ParseErr a, ParserState) }


instance Functor ParserM where
  fmap f mf = ParserM $ \s -> let (ans,s') = getParserM mf s in (fmap f ans,s')
  

instance Applicative ParserM where
  pure a    = ParserM $ \s -> (Right a, s)
  af <*> ma = ParserM $ \s -> let (ef,s')  = getParserM af s
                              in case ef of 
                                Left e  -> (Left e, s') 
                                Right f -> let (a,s'') = getParserM ma s'
                                           in (fmap f a,s'')

instance Monad ParserM where
  return a  = ParserM $ \s -> (Right a, s)
  m >>= k   = ParserM $ \s -> let (ea,s') = getParserM m s
                              in case ea of
                                Left e -> (Left e, s')
                                Right a -> (getParserM . k) a s'


-- | Run the parser.
--
runParser :: L.ByteString -> ParserM a -> Either ParseErr a
runParser bs mf = 
    fst $ getParserM mf state_zero 
  where
    -- seed the initial state with an unmatchable event
    state_zero = ParserState { pos = 0
                             , running_status = RS_STATUS_OFF
                             , input = bs }


-- | Get the Running Status flag and channel.
--
getRunningEvent :: ParserM RS_VoiceEvent
getRunningEvent = ParserM $ \s -> (Right $ running_status s, s)

-- | Set the Running Status flag and channel.
--
setRunningEvent :: RS_VoiceEvent -> ParserM ()
setRunningEvent rs = ParserM $ \s -> (Right (), s { running_status = rs })


-- | Get current Pos.
--
getPos :: ParserM Int
getPos = ParserM $ \s -> (Right $ pos s, s)


-- | Peek a Word8.
--
peek :: ParserM Word8
peek = ParserM $ \s@(ParserState n rs bs) -> case L.uncons bs of 
    Nothing      -> (Left (ParseErr n "peek - no more data."), s)
    Just (a,_)   -> (Right a, ParserState n rs bs)

-- | Conditionally get a Word8. Fails if input is finished.
-- Consumes data on if predicate succeeds, does not consume if
-- predicate fails.
--
cond :: (Word8 -> Bool) -> ParserM (Maybe Word8)
cond pf = ParserM $ \s@(ParserState n rs bs) -> case L.uncons bs of 
    Nothing      -> (Left (ParseErr n "peek - no more data."), s)
    Just (a,bs1) -> if pf a then (Right $ Just a, ParserState (n+1) rs bs1)
                            else (Right Nothing, ParserState n rs bs)
                       

-- | Drop a Word8.
--
dropW8 :: ParserM ()
dropW8 = ParserM $ \s@(ParserState n rs bs) -> case L.uncons bs of 
    Nothing      -> (Left (ParseErr n "dropW8 - no more data."), s)
    Just (_,bs') -> (Right (), ParserState (n+1) rs bs')


-- | Parse a Word8.
--
word8 :: ParserM Word8
word8 = ParserM $ \s@(ParserState n rs bs) -> case L.uncons bs of 
    Nothing      -> (Left (ParseErr n "word8 - no more data."), s)
    Just (a,bs') -> (Right a, ParserState (n+1) rs bs')


-- | Parse an Int8.
--
int8 :: ParserM Int8
int8 = fromIntegral <$> word8


-- | Parse a Word16 (big-endian).
--
word16be :: ParserM Word16
word16be = ParserM $ \s@(ParserState n rs bs) -> case uncons2 bs of
    Nothing -> (Left (ParseErr n "word16be - no more data."), s)
    Just (a,b,bs') -> (Right $ w16be a b, ParserState (n+2) rs bs')

-- | Parse a Word24 (big-endian).
--
word24be :: ParserM Word32
word24be = ParserM $ \s@(ParserState n rs bs) -> case uncons3 bs of
    Nothing -> (Left (ParseErr n "word24be - no more data."), s)
    Just (a,b,c,bs') -> (Right $ w24be a b c, ParserState (n+3) rs bs')

-- | Parse a Word32 (big-endian).
--
word32be :: ParserM Word32
word32be = ParserM $ \s@(ParserState n rs bs) -> case uncons4 bs of
    Nothing -> (Left (ParseErr n "word32be - no more data."), s)
    Just (a,b,c,d,bs') -> (Right $ w32be a b c d, ParserState (n+4) rs bs')

-- | Parse a Char.
--
char8 :: ParserM Char
char8 = (chr . fromIntegral) <$> word8

infixr 0 <??>

-- | Assign an error message to a Parser.
--
(<??>) :: ErrMsg -> ParserM a -> ParserM a
(<??>) msg p = ParserM $ \s -> 
                 case getParserM p s of
                   (Left (ParseErr n _),s') -> (Left (ParseErr n msg),s')
                   (Right a,            s') -> (Right a,s')

-- | Throw a fatal error.
--
fatalError :: ErrMsg -> ParserM a
fatalError msg = ParserM $ \s -> (Left (ParseErr (pos s) msg), s)

{-

-- It is possible to catch error, _but_ we can\'t reliably
-- recover from them so this function is not useful.
--
-- We can\'t recover as we don\'t know where in the input to 
-- start parsing from if we drop the problemmatic input.
--
-- Unlike Parsec, the MIDI format is deterministic - we don\'t 
-- need backtracking to resolve ambiguity. So we don\'t need
-- a choice or alt combinator. 
-- 

-- | This uses backtracking...
--
catchError :: ParserM a -> (ErrMsg -> ParserM a) -> ParserM a
catchError f g = ParserM $ \s -> 
                   case getParserM f s of
                     (Left (ParseErr _ msg),_)  -> getParserM (g msg) s
                     (Right a,              s') -> (Right a,s')
-}


-- | Apply the parser for /count/ times, forming a list.
--
count :: Int -> ParserM a -> ParserM [a]
count i p 
    | i <= 0    = pure []
    | otherwise = (:) <$> p <*> count (i-1) p


-- | Apply the parser for /count/ times, derive the final answer
-- from the intermediate list with the supplied function.
--
gencount :: Integral i => ParserM i -> ParserM a -> (i -> [a] -> ans) -> ParserM ans
gencount plen p constr = do 
    i   <- plen
    xs  <- count (fromIntegral i) p 
    return $ constr i xs

-- | Parse a text of length /n/.
--
text :: Int -> ParserM String
text i = count i char8

-- | Run a parser within a bounded section of the input stream.
--
boundRepeat :: Int -> ParserM a -> ParserM [a]
boundRepeat n p = getPos >>= \start -> step (start + n)
  where
    step lim = do { a <- p
                  ; i <- getPos 
                  ; case compare i lim of
                      LT -> do { as <- step lim; return (a:as) }
                      EQ -> return [a]
                      GT -> fatalError "boundRepeat - parser exceeds limit"
                  }


-- | Run a parser twice, pairing the result.
--
pair :: ParserM a -> ParserM (a,a)
pair p = (,) <$> p <*> p

--------------------------------------------------------------------------------
-- helpers

-- | Take two elements from the ByteString.
--
uncons2 :: L.ByteString -> Maybe (Word8,Word8,L.ByteString)
uncons2 bs = L.uncons bs  >>= \(a,bs1) -> 
             L.uncons bs1 >>= \(b,bs2) -> return (a,b,bs2)


-- | Take three elements from the ByteString.
--
uncons3 :: L.ByteString -> Maybe (Word8,Word8,Word8,L.ByteString)
uncons3 bs = L.uncons bs  >>= \(a,bs1) -> 
             L.uncons bs1 >>= \(b,bs2) -> 
             L.uncons bs2 >>= \(c,bs3) -> return (a,b,c,bs3)

-- | Take four elements from the ByteString.
--
uncons4 :: L.ByteString -> Maybe (Word8,Word8,Word8,Word8,L.ByteString)
uncons4 bs = L.uncons bs  >>= \(a,bs1)  -> 
             L.uncons bs1 >>= \(b,bs2) -> 
             L.uncons bs2 >>= \(c,bs3) -> 
             L.uncons bs3 >>= \(d,bs4) -> return (a,b,c,d,bs4)


-- | Build a Word16 (big endian).
--
w16be :: Word8 -> Word8 -> Word16
w16be a b       = (shiftL `flip` 8  $ fromIntegral a) + fromIntegral b

-- | Build a Word24 (big endian).
--
w24be :: Word8 -> Word8 -> Word8 -> Word32
w24be a b c     = (shiftL `flip` 16  $ fromIntegral a) 
                + (shiftL `flip`  8  $ fromIntegral b) 
                + fromIntegral c

-- | Build a Word16 (big endian).
--
w32be :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
w32be a b c d   = (shiftL `flip` 24  $ fromIntegral a) 
                + (shiftL `flip` 16  $ fromIntegral b) 
                + (shiftL `flip`  8  $ fromIntegral c) 
                + fromIntegral d