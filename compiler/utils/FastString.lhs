%
% (c) The University of Glasgow, 1997-2006
%
\begin{code}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -O -funbox-strict-fields #-}
-- We always optimise this, otherwise performance of a non-optimised
-- compiler is severely affected

-- |
-- There are two principal string types used internally by GHC:
--
-- 'FastString':
--               * A compact, hash-consed, representation of character strings.
--               * Comparison is O(1), and you can get a 'Unique.Unique' from them.
--               * Generated by 'fsLit'.
--               * Turn into 'Outputable.SDoc' with 'Outputable.ftext'.
--
-- 'LitString':
--               * Just a wrapper for the @Addr#@ of a C string (@Ptr CChar@).
--               * Practically no operations.
--               * Outputing them is fast.
--               * Generated by 'sLit'.
--               * Turn into 'Outputable.SDoc' with 'Outputable.ptext'
--
-- Use 'LitString' unless you want the facilities of 'FastString'.
module FastString
       (
        -- * FastBytes
        FastBytes,
        fastStringToFastBytes,
        mkFastStringByteString,
        fastZStringToByteString,
        unsafeMkFastBytesString,
        hashByteString,

        -- * FastZString
        FastZString,
        hPutFZS,
        zString,
        lengthFZS,

        -- * FastStrings
        FastString(..),     -- not abstract, for now.

        -- ** Construction
        fsLit,
        mkFastString,
        mkFastStringBytes,
        mkFastStringByteList,
        mkFastStringForeignPtr,
#if defined(__GLASGOW_HASKELL__)
        mkFastString#,
#endif

        -- ** Deconstruction
        unpackFS,           -- :: FastString -> String
        bytesFS,            -- :: FastString -> [Word8]

        -- ** Encoding
        zEncodeFS,

        -- ** Operations
        uniqueOfFS,
        lengthFS,
        nullFS,
        appendFS,
        headFS,
        tailFS,
        concatFS,
        consFS,
        nilFS,

        -- ** Outputing
        hPutFS,

        -- ** Internal
        getFastStringTable,
        hasZEncoding,

        -- * LitStrings
        LitString,
        
        -- ** Construction
        sLit,
#if defined(__GLASGOW_HASKELL__)
        mkLitString#,
#endif
        mkLitString,
        
        -- ** Deconstruction
        unpackLitString,
        
        -- ** Operations
        lengthLS
       ) where

#include "HsVersions.h"

import Encoding
import FastTypes
import FastFunctions
import Panic
import Util

import Data.ByteString (ByteString)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Char8    as BSC
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Unsafe   as BS
import Foreign.C
import GHC.Exts
import System.IO
import System.IO.Unsafe ( unsafePerformIO )
import Data.Data
import Data.IORef       ( IORef, newIORef, readIORef, writeIORef )
import Data.Maybe       ( isJust )
import Data.Char

import GHC.IO           ( IO(..) )

import Foreign.Safe

#if defined(__GLASGOW_HASKELL__)
import GHC.Base         ( unpackCString# )
#endif

#define hASH_TBL_SIZE          4091
#define hASH_TBL_SIZE_UNBOXED  4091#


type FastBytes = ByteString

fastStringToFastBytes :: FastString -> FastBytes
fastStringToFastBytes f = fs_fb f

fastZStringToByteString :: FastZString -> ByteString
fastZStringToByteString (FastZString bs) = bs

-- This will drop information if any character > '\xFF'
unsafeMkFastBytesString :: String -> FastBytes
unsafeMkFastBytesString = BSC.pack

hashByteString :: ByteString -> Int
hashByteString bs
    = inlinePerformIO $ BS.unsafeUseAsCStringLen bs $ \(ptr, len) ->
      return $ hashStr (castPtr ptr) len

-- -----------------------------------------------------------------------------

newtype FastZString = FastZString ByteString

hPutFZS :: Handle -> FastZString -> IO ()
hPutFZS handle (FastZString bs) = BS.hPut handle bs

zString :: FastZString -> String
zString (FastZString bs) =
    inlinePerformIO $ BS.unsafeUseAsCStringLen bs peekCAStringLen

lengthFZS :: FastZString -> Int
lengthFZS (FastZString bs) = BS.length bs

mkFastZStringString :: String -> FastZString
mkFastZStringString str = FastZString (BSC.pack str)

-- -----------------------------------------------------------------------------

{-|
A 'FastString' is an array of bytes, hashed to support fast O(1)
comparison.  It is also associated with a character encoding, so that
we know how to convert a 'FastString' to the local encoding, or to the
Z-encoding used by the compiler internally.

'FastString's support a memoized conversion to the Z-encoding via zEncodeFS.
-}

data FastString = FastString {
      uniq    :: {-# UNPACK #-} !Int, -- unique id
      n_chars :: {-# UNPACK #-} !Int, -- number of chars
      fs_fb   :: {-# UNPACK #-} !FastBytes,
      fs_ref  :: {-# UNPACK #-} !(IORef (Maybe FastZString))
  } deriving Typeable

instance Eq FastString where
  f1 == f2  =  uniq f1 == uniq f2

instance Ord FastString where
    -- Compares lexicographically, not by unique
    a <= b = case cmpFS a b of { LT -> True;  EQ -> True;  GT -> False }
    a <  b = case cmpFS a b of { LT -> True;  EQ -> False; GT -> False }
    a >= b = case cmpFS a b of { LT -> False; EQ -> True;  GT -> True  }
    a >  b = case cmpFS a b of { LT -> False; EQ -> False; GT -> True  }
    max x y | x >= y    =  x
            | otherwise =  y
    min x y | x <= y    =  x
            | otherwise =  y
    compare a b = cmpFS a b

instance Show FastString where
   show fs = show (unpackFS fs)

instance Data FastString where
  -- don't traverse?
  toConstr _   = abstractConstr "FastString"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = mkNoRepType "FastString"

cmpFS :: FastString -> FastString -> Ordering
cmpFS f1@(FastString u1 _ _ _) f2@(FastString u2 _ _ _) =
  if u1 == u2 then EQ else
  compare (fastStringToFastBytes f1) (fastStringToFastBytes f2)

#ifndef __HADDOCK__
foreign import ccall unsafe "ghc_memcmp"
  memcmp :: Ptr a -> Ptr b -> Int -> IO Int
#endif

-- -----------------------------------------------------------------------------
-- Construction

{-
Internally, the compiler will maintain a fast string symbol
table, providing sharing and fast comparison. Creation of
new @FastString@s then covertly does a lookup, re-using the
@FastString@ if there was a hit.
-}

data FastStringTable =
 FastStringTable
    {-# UNPACK #-} !Int
    (MutableArray# RealWorld [FastString])

{-# NOINLINE string_table #-}
string_table :: IORef FastStringTable
string_table =
 unsafePerformIO $ do
   tab <- IO $ \s1# -> case newArray# hASH_TBL_SIZE_UNBOXED [] s1# of
                           (# s2#, arr# #) ->
                               (# s2#, FastStringTable 0 arr# #)
   newIORef tab

lookupTbl :: FastStringTable -> Int -> IO [FastString]
lookupTbl (FastStringTable _ arr#) (I# i#) =
  IO $ \ s# -> readArray# arr# i# s#

updTbl :: IORef FastStringTable -> FastStringTable -> Int -> [FastString] -> IO ()
updTbl fs_table_var (FastStringTable uid arr#) (I# i#) ls = do
  (IO $ \ s# -> case writeArray# arr# i# ls s# of { s2# -> (# s2#, () #) })
  writeIORef fs_table_var (FastStringTable (uid+1) arr#)

mkFastString# :: Addr# -> FastString
mkFastString# a# = mkFastStringBytes ptr (ptrStrLength ptr)
  where ptr = Ptr a#

mkFastStringBytes :: Ptr Word8 -> Int -> FastString
mkFastStringBytes ptr len = unsafePerformIO $ do
  ft@(FastStringTable uid _) <- readIORef string_table
  let
   h = hashStr ptr len
   add_it ls = do
        fs <- copyNewFastString uid ptr len
        updTbl string_table ft h (fs:ls)
        {- _trace ("new: " ++ show f_str)   $ -}
        return fs
  --
  lookup_result <- lookupTbl ft h
  case lookup_result of
    [] -> add_it []
    ls -> do
       b <- bucket_match ls len ptr
       case b of
         Nothing -> add_it ls
         Just v  -> {- _trace ("re-use: "++show v) $ -} return v

-- | Create a 'FastString' from an existing 'ForeignPtr'; the difference
-- between this and 'mkFastStringBytes' is that we don't have to copy
-- the bytes if the string is new to the table.
mkFastStringForeignPtr :: Ptr Word8 -> ForeignPtr Word8 -> Int -> IO FastString
mkFastStringForeignPtr ptr fp len = do
  ft@(FastStringTable uid _) <- readIORef string_table
--  _trace ("hashed: "++show (I# h)) $
  let
    h = hashStr ptr len
    add_it ls = do
        fs <- mkNewFastString uid ptr fp len
        updTbl string_table ft h (fs:ls)
        {- _trace ("new: " ++ show f_str)   $ -}
        return fs
  --
  lookup_result <- lookupTbl ft h
  case lookup_result of
    [] -> add_it []
    ls -> do
       b <- bucket_match ls len ptr
       case b of
         Nothing -> add_it ls
         Just v  -> {- _trace ("re-use: "++show v) $ -} return v

-- | Create a 'FastString' from an existing 'ForeignPtr'; the difference
-- between this and 'mkFastStringBytes' is that we don't have to copy
-- the bytes if the string is new to the table.
mkFastStringByteString :: ByteString -> IO FastString
mkFastStringByteString bs = BS.unsafeUseAsCStringLen bs $ \(ptr, len) -> do
  ft@(FastStringTable uid _) <- readIORef string_table
--  _trace ("hashed: "++show (I# h)) $
  let
    ptr' = castPtr ptr
    h = hashStr ptr' len
    add_it ls = do
        fs <- mkNewFastStringByteString uid ptr' len bs
        updTbl string_table ft h (fs:ls)
        {- _trace ("new: " ++ show f_str)   $ -}
        return fs
  --
  lookup_result <- lookupTbl ft h
  case lookup_result of
    [] -> add_it []
    ls -> do
       b <- bucket_match ls len ptr'
       case b of
         Nothing -> add_it ls
         Just v  -> {- _trace ("re-use: "++show v) $ -} return v

-- | Creates a UTF-8 encoded 'FastString' from a 'String'
mkFastString :: String -> FastString
mkFastString str =
  inlinePerformIO $ do
    let l = utf8EncodedLength str
    buf <- mallocForeignPtrBytes l
    withForeignPtr buf $ \ptr -> do
      utf8EncodeString ptr str
      mkFastStringForeignPtr ptr buf l

-- | Creates a 'FastString' from a UTF-8 encoded @[Word8]@
mkFastStringByteList :: [Word8] -> FastString
mkFastStringByteList str =
  inlinePerformIO $ do
    let l = Prelude.length str
    buf <- mallocForeignPtrBytes l
    withForeignPtr buf $ \ptr -> do
      pokeArray (castPtr ptr) str
      mkFastStringForeignPtr ptr buf l

-- | Creates a Z-encoded 'FastString' from a 'String'
mkZFastString :: String -> FastZString
mkZFastString = mkFastZStringString

bucket_match :: [FastString] -> Int -> Ptr Word8 -> IO (Maybe FastString)
bucket_match [] _ _ = return Nothing
bucket_match (v@(FastString _ _ bs _):ls) len ptr
      | len == BS.length bs = do
         b <- BS.unsafeUseAsCString bs $ \buf ->
             cmpStringPrefix ptr (castPtr buf) len
         if b then return (Just v)
              else bucket_match ls len ptr
      | otherwise =
         bucket_match ls len ptr

mkNewFastString :: Int -> Ptr Word8 -> ForeignPtr Word8 -> Int
                -> IO FastString
mkNewFastString uid ptr fp len = do
  ref <- newIORef Nothing
  n_chars <- countUTF8Chars ptr len
  return (FastString uid n_chars (BS.fromForeignPtr fp 0 len) ref)

mkNewFastStringByteString :: Int -> Ptr Word8 -> Int -> ByteString
                          -> IO FastString
mkNewFastStringByteString uid ptr len bs = do
  ref <- newIORef Nothing
  n_chars <- countUTF8Chars ptr len
  return (FastString uid n_chars bs ref)

copyNewFastString :: Int -> Ptr Word8 -> Int -> IO FastString
copyNewFastString uid ptr len = do
  fp <- copyBytesToForeignPtr ptr len
  ref <- newIORef Nothing
  n_chars <- countUTF8Chars ptr len
  return (FastString uid n_chars (BS.fromForeignPtr fp 0 len) ref)

copyBytesToForeignPtr :: Ptr Word8 -> Int -> IO (ForeignPtr Word8)
copyBytesToForeignPtr ptr len = do
  fp <- mallocForeignPtrBytes len
  withForeignPtr fp $ \ptr' -> copyBytes ptr' ptr len
  return fp

cmpStringPrefix :: Ptr Word8 -> Ptr Word8 -> Int -> IO Bool
cmpStringPrefix ptr1 ptr2 len =
 do r <- memcmp ptr1 ptr2 len
    return (r == 0)


hashStr  :: Ptr Word8 -> Int -> Int
 -- use the Addr to produce a hash value between 0 & m (inclusive)
hashStr (Ptr a#) (I# len#) = loop 0# 0#
   where
    loop h n | n GHC.Exts.==# len# = I# h
             | otherwise  = loop h2 (n GHC.Exts.+# 1#)
          where !c = ord# (indexCharOffAddr# a# n)
                !h2 = (c GHC.Exts.+# (h GHC.Exts.*# 128#)) `remInt#`
                      hASH_TBL_SIZE#

-- -----------------------------------------------------------------------------
-- Operations

-- | Returns the length of the 'FastString' in characters
lengthFS :: FastString -> Int
lengthFS f = n_chars f

-- | Returns @True@ if this 'FastString' is not Z-encoded but already has
-- a Z-encoding cached (used in producing stats).
hasZEncoding :: FastString -> Bool
hasZEncoding (FastString _ _ _ ref) =
      inlinePerformIO $ do
        m <- readIORef ref
        return (isJust m)

-- | Returns @True@ if the 'FastString' is empty
nullFS :: FastString -> Bool
nullFS f = BS.null (fs_fb f)

-- | Unpacks and decodes the FastString
unpackFS :: FastString -> String
unpackFS (FastString _ _ bs _) =
  inlinePerformIO $ BS.unsafeUseAsCStringLen bs $ \(ptr, len) ->
        utf8DecodeString (castPtr ptr) len

-- | Gives the UTF-8 encoded bytes corresponding to a 'FastString'
bytesFS :: FastString -> [Word8]
bytesFS fs = BS.unpack $ fastStringToFastBytes fs

-- | Returns a Z-encoded version of a 'FastString'.  This might be the
-- original, if it was already Z-encoded.  The first time this
-- function is applied to a particular 'FastString', the results are
-- memoized.
--
zEncodeFS :: FastString -> FastZString
zEncodeFS fs@(FastString _ _ _ ref) =
      inlinePerformIO $ do
        m <- readIORef ref
        case m of
          Just zfs -> return zfs
          Nothing -> do
            let zfs = mkZFastString (zEncodeString (unpackFS fs))
            writeIORef ref (Just zfs)
            return zfs

appendFS :: FastString -> FastString -> FastString
appendFS fs1 fs2 = inlinePerformIO
                 $ mkFastStringByteString
                 $ BS.append (fastStringToFastBytes fs1)
                             (fastStringToFastBytes fs2)

concatFS :: [FastString] -> FastString
concatFS ls = mkFastString (Prelude.concat (map unpackFS ls)) -- ToDo: do better

headFS :: FastString -> Char
headFS (FastString _ 0 _ _) = panic "headFS: Empty FastString"
headFS (FastString _ _ bs _) =
  inlinePerformIO $ BS.unsafeUseAsCString bs $ \ptr ->
         return (fst (utf8DecodeChar (castPtr ptr)))

tailFS :: FastString -> FastString
tailFS (FastString _ 0 _ _) = panic "tailFS: Empty FastString"
tailFS (FastString _ _ bs _) =
    inlinePerformIO $ BS.unsafeUseAsCString bs $ \ptr ->
    do let (_, ptr') = utf8DecodeChar (castPtr ptr)
           n = ptr' `minusPtr` ptr
       mkFastStringByteString $ BS.drop n bs

consFS :: Char -> FastString -> FastString
consFS c fs = mkFastString (c : unpackFS fs)

uniqueOfFS :: FastString -> FastInt
uniqueOfFS (FastString u _ _ _) = iUnbox u

nilFS :: FastString
nilFS = mkFastString ""

-- -----------------------------------------------------------------------------
-- Stats

getFastStringTable :: IO [[FastString]]
getFastStringTable = do
  tbl <- readIORef string_table
  buckets <- mapM (lookupTbl tbl) [0 .. hASH_TBL_SIZE]
  return buckets

-- -----------------------------------------------------------------------------
-- Outputting 'FastString's

-- |Outputs a 'FastString' with /no decoding at all/, that is, you
-- get the actual bytes in the 'FastString' written to the 'Handle'.
hPutFS :: Handle -> FastString -> IO ()
hPutFS handle fs = BS.hPut handle $ fastStringToFastBytes fs

-- ToDo: we'll probably want an hPutFSLocal, or something, to output
-- in the current locale's encoding (for error messages and suchlike).

-- -----------------------------------------------------------------------------
-- LitStrings, here for convenience only.

-- hmm, not unboxed (or rather FastPtr), interesting
--a.k.a. Ptr CChar, Ptr Word8, Ptr (), hmph.  We don't
--really care about C types in naming, where we can help it.
type LitString = Ptr Word8
--Why do we recalculate length every time it's requested?
--If it's commonly needed, we should perhaps have
--data LitString = LitString {-#UNPACK#-}!(FastPtr Word8) {-#UNPACK#-}!FastInt

#if defined(__GLASGOW_HASKELL__)
mkLitString# :: Addr# -> LitString
mkLitString# a# = Ptr a#
#endif
--can/should we use FastTypes here?
--Is this likely to be memory-preserving if only used on constant strings?
--should we inline it? If lucky, that would make a CAF that wouldn't
--be computationally repeated... although admittedly we're not
--really intending to use mkLitString when __GLASGOW_HASKELL__...
--(I wonder, is unicode / multi-byte characters allowed in LitStrings
-- at all?)
{-# INLINE mkLitString #-}
mkLitString :: String -> LitString
mkLitString s =
 unsafePerformIO (do
   p <- mallocBytes (length s + 1)
   let
     loop :: Int -> String -> IO ()
     loop !n [] = pokeByteOff p n (0 :: Word8)
     loop n (c:cs) = do
        pokeByteOff p n (fromIntegral (ord c) :: Word8)
        loop (1+n) cs
   loop 0 s
   return p
 )

unpackLitString :: LitString -> String
unpackLitString p_ = case pUnbox p_ of
 p -> unpack (_ILIT(0))
  where
    unpack n = case indexWord8OffFastPtrAsFastChar p n of
      ch -> if ch `eqFastChar` _CLIT('\0')
            then [] else cBox ch : unpack (n +# _ILIT(1))

lengthLS :: LitString -> Int
lengthLS = ptrStrLength

-- for now, use a simple String representation
--no, let's not do that right now - it's work in other places
#if 0
type LitString = String

mkLitString :: String -> LitString
mkLitString = id

unpackLitString :: LitString -> String
unpackLitString = id

lengthLS :: LitString -> Int
lengthLS = length

#endif

-- -----------------------------------------------------------------------------
-- under the carpet

foreign import ccall unsafe "ghc_strlen"
  ptrStrLength :: Ptr Word8 -> Int

{-# NOINLINE sLit #-}
sLit :: String -> LitString
sLit x  = mkLitString x

{-# NOINLINE fsLit #-}
fsLit :: String -> FastString
fsLit x = mkFastString x

{-# RULES "slit"
    forall x . sLit  (unpackCString# x) = mkLitString#  x #-}
{-# RULES "fslit"
    forall x . fsLit (unpackCString# x) = mkFastString# x #-}
\end{code}
