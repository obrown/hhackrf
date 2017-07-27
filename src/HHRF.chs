{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module HHRF (
  HHRF.init,
  exit,
  open,
  listDevices
  ) where


import Foreign
import Foreign.C
import qualified Foreign.Concurrent as FC

import Control.Applicative


#include <libhackrf/hackrf.h>


type HRFError = String
type CHRFDevice = ()
newtype HRFDevice = HRFDevice { hrfDevice :: ForeignPtr CHRFDevice }

class FromC a b where
  fromC :: a -> IO b

type HRFDeviceList = [HRFDeviceListEntry]
data HRFDeviceListEntry = HRFDeviceListEntry {
  serialNumber  :: String,
  usbBoardId    :: Int,
  usbDeviceIdx  :: Int
} deriving Show


data CHRFDeviceList = CHRFDeviceList {
  serialNumbers  :: Ptr CString,
  usbBoardIds    :: Ptr CInt,
  usbDeviceIdxs  :: Ptr CInt,
  deviceCount    :: CInt,
  usbDevices     :: Ptr (Ptr ()),
  usbDeviceCount :: CInt
}

instance Storable CHRFDeviceList where
  sizeOf _ = {#sizeof hackrf_device_list_t #}
  alignment _ = {#alignof hackrf_device_list_t #}
  peek ptr = CHRFDeviceList
                <$> {#get hackrf_device_list_t->serial_numbers #} ptr
                <*> {#get hackrf_device_list_t->usb_board_ids #} ptr
                <*> {#get hackrf_device_list_t->usb_device_index #} ptr
                <*> {#get hackrf_device_list_t->devicecount #} ptr
                <*> {#get hackrf_device_list_t->usb_devices #} ptr
                <*> {#get hackrf_device_list_t->usb_devicecount #} ptr
  poke = undefined

instance FromC CHRFDeviceList HRFDeviceList where
  fromC c = do
    let count = fromIntegral . deviceCount $ c

    serialnumbers <- peekArray count (serialNumbers c)
    serialnumbers' <- ZipList <$> mapM peekCString serialnumbers

    usbids <- peekArray count (usbBoardIds c)
    let usbids' = ZipList $ fmap fromIntegral usbids

    usbidxs <- peekArray count (usbDeviceIdxs c)
    let usbidxs' = ZipList $ fmap fromIntegral usbidxs

    return . getZipList $ liftA3 HRFDeviceListEntry serialnumbers' usbids' usbidxs'


-- | Check a return code from libHackRF
-- Return either the error message or a value
checkReturnCode :: a -> CInt -> IO (Either HRFError a)
checkReturnCode val code
  = if code == 0 then success else failure
  where failure = Left <$> peekCString (c_hackrf_error_name code)
        success = return $ Right val


-- | Run a computation with a device
withDevicePtr :: HRFDevice -> (Ptr CHRFDevice -> IO b) -> IO b
withDevicePtr device = withForeignPtr (hrfDevice device)


foreign import ccall unsafe "hackrf_error_name" c_hackrf_error_name :: CInt -> CString

-- | Retrieve the message corresponing to a libHackRF error code
errorName :: Int -> IO String
errorName = peekCString . c_hackrf_error_name . fromIntegral


foreign import ccall unsafe "hackrf_init" c_hackrf_init :: IO CInt

-- | Initialize libHackRF
init :: IO (Either HRFError ())
init = c_hackrf_init >>= checkReturnCode ()


foreign import ccall unsafe "hackrf_exit" c_hackrf_exit :: IO CInt

-- | Cleanly shutdown libHackRF
exit :: IO (Either HRFError ())
exit = c_hackrf_exit >>= checkReturnCode ()


foreign import ccall unsafe "hackrf_device_list" c_hackrf_device_list :: IO (Ptr CHRFDeviceList)
foreign import ccall unsafe "&hackrf_device_list_free" c_hackrf_device_list_free :: FunPtr (Ptr CHRFDeviceList -> IO ())

-- | List available HackRF devices
listDevices :: IO HRFDeviceList
listDevices = do
  dlptr <- c_hackrf_device_list >>= (newForeignPtr c_hackrf_device_list_free)
  withForeignPtr dlptr peek >>= fromC


foreign import ccall unsafe "hackrf_open" c_hackrf_open :: ()
foreign import ccall unsafe "hackrf_open_by_serial" c_hackrf_open_by_serial :: CString -> (Ptr (Ptr CHRFDevice)) -> IO CInt
foreign import ccall unsafe "hackrf_close" c_hackrf_close :: Ptr CHRFDevice -> IO ()

-- | Open a HackRF device
open :: String -> IO (Either HRFError HRFDevice)
open serialNumber = do
  withCString serialNumber $ \cSerialNumber ->
    alloca $ \ptr -> do
      status <- c_hackrf_open_by_serial cSerialNumber ptr >>= checkReturnCode ()
      case status of
        Left msg -> return $ Left msg
        Right _ -> Right . HRFDevice <$> (peek ptr >>= \ptr -> FC.newForeignPtr ptr (c_hackrf_close ptr))

open0 :: IO (Either HRFError HRFDevice)
open0 = do
  devices <- listDevices
  case devices of
    [] -> return $ Left "no devices found"
    (x:_) -> open $ serialNumber x


foreign import ccall unsafe "hackrf_set_freq" c_hackrf_set_freq :: Ptr CHRFDevice -> Word64 -> IO CInt

setFreq :: (Integral a) => HRFDevice -> a -> IO (Either HRFError ())
setFreq device freq
  = withDevicePtr device $ \devptr ->
      c_hackrf_set_freq devptr (fromIntegral freq) >>= checkReturnCode ()


foreign import ccall unsafe "hackrf_set_sample_rate" c_hackrf_set_sample_rate :: Ptr CHRFDevice -> CDouble -> IO CInt

setRate :: (Integral a) => HRFDevice -> a -> IO (Either HRFError ())
setRate device rate
  = withDevicePtr device $ \devptr ->
      c_hackrf_set_sample_rate devptr (fromIntegral rate) >>= checkReturnCode ()


foreign import ccall unsafe "hackrf_set_lna_gain" c_hackrf_set_lna_gain :: Ptr CHRFDevice -> Word32 -> IO CInt

setLNAGain :: (Integral a) => HRFDevice -> a -> IO (Either HRFError ())
setLNAGain device gain
  = withDevicePtr device $ \devptr ->
      c_hackrf_set_lna_gain devptr (fromIntegral gain) >>= checkReturnCode ()


foreign import ccall unsafe "hackrf_set_vga_gain" c_hackrf_set_vga_gain :: Ptr CHRFDevice -> Word32 -> IO CInt

setVGAGain :: (Integral a) => HRFDevice -> a -> IO (Either HRFError ())
setVGAGain device gain
  = withDevicePtr device $ \devptr ->
      c_hackrf_set_vga_gain devptr (fromIntegral gain) >>= checkReturnCode ()
