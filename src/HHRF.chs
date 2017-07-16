{-# LANGUAGE ForeignFunctionInterface #-}
module HHRF (
  HHRF.init,
  exit,
  open,
  listDevices
  ) where


import Foreign
import Foreign.Storable
import Foreign.Ptr
import Foreign.C
import Foreign.ForeignPtr
import Foreign.Marshal.Array

import Control.Applicative
import Control.Monad


#include <libhackrf/hackrf.h>


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

type CHRFDevice = Ptr ()
newtype HRFDevice = HRFDevice { hrfDevice :: ForeignPtr CHRFDevice }


-- | Check a return code from libHackRF
-- Return either the error message or an empty value
checkReturnCode :: CInt -> IO (Either String ())
checkReturnCode code
  = if code == 0 then success else failure
  where failure = Left <$> peekCString (c_hackrf_error_name code)
        success = return $ Right ()


foreign import ccall unsafe "hackrf_error_name" c_hackrf_error_name :: CInt -> CString

-- | Retrieve the message corresponing to a libHackRF error code
errorName :: Int -> IO String
errorName = peekCString . c_hackrf_error_name . fromIntegral


foreign import ccall unsafe "hackrf_init" c_hackrf_init :: IO CInt

-- | Initialize libHackRF
init :: IO (Either String ())
init = c_hackrf_init >>= checkReturnCode


foreign import ccall unsafe "hackrf_exit" c_hackrf_exit :: IO CInt

-- | Cleanly shutdown libHackRF
exit :: IO (Either String ())
exit = c_hackrf_exit >>= checkReturnCode


foreign import ccall unsafe "hackrf_device_list" c_hackrf_device_list :: IO (Ptr CHRFDeviceList)
foreign import ccall unsafe "&hackrf_device_list_free" c_hackrf_device_list_free :: FunPtr (Ptr CHRFDeviceList -> IO ())

-- | List available HackRF devices
listDevices :: IO HRFDeviceList
listDevices = do
  dl <- c_hackrf_device_list >>= (newForeignPtr c_hackrf_device_list_free)
  devicelist <- withForeignPtr dl peek
  let count = fromIntegral . deviceCount $ devicelist

  serialnumbers <- peekArray count (serialNumbers devicelist)
  serialnumbers' <- ZipList <$> mapM peekCString serialnumbers

  usbids <- peekArray count (usbBoardIds devicelist)
  let usbids' = ZipList $ fmap fromIntegral usbids

  usbidxs <- peekArray count (usbDeviceIdxs devicelist)
  let usbidxs' = ZipList $ fmap fromIntegral usbidxs

  return . getZipList $ liftA3 HRFDeviceListEntry serialnumbers' usbids' usbidxs'


foreign import ccall unsafe "hackrf_open" c_hackrf_open :: ()
foreign import ccall unsafe "hackrf_open_by_serial" c_hackrf_open_by_serial :: CString -> (Ptr CHRFDevice) -> IO CInt
foreign import ccall unsafe "&hackrf_close" c_hackrf_close :: FunPtr (Ptr CHRFDevice -> IO ())

-- | Open a HackRF device
open :: String -> IO (Either String HRFDevice)
open serialNumber = undefined
