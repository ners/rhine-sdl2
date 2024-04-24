{-# LANGUAGE TemplateHaskell #-}

module Font where

import Data.LruCache (LruCache)
import Data.LruCache qualified as LruCache
import SDL qualified
import SDL.Font qualified
import SDL.Font qualified as SDL (Font)
import Prelude

type FontCache = LruCache Text SDL.Texture

data Font = Font
    { font :: SDL.Font
    , size :: CInt
    , cache :: TVar FontCache
    }

getFont :: (MonadIO m) => ByteString -> SDL.Font.PointSize -> Int -> m Font
getFont bs ps cacheSize = do
    font <- SDL.Font.decode bs ps
    cache <- liftIO . newTVarIO $ LruCache.empty cacheSize
    pure Font{font, size = fromIntegral ps, cache}

data Fonts = Fonts
    { cherry13b :: Font
    , cherry13r :: Font
    }

getFonts :: (MonadIO m) => Int -> m Fonts
getFonts cacheSize = do
    cherry13b <- getFont $(embedFileRelative "assets/cherry-13-b.ttf") 13 cacheSize
    cherry13r <- getFont $(embedFileRelative "assets/cherry-13-r.ttf") 13 cacheSize
    pure Fonts{..}
