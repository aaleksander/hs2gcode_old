module Common where
import Control.Monad.Writer

--всякие общие штуки


write f = writer (aa, [aa]) where aa = f