module Common where
import Control.Monad.Writer

--всякие общие штуки
eps = 0.0001 --минимальная дистанция

write f = writer (aa, [aa]) where aa = f

cncRound x = cncRoundCommon x 4  --(fromIntegral (round (x*1000.0)))/1000.0 -- округление до 4 знаков после запятой

cncRoundCommon x d = (fromIntegral (round (x*mn)))/mn -- округление до 4 знаков после запятой
	where mn = 10^d