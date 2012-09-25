module CV.Filter where

-- Default for gaussian
gauss :: Int -> Mat -> Mat
gauss sigma mat = gaussian 0 0 sigma 0 mat

-- Full feature gaussian
gaussian :: Int -> Int -> Int -> Int -> Mat -> Mat
gaussian kw kh sx sy (Mat a) = Mat $ fromIntegral $ c_gaussian (ci kw) (ci kh) (ci sx) (ci sy) (ci a)


