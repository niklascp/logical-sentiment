{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Pretty (Pretty, latexify, render) where
import System.Directory (setCurrentDirectory)
import System.Cmd (rawSystem)

class Pretty a where
  render :: a -> String

instance Pretty String where
  render s = concat $ map f s
             where f s = case s of
                           '$' -> "\\$"
                           x   -> [x]

latexify :: Pretty a => a -> IO ()
latexify a = do writeFile "Pretty/Content.tex" $ "\\newcommand{\\content}{$$\n" ++ (render a) ++ "\n$$}\n"
                setCurrentDirectory "Pretty"
                rawSystem "/usr/texbin/pdflatex" ["-interaction=batchmode", "Maker.tex"]
                rawSystem "open" ["Maker.pdf"]
                return ()

