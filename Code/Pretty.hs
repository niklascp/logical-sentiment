module Pretty (Pretty, latexify, render) where
import System.Directory (setCurrentDirectory)
import System.Cmd (rawSystem)

class Pretty a where
  render :: a -> String

latexify :: Pretty a => a -> IO ()
latexify a = do writeFile "Pretty/Content.tex" $ "\\newcommand{\\content}{$$\n" ++ (render a) ++ "\n$$}\n"
                setCurrentDirectory "Pretty"
                rawSystem "/usr/texbin/pdflatex" ["-interaction=batchmode", "Maker.tex"]
                rawSystem "open" ["Maker.pdf"]
                return ()

