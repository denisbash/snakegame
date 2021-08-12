import GHC.IO.Handle.FD (openFile)
import GHC.IO.IOMode (IOMode(ReadMode))
import GHC.IO.Handle.Text (hGetChar)
import Control.Exception.Base (handle)
import System.Directory
main :: IO ()
main = do 
    putStrLn "Starting the testing suite"
    pwd <- getCurrentDirectory
    print pwd
    handle <- openFile "/home/denis/Desktop/haskell/snake-game/testCursorPath.txt" ReadMode
    ch <- hGetChar handle
    putChar ch
