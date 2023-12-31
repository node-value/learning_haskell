import System.Environment
import System.Random
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

main :: IO ()
main = do  
    args <- getArgs
    let fileName = head args
    imageFile <- BC.readFile fileName
    glitched <- foldM (\bytes func -> func bytes) imageFile glitchActions
    let glitchedFileName = mconcat ["glitched_", fileName]
    BC.writeFile glitchedFileName glitched
    print "done"

glitchActions :: [BC.ByteString -> IO BC.ByteString]
glitchActions = [randomReplaceByte
                ,randomSortSection
                ,randomReplaceByte
                ,randomReverseSection
                ,randomSortSection
                ,randomReplaceByte
                ,randomReverseSection]

intToChar :: Int -> Char
intToChar int = toEnum $ int `mod` 255

intToBC :: Int -> BC.ByteString
intToBC int = BC.pack [intToChar int]

replaceByte :: Int -> Int -> BC.ByteString -> BC.ByteString
replaceByte location charVal bytes = mconcat [before, (intToBC charVal), (BC.drop 1 after)] where
    (before, after) = BC.splitAt location bytes

randomReplaceByte :: BC.ByteString -> IO BC.ByteString
randomReplaceByte bytes = do 
    let bytesLength = BC.length bytes
    location <- randomRIO (1, bytesLength)
    charVal <- randomRIO (0, 255)
    return (replaceByte location charVal bytes)

sortSection :: Int -> Int -> BC.ByteString -> BC.ByteString
sortSection start size bytes = mconcat [before,changed,after]
    where (before,rest) = BC.splitAt start bytes
          (target,after) = BC.splitAt size rest
          changed = BC.reverse (BC.sort target)

randomSortSection :: BC.ByteString -> IO BC.ByteString
randomSortSection bytes = do
    let bytesLn = BC.length bytes
    let sectionSize = 25 
    start <- randomRIO (0, bytesLn - sectionSize)
    return (sortSection start sectionSize bytes)

reverseSection :: Int -> Int -> BC.ByteString -> BC.ByteString
reverseSection start size bytes = mconcat [before, (BC.reverse target), after] where
    (before,rest) = BC.splitAt start bytes
    (target,after) = BC.splitAt size rest

randomReverseSection :: BC.ByteString -> IO BC.ByteString
randomReverseSection bytes = do
    let bytesLn = BC.length bytes
    let sectionSize = 50 
    start <- randomRIO (0, bytesLn - sectionSize)
    return (reverseSection start sectionSize bytes)