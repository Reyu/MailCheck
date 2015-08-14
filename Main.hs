import Network.HaskellNet.IMAP.SSL
import System.Environment (getArgs)
import System.Directory (getHomeDirectory)

main :: IO ()
main = do
    args <- getArgs
    hd <- getHomeDirectory
    auth <- readFile $ hd ++ "/.mailauth"
    ic <- connectIMAPSSL . head $ words auth
    let username = words auth !! 1
        password = words auth !! 2
        in authenticate ic PLAIN username password
    boxes <- lsub ic
    counts <- mapM (unSeen ic . snd) (filter (notElem Noselect . fst) boxes)
    if length args == 1 && head args == "-v"
    then mapM_ (\x -> putStrLn $ fst x ++ ": " ++ show (snd x)) $ filter (\x -> snd x /= 0) counts
    else print . sum $ map snd counts
    where unSeen ic box = select ic box >> search ic [NOTs (FLAG Seen)] >>= \a -> return (box, length a)
