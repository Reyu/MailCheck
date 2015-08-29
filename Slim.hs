import Network.HaskellNet.IMAP.SSL
import System.Environment (getArgs)
import System.Directory (getHomeDirectory)
main = getArgs >>= \args -> getHomeDirectory >>= \hd -> readFile (hd ++ "/.mailauth") >>= \auth -> (connectIMAPSSL . head $ words auth) >>= \ic -> let username = words auth !! 1; password = words auth !! 2 in authenticate ic PLAIN username password >> lsub ic >>= \boxes -> mapM ((\box -> select ic box >> search ic [NOTs (FLAG Seen)] >>= \a -> return (box, length a)) . snd) (filter (notElem Noselect . fst) boxes) >>= \counts -> if length args == 1 && head args == "-v" then mapM_ (\x -> putStrLn $ fst x ++ ": " ++ show (snd x)) $ filter (\x -> snd x /= 0) counts else print . sum $ map snd counts
