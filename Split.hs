import Network.HaskellNet.IMAP.SSL
import System.Environment (getArgs)
import System.Directory (getHomeDirectory)
main =  connectIMAP' >>= authenticate' >>= getUnreadBoxes >>= printCounts
isVerbose = getArgs >>= \a -> return (not (null a) && head a == "-v")
getAuth = getHomeDirectory >>= \hd -> readFile (hd ++ "/.mailauth") >>= split'
split' x = case x of [] -> fail "Failed to parse config"; otherwise -> return (words x)
connectIMAP' = getAuth >>= connectIMAPSSL . head
authenticate' c = getAuth >>= \(_:u:p:_) -> authenticate c PLAIN u p >> return c
getUnreadBoxes c = lsub c >>= mapM (filterUnseen c . snd) . filterSelectable
filterUnseen c b = select c b >> search c [NOTs (FLAG Seen)] >>= \a -> return (b, length a)
filterSelectable = filter (notElem Noselect . fst)
printCounts x = isVerbose >>= \isV -> if isV then printVerbose x else print . sum $ map snd x
printVerbose = mapM_ (\x -> putStrLn $ fst x ++ ": " ++ show (snd x)) . filter (\x -> snd x /= 0)
