import Network.HaskellNet.IMAP.SSL
import System.Environment (getArgs)

imapServer :: String
imapServer = "imap.gmail.com"

imapUsername :: String
imapUsername = "tim.millican@gmail.com"

imapPassword :: String
imapPassword = "lepajvaenyzmhgeq"

imapAuthType :: AuthType
imapAuthType = PLAIN

main :: IO ()
main = do
    args <- getArgs
    ic <- connectIMAPSSL imapServer
    authenticate ic imapAuthType imapUsername imapPassword
    lboxes <- lsub ic
    counts <- mapM (unSeen ic) $ selectable lboxes
    if length args == 1 && head args == "-v"
    then mapM_ (\x -> putStrLn $ fst x ++ ": " ++ show (snd x)) $ filter (\x -> snd x /= 0) counts
    else print (sum $ map snd counts)
    where
        selectable = map snd . filter (notElem Noselect . fst)
        unSeen ic box = do
            select ic box
            a <- search ic [NOTs (FLAG Seen)]
            return (box, length a)

getCount ic box = do
        count <- status ic box [MESSAGES]
        return (box, count)
