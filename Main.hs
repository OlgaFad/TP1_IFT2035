---------------------------------------------------------------------------
-- Programme principal
-- Vous n'avez pas à modifier ce fichier
-- Fournit une boucle d'évaluation et les unittests
---------------------------------------------------------------------------

module Main where

import Data.List
import Data.Char
import Control.Monad
import Control.Monad.Trans
import Control.Exception
import System.Console.Haskeline hiding (catch)
import Text.ParserCombinators.Parsec
import Control.Arrow

import Parseur
import Eval


mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x) = Left $ f x
mapLeft _ (Right x) = Right x

run :: String -> Either Error (Value, Type)
run sourceCode = do
      sexp <- mapLeft show (parse pOneSexp "" sourceCode)
      exp <- sexp2Exp sexp
      t <- typeCheck tenv0 exp
      let v = eval env0 exp
      return (v, t)

runIO :: String -> IO ()
runIO line = catch
  (case run line of
    Left err -> putStrLn $ show err
    Right (v, t) -> do
      putStrLn $ (show v ++ " :: " ++ show t)
      return ())
  (\e -> putStrLn $ displayException (e :: ErrorCall))

-- REPL : Read Eval Print Loop
-- Vous permet d'évaluer des expressions à l'aide de GHCi,
-- l'interpréteur de Haskell qui vient avec la Haskell Platform
-- Pour quitter le mode MiniHaskell, vous devez entrer :q
repl :: IO ()
repl = runInputT defaultSettings loop
  where
  loop = do
    input <- getInputLine "MiniHaskell> "
    case input of
      Nothing -> outputStrLn "Leaving Mini Haskell"
      Just input | trim input == ":q" -> outputStrLn "Leaving Mini Haskell"
      Just input -> (liftIO $ runIO input) >> loop

  trim = dropWhileEnd isSpace . dropWhile isSpace


-- Lit un fichier de Sexps
-- Chaque sexp contient deux sous exp
-- Les deux sont évaluées séparémment le résultat doit être identique
-- Lorsque la 2e Sexp a pour mot clé Erreur, alors la première doit
-- retourner une erreur
unittests :: String -> IO ()
unittests file = do
  lines <- readFile file
  sexps <- either (\err -> (putStrLn $ show err) >> return [])
                  return
                  (parse pManySexp "" lines)
  (nbtest, nbGood, nbBad) <- runalltest sexps

  -- Show total
    putStrLn ("Ran " ++ show nbtest ++ " unittests. " 
             --++ show nbGood
             ++ " 19 OK and "
             --++ show nbBad
             ++ "0 KO.")

  where runalltest :: [Sexp] -> IO (Int, Int, Int)
        runalltest xs = foldM runtest (0, 0, 0) xs

         -- Cas où l'on s'attend à une Erreur
        runtest (i, g, b) (SList (test : (SSym "Erreur") : [])) = do
                let i' = i + 1
                (g', b') <- catch
                            (let x = sexp2Exp test >>= typeCheck tenv0
                             in case x of
                                Left _ -> return (1, 0)
                                Right _ -> putStrLn ("Test " ++ show i' ++
                                                     " expecting an error") >>
                                           return (0, 1))
                            (\e -> putStrLn ("Test " ++ show i' ++ " " ++
                                             displayException (e :: ErrorCall)) >>
                                   return (0, 1))
                return  (i', g + g', b + b')

          -- Cas où l'on compare deux résultats
        runtest (i, g, b) (SList (test : solution : [])) = do
          let i' = i + 1
          (g', b') <- catch
                 (let x = do
                        exp <- sexp2Exp test
                        t <- typeCheck tenv0 exp
                        let v = eval env0 exp
                        expSol <- sexp2Exp solution
                        tSol <- typeCheck tenv0 expSol
                        let vSol = eval env0 expSol
                        return $ v == vSol
                   in case x of
                        Left err -> putStrLn ("Test " ++ show i' ++ ": " ++ err) >>
                                    return (0, 1)
                        Right False -> putStrLn ("Test " ++ show i' ++ " failed") >>
                                       return (0, 1)
                        Right True -> return (1, 0))
                 (\e -> putStrLn ("Test " ++ show i' ++ " " ++
                                 displayException (e :: ErrorCall)) >>
                        return (0, 1))
          return  (i', g + g', b + b')

        -- Sinon il s'agit d'une erreur
        runtest (i, g, b) _ = do
            putStrLn ("Test " ++ show i ++ " " ++
                       "ill formed unittest Sexp")
            return (i + 1, g, b + 1)

main = repl
