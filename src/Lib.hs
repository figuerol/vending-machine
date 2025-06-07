{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( runMachine
    ) where


import Fmt
import qualified Data.Map as Map
import Data.Map (Map, fromList, keys, insertWith)
import Text.Read (readMaybe)
import Data.Maybe (isJust, fromJust)
import Data.Char (toLower,isSpace)

-- Welcome menu
-- Initialize cart (basket) and inventory
-- Add Items to cart
-- Error handling for not enought invertory
-- Display final cart 
-- Review order
-- Checkout 

{- products and prices -}
data Product = Water | Soda | Coffee | Energy | Tea deriving(Show,Enum,Eq,Ord)

-- Calculate the price of a product
price :: Product -> Maybe Float
price Water = Just 1.0
price Soda = Just 1.5
price Coffee = Just 2.0
price Energy = Just 2.5
price Tea = Just 1.75

-- Can test this function
trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

-- Can test this function
parseProduct :: String -> Maybe Product
parseProduct input = case trim $ map toLower input of
    "water" -> Just Water
    "soda" -> Just Soda
    "coffee" -> Just Coffee
    "energy" -> Just Energy
    "tea" -> Just Tea
    _ -> Nothing

{- data and types -}
type Stock = Map Product Int
type Cart = Map Product Int
type Total = Float
-- Always return the new Stock after the order (even when failed).
type PurchaseOrder = (Cart, Stock, Total)

{- Machine internals -}
-- Can test this function
vend :: PurchaseOrder -> Product -> Int -> Either String PurchaseOrder
vend (c,s,t) p n
    | (Map.lookup p s>= Just n) && isJust (price p) = Right (insertWith (+) p n c, insertWith subtract p n s, fromIntegral n*fromJust (price p) + t)
    | otherwise = Left ("Not enough inventory left for "+||p||+", please make another selection.\n")

-- Can test this function
buyBack :: PurchaseOrder -> Product -> Int -> Either String PurchaseOrder
buyBack (c,s,t) p n
    | (Map.lookup p c>= Just n) && isJust (price p) = Right (Map.filter (>0) $ insertWith subtract p n c, insertWith (+) p n s, t - fromIntegral n*fromJust (price p))
    | otherwise = Left $ "That is more than you have, please select a number less than or equal to "+||fromJust (Map.lookup p c)||+""

getContinueShoppingChoice :: PurchaseOrder -> IO ()
getContinueShoppingChoice p = do
   choice <- getLine
   handleContinueShoppingChoice choice p

handleContinueShoppingChoice :: String -> PurchaseOrder -> IO ()
handleContinueShoppingChoice choice p@(cart,_,total) = do
    case choice of
        "y" -> shop p
        "n" -> if null (keys cart) then do
                  putStrLn "Your cart is now empty"
                  putStrLn "Exit the shop? (y/n)"
                  continueChoice <- getLine
                  handleExitShopChoice continueChoice p
              else do
                  putStrLn "\nHere are the contents of your cart:"
                  -- Show cart contents (using showCart function)
                  showCart cart
                  putStrLn $ "Total: $"+|total|+"\n"
                  putStrLn "Does your order look Ok? (y/n)"
                  getEditCartChoice p
        _ -> do putStrLn "Invalid Option, please select (y/n)"
                getContinueShoppingChoice p

handleExitShopChoice:: String -> PurchaseOrder -> IO ()
handleExitShopChoice continueChoice p = do
    case continueChoice of
        "n" -> shop p
        "y" -> putStrLn "Thank you for you visit!"
        _ -> do putStrLn "Invalid Option, please select (y/n)"
                getContinueShoppingChoice p

getEditCartChoice :: PurchaseOrder -> IO()
getEditCartChoice p@(cart,_,_) = do
    choice <- getLine
    case choice of
      "y" -> putStrLn "\nThank you for Shopping with us!"
      "n" -> do putStrLn "Select any item you would like to remove:"
                putStrLn $ unlines $ zipWith (\n key -> ""+||n||+" "+||key||+"") [1::Integer ..] (keys cart)
                getEditCartProductChoice p
      _ -> do putStrLn "Invalid Option, please select (y/n)"
              getEditCartChoice p

getEditCartProductChoice :: PurchaseOrder -> IO()
getEditCartProductChoice p = do
    itemChoice <- getLine
    let itemNumber = readMaybe itemChoice :: Maybe Int
    let productNumber = handleEditCartProductChoice itemNumber p
    case productNumber of
        Left NotNumber -> do putStrLn "Invalid input, please select a number from the list above."
                             getEditCartProductChoice p
        Left WrongNumber -> do putStrLn "Invalid input, please select a valid number."
                               getEditCartProductChoice p
        Right productValue -> editCart p productValue

data ProductNumberResult = NotNumber | WrongNumber

-- Can test this function
handleEditCartProductChoice :: Maybe Int -> PurchaseOrder -> Either ProductNumberResult Product
handleEditCartProductChoice Nothing _ = Left NotNumber
handleEditCartProductChoice (Just n) (cart,_,_) | n >= 1 && n <= length (keys cart) = Right $ keys cart!!(n-1)
                                                | otherwise = Left WrongNumber

showCart:: Cart -> IO()
showCart cart = putStrLn $ unlines $ map (\key -> ""+||fromJust (Map.lookup key cart)||+ " " +||key||+"") (keys cart)

cartLookup :: Product -> PurchaseOrder -> IO()
cartLookup item p@(cart,_,_) = handleCartLookup item p (Map.lookup item cart)

handleCartLookup :: Product -> PurchaseOrder -> Maybe Int -> IO()
handleCartLookup item _ (Just n) = putStrLn $ "How many "+||item||+"'s would you like to remove? There are currenly "+|n|+" in your basket"
handleCartLookup _ p Nothing = do putStrLn "Incorrect item selection, please select a number as in the menu"
                                  getEditCartProductChoice p

editCart :: PurchaseOrder -> Product -> IO()
editCart p item = do
    cartLookup item p
    quantityChoice <- getLine  --TODO: implement a Maybe to verify the input is an int.
    handleCartEdit quantityChoice p item

handleCartEdit :: String -> PurchaseOrder -> Product -> IO()
handleCartEdit quantityChoice p item = do
    let quantity = readMaybe quantityChoice :: Maybe Int
    case quantity of
      Just n | n>=0 -> handleProductQuantity p item n
              | otherwise -> do
                                putStrLn "Please enter a non negative amount"
                                editCart p item
      Nothing -> do
                  putStr "Invalid input, please enter an integer"
                  editCart p item

handleProductQuantity :: PurchaseOrder -> Product -> Int -> IO ()
handleProductQuantity p item n =
    case buyBack p item n of
        Right pOrder@(cart, _, total) -> do
                      putStrLn $ "Removed "+|n|+" "+||item||+"'s from cart\n"
                      putStrLn "Updated Contents of your cart:"
                      showCart cart
                      putStrLn $ "Total: $" ++ show total
                      putStrLn "\nContinue shopping? (y/n)"
                      getContinueShoppingChoice pOrder
        Left err -> do
                      putStrLn err
                      editCart p item

getOrderProductAmount :: PurchaseOrder -> Product -> IO()
getOrderProductAmount pOrder p = do
    putStrLn "How many would you like?"
    nInput <- getLine
    handleOrderProductAmount nInput pOrder p

handleOrderProductAmount :: String -> PurchaseOrder -> Product -> IO()
handleOrderProductAmount nInput pOrder p = do
    let mAmount = readMaybe nInput :: Maybe Int
    case mAmount of
      Just amount -> handleOrderProductIntAmount amount pOrder p
      Nothing -> do putStrLn "Invalid amount. Please enter a positive integer."
                    getOrderProductAmount pOrder p

handleOrderProductIntAmount :: Int -> PurchaseOrder -> Product -> IO()
handleOrderProductIntAmount amount pOrder p
    | amount<=0 = do
                  putStrLn "Invalid amount. Please enter a positive integer."
                  getOrderProductAmount pOrder p
    | otherwise = do
        case vend pOrder p amount of
                  Right pOrder' -> do logSuccessfulOrder pOrder'
                                      getContinueShoppingChoice pOrder'
                  Left e -> do putStrLn e
                               shop pOrder

logSuccessfulOrder :: PurchaseOrder -> IO ()
logSuccessfulOrder (cart, _, total) = do
    putStrLn "Added to cart!\n"
    putStrLn $ "Total: $" ++ show total
    showCart cart
    putStrLn "Continue shoppling? (y/n)"

{- User Interaction Entrypoint -}
shop :: PurchaseOrder -> IO ()
shop pOrder@(_,currentStock,_) = do
    let menu = map (\key -> ""+||key||+ ": $" +||fromJust (price key)||+"") (keys $ Map.filter (>0) currentStock)
    putStrLn "Menu:\n"
    putStrLn $ ""+|unlines menu|+""
    putStrLn "add to your order:"
    pInput <- getLine
    let mProduct = parseProduct pInput :: Maybe Product
    case mProduct of
      Just p -> getOrderProductAmount pOrder p
      Nothing -> do putStrLn "\nInvalid Product, Please Choose from the"
                    shop pOrder

runMachine :: IO ()
runMachine = do
    let maxProductInventory = 10
    let initialStock = fromList [(k,maxProductInventory) | k <- enumFrom Water]
    let newCart = Map.empty
    putStrLn "Welcome to CoffeeInk, what would you like?"
    shop (newCart,initialStock,0)

