# CoffeeInk

A simple example used as a pet project for learning how to code a CLI in Haskell. The purpose is to create a shopping cart vending machine that allows to edit the cart and checkout.

The product options are currently hard coded in `src/Lib.hs` to be 

* Water
* Soda
* Coffee
* Energy
* Tea

There are initially 10 units for each product stored in available stock map. On each order the logic checks for enough inventory to satisfy the order.

# User Guide

Must have [stack](https://docs.haskellstack.org/en/stable/) and [GHC](https://www.haskell.org/ghc/) installed.

Run with `stack run` .

Press `Ctrl+C` at any point to exit.

You can add products from the Menu to your cart

Then you can choose to remove any products you don't want anymore.

After editing your cart and checking out, you'll receive a thank you message. If your cart is empty, the message will reflect that.

Example interaction:
```
Welcome to CoffeeInk, what would you like?
Menu:

Water: $1.0
Soda: $1.5
Coffee: $2.0
Energy: $2.5
Tea: $1.75

add to your order:
>chocolate

Invalid Product, Please Choose from the
Menu:

Water: $1.0
Soda: $1.5
Coffee: $2.0
Energy: $2.5
Tea: $1.75

add to your order:
>energy
How many would you like?
>4
Added to cart!

Total: $10.0
4 Energy

Continue shoppling? (y/n)
>y
Menu:

Water: $1.0
Soda: $1.5
Coffee: $2.0
Energy: $2.5
Tea: $1.75

add to your order:
>tea
How many would you like?
>1
Added to cart!

Total: $11.75
4 Energy
1 Tea

Continue shoppling? (y/n)
>n

Here are the contents of your cart:
4 Energy
1 Tea

Total: $11.75

Does your order look Ok? (y/n)
>n
Select any item you would like to remove:
1 Energy
2 Tea

>2
How many Tea's would you like to remove? There are currenly 1 in your basket
>2
That is more than you have, please select a number less than or equal to 1
How many Tea's would you like to remove? There are currenly 1 in your basket
>1
Removed 1 Tea's from cart

Updated Contents of your cart:
4 Energy

Total: $10.0

Continue shopping? (y/n)
>n

Here are the contents of your cart:
4 Energy

Total: $10.0

Does your order look Ok? (y/n)
>y

Thank you for Shopping with us!
```

# Future Additions

Implement a small inventory database instead of had coding available units and products.

Make the program distributed, capable of serving more than client at the time, preventing data races from querying from a single inventory database.