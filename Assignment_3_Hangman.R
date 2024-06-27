# Assignment 3 - Hangman
# BTC1855
# By Trinley Palmo

# R version 4.4.0

# Plan
#' Step 1: Prepare a text file of words for hangman and save to project directory
#' Step 2: Read the file and store it as a list to a variable.
#' Step 3: Choose a random word from the list
#' Step 4: Print a message to inform the user on the length of the word
#' Step 5: Print out the rules of the game (e.g., the number of tries allowed,
#' what happens when the user runs out of tries, etc.)
#' Step 6: Loop for if the user still has tries left
#'  Step 6A: Inform the user of how many tries they have (left)
#'  Step 6B: Print out a visual cue of how they are progressing 
#'  (e.g., "b _ _ k")
#'  Step 6C: Ask the user if they want to guess a word or a letter
#'  Step 7: If the user wants to guess the word, compare the user input to word:
#'  if TRUE (the word is correct):
#'    Tell the user that they won
#'    Set the number of tries to 0
#'  if FALSE (the word is incorrect):
#'    Tell the user that the word was incorrect
#'    Subtract 1 from number of tries
#' Step 8: If the user want to guess a letter, check if it exists in the word
#'  if TRUE:
#'    Tell the letter input is correct
#'    Uncover the letters that are correct
#'    Subtract 1 from number of tries
#'  if FALSE:
#'    Tell the letter input is incorrect
#'    Subtract 1 from number of tries
#' Step 9: Print a message saying "Game Over"

# Read list of words and save it to a variable for use later
words_list <- read.delim("word_list.txt", header = FALSE)

# Choose a random word from the list that the users would be guessing
answer <- sample(words_list[[1]], size = 1)

# Inform the users on how many letters there are and the rules of the game.
print(paste("The word has", nchar(answer), "letters."))
print(paste("To win the game, you must successfully guess the word within 6 tries.",
      "For each try, you can guess a letter or a word.", 
      "If you guess wrong, you will lose an attempt. Let's start guessing!"))