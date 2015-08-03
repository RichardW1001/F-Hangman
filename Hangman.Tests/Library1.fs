namespace Hangman

module Game = 

    type GameOutcome = Continue | Win | Lose

    type GameState = { Word: string; Mask: string; PreviousGuesses: string; LivesLeft: int; Outcome: GameOutcome }

    let StartGame (word:string) = 
        let NewMask word = 
            String.map (fun c -> '_') word
        { 
            Word = word.ToUpper(); 
            Mask = NewMask word; 
            PreviousGuesses = ""; 
            LivesLeft = 6; 
            Outcome = GameOutcome.Continue 
        }

    let Guess (guess:char) gameState = 
        
        let word = gameState.Word.ToUpper()
        
        let guessUpper = guess.ToString().ToUpper()
        
        let upperPreviousGuesses = gameState.PreviousGuesses.ToUpper()
        
        let (guessList, newGuess) = if upperPreviousGuesses.Contains(guessUpper) 
                                    then (upperPreviousGuesses, false)
                                    else (upperPreviousGuesses + guessUpper,true)
        
        let newMask = String.map (fun c -> if guessList.Contains(c.ToString()) then c else '_') word
        
        let lifeCount = if newGuess && newMask = gameState.Mask then gameState.LivesLeft - 1 else gameState.LivesLeft

        let outcome = if lifeCount = 0 then GameOutcome.Lose
                      else if newMask = word then GameOutcome.Win
                      else GameOutcome.Continue

        {
            Word = word; 
            Mask = newMask;
            PreviousGuesses = guessList;
            LivesLeft = lifeCount;
            Outcome = outcome
        }
        

namespace Hangman.Tests
    
module Tests = 
    
    open Hangman.Game
    open NUnit.Framework

    [<TestFixture>]
    type HangmanTests() = 
    
        [<Test>]
        member x.NewGameHasBlankMask() =
            let state = StartGame "test"
            Assert.AreEqual("____", state.Mask)
            Assert.AreEqual(GameOutcome.Continue, state.Outcome)

        [<Test>]
        member x.GuessingALetterThatAppearsOnce() =
            let state0 = StartGame "hello"
            let state1 = Guess 'e' state0
            Assert.AreEqual("_E___", state1.Mask)
            Assert.AreEqual(GameOutcome.Continue, state1.Outcome)

        [<Test>]
        member x.GuessingALetterThatAppearsMoreThanOnce() =
            let state0 = StartGame "hello"
            let state1 = Guess 'l' state0
            Assert.AreEqual("__LL_", state1.Mask)
            Assert.AreEqual(GameOutcome.Continue, state1.Outcome)

        [<Test>]
        member x.GuessingALetterThatDoesNotAppear() =
            let state0 = StartGame "hello"
            let state1 = Guess 'x' state0
            Assert.AreEqual("_____", state1.Mask)
            Assert.AreEqual(5, state1.LivesLeft)
            Assert.AreEqual(GameOutcome.Continue, state1.Outcome)

        [<Test>]
        member x.GuessingLastLetter() = 
            let state0 = StartGame "a"
            let state1 = Guess 'a' state0
            Assert.AreEqual(GameOutcome.Win, state1.Outcome)

        [<Test>]
        member x.LosingLastLife() = 
            let state0 = { Word = "a"; LivesLeft = 1; PreviousGuesses = ""; Mask = "_"; Outcome = GameOutcome.Continue }
            let state1 = Guess 'z' state0
            Assert.AreEqual(GameOutcome.Lose, state1.Outcome)