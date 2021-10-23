# snake-game

This is a pet project that started with the goal of getting familiar with Monad transformers using a simple version of snake game to wrap around various monad transformers stacks.

The basic idea is to separate a game logic from it's realization or gameplay context. These contexts, unsurprisingly monadic,
correspond to different modes to play the game. Some examples are:

* IO monad -- corresponding to getting input from the user through stdin.
* Reader monad -- actions are read from a list of instructions instead of being input from stdin. Can be used for testing.
* State monad -- similar to the Reader case, but realized with a different monad transformer stack.
* Identity monad -- here a bot determines the actions based to the state of the game, so no input required. 

Each of these can in its turn be wrapped in WriterT (we do this for Reader, State and Identity) to record the game. This gives still more contexts.

For all this we introduce two typeclasses: one for the game logic and one for monadic gameplays wrappers: [typeclasses](https://github.com/denisbash/snakegame/blob/master/src/Classes.hs)