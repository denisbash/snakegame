# snake-game

This is a pet project that started with the goal of getting familiar with Monad transformers using a simple version of snake game to wrap around various monad transformers stacks.

The basic idea is to separate a game logic from it's realization or gameplay context. These contexts, unsurprisingly monadic,
correspond to different modes to play the game. Some examples are:

* IO monad -- corresponding to getting input from the user through stdin.
* Reader monad -- actions are read from a list of instructions instead of being input from stdin.
* State monad -- similar to the Reader case, but realized with a different monad transformer stack.
* Identity monad -- here a bot determines the actions based to the state of the game, so no input required. 