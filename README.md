

[School Runner]: The project is a *breakout game* played on the browser. The player needs to break all bricks to win the game. The project is written by Elm. 

# README
This file contains relevant information about installation and startup.

## Installation

The project can be used in these operating systems.

	[Windows/Linux/MacOS]

First, please install elm. 

Then, run the following code.

```
make
```

A file called `index.html` will generate.

Open it in the browser and you can enjoy School Run!

## Brief Explanation
1. Our model contains these following contents:

1.1 Levelframe: the levels, there are three kinds of levels in all, namely, introduction, normal and hidden. The number in this variable represents the current level. And it also helps determine what kinds of background and difficulties will be.

1.2. Playframe and Playframe_0: the common features in each level, since the introduction level is different from the normal ones, there is a special one created for it. They are also the basic frames in our project.

1.3. Page: the basis of our transformation among differnet stages, calling the endings and so on.

1.4. State: the variable that is responsible for the user interface display.

1.5. Key direction: used to send messages to control.

1.6. Score, Creative_zone and Current_flow: functional values that used for scoreboards and background moving.

1.7. seed: used to generate random numbers.

2. For update parts, namely in Update.elm. For each part, there is a view function for it.
   
2.1 Update_view: used to update shapes, characters, background and other visual elements.

2.2 Update_all: used to update changes caused by different controls, commands, and other changes in the game.

2.3 Update_gamestate: used to update the state of the game, and to call endings after each different levels.

2.4 Update_intro, and the related functions: a special part for introductional level only. Since all of the elements in this level are previously designed, for each scene, an additional funciton is called for. There is also a function especially for the moving parts in this special level.

3. Viewing part

3.1 In view.elm, most of the viewing funcitons used in the normal and endless hidden levels are written in.

3.2 For each visual element, there is a view function.

3.3 The viewing part for introductional level is in the INtroduction.elm files.

4. UI part

4.1 pause and resume

4.2 restart

4.3 skip buttons to skip the tutorial
## Usage

This is an online game written in Elm, a functional programming language. The game is a parkour simulating game that features a scene in school. It has a background story, which has been uploaded in the booklet, among the main characters as well. And it can be fetched on the website of Silver focs.


## Contributing
[Tech Part]

Wang Xinhe: write basic code of the kinetic system.

Pan Xixiao: write the overall frame of the game.

Gao Tianhong: write User Interface and view functions.

Sun Yihao: write view functions.

[Tech Comm Part]

Wang Xinhe: write booklet, do the presentation.

Pan Xixiao: write booklet, write pitchdiagram, make Logo, make Stop Motion Video.

Gao Tianhong: write pitchdiagram.

Sun Yihao: write booklet, design the plot of the game, design drawings, record meetings.


# Author team 
	[RainbowX]
[Wang Xinhe]
[Pan Xixiao]
[Gao Tianhong]
[Sun Yihao]
Our logo can be seen via the logo.png
# License 
[SilverFocs Incubator Licence](https://focs.ji.sjtu.edu.cn/silverfocs/markdown/license)

