# Particle Divide!
Can you divide the particles equally in this fun Elm app made with the [Graphic SVG Package](https://package.elm-lang.org/packages/MacCASOutreach/graphicsvg/latest/GraphicSVG)?

Can you make it past level 20? [Try for yourself!](https://benjaminkostiuk.github.io/particle-divide/)

## Getting Started

### Installation
* [Install Elm](https://guide.elm-lang.org/install.html) if you haven't already
* ```clone``` the repository
* ```cd``` into the directory
* run ```elm reactor```
* Open a browser and navigate to <http://localhost:8000/>. If you used a different port with ```--port=XXXX``` then navigate there instead.
* Have Fun!


The code for _Particle Divide_ can be found in _src/_ folder


If you want to export the elm file as .html then run ```elm make src/particledivide.elm --output nameOfFile.html```

### How To Play
The goal of __Particle Divide__ is to draw a line that splits the screen in two, dividing the number of particles equally. Doing so will grant you access to the next level!

1. First choose the color of your particles using the _Change Color_ button in the Main Menu.
2. To draw a line simply click two points on the screen and a line that splits the screen will appear.
3. The results will be displayed as
    1. Level Complete or Failed
    2. Your error percentage (and the allowed margin if you've failed)
    3. Your ratio of particles on each side of your line
4. Complete as many levels as you can.
5. Good Luck!

<h3>Beware!</h3>

While this may seem simple at first _Particle Divide_ is no easy beat! As you progress through levels the difficulty will increase:
* The allowed error margin will __decrease__
* The speed of particles will __increase__
* The number of particles will __decrease__

## Game Mechanics (Dev Stuff)
Particle Divide is made possible with the [GraphicSVG Package](https://package.elm-lang.org/packages/MacCASOutreach/graphicsvg/latest/GraphicSVG).

* A random list of particles is generated for each level with help from [Elm's Random Package](https://package.elm-lang.org/packages/elm-lang/core/latest/Random) 
* Once a player click __any two points__ on the screen the equation of the line between them is used to extrapolate and draw a line that spans the entire screen.
* To capture the player's mouse position a giant transparent rectangle is used as a backdrop.

### Authors
* Benjamin Kostiuk

### License
See [LICENSE](LICENSE).


