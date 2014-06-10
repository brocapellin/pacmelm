import Window
import World
import Graphics

main = Graphics.renderResult
        <~ Window.dimensions ~ World.runWorld
