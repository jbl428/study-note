module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

ageOn :: Planet -> Float -> Float
ageOn Earth seconds = seconds / 31557600
ageOn Mercury seconds = seconds / 31557600 / 0.2408467
ageOn Venus seconds = seconds / 31557600 / 0.61519726
ageOn Mars seconds = seconds / 31557600 / 1.8808158
ageOn Jupiter seconds = seconds / 31557600 / 11.862615
ageOn Saturn seconds = seconds / 31557600 / 29.447498
ageOn Uranus seconds = seconds / 31557600 / 84.016846
ageOn Neptune seconds = seconds / 31557600 / 164.79132
