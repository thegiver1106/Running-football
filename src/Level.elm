module Level exposing (..)

import Model exposing (..)
import Msg exposing (..)
import Random
import Objects exposing (..)
import General exposing (Identity)
import General exposing (Point)
import Blocks exposing (RecSize)
import General exposing (ConstructStatus(..))
import General exposing (Identity(..))


getLevelPerson :Model -> Model
getLevelPerson model =
    let
        time = model.durable
        seed = model.seed
        geneIdentity = generateIdByLevel model.creativeZone
        timeGuard = model.creativeZone.geneElapsed
        playframe = model.playframe
    in
        if timeGuard > time then
            model
        else
            let
                (nperson,nseed) =
                        getRandomPerson geneLocate seed geneIdentity
            in
                {model | playframe = {playframe| allPersonList = List.append [nperson] playframe.allPersonList }, seed= nseed , durable = 0}

geneLocate :  Random.Generator Point
geneLocate =  Random.map2 Point (Random.float 272 756) (Random.float -50 -10)      

getRandomPerson : Random.Generator Point-> Random.Seed -> Random.Generator Identity -> (Person,Random.Seed)
getRandomPerson genelocate seed geneIdentity =
    let
        (point, seed1)
                        = Random.step genelocate seed
        (identity,seed2)
                        = Random.step geneIdentity seed1
    in
        (makePerson point identity , seed2)

makePerson : Point -> Identity -> Person
makePerson anchor identity =
    case identity of
        Passerby ->
            Person anchor (RecSize 32 64) identity (Destroyable 1)
        Friend ->
            Person anchor (RecSize 32 64) identity (Destroyable 1)
        Enemy ->
            Person anchor (RecSize 32 64) identity (Destroyable 1)
        Me ->
            Person anchor (RecSize 32 64) identity Undestroyable

generateIdByLevel : CreativeZone -> Random.Generator Identity
generateIdByLevel creativeZone =
    let
        friendProb = creativeZone.friendProb
        passerbyProb = creativeZone.passerbyProb
        enemyProb = creativeZone.enemyProb
    in
        Random.weighted (friendProb, Friend) [(passerbyProb,Passerby),(enemyProb,Enemy)]

