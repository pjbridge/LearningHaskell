module Geometry
(sphereArea,
 sphereVolume,
 cuboidArea,
 cuboidVolume,
 cubeArea,
 cubeVolume
)
where 
import qualified Geometry.Sphere as Sphere  
import qualified Geometry.Cuboid as Cuboid  
import qualified Geometry.Cube as Cube  

sphereArea = Sphere.area

sphereVolume = Sphere.volume

cuboidArea = Cuboid.area 

cuboidVolume = Cuboid.volume

cubeArea = Cube.area 

cubeVolume = Cube.volume