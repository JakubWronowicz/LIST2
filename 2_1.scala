import scala.collection.mutable.Map
import io.Source


object LongOne{
    
    def main(args: Array[String]) = {
        val result = Source.fromFile("web-Stanford.txt").getLines.map(mapping).reduce(reducing)		//Wynik zapisywany do zmiennej 
        println()
        for(node<-result){										//wypisywanie na ekran w wymaganej postaci
             print(node._1)
             print(" ")
             print(node._2._1)
             print(" ")
             print(node._2._2)
             print(" ")
             println()
        }
        
    }

    def mapping(arg:String):Map[Int,(Int,Int)]={								// Funkcja mapująca
        val nodes = arg.split("\t").map((x)=>x.toInt).toArray
        val map = Map[Int,(Int,Int)]()
        val v0 = nodes(0)										// v0 - pierwszy node
        val v1 = nodes(1)										// v1 - drugi element
        map.addOne((v0,(0,1)))										// dodajemy pojedyńczy element 
        
        if(map.contains(v1)){										// co jeżeli mapa zawiera
            map.update(v1,(1,1))
        }else{
            map.addOne((v1,(1,0)))									// co jeżeli mapa nie zawiera
        }
        return map
    }

    def reducing(x:Map[Int,(Int,Int)],y:Map[Int,(Int,Int)]):Map[Int,(Int,Int)]={				// Funkcja redukująca
        
        for(v<-y.toArray){
            if(x.contains(v._1)){
                val y = x(v._1)
                x.update(v._1,(y._1+v._2._1,y._2+v._2._2))
            }else{
                x.addOne(v)
            }
        }
        return x

    }
    
}
