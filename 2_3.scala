import io.Source
import io.Codec
import java.nio.charset.CodingErrorAction
import scala.collection.mutable.Set
import scala.util.Random
import scala.util.control.Breaks._




object MinHashing{
    def main(args: Array[String]) = {
        val books = List("01-The-Fellowship-Of-The-Ring.txt","Romeo-and-Juliet-William-Shakesp-[ebooksread.com].txt","Secret-Adversary-Agatha-Christie-[ebooksread.com].txt","Biology-and-war-J--Arthur--John-[ebooksread.com].txt")
        val booksAsString = books.map(reading)
        val nHashes = Array(10,100,250,500)
        (4 to 13) foreach{k=>
            var shingles = booksAsString.map((x)=>kShingles(x,k)).toArray
            for(n<-nHashes){
                val nsig = signatures(shingles,n)
                (0 to booksAsString.length-1) foreach{i=>
                    (0 to booksAsString.length-1) foreach{j=>
                        print(books(i))
                        print(" and ")
                        print(books(j))
                        print(" -> n = ")
                        print(n)
                        print(" -> k = ")
                        print(k)
                        print(" J.sim = ")
                        val sim = jaccardSignature(nsig(i),nsig(j))
                        print(sim)  
                        println()
                    }
                }
            }    
        }

    }

    def reading(filename:String):String={
        val decoder = Codec.UTF8.decoder.onMalformedInput(CodingErrorAction.IGNORE)
        return Source.fromFile(filename)(decoder).getLines.mkString.replace(" ","")  
    }

    def kShingles(book:String,k:Int):Set[String]={
        var shingles = Set[String]()
        (0 to (book.length-k)) foreach {i=>
            val shingle = book.substring(i,i+k)
            shingles+=shingle
        }
        return shingles
    }

    def union_shingles(shingles:Array[Set[String]]):Array[String]={												// wywalamy powtórzenia
        var union_set = Set[String]()
        (0 to shingles.length-1) foreach {i=>
            union_set = union_set.union(shingles(i))
        }
        return union_set.toArray
    }


    def generateHashFunctions(n:Int,len:Int):Array[Array[Int]]={												// generowanie funkcji hashujących
        val range = Array.range(0,len)
        val functions = (0 to n).map((x)=> Random.shuffle(range).toArray).toArray
        return functions
    }

    def applyHash(matrix:Array[Array[Int]]):Array[Int]={													// zastosowanie	funkcji	hashujących										
        val result = Array.fill(matrix.length){0}
        val permutation = Random.shuffle(Array.range(0,matrix(0).length)).toArray
        (0 to matrix.length-1) foreach {i=>
            (0 to permutation.length-1) foreach {j=>
                breakable { if(matrix(i)(permutation(j))==1){
                    if(result(i)==0){
                        result(i)=j
                        break;
                    }
                    
                }}
            }    
        }
        return result
    }

    def jaccardSimilarity(bookA:String, bookB:String,k:Int):Float={
        val bookAShingles = kShingles(bookA,k)
        val bookBShingles = kShingles(bookB,k)
        return SetOperations(bookAShingles,bookBShingles)
    }



    def SetOperations(bookAShingles:Set[String],bookBShingles:Set[String]):Float={
        val intersection = bookAShingles.intersect(bookBShingles)
        val union = bookAShingles.union(bookBShingles)
        return intersection.size.toFloat/union.size.toFloat
    }

    def signatures(shingles:Array[Set[String]],nHashes:Int):Array[Array[Int]]={											// tworzenie 'sygnatury'
         val rows = union_shingles(shingles)
         val columns = shingles.map((x)=>Array.fill(rows.length){0})
         (0 to rows.length-1) foreach {i=>
            (0 to columns.length-1) foreach {j=>
                val value = rows(i)
                if(shingles(j).contains(value)){
                    columns(j)(i) = 1
                }else{
                    columns(j)(i) = 0
                }
            }     
        }

        val signatures = (0 to nHashes-1).map((x)=>applyHash(columns))
        return (0 to columns.length-1).map((x)=>signatures.map((y)=>y(x)).toArray).toArray
    }

    def jaccardSignature(bookASignature:Array[Int],bookBSignature:Array[Int]):Float={										// J.sim za pomoca Signature
        return (0 to bookBSignature.length-1).map((i)=> if(bookBSignature(i)==bookASignature(i)) 1 else 0 ).sum.toFloat/bookBSignature.length.toFloat		// sumujemy tam gdzie sie rowna i dzielimy przez liczbe
    }
}
