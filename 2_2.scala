import io.Source
import io.Codec
import java.nio.charset.CodingErrorAction
import scala.collection.mutable.Set




object JaccardSimilarity{
    def main(args: Array[String]) = {
        val books = List("01-The-Fellowship-Of-The-Ring.txt","Romeo-and-Juliet-William-Shakesp-[ebooksread.com].txt","Secret-Adversary-Agatha-Christie-[ebooksread.com].txt","Biology-and-war-J--Arthur--John-[ebooksread.com].txt")
        val booksAsString = books.map(reading)
        (0 to booksAsString.length-1) foreach{i=>
            (0 to booksAsString.length-1) foreach{j=>
                (4 to 13) foreach{k=>
                   print(books(i))
                   print(" and ")
                   print(books(j))
                   print(" -> k = ")
                   print(k)
                   print(" J.sim = ")
                   print(jaccardSimilarity(booksAsString(i),booksAsString(j),k))  
                   println()   
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

    def SetOperations(bookAShingles:Set[String],bookBShingles:Set[String]):Float={
        val intersection = bookAShingles.intersect(bookBShingles)
        val union = bookAShingles.union(bookBShingles)
        return intersection.size.toFloat/union.size.toFloat
    }

    def jaccardSimilarity(bookA:String, bookB:String,k:Int):Float={
        val bookAShingles = kShingles(bookA,k)
        val bookBShingles = kShingles(bookB,k)
        return SetOperations(bookAShingles,bookBShingles)
    }

}
