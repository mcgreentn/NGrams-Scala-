//Michael Green
//mcg216
//Program 3
import java.io.File
import scala.io.Source
import java.lang.String
import scala.collection.mutable.Map


object test
{
	var wordFrequency: Map[String, Map[String, Int]] = Map()

	def main(args: Array[String]): Unit = 
	{
		val files = getListOfFiles("./browncorpus")

		for(file <- files)
		{
			var wordsList = Source.fromFile(file).getLines.toList.map(y=>y.trim.split(" +").toList).filterNot(_.forall(_.isEmpty)).map(q=>q.map(j=>j.substring(0, j.indexOf("/")).toLowerCase())).flatten.filterNot(_.matches("[?,\'`.():;-]+"))
			var i = 1
			for(word <- wordsList)
			{	
				if(i < wordsList.length)
				{
					if(wordFrequency.contains(word))
					{
						var insideJob =  wordFrequency(word)
						if(insideJob.contains(wordsList(i)))
						{
							var count = insideJob(wordsList(i))
							count += 1
							insideJob.put(wordsList(i), count)
						}
						else
						{
							insideJob.put(wordsList(i), 1)
						}
						wordFrequency.put(word, insideJob)
					}
					else
					{
						var insideJob: Map[String, Int] = Map()
						insideJob.put(wordsList(i), 1)
						wordFrequency.put(word, insideJob)
					}
				}
				i += 1
			}
		}
		println(wordFrequency("forest"))
		println("Total counts for \"forest\": " + countAll("forest"))
		println("Probability for \"highway\" in \"forest\" " + p1("highway", "forest"))
		println("Most likely next word for \"the\": " + mostLikelyNextWord("the"))
		//println(wordFrequency)
	}

	def getListOfFiles(dir: String):List[File] = 
	{
		val d = new File(dir)
	 	if (d.exists && d.isDirectory) {
	    	d.listFiles.filter(_.isFile).toList
	  	} else {
	    List[File]()
	  	}
	}

	def countAll(given:String): Int =
	{
		var total = 0
		if(wordFrequency.contains(given))
		{
			var insideJob =  wordFrequency(given)
			for((k, p) <- insideJob)
			{
				total += p
			}
		}
		return total
	}

	def p1(of:String, given:String): Double =
	{
		var total = countAll(given)
		var count = 0;
		if(wordFrequency.contains(given))
		{
			var insideJob = wordFrequency(given)
			if(insideJob.contains(of))
			{
				count = insideJob(of)
			}
		}
		var probability = 0.0
		if(total != 0)
		{
			probability = 1.0 * count / total
		}
		return probability
	}

	def mostLikelyNextWord(given:String): String =
	{
		var mostLikely = ""
		var steelBeams = 0
		if(wordFrequency.contains(given))
		{
			var insideJob = wordFrequency(given)
			for((p, k) <- insideJob)
			{
				if(k > steelBeams)
				{
					mostLikely = p
				}
			}
		}
		return mostLikely
	}
}