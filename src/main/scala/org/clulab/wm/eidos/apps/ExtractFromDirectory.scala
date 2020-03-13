package org.clulab.wm.eidos.apps

import org.clulab.serialization.json.stringify
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.serialization.json.JLDCorpus
import org.clulab.wm.eidos.utils.Closer.AutoCloser
import org.clulab.wm.eidos.utils.FileUtils

object ExtractFromDirectory extends App {
  val inputDir = args(0)
  val outputDir = args(1)
  val files = FileUtils.findFiles(inputDir, "txt")
  val reader = new EidosSystem

  // For each file in the input directory:
  files.par.foreach { file =>
    // 1. Open corresponding output file
    println(s"Extracting from ${file.getName}")
    FileUtils.printWriterFromFile(s"$outputDir/${file.getName}.jsonld").autoClose { pw =>
      // 2. Get the input file contents
      val text = FileUtils.getTextFromFile(file)
      // 3. Extract causal mentions from the text
      val annotatedDocument = reader.extractFromText(text)
      // 4. Convert to JSON
      val corpus = new JLDCorpus(annotatedDocument)
      val mentionsJSONLD = corpus.serialize()
      // 5. Write to output file
      pw.println(stringify(mentionsJSONLD, pretty = true))
    }
  }
}
