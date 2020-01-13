package org.clulab.wm.eidos.context

import java.time.temporal.ChronoField.MONTH_OF_YEAR

import ai.lum.common.ConfigUtils._
import com.typesafe.config.Config
import org.yaml.snakeyaml.Yaml
import org.clulab.timenorm.scate._
import org.clulab.odin.{Mention, State, TextBoundMention}
import org.clulab.processors.Document
import org.clulab.struct.{Interval => TextInterval}
import org.clulab.wm.eidos.attachments.{Location => LocationAttachment, Time => TimeAttachment}
import org.clulab.wm.eidos.extraction.Finder
import org.clulab.wm.eidos.utils.FileUtils.getTextFromResource

import scala.collection.JavaConverters._


object SeasonFinder {

  def fromConfig(config: Config): SeasonFinder = {
    val seasonDBPath: String = config[String]("seasonsDBPath")
    val seasonDBTxt = getTextFromResource(seasonDBPath)
    val yaml = new Yaml()
    val seasonJavaMap = yaml.load(seasonDBTxt).asInstanceOf[java.util.Map[String, java.util.Map[String, java.util.Map[String, Int]]]]
    val seasonMap = seasonJavaMap.asScala.mapValues(_.asScala.mapValues(_.asScala.toMap).toMap).toMap
    new SeasonFinder(seasonMap)
  }
}


class SeasonFinder(seasonMap: Map[String, Map[String, Map[String, Int]]]) extends Finder{

  private def closestMention(sentIdx: Int, sentSize: Int, firstTokenIdx: Int, secondTokenIdx: Int, initialState: State, label: String): Option[Mention] = {
    val previousMentions = initialState.mentionsFor(sentIdx, 0 to firstTokenIdx, label).map(m => (firstTokenIdx - m.tokenInterval.start, m))
    val followingMentions = initialState.mentionsFor(sentIdx, secondTokenIdx + 1 to sentSize, label).map(m => (m.tokenInterval.start - secondTokenIdx, m))
    val sortedMentions = (previousMentions ++ followingMentions).sortBy(_._1).map(_._2)
    sortedMentions.headOption
  }

  def find(doc: Document, initialState: State): Seq[Mention] = {
    val Some(text) = doc.text // Document must have a text.
    val mentions = for {(sentence, sentenceIndex) <- doc.sentences.zipWithIndex
                        matches = sentence.lemmas.get.zipWithIndex.iterator.sliding(2, 1).filter(_(1)._1 == "season")
                        m <- matches
                        firstTokenIdx = m.head._2
                        secondTokenIdx = m.last._2
                        seasonName = m.head._1
                        geoLocation <- closestMention(sentenceIndex, sentence.size, firstTokenIdx, secondTokenIdx, initialState, "Location")
                        geoLocationID = geoLocation.attachments.head.asInstanceOf[LocationAttachment].geoPhraseID.geonameID.get
                        timex <- closestMention(sentenceIndex, sentence.size, firstTokenIdx, secondTokenIdx, initialState, "Time")
                        timeInterval = timex.attachments.head.asInstanceOf[TimeAttachment].interval.intervals.headOption
                        if seasonMap(geoLocationID).contains(seasonName) && timeInterval.isDefined
    } yield {
      val seasonStartOffset = sentence.startOffsets(firstTokenIdx)
      val seasonEndOffset = sentence.endOffsets(secondTokenIdx)
      val seasonStartMonth = seasonMap(geoLocationID)(seasonName)("start")
      val seasonEndMonth = seasonMap(geoLocationID)(seasonName)("end")

      val seasonTextInterval = TextInterval(seasonStartOffset, seasonEndOffset)
      val seasonText = text.substring(seasonStartOffset, seasonEndOffset)

      val yearInterval = Year(timeInterval.head.startDate.getYear)
      val startInterval = ThisRI(yearInterval, RepeatingField(MONTH_OF_YEAR, seasonStartMonth))
      val endInterval = ThisRI(yearInterval, RepeatingField(MONTH_OF_YEAR, seasonEndMonth))
      val seasonInterval = Between(startInterval, endInterval, startIncluded = true, endIncluded = true)
      val timeSteps: Seq[TimeStep] = Seq(TimeStep (seasonInterval.start, seasonInterval.end))
      val attachment = TimEx(seasonTextInterval, timeSteps, seasonText)

      val wordInterval = TextInterval(firstTokenIdx, secondTokenIdx + 1)
      // create the Mention for this time expression
      new TextBoundMention(
        Seq("Time"),
        wordInterval,
        sentenceIndex,
        doc,
        true,
        getClass.getSimpleName,
        Set(TimeAttachment(attachment))
      )
    }
    mentions
  }
}