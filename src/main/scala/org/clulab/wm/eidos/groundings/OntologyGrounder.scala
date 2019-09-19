package org.clulab.wm.eidos.groundings

import org.clulab.wm.eidos.attachments.{EidosAttachment, Property}
import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.document.PostProcessing
import org.clulab.wm.eidos.groundings.OntologyAliases._
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.utils.Canonicalizer
import org.clulab.wm.eidos.utils.Namer
import org.slf4j.Logger
import org.slf4j.LoggerFactory

import scala.util.matching.Regex

object OntologyAliases {
  type SingleOntologyGrounding = (Namer, Float)
  type MultipleOntologyGrounding = Seq[SingleOntologyGrounding]
  // The string is something like wm or un.
  type OntologyGroundings = Map[String, OntologyGrounding]
}

case class OntologyGrounding(grounding: MultipleOntologyGrounding = Seq.empty) {
  def nonEmpty: Boolean = grounding.nonEmpty
  def take(n: Int): MultipleOntologyGrounding = grounding.take(n)
  def headOption: Option[SingleOntologyGrounding] = grounding.headOption
  def headName: Option[String] = headOption.map(_._1.name)
}

trait OntologyGrounder {
  val isPrimary: Boolean

  def groundOntology(mention: EidosMention): OntologyGrounding = groundOntology(mention, None)
  def groundOntology(mention: EidosMention, previousGroundings: Option[OntologyGroundings]): OntologyGrounding
  def groundOntology(mention: EidosMention, previousGroundings: OntologyGroundings): OntologyGrounding = groundOntology(mention, Some(previousGroundings))

  def groundable(mention: EidosMention, previousGroundings: Option[OntologyGroundings]): Boolean
  def groundable(mention: EidosMention): Boolean = groundable(mention, None)
  def groundable(mention: EidosMention, previousGroundings: OntologyGroundings): Boolean = groundable(mention, Some(previousGroundings))
}

// It is unfortunate that the "ing" suffix is already part of grounding, so we're left with "er" even for a trait.
trait MultiOntologyGrounder {
  def groundOntology(mention: EidosMention): OntologyGroundings
}

class EidosOntologyGrounder(val name: String, val domainOntology: DomainOntology, wordToVec: EidosWordToVec, canonicalizer: Canonicalizer) extends OntologyGrounder {
  // Is not dependent on the output of other grounders
  val isPrimary = true

  val conceptEmbeddings: Seq[ConceptEmbedding] =
    0.until(domainOntology.size).map { n =>
      ConceptEmbedding(domainOntology.getNamer(n),
           wordToVec.makeCompositeVector(domainOntology.getValues(n)))
    }

  val conceptPatterns: Seq[ConceptPatterns] =
    0.until(domainOntology.size).map { n =>
      ConceptPatterns(domainOntology.getNamer(n),
        domainOntology.getPatterns(n))
    }

  def groundOntology(mention: EidosMention, previousGroundings: Option[OntologyGroundings]): OntologyGrounding = {
    // Sieve-based approach
    if (EidosOntologyGrounder.groundableType(mention)) {
      // First check to see if the text matches a regex from the ontology, if so, that is a very precise
      // grounding and we want to use it.
      val matchedPatterns = nodesPatternMatched(mention.odinMention.text, conceptPatterns)
      if (matchedPatterns.nonEmpty) {
        OntologyGrounding(matchedPatterns)
      }
      // Otherwise, back-off to the w2v-based approach
      else {
        val canonicalNameParts = canonicalizer.canonicalNameParts(mention)
        OntologyGrounding(wordToVec.calculateSimilarities(canonicalNameParts, conceptEmbeddings))
      }
    }
    else
      OntologyGrounding()
  }

  def groundable(mention: EidosMention, primaryGrounding: Option[OntologyGroundings]): Boolean = EidosOntologyGrounder.groundableType(mention)

  // For Regex Matching
  def nodesPatternMatched(s: String, nodes: Seq[ConceptPatterns]): Seq[(Namer, Float)] = {
    nodes.filter(node => nodePatternsMatch(s, node.patterns)).map(node => (node.namer, 1.0f))
  }

  def nodePatternsMatch(s: String, patterns: Option[Array[Regex]]): Boolean = {
    patterns match {
      case None => false
      case Some(rxs) =>
        for (r <- rxs) {
          if (r.findFirstIn(s).nonEmpty) return true
        }
        false
    }
  }

  // For API to reground strings
  def groundText(text: String): OntologyGrounding = {
    val matchedPatterns = nodesPatternMatched(text, conceptPatterns)
    if (matchedPatterns.nonEmpty) {
      OntologyGrounding(matchedPatterns)
    }
    // Otherwise, back-off to the w2v-based approach
    else {
      OntologyGrounding(wordToVec.calculateSimilarities(text.split(" +"), conceptEmbeddings))
    }
  }

}

// todo: surely there is a way to unify this with the PluginOntologyGrounder below -- maybe split out to a "stringMatchPlugin" and an "attachmentBasedPlugin" ?
class PropertiesOntologyGrounder(name: String, domainOntology: DomainOntology, wordToVec: EidosWordToVec, canonicalizer: Canonicalizer)
    extends EidosOntologyGrounder(name, domainOntology, wordToVec, canonicalizer) {

  override def groundable(mention: EidosMention, primaryGrounding: Option[OntologyGroundings]): Boolean = EidosOntologyGrounder.groundableType(mention) && mention.odinMention.attachments.exists(a => a.isInstanceOf[Property])

  override def groundOntology(mention: EidosMention, previousGroundings: Option[OntologyGroundings]): OntologyGrounding = {
    if (groundable(mention, previousGroundings)) {
      val propertyAttachments = mention.odinMention.attachments.filter(a => a.isInstanceOf[Property])
      // These need to be sorted after retrieval from a set.  Otherwise the order differs and
      // eventual multiplication of floats in different orders produces different results.
      val propertyTokens = propertyAttachments.flatMap(EidosAttachment.getAttachmentWords).toArray.sorted

      // FIXME - should be lemmas?
      OntologyGrounding(wordToVec.calculateSimilarities(propertyTokens, conceptEmbeddings))
    }
    else
      OntologyGrounding()
  }
}

/**
  * Used to make a secondary grounding ONLY IF the primary grounding matches the specified trigger
  * @param name name of the ontology
  * @param domainOntology the ontology to use
  * @param wordToVec the w2v to calculate the similarities
  * @param pluginGroundingTrigger the string to look for in the primary grounding
  */
class PluginOntologyGrounder(name: String, domainOntology: DomainOntology, wordToVec: EidosWordToVec, pluginGroundingTrigger: String, canonicalizer: Canonicalizer)
    extends EidosOntologyGrounder(name, domainOntology, wordToVec, canonicalizer) {

  // No, because it IS dependent on the output of other grounders
  override val isPrimary = false

  override def groundable(mention: EidosMention, previousGrounding: Option[OntologyGroundings]): Boolean = {
    val groundable = previousGrounding match {
      case Some(prev) =>
        prev.get(EidosOntologyGrounder.PRIMARY_NAMESPACE).exists(_.headName.exists(_ contains pluginGroundingTrigger))
      case _ => false
    }

    groundable
  }

  override def groundOntology(mention: EidosMention, previousGroundings: Option[OntologyGroundings]): OntologyGrounding = {
    if (groundable(mention, previousGroundings)) {
      super.groundOntology(mention, None)
    } else {
      OntologyGrounding()
    }
  }
}

class EidosMultiOntologyGrounder(ontologyGrounders: Seq[EidosOntologyGrounder]) extends MultiOntologyGrounder with PostProcessing {
  // Some plugin grounders need to be run after the primary grounders, i.e., they depend on the output of the primary grounders
  protected val (primaryGrounders, secondaryGrounders) = ontologyGrounders.partition(_.isPrimary)

  def groundOntology(mention: EidosMention): OntologyGroundings = {
    val primaryGroundings = primaryGrounders.map(ontologyGrounder =>
      (ontologyGrounder.name, ontologyGrounder.groundOntology(mention))).toMap
    val secondaryGroundings = secondaryGrounders.map(ontologyGrounder =>
      (ontologyGrounder.name, ontologyGrounder.groundOntology(mention, primaryGroundings))).toMap

    primaryGroundings ++ secondaryGroundings
  }

  def process(annotatedDocument: AnnotatedDocument): AnnotatedDocument = {
    annotatedDocument.allEidosMentions.foreach(groundOntology)
    annotatedDocument
  }
}

object EidosOntologyGrounder {
  protected val        GROUNDABLE = "Entity"
  protected val      WM_NAMESPACE = "wm" // This one isn't in-house, but for completeness...
  // Namespace strings for the different in-house ontologies we typically use
  protected val      UN_NAMESPACE = "un"
  protected val     WDI_NAMESPACE = "wdi"
  protected val     FAO_NAMESPACE = "fao"
  protected val    MESH_NAMESPACE = "mesh"
  protected val   PROPS_NAMESPACE = "props"
  protected val MITRE12_NAMESPACE = "mitre12"
  protected val     WHO_NAMESPACE = "who"
  protected val     INT_NAMESPACE = "interventions"
  protected val   ICASA_NAMESPACE = "icasa"

  val PRIMARY_NAMESPACE: String = WM_NAMESPACE // Assign the primary namespace here, publically.

  // Used for plugin ontologies
//  protected val INTERVENTION_PLUGIN_TRIGGER = "UN/interventions"
  protected val INTERVENTION_PLUGIN_TRIGGER = "wm/concept/causal_factor/intervention/"

  protected val indicatorNamespaces: Set[String] = Set(WDI_NAMESPACE, FAO_NAMESPACE, MITRE12_NAMESPACE, WHO_NAMESPACE, ICASA_NAMESPACE)

  protected lazy val logger: Logger = LoggerFactory.getLogger(this.getClass)

  def groundableType(mention: EidosMention): Boolean = mention.odinMention.matches(GROUNDABLE)

  def apply(name: String, domainOntology: DomainOntology, wordToVec: EidosWordToVec, canonicalizer: Canonicalizer): EidosOntologyGrounder =
    name match {
      case INT_NAMESPACE => new PluginOntologyGrounder(name, domainOntology, wordToVec, INTERVENTION_PLUGIN_TRIGGER, canonicalizer)
      case PROPS_NAMESPACE => new PropertiesOntologyGrounder(name, domainOntology, wordToVec, canonicalizer)
      case _ => new EidosOntologyGrounder(name, domainOntology, wordToVec, canonicalizer)
    }
}
