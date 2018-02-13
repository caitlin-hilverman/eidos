package org.clulab.wm

import CAG._
import TestUtils._

class TestCagP2 extends Test {
  
  { // S1
    val tester = new Tester(p2s1)

    val economicCrisis = NodeSpec("South Sudan's economic crisis")
    val sudanesePound = NodeSpec("value of the South Sudanese Pound", Dec("depreciating", "rapidly"))
    val hardCurrency = NodeSpec("hard currency", Dec("shortages"))
    val oilPrices = NodeSpec("oil prices", Dec("declines"))
    val dependenceOnImports = NodeSpec("dependence on imports", Quant("significant"))
    
    behavior of "p2s1"

    passingTest should "have correct edges 1" taggedAs(Zheng) in {
      tester.test(EdgeSpec(sudanesePound, Causal, economicCrisis)) should be (successful)
    }
    passingTest should "have correct edges 2" taggedAs(Zheng) in {
      tester.test(EdgeSpec(hardCurrency, Causal, economicCrisis)) should be (successful)
    }
    passingTest should "have correct edges 3" taggedAs(Zheng) in {
      tester.test(EdgeSpec(oilPrices, Causal, economicCrisis)) should be (successful)
    }
    passingTest should "have correct edges 4" taggedAs(Zheng) in {
      tester.test(EdgeSpec(dependenceOnImports, Causal, economicCrisis)) should be (successful)
    }
  }

  { // S2
    // Todo: Many causal links are not true (since hunger is a dobj of cause) because of a bad parse.
    val tester = new Tester(p2s2)
  
    val conflict = NodeSpec("Conflict")
    val insecurity = NodeSpec("insecurity")
    val marketDisruption = NodeSpec("market", Dec("disruption")) //newNodeSpec("market disruption")
    val economic = NodeSpec("economic", Dec("downturn"))
    val cropFailure = NodeSpec("localized crop", Dec("failures"))
    val foodPrices = NodeSpec("food prices", Inc("high"), Quant("high", "record"))
    val hunger = NodeSpec("hunger", Inc("spread"))

    behavior of "p2s2"

    passingTest should "have correct edges 1" taggedAs(Ajay) in {
      tester.test(EdgeSpec(conflict, Causal, foodPrices)) should be (successful)
    }
    passingTest should "have correct edges 2" taggedAs(Ajay) in {
      tester.test(EdgeSpec(insecurity, Causal, foodPrices)) should be (successful)
    }
    passingTest should "have correct edges 3" taggedAs(Ajay) in {
      tester.test(EdgeSpec(marketDisruption, Causal, foodPrices)) should be (successful)
    }
    passingTest should "have correct edges 4" taggedAs(Ajay) in {
      tester.test(EdgeSpec(economic, Causal, foodPrices)) should be (successful)
    }
    passingTest should "have correct edges 5" taggedAs(Ajay) in {
      tester.test(EdgeSpec(cropFailure, Causal, foodPrices)) should be (successful)
    }
    passingTest should "have correct singleton node 1" taggedAs(Ajay) in {
      tester.test(hunger) should be (successful)
    }
  }

  { // S3
    val tester = new Tester(p2s3)

    val conflict = NodeSpec("Conflict")
    val economic = NodeSpec("economic", Dec("decline"))
    val violence = NodeSpec("violence")
    val displacement = NodeSpec("displacement")

    behavior of "p2s3"

    passingTest should "have correct edges 1" taggedAs(Somebody) in {
      tester.test(EdgeSpec(conflict, Causal, violence)) should be (successful)
    }
    passingTest should "have correct edges 2" taggedAs(Somebody) in {
      tester.test(EdgeSpec(conflict, Causal, displacement)) should be (successful)
    }
    passingTest should "have correct edges 3" taggedAs(Ajay) in {
      tester.test(EdgeSpec(economic, Causal, violence)) should be (successful)
    }
    passingTest should "have correct edges 4" taggedAs(Ajay) in {
      tester.test(EdgeSpec(economic, Causal, displacement)) should be (successful)
    }
  }

  { // S4
    val tester = new Tester(p2s4)

    val violence = NodeSpec("Violence")
    val livestock = NodeSpec("livestock") //todo: add more modifications when we have that functionality
    val displacement = NodeSpec("displacement")
    val delayedPlanting = NodeSpec("delayed planting") //todo: adjust when more mods available


    val node1 = NodeSpec(null)
    val node2 = NodeSpec(null)

    behavior of "p2s4"

    passingTest should "have correct edges 1" taggedAs(Somebody) in {
      tester.test(EdgeSpec(violence, Causal, livestock)) should be (successful)
    }
    failingTest should "have correct edges 2" taggedAs(Fan) in {
      tester.test(EdgeSpec(displacement, Causal, delayedPlanting)) should be (successful)
    }
  }

  { // S5
    val tester = new Tester(p2s5)
  
    val impactsLivestock = NodeSpec("impacts on livestock")
    val impactsCrops = NodeSpec("crops") //fixme: any way to get diff span here with impact but not with livestock?
    // TODO: the entity below is 'livelihoods being decimated' because "being..." is an acl dependency, which modifies nouns
    val livelihoods = NodeSpec("livelihoods", Dec("decimated"))

    behavior of "p2s5"

    passingTest should "have correct edges 1" taggedAs(Mihai) in {
      tester.test(EdgeSpec(impactsLivestock, Causal, livelihoods)) should be (successful)
    }
    passingTest should "have correct edges 2" taggedAs(Mihai) in {
      tester.test(EdgeSpec(impactsCrops, Causal, livelihoods)) should be (successful)
    }
  }  
}