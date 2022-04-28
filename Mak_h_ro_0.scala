import org.apache.commons.math3.random.RandomGenerator
import org.jamesii.ml3.experiment.init.IInitialStateBuilder
import org.jamesii.ml3.model.agents.{IAgent, IAgentFactory}
import org.jamesii.ml3.model.state.{IState, IStateFactory}
import org.jamesii.ml3.model.values.IntValue
import org.jamesii.ml3.model.{Model, Parameters}

object Mak_h_ro_0 extends App {

  import sessl._
  import sessl.ml3._

  execute {
    new Experiment with Observation with ParallelExecution with ParameterMaps with CSVOutput {
      model = "Mak_h_ro_0.ml3"
      simulator = NextReactionMethod()
      parallelThreads = 0
      replications = 1

      initializeWith(() => Mark1StateBuilder)
      startTime = 0
      stopTime = 800

      //Goods market
      //Firm
      set("productivityBase" <~ 2) //firm productivity factor
      set("fullEmployment" <~ 0.05) //Non-Accelerating Inflation Rate of Unemployment (NAIRU)
      set("gamma_y" <~ 0.15) //production change sensitivity
      set("gamma_p" <~ 0.11) //price change sensitivity
      set("inventoryTarget" <~ 2) //relative inventory target
      set("zeta_h" <~ 0.65) //sensitivity of hiring firing process
      set("zeta_f" <~ 0.50) //sensitivity of employee firing process
      set("insolvencyRatio" <~ 2) //maximum debt ratio
      set("minEquityRatio" <~ 0.40) //minimum equity ratio to have sufficient collateral for external borrowing
      set("minEmployees" <~ 5) //minimum employee size when a firm goes bankrupt
      set("avgPrice" <~ 1.50) //controls average (avg) price path
      //Other
      set("delta" <~ 0.8) //global parameter
      set("tax" <~ 0.10) //government tax
      set("benefit" <~ 1)  //unemployment benefit

      //Investment market
      set("gamma_inv" <~ 1) //investment change sensitivity
      set("investmentDuration" <~ 18) //investment loan duration
      set("minInvestCycle" <~ 3) //maximum investment frequency (in periods)

      //Credit market
      set("gamma_m" <~ 0.001) //loan interest margin sensitivity (setStrategy)
      set("gamma_r" <~ 0.10) //risk level sensitivity (setStrategy)
      set("loanDuration" <~ 18) //global loan duration
      set("ratioSolvency" <~ 0.07) //due to Basel III - Tier-1 capital + conservation buffer
      set("ratioLeverage" <~ 0.03) //due to Basel III
      set("creditTarget" <~ 0.10) //buffer (10% from total credit creation potential)
      set("probabilityDefault" <~ 0.50) //set to 0.5 since expected Cashflow - debtService should be maximum = 0
      set("inflationTarget" <~ 0.02) //due to European Central Bank [2022]
      set("naturalInterest" <~ 0.10) //global interest level within Taylor rule

      /* probability formula
      1 / (1 + exp((ego.expectedCashFlow() - ego.riskLevel(?bank) * ?debtService) / ?debtService
      where ?debtService := ?coupon + ?interest
            ?coupon := ?remainingLoanRequest / ?duration
            ?interest := ?bank.interestRateRequest(?remainingLoanRequest, ?duration)

       Explanation:
       Expected profit formula: 0 <= NPV - L - EL
       NPV = net present value (see Code)
       L = loan amount
       EL = expected loss -> LGD * pd * L
       LGD = loss given default -> (L-C)/L
       C = collateral
       pd = probability default

       Since the expression should be >= 0 each value of pd under 0.5 leads probably to a positive profit.
       But this depends on collateral and expected cash flow likewise. If pd is very low (or lower than 0.5),
       it is irrelevant since we are looking for the maximum/optimum loan amount, granted by a bank.
       -> if pd < 0.5 = optimum loan amount increases, which leads automatically to a higher C.
       Hence, no credit rationing due to pd (only to C).
       -> if pd > 0.5 = optimum loan amount decreases, which leads to lower collateral and therefore to credit rationing
       */

      //Interbank market
      set("gamma_i" <~ 0.001) //ten basis points - interbank interest change sensitivity
      set("facilityRange" <~ 0.01) //interest rate corridor (standing facility of Central Bank)
      set("interbankFrequency" <~ 20) //interaction frequency
      set("fractionalReserve" <~ 0.01) //minimum reserve requirement due to European Central Bank [2022]

      //Deposit market
      set("gamma_d" <~ 0.001) //ten basis points - deposit interest change sensitivity
      set("depositBuffer" <~ 0.03) //interest spread between lending facility and deposit interest margin
      set("hhAccountChange" <~ 12) //change bank account for better conditions every year
      set("accountFluctuationHH" <~ 0.1) //deposit change probability hurdle
      set("firmAccountChange" <~ 120) //change bank account for better conditions approximately every decade


      observeAt(range(0, 1, stopTime))
      //Household
      //observe("savings" ~ expressionDistribution("Household", "ego.savings"))

      //FIRM
      //Goods market
      observe("prices" ~ expressionDistribution("Firm", "ego.price"))
      observe("minPrice" ~ expressionDistribution("Firm", "ego.minPrice()"))
      observe("production" ~ expressionDistribution("Firm", "ego.production()"))
      observe("targetProduction" ~ expressionDistribution("Firm", "ego.targetProduction"))
      observe("consumption" ~ expressionDistribution("Firm", "ego.consumption"))
      observe("consumptionUnit" ~ expressionDistribution("Firm", "ego.consumptionUnit"))
      observe("investmentConsume" ~ expressionDistribution("Firm", "ego.investmentConsume"))
      observe("investmentConsumeUnit" ~
              expressionDistribution("Firm", "ego.investmentConsumeUnit"))
      observe("strategyRate" ~ expressionDistribution("Firm", "ego.strategyChangeRate()"))
      observe("laborDemand" ~ expressionDistribution("Firm", "ego.laborDemand()"))
      observe("employees" ~ expressionDistribution("Firm", "ego.employees.size()"))
      observe("Stock" ~ expressionDistribution("Firm", "ego.inventory"))
      observe("inventoryTarget" ~ expressionDistribution("Firm", "ego.inventoryTarget()"))
      observe("turnover" ~ expressionDistribution("Firm", "ego.turnover"))
      observe("liquidity" ~ expressionDistribution("Firm", "ego.liquidity"))
      observe("equity" ~ expressionDistribution("Firm", "ego.equity"))
      observe("debt" ~ expressionDistribution("Firm", "ego.debt"))
      observe("profit" ~ expressionDistribution("Firm", "ego.profitAfterTax"))

      //Investment cycle
      observe("investmentAmount" ~ expressionDistribution("Firm", "ego.investmentAmount"))
      observe("investmentTurnover" ~ expressionDistribution("Firm", "ego.investmentTurnover"))
      observe("investmentPool" ~ expressionDistribution("Firm", "ego.investmentPool()"))

      //Credit market
      observe("interest" ~ expressionDistribution("Firm", "ego.interest()"))
      observe("coupon" ~ expressionDistribution("Firm", "ego.coupon()"))
      observe("loanLiability" ~ expressionDistribution("Firm", "ego.creditAmount()"))
      observe("solvency" ~ expressionDistribution("Firm", "ego.solvency()"))
      observe("rating" ~ expressionDistribution("Firm", "ego.ecaiRating()"))
      observe("inflationAverage" ~ expressionDistribution("Firm", "ego.inflationAverage()"))
      observe("interestLevel" ~ expressionDistribution("Firm", "ego.interestLevel()"))
      observe("countBankruptcy" ~ expressionDistribution("Firm", "ego.countBankruptcy"))
      observe("expectedCashFlow" ~ expressionDistribution("Firm", "ego.expectedCashFlow()"))
      observe("equityRatio" ~ expressionDistribution("Firm", "ego.equityRatio()"))
      observe("leverageRatio" ~ expressionDistribution("Firm", "ego.leverageRatio()"))
      observe("bankProfit" ~ expressionDistribution("Firm", "ego.bankProfit()"))

      observe("id" ~ expressionDistribution("Firm", "ego.id"))

      //VALIDATION
      observe("govBailoutGDP" ~ expressionDistribution("Firm", "ego.govBailoutGDP()"))
      observe("bankRuptcyCheck" ~ expressionDistribution("Firm", "ego.bankRuptcyCheck()"))
      observe("creditDefault" ~ expressionDistribution("Firm", "ego.bankCreditDefault()"))
      observe("investmentPeriod" ~ expressionDistribution("Firm", "ego.investmentPeriod"))
      observe("investmentQuarterly" ~
        expressionDistribution("Firm", "ego.investmentQuarterly"))
      observe("consumptionQuarterly" ~
              expressionDistribution("Firm", "ego.consumptionQuarterly"))
      observe("supplyQuarterly" ~ expressionDistribution("Firm", "ego.supplyQuarterly"))

/*
            //BANK
            //Credit market
            observe("equity" ~ expressionDistribution("Bank", "ego.equity"))
            observe("liquidity" ~ expressionDistribution("Bank", "ego.liquidity"))
            observe("margin" ~ expressionDistribution("Bank", "ego.margin"))
            observe("interestOffer" ~ expressionDistribution("Bank", "ego.makeOffer()"))
            observe("riskTaking" ~ expressionDistribution("Bank", "ego.riskLevel"))
            observe("demandSolvency" ~ expressionDistribution("Bank", "ego.creditDemandSolvency"))
            observe("demandLeverage" ~ expressionDistribution("Bank", "ego.creditDemandLeverage"))
            observe("creditAmount" ~ expressionDistribution("Bank", "ego.creditAmount()"))
            observe("creditECAI" ~ expressionDistribution("Bank", "ego.creditECAI()"))
            observe("strategyRate" ~ expressionDistribution("Bank", "ego.strategyChangeRate()"))
            observe("creditDemandRelation" ~ expressionDistribution("Bank", "ego.creditDemandRelation()"))
            observe("equityNeed" ~ expressionDistribution("Bank", "ego.equityNeed()"))
            observe("ecaiSolvency" ~ expressionDistribution("Bank", "ego.ecaiSolvency()"))
            observe("ecaiLeverage" ~ expressionDistribution("Bank", "ego.ecaiLeverage()"))

            //Interbank market
            observe("depositMargin" ~ expressionDistribution("Bank", "ego.depositMargin"))
            observe("interbankMargin" ~ expressionDistribution("Bank", "ego.interbankMargin"))
            observe("keyInterRate" ~ expressionDistribution("Bank", "ego.keyInterRate()"))
            observe("depositsTotal" ~ expressionDistribution("Bank", "ego.deposits()"))
            observe("reserveRequirement" ~ expressionDistribution("Bank", "ego.reserveRequirement()"))
            observe("reservesNeed" ~ expressionDistribution("Bank", "ego.reservesNeed()"))
            observe("reservesDemand" ~ expressionDistribution("Bank", "ego.reservesDemand"))
            observe("reservesExcess" ~ expressionDistribution("Bank", "ego.reservesExcess()"))
            observe("basisMoney" ~ expressionDistribution("Bank", "ego.basisMoney"))
            observe("centralLoan" ~ expressionDistribution("Bank", "ego.centralLoan"))
            observe("reserveOutflow" ~ expressionDistribution("Bank", "ego.expectedReserveOutflow()"))
            observe("centralCosts" ~ expressionDistribution("Bank", "ego.centralCosts_lag"))
            observe("interbankCosts" ~ expressionDistribution("Bank", "ego.interbankCosts"))
            observe("interbankProfit" ~ expressionDistribution("Bank", "ego.interbankProfit"))
            observe("interbankVolume" ~ expressionDistribution("Bank", "ego.interbankVolume"))
            observe("centralVolume" ~ expressionDistribution("Bank", "ego.interbankVolume()"))

            observe("id" ~ expressionDistribution("Bank", "ego.id"))
*/
/*
                //CENTRALBANK
                observe("keyRate" ~ expressionDistribution("CentralBank", "ego.keyRate"))
                observe("lendingFacility" ~ expressionDistribution("CentralBank", "ego.lendingFacility"))
                observe("inflationBasis" ~ expressionDistribution("CentralBank", "ego.inflationBasis"))
                observe("inflationAverage" ~ expressionDistribution("CentralBank", "ego.inflationAverage()"))

                //GOVERNMENT
                  observe("debt" ~ expressionDistribution("Government", "ego.debt"))
                  observe("liquidity" ~ expressionDistribution("Government", "ego.liquidity"))

                //CREDITLINE
                  observe("interest" ~ expressionDistribution("CreditLine", "ego.interest"))
                  observe("coupon" ~ expressionDistribution("CreditLine", "ego.coupon"))
                  observe("loanLiability" ~ expressionDistribution("CreditLine", "ego.loanLiability"))
*/

      csvOutputDirectory(() => "results")
      withExperimentResult(writeCSV)

      object Mark1StateBuilder extends IInitialStateBuilder {
        def createHousehold(af: IAgentFactory, model: Model, bank: IAgent) : IAgent = {
          val household = af.createAgent(model.getAgentDeclaration("Household"), 0)
          household.addLink("depositInstituteHH", bank)
          bank.addLink("depositorHH", household)
          household
        }

        def createFirm(af: IAgentFactory, model: Model, bank: IAgent, firm_id: Int) : IAgent = {
          val firm = af.createAgent(model.getAgentDeclaration("Firm"), 0)
          firm.setAttributeValue("id", new IntValue(firm_id))
          firm.addLink("depositInstituteFirm", bank)
          bank.addLink("depositorFirm", firm)
          firm
        }

        def createBank(af: IAgentFactory, model: Model, bank_id: Int) : IAgent = {
          val bank = af.createAgent(model.getAgentDeclaration("Bank"), 0)
          bank.setAttributeValue("id", new IntValue(bank_id))
          bank
        }

        def buildInitialState(model: Model, sf: IStateFactory, af: IAgentFactory,
                              rng: RandomGenerator, params: Parameters): IState = {
          val n_F = params.getValue("n_F").getValue.asInstanceOf[Int]
          val n_H = params.getValue("n_H").getValue.asInstanceOf[Int]
          val n_B = params.getValue("n_B").getValue.asInstanceOf[Int]
          val n_CL = params.getValue("n_CL").getValue.asInstanceOf[Int]

          val state = sf.create()

          //create mercury = auxiliary agent
          val mercury = af.createAgent(model.getAgentDeclaration("Mercury"), 0)
          state.addAgent(mercury)

          var firm_id = 0
          //create banks and respective households
          for (i <- 1 to n_B) {
            val bank = createBank(af, model, i)
            state.addAgent(bank)

            for (j <- 1 to (n_H / n_B)) {
              state.addAgent(createHousehold(af, model, bank))
            }
            for (k <- 1 to (n_F / n_B)) {
              state.addAgent(createFirm(af, model, bank, firm_id))
              firm_id = firm_id + 1
            }
          }

          val banks = state.getAgentsAliveByType("Bank").iterator()
          for (i <- 1 to (n_H % n_B)) {
            val bank = banks.next()
            state.addAgent(createHousehold(af, model, bank))
          }
          val banks2 = state.getAgentsAliveByType("Bank").iterator()
          for (i <- 1 to (n_F % n_B)) {
            val bank = banks2.next()
            state.addAgent(createFirm(af, model, bank, firm_id))
            firm_id = firm_id + 1
          }

          //create central bank
          val centralbank = af.createAgent(model.getAgentDeclaration("CentralBank"), 0)
          state.addAgent(centralbank)

          // create government
          val government = af.createAgent(model.getAgentDeclaration("Government"), 0)
          state.addAgent(government)


          state
        }
      }
    }
  }
}
