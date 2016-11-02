import com.optstack.api._
import com.optstack.api.strategies._
import com.optstack.math._
import com.optstack.api.Implicits._


class voltrader (config:BacktestConfig) extends StrategyRunnerT (config) {


  var STOCK = this.getInstrument("DIS")

var high = .432;
var low = 0.150;
  def handleData () = {

    this.plot(STOCK.studies.ImpliedVolatility(), "iv")
    // Begin Buying Straddles, Vol Rank 1-2
    if (((STOCK.studies.Adx(10)).crossesAbove(25)) && ((((low + ((high - low) / 10) * 0).<=((STOCK.studies.ImpliedVolatility()))) && ((STOCK.studies.ImpliedVolatility()).<=((low + ((high - low) / 10) * 1)))) && (this.getPortfolio.availableBuyingPower.>(0)))) {
      var strategy1 = STOCK.strategies.straddleStrangles.Straddle
      var strategyfilter1 = strategy1.selectBy(false)
        strategyfilter1.daysToExpiration(25, 45)
        strategyfilter1.delta(-100, 100)


      strategy1.rankBy.max.riskReward
      strategy1.setMaxOpenPositions(1)
      strategy1.buy("buystraddleStrangles.Straddle1", "Buy Straddle When ADX breaks out above 25").sizeBy.cost(50000)

    }
    // Begin Buying Calendar Spread, Vol Rank 1-2
    if ((((low + ((high - low) / 10) * 0).<=((STOCK.studies.ImpliedVolatility()))) && ((STOCK.studies.ImpliedVolatility()).<=((low + ((high - low) / 10) * 1)))) && (this.getPortfolio.availableBuyingPower.>(0))) {
      var strategy2 = this.getInstrument("STOCK").strategies.calendars.PutCalendar
      var strategyfilter2 = strategy2.selectBy(false)
        strategyfilter2.daysBetweenOptions(60, 90)
        strategy2.selectShorterTermOptionBy().deltaAbs(45, 55)
        strategy2.selectShorterTermOptionBy().daysToExpiration(20, 40)
        strategy2.selectLongerTermOptionBy().deltaAbs(45, 55)
        strategy2.selectLongerTermOptionBy().daysToExpiration(80, 120)


      strategy2.rankBy.max.riskReward
      strategy2.setMaxOpenPositions(1)
      strategy2.buy("buycalendars.PutCalendar1", "Enter Calendar When Volatility Is Low").sizeBy.quantity(10)

    }
    // Begin Buying Naked Options, Vol Rank 1-3
    if (((((low + ((high - low) / 10) * 0).<=((STOCK.studies.ImpliedVolatility()))) && ((STOCK.studies.ImpliedVolatility()).<=((low + ((high - low) / 10) * 2)))) && (this.getPortfolio.availableBuyingPower.>(0))) && ((STOCK.studies.Rsi(10)).crossesAbove(30))) {
      var strategy3 = STOCK.strategies.single.Call
      var strategyfilter3 = strategy3.selectBy(false)
        strategyfilter3.daysToExpiration(30, 45)
        strategyfilter3.deltaAbs(40, 50)


      strategy3.rankBy.max.riskReward
      strategy3.setMaxOpenPositions(1)
      strategy3.buy("buysingle.Call1", "Buy Call When Market is underbought").sizeBy.cost(10000)

    }
    if (((((low + ((high - low) / 10) * 0).<=((STOCK.studies.ImpliedVolatility()))) && ((STOCK.studies.ImpliedVolatility()).<=((low + ((high - low) / 10) * 2)))) && (this.getPortfolio.availableBuyingPower.>(0))) && ((STOCK.studies.Rsi(10)).crossesAbove(70))) {
      var strategy4 = STOCK.strategies.single.Put
      var strategyfilter4 = strategy4.selectBy(false)
        strategyfilter4.daysToExpiration(30, 45)
        strategyfilter4.deltaAbs(40, 50)


      strategy4.rankBy.max.riskReward
      strategy4.setMaxOpenPositions(1)
      strategy4.buy("buysingle.Put1", "Buy Put When Market is overbought").sizeBy.cost(10000)

    }
    // Begin Iron Condor, Vol Rank 1-5
    if (((((low + ((high - low) / 10) * 8).<=((STOCK.studies.ImpliedVolatility()))) && ((STOCK.studies.ImpliedVolatility()).<=((low + ((high - low) / 10) * 10)))) && (this.getPortfolio.availableBuyingPower.>(0))) && (((STOCK.studies.HistoricalVolatility(10)).Rank(20)).crossesAbove(90))) {
      var strategy5 = STOCK.strategies.condors.IronCondor
      var strategyfilter5 = strategy5.selectBy(false)
        strategyfilter5.daysToExpiration(10, 100)
        strategy5.selectHigherPutStrikeBy().deltaAbs(10, 15)
        strategy5.selectLowerCallStrikeBy().deltaAbs(10, 15)
        strategyfilter5.riskReward(0.15, 0.25)


      strategy5.rankBy.max.riskReward
      strategy5.setMaxOpenPositions(1)
      strategy5.buy("buycondors.IronCondor1", "Sell Iron Condor On High Volatility").sizeBy.quantity(10)

    }
    // Begin Ratio Spreads, Vol Rank 8-10
    if ((((low + ((high - low) / 10) * 8).<=((STOCK.studies.ImpliedVolatility()))) && ((STOCK.studies.ImpliedVolatility()).<=((low + ((high - low) / 10) * 10)))) && (this.getPortfolio.availableBuyingPower.>(0))) {
      var strategy6 = STOCK.strategies.ratios.PutRatioSpread(1,2)
      var strategyfilter6 = strategy6.selectBy(false)
        strategyfilter6.daysToExpiration(80, 100)
        strategy6.selectSmallerRatioOptionBy().outOfTheMoneyPercentage(0, 0.02)
        strategy6.selectLargerRatioOptionBy().outOfTheMoneyPercentage(0.12, 0.15)


      strategy6.rankBy.max.riskReward
      strategy6.setMaxOpenPositions(1)
      strategy6.buy("buyratios.PutRatioSpread1", "Buy 1 x 5 Put Ratio Spread").sizeBy.quantity(10)

      var strategy7 = STOCK.strategies.ratios.CallRatioSpread(1,2)
      var strategyfilter7 = strategy7.selectBy(false)
        strategyfilter7.daysToExpiration(80, 100)
        strategy7.selectSmallerRatioOptionBy().outOfTheMoneyPercentage(0, 0.02)
        strategy7.selectLargerRatioOptionBy().outOfTheMoneyPercentage(0.12, 0.15)


      strategy7.rankBy.max.riskReward
      strategy7.setMaxOpenPositions(1)
      strategy7.buy("buyratios.CallRatioSpread1", "Buy 1 x 5 Call  Ratio Spread").sizeBy.quantity(10)

    }
    // Sell double verticals, Vol Rank 7-10
    if (((((low + ((high - low) / 10) * 7).<=((STOCK.studies.ImpliedVolatility()))) && ((STOCK.studies.ImpliedVolatility()).<=((low + ((high - low) / 10) * 10)))) && (this.getPortfolio.availableBuyingPower.>(0))) && ((STOCK.studies.MovingAverage(15)).crossesAbove((STOCK.studies.MovingAverage(30))))) {
      var strategy8 = STOCK.strategies.verticals.PutVertical
      var strategyfilter8 = strategy8.selectBy(false)
        strategyfilter8.daysToExpiration(20, 30)
        strategyfilter8.deltaAbs(15, 20)
        strategy8.selectHigherStrikeOptionBy().deltaAbs(25, 30)


      strategy8.rankBy.max.riskReward
      strategy8.setMaxOpenPositions(1)
      strategy8.sell("sellverticals.PutVertical1", "Sell Put Vertical On Bullish Crossover").sizeBy.credit(12000)

    }
    if (((((low + ((high - low) / 10) * 0).<=((STOCK.studies.ImpliedVolatility()))) && ((STOCK.studies.ImpliedVolatility()).<=((low + ((high - low) / 10) * 3)))) && (this.getPortfolio.availableBuyingPower.>(0))) && ((STOCK.studies.MovingAverage(15)).crossesBelow((STOCK.studies.MovingAverage(30))))) {
      var strategy9 = STOCK.strategies.verticals.CallVertical
      var strategyfilter9 = strategy9.selectBy(false)
        strategyfilter9.daysToExpiration(20, 30)
        strategyfilter9.deltaAbs(15, 20)
        strategy9.selectHigherStrikeOptionBy().deltaAbs(25, 30)


      strategy9.rankBy.max.riskReward
      strategy9.setMaxOpenPositions(1)
      strategy9.sell("sellverticals.CallVertical1", "Sell Call Vertical On Bearish Crossover").sizeBy.credit(12000)

    }
    // Sell naked puts, Vol Rank 9-10
    if ((((low + ((high - low) / 10) * 9).<=((STOCK.studies.ImpliedVolatility()))) && ((STOCK.studies.ImpliedVolatility()).<=((low + ((high - low) / 10) * 10)))) && (this.getPortfolio.availableBuyingPower.>(0))) {
      var strategy10 = this.getInstrument("STOCK").strategies.single.Put
      var strategyfilter10 = strategy10.selectBy(false)
        strategy10.selectPutOptionBy().deltaAbs(10, 20)
        strategyfilter10.daysToExpiration(20, 45)


      strategy10.rankBy.max.riskReward
      strategy10.setMaxOpenPositions(1)
      strategy10.sell("sellsingle.Put1", "Sell Naked Put When VIX Is At 1 Month High").sizeBy.credit(50000)

    }
    // Sell straddles, Vol Rank 9-10
    if ((((low + ((high - low) / 10) * 9).<=((STOCK.studies.ImpliedVolatility()))) && ((STOCK.studies.ImpliedVolatility()).<=((low + ((high - low) / 10) * 10)))) && (this.getPortfolio.availableBuyingPower.>(0))) {
      var strategy11 = this.getInstrument("STOCK").strategies.straddleStrangles.Straddle
      var strategyfilter11 = strategy11.selectBy(false)
        strategyfilter11.daysToExpiration(30, 45)
        strategy11.selectCallOptionBy().deltaAbs(15, 20)
        strategy11.selectPutOptionBy().deltaAbs(15, 20)


      strategy11.rankBy.max.riskReward
      strategy11.setMaxOpenPositions(1)
      strategy11.sell("sellstraddleStrangles.Straddle1", "Sell Strangle When Volatility Is High").sizeBy.credit(20000)

    }

    var positionLoopIndex = 0
    positionLoopIndex = 1
    this.selectPosition(orderID="buystraddleStrangles.Straddle1").foreach ({ curItem =>
        var curItemAsStrategy = curItem.asStrategy.straddle

        if ((curItemAsStrategy.daysToLastTradeableDate).<=(1)) {
            curItemAsStrategy.close("Close @ Expiration"); // Strategy AtExpiration is close
        }

        if ((curItemAsStrategy.returnOnCostBasis).<=(-0.5)) {
          curItemAsStrategy.close("Stop Loss When -100% Loss or worse")
        }
        if ((curItemAsStrategy.returnOnCostBasis).>=(0.2)) {
          curItemAsStrategy.close("Take profit when +20% return or greater")
        }

        positionLoopIndex += 1
    })
    positionLoopIndex = 1
    this.selectPosition(orderID="buycalendars.PutCalendar1").foreach ({ curItem =>
        var curItemAsStrategy = curItem.asStrategy.calendar

        if ((curItemAsStrategy.daysToLastTradeableDate).<=(1)) {
            curItemAsStrategy.close("Close @ Expiration"); // Strategy AtExpiration is close
        }

        if ((curItemAsStrategy.returnOnCostBasis).>=(0.3)) {
          curItemAsStrategy.close("Exit Calendar When Profit Target Reached")
        }

        positionLoopIndex += 1
    })
    positionLoopIndex = 1
    this.selectPosition(orderID="buysingle.Call1").foreach ({ curItem =>
        var curItemAsStrategy = curItem.asStrategy.callOption

        if ((curItemAsStrategy.daysToLastTradeableDate).<=(1)) {
            curItemAsStrategy.close("Close @ Expiration"); // Strategy AtExpiration is close
        }

        if ((curItemAsStrategy.returnOnCostBasis).>=(1.25)) {
          curItemAsStrategy.close("Take profit when profit exceeds 125%")
        }
        if ((curItemAsStrategy.returnOnCostBasis).<=(-0.5)) {
          curItemAsStrategy.close("Stop loss when loss exceeds -50%")
        }

        positionLoopIndex += 1
    })
    positionLoopIndex = 1
    this.selectPosition(orderID="buysingle.Put1").foreach ({ curItem =>
        var curItemAsStrategy = curItem.asStrategy.putOption

        if ((curItemAsStrategy.daysToLastTradeableDate).<=(1)) {
            curItemAsStrategy.close("Close @ Expiration"); // Strategy AtExpiration is close
        }

        if ((curItemAsStrategy.returnOnCostBasis).<=(-0.5)) {
          curItemAsStrategy.close("Stop loss when loss exceeds -50%")
        }
        if ((curItemAsStrategy.returnOnCostBasis).>=(1.25)) {
          curItemAsStrategy.close("Take profit when profit exceeds 125%")
        }

        positionLoopIndex += 1
    })
    positionLoopIndex = 1
    this.selectPosition(orderID="buycondors.IronCondor1").foreach ({ curItem =>
        var curItemAsStrategy = curItem.asStrategy.ironCondor

        if ((curItemAsStrategy.daysToLastTradeableDate).<=(1)) {
            curItemAsStrategy.close("Close @ Expiration"); // Strategy AtExpiration is close
        }

        if ((curItemAsStrategy.returnOnMaxReward).<=(-1.5)) {
          curItemAsStrategy.close("Stop Loss At Loss of (-150%) Of Max Credit")
        }
        if ((curItemAsStrategy.returnOnMaxReward).>=(0.7)) {
          curItemAsStrategy.close("Exit At Profit  Of 70% of Max Credit")
        }

        positionLoopIndex += 1
    })
    positionLoopIndex = 1
    this.selectPosition(orderID="buyratios.PutRatioSpread1").foreach ({ curItem =>
        var curItemAsStrategy = curItem.asStrategy.ratioSpread

        if ((curItemAsStrategy.daysToLastTradeableDate).<=(1)) {
            curItemAsStrategy.close("Close @ Expiration"); // Strategy AtExpiration is close
        }

        if ((curItemAsStrategy.selectLargerRatio.daysToExpiration).<=(1)) {
          curItemAsStrategy.close("Close At Expiration")
        }
        if ((curItemAsStrategy.returnOnMargin).<=(-0.15)) {
          curItemAsStrategy.close("Close Position When Loss Exceeds 15%")
        }

        positionLoopIndex += 1
    })
    positionLoopIndex = 1
    this.selectPosition(orderID="buyratios.CallRatioSpread1").foreach ({ curItem =>
        var curItemAsStrategy = curItem.asStrategy.ratioSpread

        if ((curItemAsStrategy.daysToLastTradeableDate).<=(1)) {
            curItemAsStrategy.close("Close @ Expiration"); // Strategy AtExpiration is close
        }

        if ((curItemAsStrategy.selectLargerRatio.daysToExpiration).<=(1)) {
          curItemAsStrategy.close("Close At Expiration")
        }
        if ((curItemAsStrategy.returnOnMargin).<=(-0.15)) {
          curItemAsStrategy.close("Close Position When Loss Exceeds 15%")
        }

        positionLoopIndex += 1
    })
    positionLoopIndex = 1
    this.selectPosition(orderID="sellverticals.PutVertical1").foreach ({ curItem =>
        var curItemAsStrategy = curItem.asStrategy.vertical

        if ((curItemAsStrategy.daysToLastTradeableDate).<=(1)) {
            curItemAsStrategy.close("Close @ Expiration"); // Strategy AtExpiration is close
        }

        if ((curItemAsStrategy.returnOnMaxReward).>=(0.3)) {
          curItemAsStrategy.close("Close Put Vertical On +30% of max profit")
        }

        positionLoopIndex += 1
    })
    positionLoopIndex = 1
    this.selectPosition(orderID="sellverticals.CallVertical1").foreach ({ curItem =>
        var curItemAsStrategy = curItem.asStrategy.vertical

        if ((curItemAsStrategy.daysToLastTradeableDate).<=(1)) {
            curItemAsStrategy.close("Close @ Expiration"); // Strategy AtExpiration is close
        }

        if ((curItemAsStrategy.returnOnMaxReward).>=(0.3)) {
          curItemAsStrategy.close("Close Call Vertical On +30% of max profit")
        }

        positionLoopIndex += 1
    })
    positionLoopIndex = 1
    this.selectPosition(orderID="sellsingle.Put1").foreach ({ curItem =>
        var curItemAsStrategy = curItem.asStrategy.putOption

        if ((curItemAsStrategy.daysToLastTradeableDate).<=(1)) {
            curItemAsStrategy.close("Close @ Expiration"); // Strategy AtExpiration is close
        }

        if ((curItemAsStrategy.gainLoss).>=(20000)) {
          curItemAsStrategy.close("Take profit when up $20K")
        }
        if ((curItemAsStrategy.gainLoss).<=(-40000)) {
          curItemAsStrategy.close("Stop loss when down (-$40K)")
        }

        positionLoopIndex += 1
    })
    positionLoopIndex = 1
    this.selectPosition(orderID="sellstraddleStrangles.Straddle1").foreach ({ curItem =>
        var curItemAsStrategy = curItem.asStrategy.straddle

        if ((curItemAsStrategy.daysToLastTradeableDate).<=(1)) {
            curItemAsStrategy.close("Close @ Expiration"); // Strategy AtExpiration is close
        }

        if ((curItemAsStrategy.returnOnMaxReward).>=(0.3)) {
          curItemAsStrategy.close("Take profit when 30% of max credit is exceeded")
        }
        if ((curItemAsStrategy.returnOnMaxReward).<=(-1.25)) {
          curItemAsStrategy.close("Stop loss when loss exceeds -125% of max credit")
        }

        positionLoopIndex += 1
    })
  }
  def initialize () = {  }
}

