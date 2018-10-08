package com.lightning.olympus

import com.lightning.walletapp.ln._
import scala.collection.JavaConverters._
import com.lightning.olympus.JsonHttpUtils._
import org.knowm.xchange.currency.CurrencyPair._
import java.util.concurrent.ConcurrentHashMap
import scala.concurrent.duration.DurationInt
import com.lightning.walletapp.ln.Tools.none
import org.knowm.xchange.ExchangeFactory
import scala.util.Try

import org.knowm.xchange.coinmarketcap.CoinMarketCapExchange
import org.knowm.xchange.currency.CurrencyPair


class AveragePrice(val pair: CurrencyPair, val code: String) {
  val history = new ConcurrentHashMap[String, PriceHistory].asScala
  val exchanges = List.empty[String]
  type PriceTry = Try[BigDecimal]

  def update: Unit = for (exchangeName <- exchanges)
    history.getOrElseUpdate(exchangeName, new PriceHistory) add Try {
      val exchangeInstance = ExchangeFactory.INSTANCE createExchange exchangeName
      exchangeInstance.getMarketDataService.getTicker(pair).getLast: BigDecimal
    }

  def average = Try {
    val recentPrices = history.values.flatMap(_.recentValue)
    recentPrices.map(_.get).sum / recentPrices.size
  } getOrElse BigDecimal(0)

  class PriceHistory {
    var prices = List.empty[PriceTry]
    def recentValue = prices.find(_.isSuccess)
    def add(item: PriceTry) = prices = item :: prices take 5
  }
}

class ExchangeRates {
  val currencies = new AveragePrice(CurrencyPair.BCA_USD, "dollar") {
    override val exchanges = classOf[CoinMarketCapExchange].getName :: Nil
  } :: Nil

  def displayState = for {
    average: AveragePrice <- currencies
    exchange \ history <- average.history
    humanHistory = history.prices mkString "\r\n-- "
  } yield s"${average.pair} $exchange \r\n-- $humanHistory"

  def update(some: Any) = for (average <- currencies) average.update
  retry(obsOnIO map update, pickInc, 4 to 6).repeatWhen(_ delay 30.minutes)
    .doOnNext(_ => Tools log "Exchange rates were updated").subscribe(none)
}
