package com.ing.baker.types.modules

import java.time._

import com.ing.baker.types
import com.ing.baker.types.Converters
import org.scalacheck.Gen
import org.scalacheck.Test.Parameters.defaultVerbose
import org.scalatest.prop.Checkers
import org.scalatest.{Matchers, WordSpecLike}


class JavaTimeModuleSpec extends WordSpecLike with Matchers with Checkers {

  val minSuccessfulTests = 100

  // Long.MaxValue is not supported by joda time for local dates, resulting in a integer overflow
  // This shifts the long max value 1 bit to the right (divides by 2)
  // This translates to the date: Fri Apr 24 17:36:27 CEST 146140482
  val maxMillis: Long = Long.MaxValue >> 1

  val numGen: Gen[Long] = Gen.chooseNum[Long](
    0L, maxMillis, 0, maxMillis
  )

  "The JavaTimeModule" should {

    "be able to parse the types of DateTime, LocalDateTime and LocalDate" in {

      Converters.readJavaType[ZonedDateTime] shouldBe types.Date
      Converters.readJavaType[LocalDateTime] shouldBe types.Date
      Converters.readJavaType[LocalDate] shouldBe types.Date
    }

    "be able to read/write all ZonedDateTime instances" in {

      val dateTimeGen: Gen[ZonedDateTime] = numGen.map(millis => ZonedDateTime.ofInstant(Instant.ofEpochMilli(millis), ZoneId.of("Z")))

      check(transitivityProperty[ZonedDateTime](dateTimeGen), defaultVerbose.withMinSuccessfulTests(minSuccessfulTests))
    }

    "be able to read/write all LocalDateTime instances" in {

      val localDateTimeGen: Gen[LocalDateTime] = numGen.map(millis => LocalDateTime.ofInstant(Instant.ofEpochMilli(millis), ZoneId.of("Z")))

      check(transitivityProperty[LocalDateTime](localDateTimeGen), defaultVerbose.withMinSuccessfulTests(minSuccessfulTests))
    }

    "be able to read/write all LocalDate instances" in {

      val localDateGen: Gen[LocalDate] = numGen.map(millis => LocalDate.ofInstant(Instant.ofEpochMilli(millis), ZoneId.of("Z")))

      check(transitivityProperty[LocalDate](localDateGen), defaultVerbose.withMinSuccessfulTests(minSuccessfulTests))
    }
  }
}
