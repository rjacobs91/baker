package com.ing.baker.types.modules

import java.time._

import com.ing.baker.types._

class JavaTimeModule extends TypeModule {

  override def isApplicable(javaType: java.lang.reflect.Type): Boolean =
      isAssignableToBaseClass(javaType, classOf[ZonedDateTime]) ||
      isAssignableToBaseClass(javaType, classOf[LocalDateTime]) ||
      isAssignableToBaseClass(javaType, classOf[LocalDate])

  override def readType(context: TypeAdapter, javaType: java.lang.reflect.Type): Type = Date

  override def toJava(context: TypeAdapter, value: Value, javaType: java.lang.reflect.Type): Any =
    (value, javaType) match {
      case (NullValue, _) => null
      case (PrimitiveValue(epoch: Long), clazz: Class[_]) if classOf[ZonedDateTime].isAssignableFrom(clazz) =>
        ZonedDateTime.ofInstant(Instant.ofEpochMilli(epoch), ZoneId.of("Z"))
      case (PrimitiveValue(epoch: Long), clazz: Class[_]) if classOf[LocalDateTime].isAssignableFrom(clazz) =>
        LocalDateTime.ofInstant(Instant.ofEpochMilli(epoch), ZoneId.of("Z"))
      case (PrimitiveValue(epoch: Long), clazz: Class[_]) if classOf[LocalDate].isAssignableFrom(clazz) =>
        LocalDate.ofInstant(Instant.ofEpochMilli(epoch), ZoneId.of("Z"))
    }

  override def fromJava(context: TypeAdapter, obj: Any): Value =
    obj match {
      case localDate: LocalDate => PrimitiveValue(localDate.atStartOfDay().toInstant(ZoneOffset.of("Z")).toEpochMilli)
      case localDateTime: LocalDateTime => PrimitiveValue(localDateTime.toInstant(ZoneOffset.of("Z")).toEpochMilli)
      case zonedDateTime: ZonedDateTime => PrimitiveValue(zonedDateTime.toInstant.toEpochMilli)
    }
}