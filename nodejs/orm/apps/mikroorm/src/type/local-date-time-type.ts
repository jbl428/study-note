import { Type, Platform, EntityProperty } from '@mikro-orm/core';
import { convert, LocalDateTime, nativeJs } from '@js-joda/core';

export class LocalDateTimeType extends Type<LocalDateTime, Date> {
  convertToDatabaseValue(
    value: LocalDateTime | Date,
    platform: Platform,
  ): Date {
    if (value instanceof Date) {
      return value;
    }

    return convert(value).toDate();
  }

  convertToJSValue(
    value: LocalDateTime | Date,
    platform: Platform,
  ): LocalDateTime {
    if (value instanceof LocalDateTime) {
      return value;
    }

    return LocalDateTime.from(nativeJs(value));
  }

  getColumnType(prop: EntityProperty, platform: Platform) {
    return `timestamptz`;
  }
}
