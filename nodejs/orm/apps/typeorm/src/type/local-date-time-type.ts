import { ValueTransformer } from 'typeorm';
import { convert, LocalDateTime, nativeJs } from '@js-joda/core';

export class LocalDateTimeType implements ValueTransformer {
  to(entityValue: LocalDateTime): Date {
    return convert(entityValue).toDate();
  }

  from(databaseValue: Date): LocalDateTime {
    return LocalDateTime.from(nativeJs(databaseValue));
  }
}
