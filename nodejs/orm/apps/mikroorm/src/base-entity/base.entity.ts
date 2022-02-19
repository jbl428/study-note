import { PrimaryKey, Property } from '@mikro-orm/core';
import { LocalDateTimeType } from '../type/local-date-time-type';
import { LocalDateTime } from '@js-joda/core';
import { NativeBigintType } from '../type/native-bigint-type';

export abstract class BaseEntity {
  @PrimaryKey({ type: NativeBigintType })
  id: bigint;

  @Property({ type: LocalDateTimeType, onCreate: () => LocalDateTime.now() })
  createdAt: LocalDateTime;

  @Property({
    type: LocalDateTimeType,
    onCreate: () => LocalDateTime.now(),
    onUpdate: () => LocalDateTime.now(),
  })
  updatedAt: LocalDateTime;
}
