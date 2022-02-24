import { LocalDateTimeType } from '../type/local-date-time-type';
import { LocalDateTime } from '@js-joda/core';
import { NativeBigintType } from '../type/native-bigint-type';
import {
  BeforeInsert,
  BeforeUpdate,
  Column,
  Generated,
  PrimaryColumn,
} from 'typeorm';

export abstract class BaseEntity {
  @Generated('increment')
  @PrimaryColumn({ type: 'bigint', transformer: new NativeBigintType() })
  id: number;

  @Column({
    type: 'timestamptz',
    transformer: new LocalDateTimeType(),
    nullable: false,
    update: false,
  })
  createdAt: LocalDateTime;

  @Column({
    type: 'timestamptz',
    transformer: new LocalDateTimeType(),
    nullable: false,
  })
  updatedAt: LocalDateTime;

  @BeforeInsert()
  protected beforeInsert() {
    this.createdAt = LocalDateTime.now();
    this.updatedAt = LocalDateTime.now();
  }

  @BeforeUpdate()
  protected beforeUpdate() {
    this.updatedAt = LocalDateTime.now();
  }
}
