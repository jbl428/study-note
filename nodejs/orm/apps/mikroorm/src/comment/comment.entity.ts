import { Entity, Property } from '@mikro-orm/core';
import { BaseEntity } from '../base.entity';

@Entity()
export class Comment extends BaseEntity {
  @Property()
  content: string;

  @Property()
  like: number;

  @Property()
  memo?: string;
}
