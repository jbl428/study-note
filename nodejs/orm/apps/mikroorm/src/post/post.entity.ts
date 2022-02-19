import { Entity, Property } from '@mikro-orm/core';
import { BaseEntity } from '../base-entity/base.entity';

@Entity()
export class Post extends BaseEntity {
  @Property()
  name: string;

  @Property()
  content: string;

  @Property()
  memo?: string;
}
