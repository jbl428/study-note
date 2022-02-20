import { Collection, Entity, OneToMany, Property } from '@mikro-orm/core';
import { BaseEntity } from '../base-entity/base.entity';
import { Comment } from '../comment/comment.entity';

@Entity()
export class Post extends BaseEntity {
  @Property()
  name: string;

  @Property()
  content: string;

  @Property()
  memo?: string;

  @OneToMany(() => Comment, (comment) => comment.post)
  comments = new Collection<Comment>(this);
}
