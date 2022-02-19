import { Entity, ManyToOne, Property } from '@mikro-orm/core';
import { BaseEntity } from '../base-entity/base.entity';
import { Post } from '../post/post.entity';

@Entity()
export class Comment extends BaseEntity {
  @Property()
  content: string;

  @Property()
  like: number;

  @Property()
  memo?: string;

  @ManyToOne()
  post: Post;

  // @ManyToOne(() => Post, { mapToPk: true })
  // postId: number;
}
