import {
  Entity,
  IdentifiedReference,
  ManyToOne,
  Property,
  Reference,
} from '@mikro-orm/core';
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
  post: IdentifiedReference<Post>;

  // @ManyToOne(() => Post, { mapToPk: true })
  // postId: number;

  static createBy(post: Post, content: string, like: number, memo?: string) {
    const entity = new Comment();
    entity.content = content;
    entity.like = like;
    entity.memo = memo;
    entity.post = Reference.create(post);

    return entity;
  }

  static createById(
    postId: bigint,
    content: string,
    like: number,
    memo?: string,
  ) {
    const entity = new Comment();
    entity.content = content;
    entity.like = like;
    entity.memo = memo;
    entity.post = Reference.createFromPK(Post, postId);

    return entity;
  }
}
