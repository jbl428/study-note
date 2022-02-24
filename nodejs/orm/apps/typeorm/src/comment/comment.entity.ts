import { BaseEntity } from '../base-entity/base.entity';
import { Post } from '../post/post.entity';
import { Column, Entity, Index, JoinColumn, ManyToOne } from 'typeorm';

@Entity()
export class Comment extends BaseEntity {
  @Column()
  content: string;

  @Column()
  like: number;

  @Column({ nullable: true })
  memo?: string;

  @ManyToOne(() => Post, {
    createForeignKeyConstraints: false,
    nullable: false,
  })
  @Index()
  @JoinColumn({ name: 'post_id', referencedColumnName: 'id' })
  post: Post;
}
