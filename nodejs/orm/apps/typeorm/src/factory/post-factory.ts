import { Post } from '../post/post.entity';
import { PostStatus } from '../post/post-status';
import { faker } from '@mikro-orm/seeder';

export class PostFactory {
  static make(entity: Partial<Post> = {}): Post {
    return Object.assign(new Post(), {
      name: faker.name.findName(),
      content: faker.lorem.sentence(5),
      memo: faker.lorem.sentence(3),
      status: PostStatus.PUBLIC,
      ...entity,
    });
  }
}
