import { PostStatus } from '../post/post-status';
import { ValueTransformer } from 'typeorm';

export class PostStatusType implements ValueTransformer {
  to(entityValue: PostStatus) {
    return entityValue.code;
  }

  from(databaseValue: string): PostStatus {
    return PostStatus.find(databaseValue);
  }
}
