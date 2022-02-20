import { Type, Platform, EntityProperty } from '@mikro-orm/core';
import { PostStatus } from '../post/post-status';

export class PostStatusType extends Type<PostStatus, string> {
  convertToDatabaseValue(value: PostStatus | string): string {
    if (typeof value === 'string') {
      return value;
    }

    return value.code;
  }

  convertToJSValue(value: PostStatus | string, platform: Platform): PostStatus {
    if (typeof value === 'string') {
      return PostStatus.find(value);
    }

    return value;
  }

  getColumnType(prop: EntityProperty, platform: Platform) {
    return 'varchar(50)';
  }
}
