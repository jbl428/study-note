import { ValueTransformer } from 'typeorm';

export class NativeBigintType implements ValueTransformer {
  to(entityValue: bigint) {
    return entityValue;
  }

  from(databaseValue: string): bigint {
    return BigInt(databaseValue);
  }
}
