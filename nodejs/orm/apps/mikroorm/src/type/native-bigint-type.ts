import { BigIntType } from '@mikro-orm/core';

export class NativeBigintType extends BigIntType {
  convertToJSValue(value: any): any {
    if (!value) {
      return value;
    }

    return BigInt(value);
  }
}
